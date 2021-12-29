#' util_fbs_child_v3dat_home: Process raw qualtrics visit 3 home data for the child
#'
#' This function loads the .sav raw data file for the child visit 3 data that was collected via Qualtrics at the child's home when the procedure was split due to covid and cleans the data. Cleaning the data involves:
#' 1) extracting all variable descriptions,
#' 2) selecting relevant data columns,
#' 3) removing all practice events (e.g., 999)
#' 4) re-ordering and re-name data columns
#' 5) reformatting dates to be appropriate and computer readable: YYYY-MM-DD
#' 6) re-calculate manual variables
#' 7) re-ordering factor levels to start with value 0
#' 8) random fixes to factor level names and variable descriptions
#'
#' The databases MUST follow the naming convention: Child_V3_Home_YYYY-MM-DD.sav
#'
#' @inheritParams util_fbs_parent_v1dat
#' @inheritParams util_fbs_parent_v1dat
#'
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from child visit 3 Qualtrics and 2) dict: all variable descriptions
#'
#' @examples
#' #if in same working directory as data:
#' ch_v3_dat_home <- util_fbs_child_v3dat_home('2021-10-11')
#'
#' \dontrun{
#' #date must be a string. The following will not run:
#' ch_v3_dat_home <- util_fbs_child_v3dat_home(2021-10-11)
#'
#' #date must match the file name - for file named 'Child_V3_Home_2021_09_16', the
#' following will not run:
#' ch_v3_dat_home <- util_fbs_child_v3dat_home('2021_10_11')
#' }
#'
#'
#' @export
#'
util_fbs_child_v3dat_home <- function(date_str, data_path) {

    #### 1. Set up/initial checks #####

    # check that date_str exist and is a string

    datestr_arg <- methods::hasArg(date_str)

    if (isTRUE(datestr_arg) & !is.character(date_str)) {
        stop("date_str must be entered as a string: e.g., '2021_10_11'")
    } else if (isFALSE(datestr_arg)) {
        stop("date_str must set to the data string from the child visit 3 file name: e.g., '2021_09_16'")
    }

    # check datapath
    datapath_arg <- methods::hasArg(data_path)

    if (isTRUE(datapath_arg)) {
        if (!is.character(date_str)) {
            stop("data_path must be entered as a string: e.g., '.../Participant_Data/untouchedRaw/'")
        }
    }


    #### 2. Load Data #####

    if (isTRUE(datapath_arg)) {
        qv3_child_path <- paste0(data_path, "/Final_CovidAtHome/Child_V3_Home_", date_str, ".sav")
    } else {
        qv3_child_path <- paste0("Final_CovidAtHome/Child_V3_Home", date_str, ".sav")
    }

    # check if file exists
    qv3_child_exists <- file.exists(qv3_child_path)

    # load data if it exists
    if (isTRUE(qv3_child_exists)) {
        qv3_child_dat <- as.data.frame(haven::read_spss(qv3_child_path))

    } else {

        #check if in the main database rather than 'Final_CovidAtHome' database
        if (isTRUE(datapath_arg)) {
            qv3_child_path2 <- paste0(data_path, "/Child_V3_Home_", date_str, ".sav")
        } else {
            qv3_child_path2 <- paste0("Child_V3_Home", date_str, ".sav")
        }

        # check if file exists
        qv3_child_exists2 <- file.exists(qv3_child_path2)

        # load data if it exists
        if (isTRUE(qv3_child_exists2)) {
            qv3_child_dat <- as.data.frame(haven::read_spss(qv3_child_path2))

        } else {
            if (isTRUE(datapath_arg)) {
                stop("File does not exist. Check date_str and data_path entered")
            } else {
                stop("File does not exist. Check date_str and that the data exists in current working directory")
            }
        }
    }

    #### 3. Clean Data #####

    # 1) extract variable labels/descriptions ####
    qv3_child_labels <- lapply(qv3_child_dat, function(x) attributes(x)$label)

    # 2) selecting relevant data columns ####
    qv3_child_clean <- qv3_child_dat[c(1, 18:87)]

    ## update labels
    qv3_child_clean_labels <- qv3_child_labels[c(1, 18:87)]


    # 3) removing all practice events (e.g., 999)  ####

    #Note, ID variable is ID_number

    qv3_child_clean <- qv3_child_clean[!is.na(qv3_child_clean[["ID_number"]]) & qv3_child_clean[["ID_number"]] < 999, ]

    # 4) re-ordering and re-name data columns  ####

    # general order: 1) child information (ID, start date), 2) DD

    qv3_child_clean <- qv3_child_clean[c(2, 1, 3:71)]

    qv3_child_clean_labels <- qv3_child_clean_labels[c(2, 1, 3:71)]

    ## re-name variables -- make lowercase
    names(qv3_child_clean) <- tolower(names(qv3_child_clean))

    ## manually update variables
    names(qv3_child_clean)[names(qv3_child_clean) == "startdate"] <- "start_date"
    names(qv3_child_clean)[names(qv3_child_clean) == "id_number"] <- "id"

    ## update data labels
    names(qv3_child_clean_labels) <- names(qv3_child_clean)

    # 5) reformatting dates to be appropriate and computer readable YYYY-MM-DD  ####
    qv3_child_clean[["start_date"]] <- lubridate::ymd(as.Date(qv3_child_clean[["start_date"]]))
    qv3_child_clean_labels[["start_date"]] <- "start_date from qualtrics survey meta-data converted to format yyyy-mm-dd in R"

    # 6) random fixes to factor level names and variable descriptions ####

    ## id label
    qv3_child_clean_labels[["id"]] <- "participant ID"

    #### 7) Format for export ####

    #put data in order of participant ID for ease
    qv3_child_clean <- qv3_child_clean[order(qv3_child_clean[["id"]]), ]

    # make sure the variable labels match in the dataset
    qv3_child_clean = sjlabelled::set_label(qv3_child_clean, label = matrix(unlist(qv3_child_clean_labels, use.names = FALSE)))

    ## make list of data frame and associated labels
    qv3_child <- list(data = qv3_child_clean, dict = qv3_child_clean_labels)

    ## want an export options??

    return(qv3_child)
}
