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
#' #if in same working directory as data. Note - there is no need to add the COVID protocol (i.e., 'Home' or 'Lab') to file_pattern:
#' child_v3_dat_home <- util_fbs_child_v3dat_home('Child_V3')
#'
#' \dontrun{
#' #file_pattern must be a string. The following will not run:
#' child_v3_dat_home <- util_fbs_child_v3dat_home(Child_V3)
#'
#' #file_pattern must have the respondent ('Child') and visit number ('V3'). If just enter 'Child', the script will not run because it will return multiple files for different parent visits. The following will not run:
#' child_v3_dat_home <- util_fbs_child_v3dat_home('Child')
#' }
#'
#'
#' @export
#'
util_fbs_child_v3dat_home <- function(file_pattern, data_path) {

    #### 1. Set up/initial checks #####

    # check that file_pattern exist and is a string

    filepat_arg <- methods::hasArg(file_pattern)

    if (isTRUE(filepat_arg) & !is.character(file_pattern)) {
        stop("file_pattern must be entered as a string: e.g., 'Child_V3'")
    } else if (isFALSE(filepat_arg)) {
        stop("file_pattern must set to the a string matching the name of the raw data file for child visit: e.g., 'Child_V3'")
    }

    # check datapath
    datapath_arg <- methods::hasArg(data_path)

    if (isTRUE(datapath_arg)) {
        if (!is.character(data_path)) {
            stop("data_path must be entered as a string: e.g., '.../Participant_Data/untouchedRaw/'")
        }
    }


    #### 2. Load Data #####

    if (isTRUE(datapath_arg)) {
        #check pattern of directories specified in Data manual
        qv3_child_path <- list.files(path = paste0(data_path, '/Final_Covid/'), pattern = paste0(file_pattern, '_Home'), full.names = TRUE)

        #if no files found, check direct data_path entered
        if (length(qv3_child_path) == 0) {
            qv3_child_path <- list.files(path = data_path, pattern = paste0(file_pattern, '_Home'), full.names = TRUE)
        }
    } else {
        qv3_child_path <- paste0(pattern = paste0(file_pattern, '_Home'), full.names = TRUE)
    }

    # check number of files found
    if (length(qv3_child_path) > 1) {
        stop("More than one file matched after adding '_Home' to the file_pattern . Be sure thefile_pattern specifies both the respondent (Parent/Child) and visit number (V#). If have more than 1 file matching the pattern in the directory, may need to move to enter a more specific file_pattern than is standard.")
    } else if (length(qv3_child_path) == 0) {
        stop("No files found after adding '_Home' to file_pattern. Be sure the data_path and file_pattern are correct and that the file exists.")
    }

    # check that file is of type '.sav'
    if (!grepl('.sav', qv3_child_path, fixed = TRUE)){
        stop("The file found is not an SPSS database (.sav)")
    }

    # check if file exists
    qv3_child_exists <- file.exists(qv3_child_path)

    # load data if it exists
    if (isTRUE(qv3_child_exists)) {
        qv3_child_dat <- as.data.frame(haven::read_spss(qv3_child_path))

    } else {

        if (isTRUE(datapath_arg)) {
            stop("File does not exist. Check file_pattern and data_path entered")
        } else {
            stop("File does not exist. Check file_pattern and that the data exists in current working directory")
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
