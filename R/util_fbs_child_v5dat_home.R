#' util_fbs_child_v5dat_home: Process raw qualtrics visit 5 home data for the child
#'
#' This function loads the .sav raw data file for the child visit 4 data that was via Qualtrics at the child's home when the procedure was split due to covid and cleans the data. Cleaning the data involves:
#' 1) extracting all variable descriptions,
#' 2) selecting relevant data columns,
#' 3) removing all practice events (e.g., 999)
#' 4) re-ordering and re-name data columns
#' 5) reformatting dates to be appropriate and computer readable: YYYY-MM-DD
#' 6) re-calculate manual variables
#' 7) fix 99's / create prefer not to answer database
#' 8) random fixes to factor level names and variable descriptions
#'
#' The databases MUST follow the naming convention: Child_V5_Home_YYYY-MM-DD.sav
#'
#' @inheritParams util_fbs_parent_v1dat
#' @inheritParams util_fbs_parent_v1dat
#'
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from child visit 5 Qualtrics collected in the home and 2) dict: all variable descriptions
#'
#' @examples
#' #if in same working directory as data. Note - there is no need to add the COVID protocol (i.e., 'Home' or 'Lab') to file_pattern:
#' child_v5_dat_home <- util_fbs_child_v5dat_home('Child_V5')
#'
#' \dontrun{
#' #file_pattern must be a string. The following will not run:
#' child_v5_dat_home <- util_fbs_child_v5dat_home(Child_V5)
#'
#' #file_pattern must have the respondent ('Child') and visit number ('V5'). If just enter 'Child', the script will not run because it will return multiple files for different parent visits. The following will not run:
#' child_v5_dat_home <- util_fbs_child_v5dat_home('Child')
#' }
#'
#'
#' @export
#'
util_fbs_child_v5dat_home <- function(file_pattern, data_path) {

    #### 1. Set up/initial checks #####

    # check that file_pattern exist and is a string

    filepat_arg <- methods::hasArg(file_pattern)

    if (isTRUE(filepat_arg) & !is.character(file_pattern)) {
        stop("file_pattern must be entered as a string: e.g., 'Child_V5'")
    } else if (isFALSE(filepat_arg)) {
        stop("file_pattern must set to the a string matching the name of the raw data file for child visit: e.g., 'Child_V5'")
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
        qv5_child_path <- list.files(path = paste0(data_path, '/Final_Covid/'), pattern = paste0(file_pattern, '_Home'), full.names = TRUE)

        #if no files found, check direct data_path entered
        if (length(qv5_child_path) == 0) {
            qv5_child_path <- list.files(path = data_path, pattern = paste0(file_pattern, '_Home'), full.names = TRUE)
        }
    } else {
        qv5_child_path <- paste0(pattern = paste0(file_pattern, '_Home'), full.names = TRUE)
    }

    # check number of files found
    if (length(qv5_child_path) > 1) {
        stop("More than one file matched after adding '_Home' to the file_pattern . Be sure thefile_pattern specifies both the respondent (Parent/Child) and visit number (V#). If have more than 1 file matching the pattern in the directory, may need to move to enter a more specific file_pattern than is standard.")
    } else if (length(qv5_child_path) == 0) {
        stop("No files found after adding '_Home' to file_pattern. Be sure the data_path and file_pattern are correct and that the file exists.")
    }

    # check that file is of type '.sav'
    if (!grepl('.sav', qv5_child_path, fixed = TRUE)){
        stop("The file found is not an SPSS database (.sav)")
    }

    # check if file exists
    qv5_child_exists <- file.exists(qv5_child_path)

    # load data if it exists
    if (isTRUE(qv5_child_exists)) {
        qv5_child_dat <- as.data.frame(haven::read_spss(qv5_child_path))

    } else {

        #check if in the main database rather than 'Final_CovidAtHome' database
        if (isTRUE(datapath_arg)) {
            qv5_child_path2 <- paste0(data_path, "/Child_V5_Home_",  date_str, ".sav")
        } else {
            qv5_child_path2 <- paste0("Child_V5_Home", date_str,  ".sav")
        }

        # check if file exists
        qv5_child_exists2 <- file.exists(qv5_child_path2)

        # load data if it exists
        if (isTRUE(qv5_child_exists2)) {
            qv5_child_dat <- as.data.frame(haven::read_spss(qv5_child_path2))

        } else {

            if (isTRUE(datapath_arg)) {
                stop("File does not exist. Check date_str and data_path entered")
            } else {
                stop("File does not exist. Check date_str and that the data exists in current working directory")
            }
        }
    }

    #### 3. Clean Data #####

    # 1) extract variable labels/descriptions
    qv5_child_labels <- lapply(qv5_child_dat, function(x) attributes(x)$label)

    # 2) selecting relevant data columns
    qv5_child_clean <- qv5_child_dat[c(1, 18:34)]

    ## update labels
    qv5_child_clean_labels <- qv5_child_labels[c(1, 18:34)]


    # 3) removing all practice events (e.g., 999) Note, ID variable is 'ID'
    qv5_child_clean <- qv5_child_clean[!is.na(qv5_child_clean[["ID"]]) & qv5_child_clean[["ID"]] <
        999, ]

    # 4) re-ordering and re-name data columns general order: 1) child information
    # (ID, start date), 2) ctc

    qv5_child_clean <- qv5_child_clean[c(2, 1, 3:18)]

    qv5_child_clean_labels <- qv5_child_clean_labels[c(2, 1, 3:18)]

    ## re-name variables -- make lowercase
    names(qv5_child_clean) <- tolower(names(qv5_child_clean))

    ## re-name variables -- remove v5 prefix
    for (var in 1:length(names(qv5_child_clean))) {
        var_name <- as.character(names(qv5_child_clean)[var])

        # remove v5 prefix from labels
        if (grepl("v5_", var_name, fixed = TRUE)) {
            names(qv5_child_clean)[var] <- gsub("v5_", "", names(qv5_child_clean)[var])
        }

        if (grepl("v5", var_name, fixed = TRUE)) {
            names(qv5_child_clean)[var] <- gsub("v5", "", names(qv5_child_clean)[var])
        }
    }

    ## manually update variable names
    names(qv5_child_clean)[2] <- "start_date"

    ## update data labels
    names(qv5_child_clean_labels) <- names(qv5_child_clean)

    # 5) reformatting dates to be appropriate and computer readable ####
    # YYYY-MM-DD
    qv5_child_clean[["start_date"]] <- lubridate::ymd(as.Date(qv5_child_clean[["start_date"]]))
    qv5_child_clean_labels[["start_date"]] <- "start_date from qualtrics survey meta-data converted to format yyyy-mm-dd in R"

    # 6) re-level ctc questions so that 99 - skip is changed to -99 ####
    ctc_names <- names(qv5_child_clean)[15:18]

    for (var in 1:length(ctc_names)) {
        var_name <- as.character(ctc_names[var])

        qv5_child_clean[[var_name]] <- sjlabelled::set_labels(qv5_child_clean[[var_name]], labels = c(`Not at all` = 1, `A little` = 2, `Not sure/in the middle` = 3, `Somewhat` = 4, `A lot` = 5, `Skip` = -99))
        set_attr <- attributes(qv5_child_clean[[var_name]])

        qv5_child_clean[[var_name]] <- ifelse(is.na(qv5_child_clean[[var_name]]),  NA, ifelse(qv5_child_clean[[var_name]] == 99, -99, qv5_child_clean[[var_name]]))

        attributes(qv5_child_clean[[var_name]]) <- set_attr
        qv5_child_clean_labels[[var_name]] <- paste0(qv5_child_clean_labels[[var_name]], " - re-leveled in R so skip = -99")
    }

    # 7) random fixes to factor level names and variable descriptions
    for (var in 1:length(names(qv5_child_clean))) {
        var_name <- as.character(names(qv5_child_clean)[var])

        # remove v5 prefix from labels
        if (grepl("Visit 7", qv5_child_clean_labels[[var_name]], fixed = TRUE)) {
            qv5_child_clean_labels[[var_name]] <- gsub("Visit 7 ", "", qv5_child_clean_labels[[var_name]])
        }
    }

    ## id label
    qv5_child_clean_labels[["id"]] <- "participant ID"

    #### 8) Format for export ####

    #put data in order of participant ID for ease
    qv5_child_clean <- qv5_child_clean[order(qv5_child_clean[["id"]]), ]

    # make sure the variable labels match in the dataset
    qv5_child_clean = sjlabelled::set_label(qv5_child_clean, label = matrix(unlist(qv5_child_clean_labels, use.names = FALSE)))

    ## make list of data frame and associated labels
    qv5_child <- list(data = qv5_child_clean, dict = qv5_child_clean_labels)

    ## want an export options??

    return(qv5_child)
}
