#' util_fbs_parent_v5dat: Process raw Qualtrics visit 5 data for the parent
#'
#' This function loads the .sav raw data file for the parent visit 5 data that was
#' collected via Qualtrics and cleans the data. Cleaning the data involves:
#' 1) extracting all variable descriptions,
#' 2) selecting relevant data columns,
#' 3) removing all practice events (e.g., 999)
#' 4) re-ordering and re-name data columns
#' 5) general fixes to variable labels (remove ' - 1')
#' 6) fix variables with 99 issue for 'prefer not to answer'
#' 8) fix factor levels to match questionnaire scoring
#'
#' The databases MUST follow the naming convention: Parent_V5_YYYY-MM-DD.sav
#'
#' @inheritParams util_fbs_parent_v1dat
#' @inheritParams util_fbs_parent_v1dat
#'
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from parent visit 5 Qualtrics;
#'  2) dict: all variable descriptions; 3) pna_data: data.frame marking participants who 'prefered not to answer' (pna) specific questions; and 4) pna_dict: all variable descriptions for pna_data
#'
#' @examples
#' #if in same working directory as data:
#' p_v5_dat <- util_fbs_parent_v5dat('Parent_V5')
#'
#' \dontrun{
#' #file_pattern must be a string. The following will not run:
#' p_v5_dat <- util_fbs_parent_v5dat(Parent_V5)
#'
#' #file_pattern must have the respondent ('Parent') and visit number ('V1'). If just enter 'Parent', the script will not run because it will return multiple files for different parent visits. The following will not run:
#' p_v5_dat <- util_fbs_parent_v5dat('Parent')
#' }
#'
#'
#' @export
#'
util_fbs_parent_v5dat <- function(file_pattern, data_path) {

    #### 1. Set up/initial checks #####

    # check that file_pattern exist and is a string

    filepat_arg <- methods::hasArg(file_pattern)

    if (isTRUE(filepat_arg) & !is.character(file_pattern)) {
        stop("file_pattern must be entered as a string: e.g., 'Parent_V5'")
    } else if (isFALSE(filepat_arg)) {
        stop("file_pattern must set to the a string matching the name of the raw data file for parent visit: e.g., 'Parent_V5'")
    }

    # check datapath
    datapath_arg <- methods::hasArg(data_path)

    if (isTRUE(datapath_arg)) {
        if (!is.character(data_path)) {
            stop("data_path must be entered as a string: e.g., '.../Participant_Data/untouchedRaw/'")
        }
    }

    #### 2. Load Data #####
    # Verified visit dates
    if (isTRUE(datapath_arg)) {

        #check pattern of directories specified in Data manual
        visit_dates_path <- list.files(path = data_path, pattern = 'verified_visit_dates', full.names = TRUE)

    } else {
        visit_dates_path <- list.files(pattern = 'verified_visit_dates', full.names = TRUE)
    }

    # check number of files found
    if (length(visit_dates_path) > 1) {
        stop("More than one file matched 'verified_visit_dates'. If have more than 1 file matching the pattern in the directory, may need to move one.")
    } else if (length(visit_dates_path) == 0) {
        stop("No files found for file_pattern 'verified_visit_dates'. Be sure the data_path is correct and that the file exists.")
    }

    # check if file exists
    visit_dates_exists <- file.exists(visit_dates_path)

    # load data if it exists
    if (isTRUE(visit_dates_exists)) {
        visit_dates <- read.csv(visit_dates_path, header = TRUE)

    } else {

        if (isTRUE(datapath_arg)) {
            stop("File does not exist. Check data_path entered")
        } else {
            stop("File does not exist. Check that the data exists in current working directory")
        }
    }

    # Qualtrics data
    if (isTRUE(datapath_arg)) {
        qv5_parent_path <- list.files(path = data_path, pattern = file_pattern, full.names = TRUE)
    } else {
        qv5_parent_path <- paste0(pattern = file_pattern, full.names = TRUE)
    }

    # check number of files found
    if (length(qv5_parent_path) > 1) {
        stop("More than one file matched the file_pattern. Be sure thefile_pattern specifies both the respondent (Parent/Child) and visit number (V#). If have more than 1 file matching the pattern in the directory, may need to move to enter a more specific file_pattern than is standard.")
    } else if (length(qv5_parent_path) == 0) {
        stop('No files found. Be sure the data_path and file_pattern are correct and that the file exists')
    }

    # check that file is of type '.sav'
    if (!grepl('.sav', qv5_parent_path, fixed = TRUE)){
        stop("The file found is not an SPSS database (.sav)")
    }

    # check if file exists
    qv5_parent_exists <- file.exists(qv5_parent_path)

    # load data if it exists
    if (isTRUE(qv5_parent_exists)) {
        qv5_parent_dat <- as.data.frame(haven::read_spss(qv5_parent_path))

    } else {
        if (isTRUE(datapath_arg)) {
            stop("File does not exist. Check date_str and data_path entered")
        } else {
            stop("File does not exist. Check date_str and that the data exists in current working directory")
        }
    }

    #### 3. Clean Data #####

    # 3a) extract variable labels/descriptions
    qv5_parent_labels <- lapply(qv5_parent_dat, function(x) attributes(x)$label)

    # 3b) selecting relevant data columns
    qv5_parent_clean <- qv5_parent_dat[c(1, 11:35)]

    ## update labels
    qv5_parent_clean_labels <- qv5_parent_labels[c(1, 11:35)]

    # 3c) removing all practice events (e.g., 999)
    qv5_parent_clean <- qv5_parent_clean[!is.na(qv5_parent_clean[["ID"]]) & qv5_parent_clean[["ID"]] < 999, ]

    # 4) re-ordering and re-name data columns general order #### 1) demographics - AUDIT, 2) fasting, 3) updates

    qv5_parent_clean <- qv5_parent_clean[c(2, 1, 17:26, 3:16)]

    qv5_parent_clean_labels <- qv5_parent_clean_labels[c(2, 1, 17:26, 3:16)]

    ## re-name variables

    # make lower case
    names(qv5_parent_clean) <- tolower(names(qv5_parent_clean))

    # start date rename
    names(qv5_parent_clean)[2] <- "start_date"

    # remove 'v5'
    for (var in 1:length(names(qv5_parent_clean))) {
        var_name <- as.character(names(qv5_parent_clean)[var])

        # remove trailing 'v5' from names
        if (grepl("v5", var_name, fixed = TRUE)) {
            names(qv5_parent_clean)[var] <- gsub("v5", "", var_name)
        }
    }

    ## update data labels
    names(qv5_parent_clean_labels) <- names(qv5_parent_clean)

    ## 5) general fixes to labels (add visit, remove '- 1') ####

    ## remove formatting errors
    for (var in 1:length(names(qv5_parent_clean))) {
        var_name <- as.character(names(qv5_parent_clean)[var])

        # remove ' \' ' from apostrophes (e.g., child\'s)
        if (grepl("'s", qv5_parent_clean_labels[[var_name]], fixed = TRUE)) {
            qv5_parent_clean_labels[[var_name]] <- gsub("\\'s", "", qv5_parent_clean_labels[[var_name]])
        }

        # remove trailing 'v5 ' from labels
        if (grepl("V5", qv5_parent_clean_labels[[var_name]], fixed = TRUE)) {
            qv5_parent_clean_labels[[var_name]] <- gsub("\\V5 - ", "", qv5_parent_clean_labels[[var_name]])
            qv5_parent_clean_labels[[var_name]] <- gsub("\\V5 ", "", qv5_parent_clean_labels[[var_name]])
        }
    }

    #### 6) fix 99's and other poor categories ####

    ## check for labels/99 option: 1) if 99's exist, make a 'prefere not to answer' (pna) variable to go in pna database, 2) replace 99's with NA and make variable numeric

    ## make pna database
    qv5_parent_pna <- data.frame(id = qv5_parent_clean[["id"]])
    qv5_parent_pna_labels <- lapply(qv5_parent_pna, function(x) attributes(x)$label)
    qv5_parent_pna_labels[["id"]] <- qv5_parent_clean_labels[["id"]]

    pna_label <- "Note: prefer not to answer (pna) marked NA - see pna database for which were pna rather than missing NA"

    ## 6a) categorical variables with 99's data ####
    level99_issue_catvars <- names(qv5_parent_clean)[c(3:20)]

    for (v in 1:length(level99_issue_catvars)) {
        # get variable name
        pvar <- level99_issue_catvars[v]

        # if has '99' value, create new pna variable marking pna == 1
        if (is.element(99, qv5_parent_clean[[pvar]])) {
            pna_dat <- ifelse(is.na(qv5_parent_clean[[pvar]]), 0, ifelse(qv5_parent_clean[[pvar]] == 99, 1, 0))

            new_pna <- length(names(qv5_parent_pna)) + 1
            qv5_parent_pna[[new_pna]] <- pna_dat

            names(qv5_parent_pna)[new_pna] <- paste0(pvar, "_pna")

            # add label to pna database
            qv5_parent_pna_labels[[paste0(pvar, "_pna")]] <- paste0("prefer not to answer marked for variable ", pvar,  ": ", qv5_parent_clean_labels[[pvar]])

            # update true data label (only want to pna label if needed)
            qv5_parent_clean_labels[[pvar]] <- paste0(qv5_parent_clean_labels[[pvar]], " -- ", pna_label)

        }

        # drop 99 level label labels only update if had 99 - done in if statement above
        qv5_parent_clean[[pvar]] <- sjlabelled::remove_labels(qv5_parent_clean[[pvar]], labels = "Don't want to answer")

        # extract variable attributes
        pvar_attr <- attributes(qv5_parent_clean[[pvar]])

        # replace 99 values
        qv5_parent_clean[[pvar]] <- ifelse(is.na(qv5_parent_clean[[pvar]]) | qv5_parent_clean[[pvar]] == 99, NA, qv5_parent_clean[[pvar]])

        # replace attributes
        attributes(qv5_parent_clean[[pvar]]) <- pvar_attr
    }

    #### 7) reformatting dates/times ####

    ##7a) dates (start, dobs)
    #format start date
    qv5_parent_clean[["start_date"]] <- lubridate::ymd(as.Date(qv5_parent_clean[["start_date"]]))

    # dates are fomrated as dd-mstr-yy
    visit_dates[['RO1_V5_Date']] <- lubridate::ymd(as.Date(visit_dates[['RO1_V5_Date']], format = "%d-%b-%y"))

    # add validated dates
    names(visit_dates)[1] <- 'id'
    qv5_parent_clean <- merge(qv5_parent_clean, visit_dates[c('id', 'RO1_V5_Date')], by = 'id', all.x = TRUE, all.y = FALSE)

    #update start_date
    qv5_parent_clean[["start_date"]] <- ifelse(!is.na(qv5_parent_clean[['RO1_V5_Date']]), as.character(qv5_parent_clean[['RO1_V5_Date']]), as.character(qv5_parent_clean[["start_date"]]))

    #remove RO1_V date column
    qv5_parent_clean <- qv5_parent_clean[, names(qv5_parent_clean) != "RO1_V5_Date"]

    # add label
    qv5_parent_clean_labels[["start_date"]] <- "date from participant contacts databases ('verified_visit_dates*.csv) converted to format yyyy-mm-dd in R. If no date in database, uses start_date metadata from qualtrics"

    #### 8) Format for export ####

    ## 8a) add attributes to pna data
    qv5_parent_pna[2:ncol(qv5_parent_pna)] <- as.data.frame(lapply(qv5_parent_pna[2:ncol(qv5_parent_pna)], function(x) sjlabelled::add_labels(x, labels = c(`Did not skip due to prefer not to answer` = 0, `Prefer not to answer` = 1))))

    for (v in 2:ncol(qv5_parent_pna)){
        class(qv5_parent_pna[[v]]) <- c("haven_labelled", "vctrs_vctr", "double")
    }

    ## 8b) put data in order of participant ID for ease
    qv5_parent_clean <- qv5_parent_clean[order(qv5_parent_clean[["id"]]), ]
    qv5_parent_pna <- qv5_parent_pna[order(qv5_parent_pna[["id"]]), ]

    ## 8c) make sure the variable labels match in the dataset
    qv5_parent_clean = sjlabelled::set_label(qv5_parent_clean, label = matrix(unlist(qv5_parent_clean_labels, use.names = FALSE)))

    qv5_parent_pna = sjlabelled::set_label(qv5_parent_pna, label = matrix(unlist(qv5_parent_pna_labels, use.names = FALSE)))

    # make list of data frame and associated labels
    qv5_parent <- list(data = qv5_parent_clean, dict = qv5_parent_clean_labels, pna_data = qv5_parent_pna, pna_dict = qv5_parent_pna_labels)

    return(qv5_parent)
}
