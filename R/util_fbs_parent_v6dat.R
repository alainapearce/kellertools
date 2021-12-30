#' util_fbs_parent_v6dat: Process raw Qualtrics visit 6 data for the parent
#'
#' This function loads the .sav raw data file for the parent visit 6 data that was
#' collected via Qualtrics and cleans the data. Cleaning the data involves:
#' 1) extracting all variable descriptions,
#' 2) selecting relevant data columns,
#' 3) removing all practice events (e.g., 999)
#' 4) re-ordering and re-name data columns
#' 5) general fixes to variable labels (remove ' - 1')
#' 6) fix variables with 99 issue for 'prefer not to answer'
#' 7) reformatting dates to be appropriate and computer readable: YYYY-MM-DD
#'
#' @inheritParams util_fbs_parent_v1dat
#' @inheritParams util_fbs_parent_v1dat
#'
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from parent visit 6 Qualtrics;
#'  2) dict: all variable descriptions; 3) pna_data: data.frame marking participants who 'prefered not to answer' (pna) specific questions; and 4) pna_dict: all variable descriptions for pna_data
#'
#' @examples
#' #if in same working directory as data:
#' p_v6_dat <- util_fbs_parent_v6dat('Parent_V6')
#'
#' \dontrun{
#' #file_pattern must be a string. The following will not run:
#' p_v6_dat <- util_fbs_parent_v6dat(Parent_V6)
#'
#' #file_pattern must have the respondent ('Parent') and visit number ('V1'). If just enter 'Parent', the script will not run because it will return multiple files for different parent visits. The following will not run:
#' p_v6_dat <- util_fbs_parent_v6dat('Parent')
#' }
#'
#'
#' @export
#'
util_fbs_parent_v6dat <- function(file_pattern, data_path) {

    #### 1. Set up/initial checks #####

    # check that file_pattern exist and is a string

    filepat_arg <- methods::hasArg(file_pattern)

    if (isTRUE(filepat_arg) & !is.character(file_pattern)) {
        stop("file_pattern must be entered as a string: e.g., 'Parent_V6'")
    } else if (isFALSE(filepat_arg)) {
        stop("file_pattern must set to the a string matching the name of the raw data file for parent visit: e.g., 'Parent_V6'")
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
        qv6_parent_path <- list.files(path = data_path, pattern = file_pattern, full.names = TRUE)
    } else {
        qv6_parent_path <- paste0(pattern = file_pattern, full.names = TRUE)
    }

    # check number of files found
    if (length(qv6_parent_path) > 1) {
        stop("More than one file matched the file_pattern. Be sure thefile_pattern specifies both the respondent (Parent/Child) and visit number (V#). If have more than 1 file matching the pattern in the directory, may need to move to enter a more specific file_pattern than is standard.")
    } else if (length(qv6_parent_path) == 0) {
        stop('No files found. Be sure the data_path and file_pattern are correct and that the file exists')
    }

    # check that file is of type '.sav'
    if (!grepl('.sav', qv6_parent_path, fixed = TRUE)){
        stop("The file found is not an SPSS database (.sav)")
    }

    # check if file exists
    qv6_parent_exists <- file.exists(qv6_parent_path)

    # load data if it exists
    if (isTRUE(qv6_parent_exists)) {
        qv6_parent_dat <- as.data.frame(haven::read_spss(qv6_parent_path))

    } else {
        if (isTRUE(datapath_arg)) {
            stop("File does not exist. Check date_str and data_path entered")
        } else {
            stop("File does not exist. Check date_str and that the data exists in current working directory")
        }
    }

    #### 3. Clean Data #####

    # 3a) extract variable labels/descriptions
    qv6_parent_labels <- lapply(qv6_parent_dat, function(x) attributes(x)$label)

    # 3b) selecting relevant data columns
    qv6_parent_clean <- qv6_parent_dat[c(1, 11:27)]

    ## update labels
    qv6_parent_clean_labels <- qv6_parent_labels[c(1, 11:27)]

    # 3c) removing all practice events (e.g., 999)
    qv6_parent_clean <- qv6_parent_clean[!is.na(qv6_parent_clean[["ID"]]) & qv6_parent_clean[["ID"]] < 999, ]

    # 4) re-ordering and re-name data columns general order #### 1) demographics - AUDIT, 2) fasting, 3) updates

    qv6_parent_clean <- qv6_parent_clean[c(2, 1, 3:18)]

    qv6_parent_clean_labels <- qv6_parent_clean_labels[c(2, 1, 3:18)]

    ## re-name variables

    # make lower case
    names(qv6_parent_clean) <- tolower(names(qv6_parent_clean))

    # start date rename
    names(qv6_parent_clean)[2] <- "start_date"

    # remove 'v6'
    for (var in 1:length(names(qv6_parent_clean))) {
        var_name <- as.character(names(qv6_parent_clean)[var])

        # remove trailing 'v6' from names
        if (grepl("v6", var_name, fixed = TRUE)) {
            names(qv6_parent_clean)[var] <- gsub("v6", "", var_name)
        }
    }

    ## update data labels
    names(qv6_parent_clean_labels) <- names(qv6_parent_clean)

    ## 5) general fixes to labels (add visit, remove '- 1') ####

    ## remove formatting errors
    for (var in 1:length(names(qv6_parent_clean))) {
        var_name <- as.character(names(qv6_parent_clean)[var])

        # remove ' \' ' from apostrophes (e.g., child\'s)
        if (grepl("'s", qv6_parent_clean_labels[[var_name]], fixed = TRUE)) {
            qv6_parent_clean_labels[[var_name]] <- gsub("\\'s", "", qv6_parent_clean_labels[[var_name]])
        }

        # remove trailing 'v6 ' from labels
        if (grepl("V6", qv6_parent_clean_labels[[var_name]], fixed = TRUE)) {
            qv6_parent_clean_labels[[var_name]] <- gsub("\\V6 - ", "", qv6_parent_clean_labels[[var_name]])
            qv6_parent_clean_labels[[var_name]] <- gsub("\\V6 ", "", qv6_parent_clean_labels[[var_name]])
        }
    }

    #### 6) fix 99's and other poor categories ####

    ## check for labels/99 option: 1) if 99's exist, make a 'prefere not to answer' (pna) variable to go in pna
    ## database, 2) replace 99's with NA and make variable numeric

    ## make pna database
    qv6_parent_pna <- data.frame(id = qv6_parent_clean[["id"]])
    qv6_parent_pna_labels <- lapply(qv6_parent_pna, function(x) attributes(x)$label)
    qv6_parent_pna_labels[["id"]] <- qv6_parent_clean_labels[["id"]]

    pna_label <- "Note: prefer not to answer (pna) marked NA - see pna database for which were pna rather than missing NA"

    ## 6a) categorical variables with 99's data ####
    level99_issue_catvars <- names(qv6_parent_clean)[c(3:11)]

    for (v in 1:length(level99_issue_catvars)) {
        # get variable name
        pvar <- level99_issue_catvars[v]

        # if has '99' value, create new pna variable marking pna == 1
        if (is.element(99, qv6_parent_clean[[pvar]])) {
            pna_dat <- ifelse(is.na(qv6_parent_clean[[pvar]]), 0, ifelse(qv6_parent_clean[[pvar]] == 99, 1, 0))

            new_pna <- length(names(qv6_parent_pna)) + 1
            qv6_parent_pna[[new_pna]] <- pna_dat

            names(qv6_parent_pna)[new_pna] <- paste0(pvar, "_pna")

            # add label to pna database
            qv6_parent_pna_labels[[paste0(pvar, "_pna")]] <- paste0("prefer not to answer marked for variable ", pvar,
                ": ", qv6_parent_clean_labels[[pvar]])

            # update true data label (only want to pna label if needed)
            qv6_parent_clean_labels[[pvar]] <- paste0(qv6_parent_clean_labels[[pvar]], " -- ", pna_label)

        }

        # drop 99 level label labels only update if had 99 - done in if statement above
        qv6_parent_clean[[pvar]] <- sjlabelled::remove_labels(qv6_parent_clean[[pvar]], labels = "Don't want to answer")

        # extract variable attributes
        pvar_attr <- attributes(qv6_parent_clean[[pvar]])

        # replace 99 values
        qv6_parent_clean[[pvar]] <- ifelse(is.na(qv6_parent_clean[[pvar]]) | qv6_parent_clean[[pvar]] == 99, NA, qv6_parent_clean[[pvar]])

        # replace attributes
        attributes(qv6_parent_clean[[pvar]]) <- pvar_attr
    }

    #### 7) reformatting dates/times #### 7a) dates (start, dobs) ####
    qv6_parent_clean[["start_date"]] <- lubridate::ymd(as.Date(qv6_parent_clean[["start_date"]]))
    qv6_parent_clean_labels[["start_date"]] <- "start_date from qualtrics survey meta-data converted to format yyyy-mm-dd in R"

    #### 8) Format for export ####

    #add attributes to pna data
    if (ncol(qv6_parent_pna) > 2){
        qv6_parent_pna[2:ncol(qv6_parent_pna)] <- as.data.frame(lapply(qv6_parent_pna[2:ncol(qv6_parent_pna)], function(x) sjlabelled::add_labels(x, labels = c(`Did not skip due to prefer not to answer` = 0, `Prefer not to answer` = 1))))

        for (v in 2:ncol(qv6_parent_pna)){
            class(qv6_parent_pna[[v]]) <- c("haven_labelled", "vctrs_vctr", "double")
        }
    }

    ##put data in order of participant ID for ease
    qv6_parent_clean <- qv6_parent_clean[order(qv6_parent_clean[["id"]]), ]
    qv6_parent_pna <- qv6_parent_pna[order(qv6_parent_pna[["id"]]), ]

    # make sure the variable labels match in the dataset
    qv6_parent_clean = sjlabelled::set_label(qv6_parent_clean, label = matrix(unlist(qv6_parent_clean_labels, use.names = FALSE)))
    qv6_parent_pna = sjlabelled::set_label(qv6_parent_pna, label = matrix(unlist(qv6_parent_pna_labels, use.names = FALSE)))

    # make list of data frame and associated labels
    qv6_parent <- list(data = qv6_parent_clean, dict = qv6_parent_clean_labels, pna_data = qv6_parent_pna, pna_dict = qv6_parent_pna_labels)

    ## want an export options??

    return(qv6_parent)
}
