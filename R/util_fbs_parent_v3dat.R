#' util_fbs_parent_v3dat: Process raw Qualtrics visit 3 data for the parent
#'
#' This function loads the .sav raw data file for the parent visit 3 data that was
#' collected via Qualtrics and cleans the data. Cleaning the data involves:
#' 1) extracting all variable descriptions,
#' 2) selecting relevant data columns,
#' 3) removing all practice events (e.g., 999)
#' 4) re-ordering and re-name data columns
#' 5) general fixes to variable labels (remove ' - 1')
#' 6) fix variables with 99 issue for 'prefer not to answer'
#' 7) reformatting dates to be appropriate and computer readable: YYYY-MM-DD
#' 8) fix factor levels to match questionnaire scoring
#'
#' The databases MUST follow the naming convention: Parent_V3_YYYY-MM-DD.sav
#'
#' @inheritParams util_fbs_parent_v1dat
#' @inheritParams util_fbs_parent_v1dat
#'
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from parent visit 3 Qualtrics;
#'  2) dict: all variable descriptions; 3) pna_data: data.frame marking participants who 'prefered not to answer' (pna) specific questions; and 4) pna_dict: all variable descriptions for pna_data
#'
#' @examples
#' #if in same working directory as data:
#' p_v3_dat <- util_fbs_parent_v3dat('Parent_V3')
#'
#' \dontrun{
#' #file_pattern must be a string. The following will not run:
#' p_v3_dat <- util_fbs_parent_v3dat(Parent_V3)
#'
#' #file_pattern must have the respondent ('Parent') and visit number ('V1'). If just enter 'Parent', the script will not run because it will return multiple files for different parent visits. The following will not run:
#' p_v3_dat <- util_fbs_parent_v3dat('Parent')
#' }
#'
#'
#' @export
#'
util_fbs_parent_v3dat <- function(file_pattern, data_path) {

    #### 1. Set up/initial checks #####

    # check that file_pattern exist and is a string

    filepat_arg <- methods::hasArg(file_pattern)

    if (isTRUE(filepat_arg) & !is.character(file_pattern)) {
        stop("file_pattern must be entered as a string: e.g., 'Parent_V3'")
    } else if (isFALSE(filepat_arg)) {
        stop("file_pattern must set to the a string matching the name of the raw data file for parent visit: e.g., 'Parent_V3'")
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
        qv3_parent_path <- list.files(path = data_path, pattern = file_pattern, full.names = TRUE)
    } else {
        qv3_parent_path <- paste0(pattern = file_pattern, full.names = TRUE)
    }

    # check number of files found
    if (length(qv3_parent_path) > 1) {
        stop("More than one file matched the file_pattern. Be sure thefile_pattern specifies both the respondent (Parent/Child) and visit number (V#). If have more than 1 file matching the pattern in the directory, may need to move to enter a more specific file_pattern than is standard.")
    } else if (length(qv3_parent_path) == 0) {
        stop('No files found. Be sure the data_path and file_pattern are correct and that the file exists')
    }

    # check that file is of type '.sav'
    if (!grepl('.sav', qv3_parent_path, fixed = TRUE)){
        stop("The file found is not an SPSS database (.sav)")
    }

    # check if file exists
    qv3_parent_exists <- file.exists(qv3_parent_path)

    # load data if it exists
    if (isTRUE(qv3_parent_exists)) {
        qv3_parent_dat <- as.data.frame(haven::read_spss(qv3_parent_path))

    } else {
        if (isTRUE(datapath_arg)) {
            stop("File does not exist. Check date_str and data_path entered")
        } else {
            stop("File does not exist. Check date_str and that the data exists in current working directory")
        }
    }

    #### 3. Clean Data #####

    # 3a) extract variable labels/descriptions
    qv3_parent_labels <- lapply(qv3_parent_dat, function(x) attributes(x)$label)

    # 3b) selecting relevant data columns
    qv3_parent_clean <- qv3_parent_dat[c(1, 18, 20:205)]

    ## update labels
    qv3_parent_clean_labels <- qv3_parent_labels[c(1, 18, 20:205)]

    # 3c) removing all practice events (e.g., 999)
    qv3_parent_clean <- qv3_parent_clean[!is.na(qv3_parent_clean[["ID"]]) & qv3_parent_clean[["ID"]] <
        999, ]

    # 4) re-ordering and re-name data columns general order #### 1) child
    # information/demo (sex, dob, h/w, puberty), 2) fasting, 3) sleep (CHSQ), 4)
    # LBC, PWLB, 5) SPSRQP, BIS/BAS, 6) updates

    qv3_parent_clean <- qv3_parent_clean[c(2, 1, 3, 17:36, 109:188, 37:85, 107,
        86:104, 108, 105:106, 4:16)]

    qv3_parent_clean_labels <- qv3_parent_clean_labels[c(2, 1, 3, 17:36, 109:188,
        37:85, 107, 86:104, 108, 105:106, 4:16)]

    ## re-name variables

    # make lower case
    names(qv3_parent_clean) <- tolower(names(qv3_parent_clean))

    # start date rename
    names(qv3_parent_clean)[2] <- "start_date"

    # remove 'v3'
    for (var in 1:length(names(qv3_parent_clean))) {
        var_name <- as.character(names(qv3_parent_clean)[var])

        # remove trailing 'v3' from names
        if (grepl("v3", var_name, fixed = TRUE)) {
            names(qv3_parent_clean)[var] <- gsub("v3", "", var_name)
        }

        # remove p from spsrq
        if (grepl("spsrqp", var_name, fixed = TRUE)) {
            names(qv3_parent_clean)[var] <- gsub("spsrqp", "spsrq", var_name)
        }

        # fix 'PLWB' for parent weigh loss behavior questionnaire
        if (grepl("plwb", var_name, fixed = TRUE)) {
            names(qv3_parent_clean)[var] <- gsub("plwb", "pwlb", var_name)
        }


    }

    ## fix LBC names
    names(qv3_parent_clean)[c(4:23)] <- c("lbc6", "lbc7", "lbc8", "lbc9", "lbc10",
        "lbc11", "lbc12", "lbc13", "lbc14", "lbc15", "lbc16", "lbc17", "lbc18",
        "lbc19", "lbc20", "lbc21", "lbc22", "lbc23", "lbc24", "lbc25")

    ## update data labels
    names(qv3_parent_clean_labels) <- names(qv3_parent_clean)

    ## 5) general fixes to labels (add visit, remove '- 1') ####

    ## remove formatting errors
    for (var in 1:length(names(qv3_parent_clean))) {
        var_name <- as.character(names(qv3_parent_clean)[var])

        # remove ' \' ' from apostrophes (e.g., child\'s)
        if (grepl("'s", qv3_parent_clean_labels[[var_name]], fixed = TRUE)) {
            qv3_parent_clean_labels[[var_name]] <- gsub("\\'s", "", qv3_parent_clean_labels[[var_name]])
        }

        # remove trailing 'v3 ' from labels
        if (grepl("V3", qv3_parent_clean_labels[[var_name]], fixed = TRUE)) {
            qv3_parent_clean_labels[[var_name]] <- gsub("\\V3 - ", "", qv3_parent_clean_labels[[var_name]])
            qv3_parent_clean_labels[[var_name]] <- gsub("\\V3 ", "", qv3_parent_clean_labels[[var_name]])
        }

        # adjust labels for SPSR-Q
        if (grepl("SPSRQP", qv3_parent_clean_labels[[var_name]], fixed = TRUE)) {
            qv3_parent_clean_labels[[var_name]] <- gsub("\\SPSRQP", "SPSRQ Parent",
                qv3_parent_clean_labels[[var_name]])
        }

        # adjust labels for PWLB where miss-spelled
        if (grepl("PLWB", qv3_parent_clean_labels[[var_name]], fixed = TRUE)) {
            qv3_parent_clean_labels[[var_name]] <- gsub("\\PLWB", "PWLB", qv3_parent_clean_labels[[var_name]])
        }
    }

    ## fix LBC
    LBC_vars <- names(qv3_parent_clean)[c(4:23)]

    for (v in 1:length(LBC_vars)) {
        # get variable name
        pvar <- LBC_vars[v]

        qnum <- gsub("lbc", "", pvar)

        # Fix Label
        qv3_parent_clean_labels[[pvar]] <- gsub(".*[.] ", "", qv3_parent_clean_labels[[pvar]])

        qv3_parent_clean_labels[[pvar]] <- paste0("LBC - ", qnum, ". ", qv3_parent_clean_labels[[pvar]])
    }

    #### 6) fix 99's and other poor categories ####

    ## check for labels/99 option: 1) if 99's exist, make a 'prefere not to answer'
    ## (pna) variable to go in pna database, 2) replace 99's with NA and make
    ## variable numeric

    ## make pna database
    qv3_parent_pna <- data.frame(id = qv3_parent_clean[["id"]])
    qv3_parent_pna_labels <- lapply(qv3_parent_pna, function(x) attributes(x)$label)
    qv3_parent_pna_labels[["id"]] <- qv3_parent_clean_labels[["id"]]

    pna_label <- "Note: prefer not to answer (pna) marked NA - see pna database for which were pna rather than missing NA"

    ## 6a) categorical variables with 99's data ####
    level99_issue_catvars <- names(qv3_parent_clean)[c(3:47, 49:182)]

    for (v in 1:length(level99_issue_catvars)) {
        # get variable name
        pvar <- level99_issue_catvars[v]

        # if has '99' value, create new pna variable marking pna == 1
        if (is.element(99, qv3_parent_clean[[pvar]])) {
            pna_dat <- ifelse(is.na(qv3_parent_clean[[pvar]]), 0, ifelse(qv3_parent_clean[[pvar]] ==
                99, 1, 0))

            new_pna <- length(names(qv3_parent_pna)) + 1
            qv3_parent_pna[[new_pna]] <- pna_dat

            names(qv3_parent_pna)[new_pna] <- paste0(pvar, "_pna")

            # add label to pna database
            qv3_parent_pna_labels[[paste0(pvar, "_pna")]] <- paste0("prefer not to answer marked for variable ",
                pvar, ": ", qv3_parent_clean_labels[[pvar]])

            # update true data label (only want to pna label if needed)
            qv3_parent_clean_labels[[pvar]] <- paste0(qv3_parent_clean_labels[[pvar]],
                " -- ", pna_label)

        }

        # drop 99 level label labels only update if had 99 - done in if statement
        # above
        qv3_parent_clean[[pvar]] <- sjlabelled::remove_labels(qv3_parent_clean[[pvar]],
            labels = "Don't want to answer")

        # extract variable attributes
        pvar_attr <- attributes(qv3_parent_clean[[pvar]])

        # replace 99 values
        qv3_parent_clean[[pvar]] <- ifelse(is.na(qv3_parent_clean[[pvar]]) | qv3_parent_clean[[pvar]] ==
            99, NA, qv3_parent_clean[[pvar]])

        # replace attributes
        attributes(qv3_parent_clean[[pvar]]) <- pvar_attr
    }

    ## 6b) fix LBC levels
    LBC_vars <- names(qv3_parent_clean)[c(4:23)]

    for (v in 1:length(LBC_vars)) {
        # get variable name
        pvar <- LBC_vars[v]

        # drop 99 level label labels only update if had 99 - done in if statement
        # above
        qv3_parent_clean[[pvar]] <- sjlabelled::remove_labels(qv3_parent_clean[[pvar]],
            labels = "4 Somewhat")

        qv3_parent_clean[[pvar]] <- sjlabelled::set_labels(qv3_parent_clean[[pvar]],
            labels = c(`Not at all` = 1, `A little (-)` = 2, `A little (+)` = 3,
                Somewhat = 4, `Much (-)` = 5, `Much(+)` = 6, `Very much` = 7))

    }

    #### 7) reformatting dates/times ####

    ##7a) dates (start, dobs)
    #format start date
    qv3_parent_clean[["start_date"]] <- lubridate::ymd(as.Date(qv3_parent_clean[["start_date"]]))

    # dates are fomrated as dd-mstr-yy
    visit_dates[['RO1_V3_Date']] <- lubridate::ymd(as.Date(visit_dates[['RO1_V3_Date']], format = "%d-%b-%y"))

    # add validated dates
    names(visit_dates)[1] <- 'id'
    qv3_parent_clean <- merge(qv3_parent_clean, visit_dates[c('id', 'RO1_V3_Date')], by = 'id', all.x = TRUE, all.y = FALSE)

    #update start_date
    qv3_parent_clean[["start_date"]] <- ifelse(!is.na(qv3_parent_clean[['RO1_V3_Date']]), as.character(qv3_parent_clean[['RO1_V3_Date']]), as.character(qv3_parent_clean[["start_date"]]))

    #remove RO1_V date column
    qv3_parent_clean <- qv3_parent_clean[, names(qv3_parent_clean) != "RO1_V3_Date"]

    # add label
    qv3_parent_clean_labels[["start_date"]] <- "date from participant contacts databases ('verified_visit_dates*.csv) converted to format yyyy-mm-dd in R. If no date in database, uses start_date metadata from qualtrics"

    #### 8) Format for export ####

    ## 8a) add attributes to pna data
    qv3_parent_pna[2:ncol(qv3_parent_pna)] <- as.data.frame(lapply(qv3_parent_pna[2:ncol(qv3_parent_pna)], function(x) sjlabelled::add_labels(x, labels = c(`Did not skip due to prefer not to answer` = 0, `Prefer not to answer` = 1))))

    for (v in 2:ncol(qv3_parent_pna)){
        class(qv3_parent_pna[[v]]) <- c("haven_labelled", "vctrs_vctr", "double")
    }

    ## 8b) put data in order of participant ID for ease
    qv3_parent_clean <- qv3_parent_clean[order(qv3_parent_clean[["id"]]), ]
    qv3_parent_pna <- qv3_parent_pna[order(qv3_parent_pna[["id"]]), ]

    ## 8c) make sure the variable labels match in the dataset
    qv3_parent_clean = sjlabelled::set_label(qv3_parent_clean, label = matrix(unlist(qv3_parent_clean_labels,
        use.names = FALSE)))
    qv3_parent_pna = sjlabelled::set_label(qv3_parent_pna, label = matrix(unlist(qv3_parent_pna_labels,
        use.names = FALSE)))


    # make list of data frame and associated labels
    qv3_parent <- list(data = qv3_parent_clean, dict = qv3_parent_clean_labels,
        pna_data = qv3_parent_pna, pna_dict = qv3_parent_pna_labels)

    ## want an export options??

    return(qv3_parent)
}
