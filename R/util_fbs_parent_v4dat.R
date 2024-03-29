#' util_fbs_parent_v4dat: Process raw Qualtrics visit 4 data for the parent
#'
#' This function loads the .sav raw data file for the parent visit 4 data that was
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
#' The databases MUST follow the naming convention: Parent_V4_YYYY-MM-DD.sav
#'
#' @inheritParams util_fbs_parent_v1dat
#' @inheritParams util_fbs_parent_v1dat
#'
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from parent visit 4 Qualtrics;
#'  2) dict: all variable descriptions; 3) pna_data: data.frame marking participants who 'prefered not to answer' (pna) specific questions; and 4) pna_dict: all variable descriptions for pna_data
#'
#' @examples
#' #if in same working directory as data:
#' p_v4_dat <- util_fbs_parent_v4dat('Parent_V4')
#'
#' \dontrun{
#' #file_pattern must be a string. The following will not run:
#' p_v4_dat <- util_fbs_parent_v4dat(Parent_V4)
#'
#' #file_pattern must have the respondent ('Parent') and visit number ('V1'). If just enter 'Parent', the script will not run because it will return multiple files for different parent visits. The following will not run:
#' p_v4_dat <- util_fbs_parent_v4dat('Parent')
#' }
#'
#'
#' @export
#'
util_fbs_parent_v4dat <- function(file_pattern, data_path) {

    #### 1. Set up/initial checks #####

    # check that file_pattern exist and is a string

    filepat_arg <- methods::hasArg(file_pattern)

    if (isTRUE(filepat_arg) & !is.character(file_pattern)) {
        stop("file_pattern must be entered as a string: e.g., 'Parent_V4'")
    } else if (isFALSE(filepat_arg)) {
        stop("file_pattern must set to the a string matching the name of the raw data file for parent visit: e.g., 'Parent_V4'")
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
        qv4_parent_pathlist <- list.files(path = data_path, pattern = file_pattern, full.names = TRUE)
    } else {
        qv4_parent_pathlist <- paste0(pattern = file_pattern, full.names = TRUE)
    }

    # check number of files found
    if (length(qv4_parent_pathlist) > 1) {
        stop("More than one file matched the file_pattern. Be sure thefile_pattern specifies both the respondent (Parent/Child) and visit number (V#). If have more than 1 file matching the pattern in the directory, may need to move to enter a more specific file_pattern than is standard.")
    } else if (length(qv4_parent_pathlist) == 0) {
        stop('No files found. Be sure the data_path and file_pattern are correct and that the file exists')
    } else {
        qv4_parent_path <- qv4_parent_pathlist
    }


    # check that file is of type '.sav'
    if (!grepl('.sav', qv4_parent_path, fixed = TRUE)){
        stop("The file found is not an SPSS database (.sav)")
    }

    # check if file exists
    qv4_parent_exists <- file.exists(qv4_parent_path)

    # load data if it exists
    if (isTRUE(qv4_parent_exists)) {
        qv4_parent_dat <- as.data.frame(haven::read_spss(qv4_parent_path))

    } else {
        if (isTRUE(datapath_arg)) {
            stop("File does not exist. Check date_str and data_path entered")
        } else {
            stop("File does not exist. Check date_str and that the data exists in current working directory")
        }
    }

    #### 3. Clean Data #####

    # 3a) extract variable labels/descriptions
    qv4_parent_labels <- lapply(qv4_parent_dat, function(x) attributes(x)$label)

    # 3b) selecting relevant data columns
    qv4_parent_clean <- qv4_parent_dat[c(1, 11:162)]

    ## update labels
    qv4_parent_clean_labels <- qv4_parent_labels[c(1, 11:162)]

    # 3c) removing all practice events (e.g., 999)
    qv4_parent_clean <- qv4_parent_clean[!is.na(qv4_parent_clean[["ID"]]) & qv4_parent_clean[["ID"]] < 999, ]

    # 4) re-ordering and re-name data columns general order #### 1) demographics - HFSSM, HFIAS, CCHIP, 2) fasting,
    # 3) BRIEF, 4) updates

    qv4_parent_clean <- qv4_parent_clean[c(2, 1, 86:88, 17:85, 3, 89, 91:153, 4:16)]

    qv4_parent_clean_labels <- qv4_parent_clean_labels[c(2, 1, 86:88, 17:85, 3, 89, 91:153, 4:16)]

    ## re-name variables

    # make lower case
    names(qv4_parent_clean) <- tolower(names(qv4_parent_clean))

    # start date rename
    names(qv4_parent_clean)[2] <- "start_date"

    # remove 'v4'
    for (var in 1:length(names(qv4_parent_clean))) {
        var_name <- as.character(names(qv4_parent_clean)[var])

        # remove trailing 'v4' from names
        if (grepl("v4", var_name, fixed = TRUE)) {
            names(qv4_parent_clean)[var] <- gsub("v4", "", var_name)
        }

        # remove '_4' from BRIEF
        if (grepl("_4", var_name, fixed = TRUE)) {
            names(qv4_parent_clean)[var] <- gsub("_4", "", var_name)
        }
    }

    ## fix HFSSM names
    names(qv4_parent_clean)[c(6:24)] <- c("hfssm_hh1", "hfssm_hh2", "hfssm_hh3", "hfssm_hh4", "hfssm_ad1", "hfssm_ad1a", "hfssm_ad2", "hfssm_ad3", "hfssm_ad4", "hfssm_ad5", "hfssm_ad5a", "hfssm_ch1", "hfssm_ch2", "hfssm_ch3", "hfssm_ch4", "hfssm_ch5", "hfssm_ch5a", "hfssm_ch6", "hfssm_ch7")

    ## update data labels
    names(qv4_parent_clean_labels) <- names(qv4_parent_clean)

    ## 5) general fixes to labels (add visit, remove '- 1') ####

    ## remove formatting errors
    for (var in 1:length(names(qv4_parent_clean))) {
        var_name <- as.character(names(qv4_parent_clean)[var])

        # remove ' \' ' from apostrophes (e.g., child\'s)
        if (grepl("'s", qv4_parent_clean_labels[[var_name]], fixed = TRUE)) {
            qv4_parent_clean_labels[[var_name]] <- gsub("\\'s", "", qv4_parent_clean_labels[[var_name]])
        }

        # remove trailing 'v4 ' from labels
        if (grepl("V4", qv4_parent_clean_labels[[var_name]], fixed = TRUE)) {
            qv4_parent_clean_labels[[var_name]] <- gsub("\\V4 - ", "", qv4_parent_clean_labels[[var_name]])
            qv4_parent_clean_labels[[var_name]] <- gsub("\\V4 ", "", qv4_parent_clean_labels[[var_name]])
        }
    }

    ## fix HFSSM labels
    hfssm_vars <- names(qv4_parent_clean)[c(6:24)]

    for (var in 1:length(hfssm_vars)) {
        var_name <- hfssm_vars[var]

        # update label
        qv4_parent_clean_labels[[var_name]] <- paste0("HFSSM ", qv4_parent_clean_labels[[var_name]])
    }

    #### 6) fix 99's and other poor categories ####

    ## check for labels/99 option: 1) if 99's exist, make a 'prefere not to answer' (pna) variable to go in pna database, 2) replace 99's with NA and make variable numeric

    ## make pna database
    qv4_parent_pna <- data.frame(id = qv4_parent_clean[["id"]])
    qv4_parent_pna_labels <- lapply(qv4_parent_pna, function(x) attributes(x)$label)
    qv4_parent_pna_labels[["id"]] <- qv4_parent_clean_labels[["id"]]

    pna_label <- "Note: prefer not to answer (pna) marked NA - see pna database for which were pna rather than missing NA"

    ## 6a) categorical variables with 99's data ####
    level99_issue_catvars <- names(qv4_parent_clean)[c(25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 47, 51, 55, 59, 63, 67, 71, 75:146)]

    for (v in 1:length(level99_issue_catvars)) {
        # get variable name
        pvar <- level99_issue_catvars[v]

        # if has '99' value, create new pna variable marking pna == 1
        if (is.element(99, qv4_parent_clean[[pvar]])) {
            pna_dat <- ifelse(is.na(qv4_parent_clean[[pvar]]), 0, ifelse(qv4_parent_clean[[pvar]] == 99, 1, 0))

            if (length(names(qv4_parent_pna)) == 0) {
                new_pna <- 1
                qv4_parent_pna <- data.frame(pna_dat)
            } else {
                new_pna <- length(names(qv4_parent_pna)) + 1
                qv4_parent_pna[[new_pna]] <- pna_dat
            }

            names(qv4_parent_pna)[new_pna] <- paste0(pvar, "_pna")

            # add label to pna database
            qv4_parent_pna_labels[[paste0(pvar, "_pna")]] <- paste0("prefer not to answer marked for variable ", pvar, ": ", qv4_parent_clean_labels[[pvar]])

            # update true data label (only want to pna label if needed)
            qv4_parent_clean_labels[[pvar]] <- paste0(qv4_parent_clean_labels[[pvar]], " -- ", pna_label)

        }

        # drop 99 level label labels only update if had 99 - done in if statement above
        qv4_parent_clean[[pvar]] <- sjlabelled::remove_labels(qv4_parent_clean[[pvar]], labels = "Don't want to answer")

        # extract variable attributes
        pvar_attr <- attributes(qv4_parent_clean[[pvar]])

        # replace 99 values
        qv4_parent_clean[[pvar]] <- ifelse(is.na(qv4_parent_clean[[pvar]]) | qv4_parent_clean[[pvar]] == 99, NA, qv4_parent_clean[[pvar]])

        # replace attributes
        attributes(qv4_parent_clean[[pvar]]) <- pvar_attr
    }

    ## 6a) continuous variables with 99's data ####
    level99_issue_contvars <- names(qv4_parent_clean)[c(4:5, 44:46, 48:50, 52:54, 56:58, 60:62, 64:66, 68:70, 72:74)]

    for (v in 1:length(level99_issue_contvars)) {
        # get variable name
        pvar <- level99_issue_contvars[v]

        # if has '99' value, create new pna variable marking pna == 1
        if (is.element(99, qv4_parent_clean[[pvar]])) {
            pna_dat <- ifelse(is.na(qv4_parent_clean[[pvar]]), 0, ifelse(qv4_parent_clean[[pvar]] == 99, 1, 0))

            if (length(names(qv4_parent_pna)) == 0) {
                new_pna <- 1
                qv4_parent_pna <- data.frame(pna_dat)
            } else {
                new_pna <- length(names(qv4_parent_pna)) + 1
                qv4_parent_pna[[new_pna]] <- pna_dat
            }

            names(qv4_parent_pna)[new_pna] <- paste0(pvar, "_pna")

            # add label to pna database
            qv4_parent_pna_labels[[paste0(pvar, "_pna")]] <- paste0("prefer not to answer marked for variable ", pvar, ": ", qv4_parent_clean_labels[[pvar]])

            # update true data label (only want to pna label if needed)
            qv4_parent_clean_labels[[pvar]] <- paste0(qv4_parent_clean_labels[[pvar]], " -- ", pna_label)
        }

        # convert 99 to NA and make numeric variable labels only update if had 99 - done in if statement above
        qv4_parent_clean[[pvar]] <- ifelse(qv4_parent_clean[[pvar]] == 99, NA, as.numeric(qv4_parent_clean[[pvar]]))
    }

    ## 6b) fix HFSSM value coding and set dont know to -99 ####
    often_sometimes_vars <- c('hfssm_hh2', 'hfssm_hh3', 'hfssm_hh4', 'hfssm_ch1', 'hfssm_ch2', 'hfssm_ch3')
    wk_freq_vars <- c('hfssm_ad1a', 'hfssm_ad5a')

    for (var in 1:length(hfssm_vars)) {
        var_name <- hfssm_vars[var]

        if (var_name %in% often_sometimes_vars){
            #save attributes
            set_attr <- attributes(qv4_parent_clean[[var_name]])

            #re-level
            qv4_parent_clean[[var_name]] <- ifelse(is.na(qv4_parent_clean[[var_name]]), NA, ifelse(qv4_parent_clean[[var_name]] >= 1, 1, ifelse(qv4_parent_clean[[var_name]] == 99, -99, 0)))

            #set attributes
            attributes(qv4_parent_clean[[var_name]]) <- set_attr

            # remove 'Often' and dont know label
            qv4_parent_clean[[var_name]] <- sjlabelled::remove_labels(qv4_parent_clean[[var_name]], labels = c("Often True", "Sometimes True", "I don't know or Don't want to answer"))

            # add Often True = 1 label
            qv4_parent_clean[[var_name]] <- sjlabelled::add_labels(qv4_parent_clean[[var_name]], labels = c(`Often True` = 1, `Sometimes True` = 1, `I don't know or Don't want to answer` = -99))

            #update label
            qv4_parent_clean_labels[[var_name]] <- paste0(qv4_parent_clean_labels[[var_name]], " re-leveled in R so don't know = -99 AND 'often' and 'sometimes' both coded as 1")
        } else if (var_name %in% wk_freq_vars){

            #save attributes
            set_attr <- attributes(qv4_parent_clean[[var_name]])

            #re-level
            qv4_parent_clean[[var_name]] <- ifelse(is.na(qv4_parent_clean[[var_name]]), NA, ifelse(qv4_parent_clean[[var_name]] > 1, 1, ifelse(qv4_parent_clean[[var_name]] == 1, 0, ifelse(qv4_parent_clean[[var_name]] == 99, -99, qv4_parent_clean[[var_name]]))))

            #set attributes
            attributes(qv4_parent_clean[[var_name]]) <- set_attr

            # remove all labels
            qv4_parent_clean[[var_name]] <- sjlabelled::remove_labels(qv4_parent_clean[[var_name]], labels = c("Almost every month", "Some months, but not every month", "Only 1 or 2 months", "I don't know or Don't want to answer"))

            # add labels back with correct values
            qv4_parent_clean[[var_name]] <- sjlabelled::add_labels(qv4_parent_clean[[var_name]], labels = c(`Often True` = 1, `Some months, but not every month` = 1, `Only 1 or 2 months` = 0, `I don't know or Don't want to answer` = -99))

            #update label
            qv4_parent_clean_labels[[var_name]] <- paste0(qv4_parent_clean_labels[[var_name]], " re-leveled in R so don't know = -99 AND 'Almost every month' and 'Some months, but not every month' both coded as 1")
        } else {
            # save attributes
            set_attr <- attributes(qv4_parent_clean[[var_name]])

            # update values
            qv4_parent_clean[[var_name]] <- ifelse(is.na(qv4_parent_clean[[var_name]]), NA, ifelse(qv4_parent_clean[[var_name]] == 99, -99, qv4_parent_clean[[var_name]]))

            # reset attributes
            attributes(qv4_parent_clean[[var_name]]) <- set_attr

            # remove 99 label
            qv4_parent_clean[[var_name]] <- sjlabelled::remove_labels(qv4_parent_clean[[var_name]], labels = "I don't know or Don't want to answer")

            # add -99 label
            qv4_parent_clean[[var_name]] <- sjlabelled::add_labels(qv4_parent_clean[[var_name]], labels = c(`I don't know or Don't want to answer` = -99))

            # update variable labels
            qv4_parent_clean_labels[[var_name]] <- paste0(qv4_parent_clean_labels[[var_name]], " re-leveled in R so don't know = -99")
        }
    }

    ## 6c) fix sex levels ####
    qv4_parent_clean[['sex']] <- sjlabelled::set_labels(qv4_parent_clean[['sex']], labels = c(Male = 0, Female = 1))
    set_attr <- attributes(qv4_parent_clean[["sex"]])
    qv4_parent_clean[['sex']] <- ifelse(is.na(qv4_parent_clean[['sex']]), NA, ifelse(qv4_parent_clean[['sex']] == 1, 0, 1))
    attributes(qv4_parent_clean[['sex']]) <- set_attr
    qv4_parent_clean_labels[["sex"]] <- paste0(qv4_parent_clean_labels[["sex"]], " re-leveled in R to start with 0")

    #### 7) reformatting dates/times ####
    ## 7a) dates (start, dobs)
    #format start date
    qv4_parent_clean[["start_date"]] <- lubridate::ymd(as.Date(qv4_parent_clean[["start_date"]]))

    # dates are fomrated as dd-mstr-yy
    visit_dates[['RO1_V4_Date']] <- lubridate::ymd(as.Date(visit_dates[['RO1_V4_Date']], format = "%d-%b-%y"))

    # add validated dates
    names(visit_dates)[1] <- 'id'
    qv4_parent_clean <- merge(qv4_parent_clean, visit_dates[c('id', 'RO1_V4_Date')], by = 'id', all.x = TRUE, all.y = FALSE)

    #update start_date
    qv4_parent_clean[["start_date"]] <- ifelse(!is.na(qv4_parent_clean[['RO1_V4_Date']]), as.character(qv4_parent_clean[['RO1_V4_Date']]), as.character(qv4_parent_clean[["start_date"]]))

    #remove RO1_V date column
    qv4_parent_clean <- qv4_parent_clean[, names(qv4_parent_clean) != "RO1_V4_Date"]

    # add label
    qv4_parent_clean_labels[["start_date"]] <- "date from participant contacts databases ('verified_visit_dates*.csv) converted to format yyyy-mm-dd in R. If no date in database, uses start_date metadata from qualtrics"

    #### 8) Format for export ####

    ## 8a) add attributes to pna data
    qv4_parent_pna[2:ncol(qv4_parent_pna)] <- as.data.frame(lapply(qv4_parent_pna[2:ncol(qv4_parent_pna)], function(x) sjlabelled::add_labels(x, labels = c(`Did not skip due to prefer not to answer` = 0, `Prefer not to answer` = 1))))

    for (v in 2:ncol(qv4_parent_pna)){
        class(qv4_parent_pna[[v]]) <- c("haven_labelled", "vctrs_vctr", "double")
    }

    ## 8b) put data in order of participant ID for ease
    qv4_parent_clean <- qv4_parent_clean[order(qv4_parent_clean[["id"]]), ]
    qv4_parent_pna <- qv4_parent_pna[order(qv4_parent_pna[["id"]]), ]

    ## 8c) make sure the variable labels match in the dataset
    qv4_parent_clean = sjlabelled::set_label(qv4_parent_clean, label = matrix(unlist(qv4_parent_clean_labels, use.names = FALSE)))
    qv4_parent_pna = sjlabelled::set_label(qv4_parent_pna, label = matrix(unlist(qv4_parent_pna_labels, use.names = FALSE)))

    # make list of data frame and associated labels
    qv4_parent <- list(data = qv4_parent_clean, dict = qv4_parent_clean_labels, pna_data = qv4_parent_pna, pna_dict = qv4_parent_pna_labels)

    ## want an export options??

    return(qv4_parent)
}
