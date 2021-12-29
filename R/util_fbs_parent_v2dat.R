#' util_fbs_parent_v2dat: Process raw Qualtrics visit 2 data for the parent
#'
#' This function loads the .sav raw data file for the parent visit 2 data that was
#' collected via Qualtrics and cleans the data. Cleaning the data involves:
#' 1) extracting all variable descriptions,
#' 2) selecting relevant data columns,
#' 3) removing all practice events (e.g., 999)
#' 4) re-ordering and re-name data columns
#' 5) general fixes to variable labels (add visit)
#' 6) fix variables with 99 issue for 'prefer not to answer'
#' 7) reformatting dates to be appropriate and computer readable: YYYY-MM-DD
#' 8) fix factor levels to match questionnaire scoring
#'
#' The databases MUST follow the naming convention: Parent_V2_YYYY-MM-DD.sav
#'
#' @inheritParams util_fbs_parent_v1dat
#' @inheritParams util_fbs_parent_v1dat
#'
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from parent visit 2 Qualtrics;
#'  2) dict: all variable descriptions; 3) pna_data: data.frame marking participants who 'prefered not to answer' (pna) specific questions; and 4) pna_dict: all variable descriptions for pna_data
#'
#' @examples
#' #if in same working directory as data:
#' p_v2_dat <- util_fbs_parent_v2dat('2021-10-11')
#'
#' \dontrun{
#' #date must be a string. The following will not run:
#' p_v2_dat <- util_fbs_parent_v2dat(2021-10-11)
#'
#' #date must match the file name - for file named 'Parent_v2_2021_09_16', the
#' following will not run:
#' p_v2_dat <- util_fbs_parent_v2dat('2021_10_11')
#' }
#'
#'
#' @export
#'
util_fbs_parent_v2dat <- function(date_str, data_path) {

    #### 1. Set up/initial checks #####

    # check that date_str exist and is a string

    datestr_arg <- methods::hasArg(date_str)

    if (isTRUE(datestr_arg) & !is.character(date_str)) {
        stop("date_str must be entered as a string: e.g., '2021_10_11'")
    } else if (isFALSE(datestr_arg)) {
        stop("date_str must set to the data string from the parent visit 1 file name: e.g., '2021_09_16'")
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
        qv2_parent_path <- paste0(data_path, "/Parent_V2_", date_str, ".sav")
    } else {
        qv2_parent_path <- paste0("Parent_V2_", date_str, ".sav")
    }

    # check if file exists
    qv2_parent_exists <- file.exists(qv2_parent_path)

    # load data if it exists
    if (isTRUE(qv2_parent_exists)) {
        qv2_parent_dat <- as.data.frame(haven::read_spss(qv2_parent_path))

    } else {
        if (isTRUE(datapath_arg)) {
            stop("File does not exist. Check date_str and data_path entered")
        } else {
            stop("File does not exist. Check date_str and that the data exists in current working directory")
        }
    }

    #### 3. Clean Data #####

    # 3a) extract variable labels/descriptions
    qv2_parent_labels <- lapply(qv2_parent_dat, function(x) attributes(x)$label)

    # 3b) selecting relevant data columns
    qv2_parent_clean <- qv2_parent_dat[c(1, 11:238)]

    ## update labels
    qv2_parent_clean_labels <- qv2_parent_labels[c(1, 11:238)]

    # 3c) removing all practice events (e.g., 999)
    qv2_parent_clean <- qv2_parent_clean[!is.na(qv2_parent_clean[["ID"]]) & qv2_parent_clean[["ID"]] < 999, ]

    # 4) re-ordering and re-name data columns general order #### 1) child information/demo (sex, dob, h/w,
    # puberty), 2) fasting, 3) sleep (CSHQ), 4) CEBQ, CFQ, BES, FFB, 5) CBQ, 6) updates

    qv2_parent_clean <- qv2_parent_clean[c(2, 1, 3, 212:229, 111:211, 17:18, 80, 19:25, 81, 26:29, 82, 30, 83:84, 31,
        85, 32:34, 86, 35:42, 87:89, 43:48, 90, 49:52, 91:93, 53:54, 94:95, 55, 96, 56:58, 97:98, 59:64, 99, 65:69, 100:101,
        70:71, 102, 72, 103, 73, 104:106, 74:78, 107:110, 79, 4:16)]

    qv2_parent_clean_labels <- qv2_parent_clean_labels[c(2, 1, 3, 212:229, 111:211, 17:18, 80, 19:25, 81, 26:29, 82,
        30, 83:84, 31, 85, 32:34, 86, 35:42, 87:89, 43:48, 90, 49:52, 91:93, 53:54, 94:95, 55, 96, 56:58, 97:98, 59:64,
        99, 65:69, 100:101, 70:71, 102, 72, 103, 73, 104:106, 74:78, 107:110, 79, 4:16)]

    ## re-name variables

    # make lower case
    names(qv2_parent_clean) <- tolower(names(qv2_parent_clean))

    # start date rename
    names(qv2_parent_clean)[2] <- "start_date"

    # fix cshq so reflects that it is the adapted version
    for (var in 1:length(names(qv2_parent_clean))){
        var_name <- as.character(names(qv2_parent_clean)[var])

        # add '-a' to cshq names
        if (grepl("cshq", var_name, fixed = TRUE)) {
            names(qv2_parent_clean)[var] <- gsub("cshq", "cshq_a", var_name)
        }

        # add 's' to ffb names
        if (grepl("ffb", var_name, fixed = TRUE)) {
            names(qv2_parent_clean)[var] <- gsub("ffb", "ffbs", var_name)
        }

    }

    # remove 'v2'
    for (var in 1:length(names(qv2_parent_clean))) {
        var_name <- as.character(names(qv2_parent_clean)[var])

        # remove trailing 'v2' from names
        if (grepl("v2", var_name, fixed = TRUE)) {
            names(qv2_parent_clean)[var] <- gsub("v2", "", var_name)
        }
    }

    ## update data labels
    names(qv2_parent_clean_labels) <- names(qv2_parent_clean)

    ## 5) general fixes to labels (add visit, remove '- 1') ####

    ## remove formatting errors
    for (var in 1:length(names(qv2_parent_clean))) {
        var_name <- as.character(names(qv2_parent_clean)[var])

        # remove ' \' ' from apostrophes (e.g., child\'s)
        if (grepl("'s", qv2_parent_clean_labels[[var_name]], fixed = TRUE)) {
            qv2_parent_clean_labels[[var_name]] <- gsub("\\'s", "", qv2_parent_clean_labels[[var_name]])
        }

        # remove trailing 'v2 ' from labels
        if (grepl("V2", qv2_parent_clean_labels[[var_name]], fixed = TRUE)) {
            qv2_parent_clean_labels[[var_name]] <- gsub("\\V2 - ", "", qv2_parent_clean_labels[[var_name]])
            qv2_parent_clean_labels[[var_name]] <- gsub("\\V2 ", "", qv2_parent_clean_labels[[var_name]])
        }
    }

    #### 6) fix 99's and other poor categories ####

    ## check for labels/99 option: 1) if 99's exist, make a 'prefere not to answer' (pna) variable to go in pna
    ## database, 2) replace 99's with NA and make variable numeric

    ## make pna database
    qv2_parent_pna <- data.frame(id = qv2_parent_clean[["id"]])
    qv2_parent_pna_labels <- lapply(qv2_parent_pna, function(x) attributes(x)$label)
    qv2_parent_pna_labels[["id"]] <- qv2_parent_clean_labels[["id"]]

    pna_label <- "Note: prefer not to answer (pna) marked NA - see pna database for which were pna rather than missing NA"

    ## 6a) categorical variables with 99's data ####
    level99_issue_catvars <- names(qv2_parent_clean)[c(3:122, 217:223)]

    for (v in 1:length(level99_issue_catvars)) {
        # get variable name
        pvar <- level99_issue_catvars[v]

        # if has '99' value, create new pna variable marking pna == 1
        if (is.element(99, qv2_parent_clean[[pvar]])) {
            pna_dat <- ifelse(is.na(qv2_parent_clean[[pvar]]), 0, ifelse(qv2_parent_clean[[pvar]] == 99, 1, 0))

            new_pna <- length(names(qv2_parent_pna)) + 1
            qv2_parent_pna[[new_pna]] <- pna_dat

            names(qv2_parent_pna)[new_pna] <- paste0(pvar, "_pna")

            # add label to pna database
            qv2_parent_pna_labels[[paste0(pvar, "_pna")]] <- paste0("prefer not to answer marked for variable ", pvar,
                ": ", qv2_parent_clean_labels[[pvar]])

            # update true data label (only want to pna label if needed)
            qv2_parent_clean_labels[[pvar]] <- paste0(qv2_parent_clean_labels[[pvar]], " -- ", pna_label)

        }

        # drop 99 level label labels only update if had 99 - done in if statement above
        qv2_parent_clean[[pvar]] <- sjlabelled::remove_labels(qv2_parent_clean[[pvar]], labels = "Don't want to answer")

        # extract variable attributes
        pvar_attr <- attributes(qv2_parent_clean[[pvar]])

        # replace 99 values
        qv2_parent_clean[[pvar]] <- ifelse(is.na(qv2_parent_clean[[pvar]]) | qv2_parent_clean[[pvar]] == 99, NA, qv2_parent_clean[[pvar]])

        # replace attributes
        attributes(qv2_parent_clean[[pvar]]) <- pvar_attr
    }

    #### 7) reformatting dates/times #### 7a) dates (start, dobs) ####
    qv2_parent_clean[["start_date"]] <- lubridate::ymd(as.Date(qv2_parent_clean[["start_date"]]))
    qv2_parent_clean_labels[["start_date"]] <- "start_date from qualtrics survey meta-data converted to format yyyy-mm-dd in R"

    # 8) fixing factor level values to match questionnaire scoring ####

    ## 8a) CSHQ has wrong level values - should be 1-5
    cshq_vars <- names(qv2_parent_clean)[4:21]

    for (var in 1:length(cshq_vars)) {
        var_name <- cshq_vars[var]

        # reset all labels
        qv2_parent_clean[[var_name]] <- sjlabelled::set_labels(qv2_parent_clean[[var_name]], labels = c(Never = 1, Rarely = 2,
            Sometimes = 3, Usually = 4, Always = 5))

        # save attributes
        set_attr <- attributes(qv2_parent_clean[[var_name]])

        # re-set values to match new level values
        qv2_parent_clean[[var_name]] <- ifelse(is.na(qv2_parent_clean[[var_name]]), NA, ifelse(qv2_parent_clean[[var_name]] ==
            0, 1, ifelse(qv2_parent_clean[[var_name]] == 1, 2, ifelse(qv2_parent_clean[[var_name]] == 5, 4, ifelse(qv2_parent_clean[[var_name]] ==
            7, 5, qv2_parent_clean[[var_name]])))))

        # reset attributes
        attributes(qv2_parent_clean[[var_name]]) <- set_attr

        # update variable labels
        qv2_parent_clean_labels[[var_name]] <- paste0(qv2_parent_clean_labels[[var_name]], " re-leveled in R to range from 1 (Never) - 5 (Always)")
    }

    ## 8b) CFQ has wrong level values - should be unconcerned to very concerned
    cfq_vars <- c("cfq14", "cfq15", "cfq15")

    for (var in 1:length(cfq_vars)) {
        var_name <- cfq_vars[var]

        # reset all labels
        qv2_parent_clean[[var_name]] <- sjlabelled::set_labels(qv2_parent_clean[[var_name]], labels = c(Unconcerned = 1,
            `Slightly Concerned` = 2, Neutral = 3, `Slightly Concerned` = 4, Concerned = 5))

        # save attributes
        set_attr <- attributes(qv2_parent_clean[[var_name]])

        # reset attributes
        attributes(qv2_parent_clean[[var_name]]) <- set_attr

        # update variable labels
        qv2_parent_clean_labels[[var_name]] <- paste0(qv2_parent_clean_labels[[var_name]], " re-leveled in R to range from 1 (Unconcerned) - 5 (Very Concerned)")
    }

    ## 8c) FFBS has wrong level values - should start at 0 - Never True to 4 - Always True
    ffb_vars <- names(qv2_parent_clean)[103:122]

    for (var in 1:length(ffb_vars)) {
        var_name <- ffb_vars[var]

        # reset all labels
        qv2_parent_clean[[var_name]] <- sjlabelled::set_labels(qv2_parent_clean[[var_name]], labels = c(`Never True` = 0,
            `Rarely True` = 1, Sometimes = 2, `Often True` = 3, `Always True` = 4))

        # save attributes
        set_attr <- attributes(qv2_parent_clean[[var_name]])

        # subtract 1 from each data value to match new level values
        qv2_parent_clean[[var_name]] <- ifelse(is.na(qv2_parent_clean[[var_name]]), NA, qv2_parent_clean[[var_name]] -
            1)

        # reset attributes
        attributes(qv2_parent_clean[[var_name]]) <- set_attr

        # update variable labels
        qv2_parent_clean_labels[[var_name]] <- paste0(qv2_parent_clean_labels[[var_name]], " re-leveled in R to range from 0 (Never True) - 4 (Always True)")
    }

    ## 8d) CBQ - change NA option to be more clear and set to -99 CBQ states: 'If you cannot answer one of the items because you have never seen the child in that situation, for example, if the statement is about the child's reaction to your singing and you have never sung to your child, then circle NA (not applicable)'

    cbq_vars <- names(qv2_parent_clean)[123:216]

    for (var in 1:length(cbq_vars)) {
        var_name <- cbq_vars[var]

        # reset all labels
        qv2_parent_clean[[var_name]] <- sjlabelled::set_labels(qv2_parent_clean[[var_name]], labels = c(`Extremely Untrue` = 1,
            `Quite Untrue` = 2, `Sightly Untrue` = 3, `Neither True nor False` = 4, `Slightly True` = 5, `Quite True` = 6,
            `Extremely True` = 7, `NA or Don't want to answer` = -99))

        # save attributes
        set_attr <- attributes(qv2_parent_clean[[var_name]])

        # subtract 1 from each data value to match new level values
        qv2_parent_clean[[var_name]] <- ifelse(is.na(qv2_parent_clean[[var_name]]), NA, ifelse(qv2_parent_clean[[var_name]] ==
            99, -99, qv2_parent_clean[[var_name]]))

        # reset attributes
        attributes(qv2_parent_clean[[var_name]]) <- set_attr

        # update variable labels
        qv2_parent_clean_labels[[var_name]] <- paste0(qv2_parent_clean_labels[[var_name]], " re-leveled in R to NA value to -99 as it reflects not knowing the child's reaction - see CBQ instructions")
    }

    #### 9) Format for export ####

    ## 9a) add attributes to pna data
    n_pna_cols <- length(names(qv2_parent_pna))
    qv2_parent_pna[2:n_pna_cols] <- as.data.frame(lapply(qv2_parent_pna[2:n_pna_cols], function(x) sjlabelled::add_labels(x,
        labels = c(`Did not skip due to prefer not to answer` = 0, `Prefer not to answer` = 1))))

    ## 9b) put data in order of participant ID for ease
    qv2_parent_clean <- qv2_parent_clean[order(qv2_parent_clean[["id"]]), ]
    qv2_parent_pna <- qv2_parent_pna[order(qv2_parent_pna[["id"]]), ]

    ## 9c) make sure the variable labels match in the dataset
    qv2_parent_clean = sjlabelled::set_label(qv2_parent_clean, label = matrix(unlist(qv2_parent_clean_labels, use.names = FALSE)))
    qv2_parent_pna = sjlabelled::set_label(qv2_parent_pna, label = matrix(unlist(qv2_parent_pna_labels, use.names = FALSE)))

    # make list of data frame and associated labels
    qv2_parent <- list(data = qv2_parent_clean, dict = qv2_parent_clean_labels, pna_data = qv2_parent_pna, pna_dict = qv2_parent_pna_labels)

    ## want an export options??

    return(qv2_parent)
}
