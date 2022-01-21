#' util_fbs_child_v7dat_home: Process raw qualtrics visit 7 data collected at home for the child
#'
#' This function loads the .sav raw data file for the child visit 7 data that was collected via Qualtrics at home when the protocol was split due to covid and cleans the data. Cleaning the data involves:
#' 1) extracting all variable descriptions,
#' 2) selecting relevant data columns,
#' 3) removing all practice events (e.g., 999)
#' 4) re-ordering and re-name data columns
#' 5) reformatting dates to be appropriate and computer readable: YYYY-MM-DD
#' 6) re-calculate manual variables
#' 7) re-ordering factor levels
#' 8) random fixes to factor level names and variable descriptions
#'
#' The databases MUST follow the naming convention: Child_V7_YYYY-MM-DD.sav
#'
#' @inheritParams util_fbs_parent_v1dat
#' @inheritParams util_fbs_parent_v1dat
#'
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from child visit 7 Qualtrics; 2) dict: all variable descriptions; 3) pna_data: data.frame marking participants who 'prefered not to answer' (pna) specific questions; and 4) pna_dict: all variable descriptions for pna_data
#'
#' @examples
#' #if in same working directory as data. Note - there is no need to add the COVID protocol (i.e., 'Home' or 'Lab') to file_pattern:
#' child_v7_dat_home <- util_fbs_child_v7dat_home('Child_V7')
#'
#' \dontrun{
#' #file_pattern must be a string. The following will not run:
#' child_v7_dat_home <- util_fbs_child_v7dat_home(Child_V7)
#'
#' #file_pattern must have the respondent ('Child') and visit number ('V7'). If just enter 'Child', the script will not run because it will return multiple files for different parent visits. The following will not run:
#' child_v7_dat_home <- util_fbs_child_v7dat_home('Child')
#' }
#'
#'
#' @export
#'
util_fbs_child_v7dat_home <- function(file_pattern, data_path) {

    #### 1. Set up/initial checks #####

    # check that file_pattern exist and is a string

    filepat_arg <- methods::hasArg(file_pattern)

    if (isTRUE(filepat_arg) & !is.character(file_pattern)) {
        stop("file_pattern must be entered as a string: e.g., 'Child_V7'")
    } else if (isFALSE(filepat_arg)) {
        stop("file_pattern must set to the a string matching the name of the raw data file for child visit: e.g., 'Child_V7'")
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
        qv7_child_path <- list.files(path = paste0(data_path, '/Final_Covid/'), pattern = paste0(file_pattern, '_Home'), full.names = TRUE)

        #if no files found, check direct data_path entered
        if (length(qv7_child_path) == 0) {
            qv7_child_path <- list.files(path = data_path, pattern = paste0(file_pattern, '_Home'), full.names = TRUE)
        }
    } else {
        qv7_child_path <- paste0(pattern = paste0(file_pattern, '_Home'), full.names = TRUE)
    }

    # check number of files found
    if (length(qv7_child_path) > 1) {
        stop("More than one file matched after adding '_Home' to the file_pattern . Be sure thefile_pattern specifies both the respondent (Parent/Child) and visit number (V#). If have more than 1 file matching the pattern in the directory, may need to move to enter a more specific file_pattern than is standard.")
    } else if (length(qv7_child_path) == 0) {
        stop("No files found after adding '_Home' to file_pattern. Be sure the data_path and file_pattern are correct and that the file exists.")
    }

    # check that file is of type '.sav'
    if (!grepl('.sav', qv7_child_path, fixed = TRUE)){
        stop("The file found is not an SPSS database (.sav)")
    }

    # check if file exists
    qv7_child_exists <- file.exists(qv7_child_path)

    # load data if it exists
    if (isTRUE(qv7_child_exists)) {
        qv7_child_dat <- as.data.frame(haven::read_spss(qv7_child_path))

    } else {
        if (isTRUE(datapath_arg)) {
            stop("File does not exist. Check file_pattern and data_path entered")
        } else {
            stop("File does not exist. Check file_pattern and that the data exists in current working directory")
        }
    }

    #### 3. Clean Data #####

    # 1) extract variable labels/descriptions  ####
    qv7_child_labels <- lapply(qv7_child_dat, function(x) attributes(x)$label)

    # 2) selecting relevant data columns ####
    qv7_child_clean <- qv7_child_dat[c(1, 18:41, 43:56)]

    ## update labels
    qv7_child_clean_labels <- qv7_child_labels[c(1, 18:41, 43:56)]


    # 3) removing all practice events (e.g., 999) ####
    qv7_child_clean <- qv7_child_clean[!is.na(qv7_child_clean[["ID"]]) & qv7_child_clean[["ID"]] < 999, ]

    # 4) re-ordering and re-name data columns general order ####

    #1) child information (sex, dob, h/w, bmi, puberty), 2) freddies, 3) food VAS 4) intakes (preMeal, EAH, meal duration), 5) wanting, LOC, 6) CTC, CWC, etc 7) notes

    qv7_child_clean <- qv7_child_clean[c(2, 1, 3:4, 26:39, 5:25)]

    qv7_child_clean_labels <- qv7_child_clean_labels[c(2, 1, 3:4, 26:39, 5:25)]

    ## make lower case
    names(qv7_child_clean) <- tolower(names(qv7_child_clean))

    ## re-name variables
    for (var in 1:length(names(qv7_child_clean))) {
        var_name <- as.character(names(qv7_child_clean)[var])

        # remove v7 prefix from labels
        if (grepl("v7_", var_name, fixed = TRUE)) {
            names(qv7_child_clean)[var] <- gsub("v7_", "", names(qv7_child_clean)[var])
        }

        # remove v7 prefix from labels
        if (grepl("v7", var_name, fixed = TRUE)) {
            names(qv7_child_clean)[var] <- gsub("v7", "", names(qv7_child_clean)[var])
        }

    }

    ## re-name variables
    names(qv7_child_clean)[2:18] <- c("start_date", "sex", "dob", "pds_1", "pds_2", "pds_3", "pds_4m", "pds_5m", "pds_6m", "pds_4f", "pds_5fa", "pds_5fb", "pds_5fc", "pds_5fd", "pds_6f", "tanner_male", "tanner_female")

    ## update data labels
    names(qv7_child_clean_labels) <- names(qv7_child_clean)

    # 5) reformatting dates to be appropriate and computer readable YYYY-MM-DD ####
    qv7_child_clean[["start_date"]] <- lubridate::ymd(as.Date(qv7_child_clean[["start_date"]]))
    qv7_child_clean_labels[["start_date"]] <- "start_date from qualtrics survey meta-data converted to format yyyy-mm-dd in R"

    qv7_child_clean[["dob"]] <- as.Date(qv7_child_clean[["dob"]], format = "%m/%d/%Y")
    qv7_child_clean_labels[["dob"]] <- "date of birth converted to format yyyy-mm-dd in R"

    # 6) re-ordering factor levels ####

    ## sex - make sure always matches across parent/child and visits
    qv7_child_clean[['sex']]<- sjlabelled::set_labels(qv7_child_clean[['sex']], labels = c(Male = 0, Female = 1))
    set_attr <- attributes(qv7_child_clean[['sex']])
    qv7_child_clean[['sex']] <- ifelse(is.na(qv7_child_clean[['sex']]), NA, ifelse(qv7_child_clean[['sex']] == 1, 0, 1))
    attributes(qv7_child_clean[['sex']]) <- set_attr
    qv7_child_clean_labels[["sex"]] <- paste0(qv7_child_clean_labels[["sex"]], " re-leveled in R to start with 0")

    # make pna database
    qv7_child_pna <- data.frame(id = qv7_child_clean[["id"]])
    qv7_child_pna_labels <- lapply(qv7_child_pna, function(x) attributes(x)$label)
    qv7_child_pna_labels[["id"]] <- qv7_child_clean_labels[["id"]]

    pna_label <- "Note: prefer not to answer (pna) marked NA - see pna database for which were pna rather than missing NA"

    ## Fix 99/skip in ctc and cwc. Note, only ctc items 1-8 have skip levels
    level99_issue_catvars <- names(qv7_child_clean)[c(19:26, 35:39)]

    for (v in 1:length(level99_issue_catvars)) {
        # get variable name
        pvar <- level99_issue_catvars[v]

        # if has '99' value, create new pna variable marking pna == 1
        if (is.element(99, qv7_child_clean[[pvar]])) {
            pna_dat <- ifelse(is.na(qv7_child_clean[[pvar]]), 0, ifelse(qv7_child_clean[[pvar]] == 99, 1, 0))

            new_pna <- length(names(qv7_child_pna)) + 1
            qv7_child_pna[[new_pna]] <- pna_dat

            names(qv7_child_pna)[new_pna] <- paste0(pvar, "_pna")

            # add label to pna database
            qv7_child_pna_labels[[paste0(pvar, "_pna")]] <- paste0("prefer not to answer marked for variable ", pvar, ": ", qv7_child_clean_labels[[pvar]])

            # update true data label (only want to pna label if needed)
            qv7_child_clean_labels[[pvar]] <- paste0(qv7_child_clean_labels[[pvar]],  " -- ", pna_label)

        }

        # drop 99 level label labels only update if had 99 - done in if statement above
        qv7_child_clean[[pvar]] <- sjlabelled::remove_labels(qv7_child_clean[[pvar]], labels = "Don't want to answer")

        # extract variable attributes
        pvar_attr <- attributes(qv7_child_clean[[pvar]])

        # replace 99 values
        qv7_child_clean[[pvar]] <- ifelse(is.na(qv7_child_clean[[pvar]]) | qv7_child_clean[[pvar]] == 99, NA, qv7_child_clean[[pvar]])

        # replace attributes
        attributes(qv7_child_clean[[pvar]]) <- pvar_attr
    }


    # re-level ctc questions so that 99 - skip is changed to -99
    ctc_names <- names(qv7_child_clean)[19:34]

    for (var in 1:length(ctc_names)) {
        var_name <- as.character(ctc_names[var])

        qv7_child_clean[[var_name]] <- sjlabelled::set_labels(qv7_child_clean[[var_name]], labels = c(`Not at all` = 1, `A little` = 2, `Not sure/in the middle` = 3, `Somewhat` = 4, `A lot` = 5, `Skip` = -99))

        set_attr <- attributes(qv7_child_clean[[var_name]])

        qv7_child_clean[[var_name]] <- ifelse(is.na(qv7_child_clean[[var_name]]),  NA, ifelse(qv7_child_clean[[var_name]] == 99, -99, qv7_child_clean[[var_name]]))

        attributes(qv7_child_clean[[var_name]]) <- set_attr
        qv7_child_clean_labels[[var_name]] <- paste0(qv7_child_clean_labels[[var_name]], " - re-leveled in R so skip = -99")
    }

    ## Fix continuous variables ####
    level99_issue_contvars <- names(qv7_child_clean)[c(13, 15)]

    #update 1 label
    qv7_child_clean_labels[['pds_5fd']] <- paste0(qv7_child_clean_labels[['pds_5fd']], 'in weeks')

    for (v in 1:length(level99_issue_contvars)) {
        # get variable name
        pvar <- level99_issue_contvars[v]

        # if has '99' value, create new pna variable marking pna == 1
        if (is.element(99, qv7_child_clean[[pvar]])) {
            pna_dat <- ifelse(is.na(qv7_child_clean[[pvar]]), 0, ifelse(qv7_child_clean[[pvar]] == 99, 1, 0))

            new_pna <- length(names(qv1_parent_pna)) + 1
            qv1_parent_pna[[new_pna]] <- pna_dat

            names(qv1_parent_pna)[new_pna] <- paste0(pvar, "_pna")

            # add label to pna database
            qv1_parent_pna_labels[[paste0(pvar, "_pna")]] <- paste0("prefer not to answer marked for variable ", pvar, ": ", qv7_child_clean_labels[[pvar]])

            # update true data label (only want to pna label if needed)
            qv7_child_clean_labels[[pvar]] <- paste0(qv7_child_clean_labels[[pvar]], " -- ", pna_label)
        }

        # convert 99 to NA and make numeric variable labels only update if had 99 - done in if statement above
        qv7_child_clean[[pvar]] <- ifelse(qv7_child_clean[[pvar]] == 99, NA, as.numeric(qv7_child_clean[[pvar]]))
    }

    # 8) fix labels ####

    ## remove 'V7' and 'V1' in labels
    for (var in 1:length(names(qv7_child_clean))) {
        var_name <- as.character(names(qv7_child_clean)[var])

        if (grepl("Visit 7 ", qv7_child_clean_labels[[var_name]], fixed = TRUE)) {
            qv7_child_clean_labels[[var_name]] <- gsub("Visit 7 ", "", qv7_child_clean_labels[[var_name]])
        }

        if (grepl("V1", qv7_child_clean_labels[[var_name]], fixed = TRUE)) {
            qv7_child_clean_labels[[var_name]] <- gsub("V1 ", "", qv7_child_clean_labels[[var_name]])
        }
    }

    ## remove trailing '... - 1' from labels
    for (var in 1:length(names(qv7_child_clean))) {
        var_name <- as.character(names(qv7_child_clean)[var])
        if (grepl(" - 1", qv7_child_clean_labels[[var_name]], fixed = TRUE)) {
            qv7_child_clean_labels[[var_name]] <- gsub("\\ - 1.*", "", qv7_child_clean_labels[[var_name]])
        }
    }


    #### 9) Format for export ####

    ## 9a) add attributes to pna data
    qv7_child_pna[2:ncol(qv7_child_pna)] <- as.data.frame(lapply(qv7_child_pna[2:ncol(qv7_child_pna)], function(x) sjlabelled::add_labels(x, labels = c(`Did not skip due to prefer not to answer` = 0, `Prefer not to answer` = 1))))

    for (v in 2:ncol(qv7_child_pna)){
        class(qv7_child_pna[[v]]) <- c("haven_labelled", "vctrs_vctr", "double")
    }

    #put data in order of participant ID for ease
    qv7_child_clean <- qv7_child_clean[order(qv7_child_clean[["id"]]), ]

    qv7_child_pna <- qv7_child_pna[order(qv7_child_pna[["id"]]), ]

    # make sure the variable labels match in the dataset
    qv7_child_clean = sjlabelled::set_label(qv7_child_clean, label = matrix(unlist(qv7_child_clean_labels, use.names = FALSE)))

    qv7_child_pna = sjlabelled::set_label(qv7_child_pna, label = matrix(unlist(qv7_child_pna_labels, use.names = FALSE)))

    # make list of data frame and associated labels
    qv7_child <- list(data = qv7_child_clean, dict = qv7_child_clean_labels, pna_data = qv7_child_pna, pna_dict = qv7_child_pna_labels)


    return(qv7_child)
}
