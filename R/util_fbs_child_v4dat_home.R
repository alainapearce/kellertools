#' util_fbs_child_v4dat_home: Process raw qualtrics visit 4 home data for the child
#'
#' This function loads the .sav raw data file for the child visit 4 data that was collected via Qualtrics at the child's home when the procedure was split due to covid and cleans the data. Cleaning the data involves:
#' 1) extracting all variable descriptions,
#' 2) selecting relevant data columns,
#' 3) removing all practice events (e.g., 999)
#' 4) re-ordering and re-name data columns
#' 5) reformatting dates to be appropriate and computer readable: YYYY-MM-DD
#' 6) re-calculate manual variables
#' 7) fix 99's / create prefer not to answer database
#' 8) random fixes to factor level names and variable descriptions
#'
#' The databases MUST follow the naming convention: Child_V4_Home_YYYY-MM-DD.sav
#'
#' @inheritParams util_fbs_parent_v1dat
#' @inheritParams util_fbs_parent_v1dat
#'
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from child visit 4 Qualtrics; 2) dict: all variable descriptions; 3) pna_data: data.frame marking participants who 'prefered not to answer' (pna) specific questions; and 4) pna_dict: all variable descriptions for pna_data
#'
#' @examples
#' #if in same working directory as data. Note - there is no need to add the COVID protocol (i.e., 'Home' or 'Lab') to file_pattern:
#' child_v4_dat_home <- util_fbs_child_v4dat_home('Child_V4')
#'
#' \dontrun{
#' #file_pattern must be a string. The following will not run:
#' child_v4_dat_home <- util_fbs_child_v4dat_home(Child_V4)
#'
#' #file_pattern must have the respondent ('Child') and visit number ('V4'). If just enter 'Child', the script will not run because it will return multiple files for different parent visits. The following will not run:
#' child_v4_dat_home <- util_fbs_child_v4dat_home('Child')
#' }
#'
#'
#' @export
#'
util_fbs_child_v4dat_home <- function(file_pattern, data_path) {

    #### 1. Set up/initial checks #####

    # check that file_pattern exist and is a string

    filepat_arg <- methods::hasArg(file_pattern)

    if (isTRUE(filepat_arg) & !is.character(file_pattern)) {
        stop("file_pattern must be entered as a string: e.g., 'Child_V4'")
    } else if (isFALSE(filepat_arg)) {
        stop("file_pattern must set to the a string matching the name of the raw data file for child visit: e.g., 'Child_V4'")
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
        qv4_child_pathlist <- list.files(path = paste0(data_path, '/Final_Covid/'), pattern = paste0(file_pattern, '_Home'), full.names = TRUE)

        #if no files found, check direct data_path entered
        if (length(qv4_child_pathlist) == 0) {
            qv4_child_pathlist <- list.files(path = data_path, pattern = paste0(file_pattern, '_Home'), full.names = TRUE)
        }
    } else {
        qv4_child_pathlist <- paste0(pattern = paste0(file_pattern, '_Home'), full.names = TRUE)
    }

    # check number of files found
    if (length(qv4_child_pathlist) > 1) {
        stop("More than one file matched after adding '_Home' to the file_pattern . Be sure thefile_pattern specifies both the respondent (Parent/Child) and visit number (V#). If have more than 1 file matching the pattern in the directory, may need to move to enter a more specific file_pattern than is standard.")
    } else if (length(qv4_child_pathlist) == 0) {
        stop("No files found after adding '_Home' to file_pattern. Be sure the data_path and file_pattern are correct and that the file exists.")
    } else {
        qv4_child_path <- qv4_child_pathlist
    }

    # check that file is of type '.sav'
    if (!grepl('.sav', qv4_child_path, fixed = TRUE)){
        stop("The file found is not an SPSS database (.sav)")
    }

    # check if file exists
    qv4_child_exists <- file.exists(qv4_child_path)

    # load data if it exists
    if (isTRUE(qv4_child_exists)) {
        qv4_child_dat <- as.data.frame(haven::read_spss(qv4_child_path))

    } else {

        if (isTRUE(datapath_arg)) {
            stop("File does not exist. Check file_pattern and data_path entered")
        } else {
            stop("File does not exist. Check file_pattern and that the data exists in current working directory")
        }
    }

    #### 3. Clean Data #####

    # 1) extract variable labels/descriptions ####
    qv4_child_labels <- lapply(qv4_child_dat, function(x) attributes(x)$label)

    # 2) selecting relevant data columns ####
    qv4_child_clean <- qv4_child_dat[c(1, 18:23, 25:38)]

    ## update labels
    qv4_child_clean_labels <- qv4_child_labels[c(1, 18:23, 25:38)]


    # 3) removing all practice events (e.g., 999) Note, ID variable is 'ID' ####
    qv4_child_clean <- qv4_child_clean[!is.na(qv4_child_clean[["ID"]]) & qv4_child_clean[["ID"]] < 999, ]

    # 4) re-ordering and re-name data columns ####

    # general order: 1) child information (ID, start date), 2) DD

    qv4_child_clean <- qv4_child_clean[c(2, 1, 3:21)]

    qv4_child_clean_labels <- qv4_child_clean_labels[c(2, 1, 3:21)]

    ## manually update variable names
    names(qv4_child_clean) <- c("id", "start_date", "cwc1", "cwc2", "cwc3", "cwc4", "cwc5", "cbis_perc_male", "cbis_ideal_male", "cbis_perc_female", "cbis_ideal_female", "psi_resp_mom1", "psi_resp_mom2", "psi_resp_mom3", "psi_resp_mom4", "psi_resp_mom5", "psi_resp_dad1", "psi_resp_dad2", "psi_resp_dad3", "psi_resp_dad4", "psi_resp_dad5")

    ## update data labels
    names(qv4_child_clean_labels) <- names(qv4_child_clean)

    # 5) reformatting dates to be appropriate and computer readable #### YYYY-MM-DD ####
    qv4_child_clean[["start_date"]] <- lubridate::ymd(as.Date(qv4_child_clean[["start_date"]]))
    qv4_child_clean_labels[["start_date"]] <- "start_date from qualtrics survey meta-data converted to format yyyy-mm-dd in R"

    # 6) fix 99's ####

    ## check for labels/99 option: 1) if 99's exist, make a 'prefer not to answer' (pna) variable to go in pna
    ## database, 2) replace 99's with NA and make variable numeric

    ## make pna database
    qv4_child_pna <- data.frame(id = qv4_child_clean[["id"]])
    qv4_child_pna_labels <- lapply(qv4_child_pna, function(x) attributes(x)$label)
    qv4_child_pna_labels[["id"]] <- qv4_child_clean_labels[["id"]]

    pna_label <- "Note: prefer not to answer (pna) marked NA - see pna database for which were pna rather than missing NA"

    ## Fix 99/Don't want to answer in CWC, CBIS, PSI - Parent Responsiveness (levels are OK starting with 1; all
    ## are categorical variables)
    level99_issue_catvars <- names(qv4_child_clean)[c(3:21)]

    for (v in 1:length(level99_issue_catvars)) {
        # get variable name
        pvar <- level99_issue_catvars[v]

        # if has '99' value, create new pna variable marking pna == 1
        if (is.element(99, qv4_child_clean[[pvar]])) {
            pna_dat <- ifelse(is.na(qv4_child_clean[[pvar]]), 0, ifelse(qv4_child_clean[[pvar]] == 99, 1, 0))

            new_pna <- length(names(qv4_child_pna)) + 1
            qv4_child_pna[[new_pna]] <- pna_dat

            names(qv4_child_pna)[new_pna] <- paste0(pvar, "_pna")

            # add label to pna database
            qv4_child_pna_labels[[paste0(pvar, "_pna")]] <- paste0("prefer not to answer marked for variable ", pvar,
                                                                   ": ", qv4_child_clean_labels[[pvar]])

            # update true data label (only want to pna label if needed)
            qv4_child_clean_labels[[pvar]] <- paste0(qv4_child_clean_labels[[pvar]], " -- ", pna_label)

        }

        # drop 99 level label labels only update if had 99 - done in if statement above
        qv4_child_clean[[pvar]] <- sjlabelled::remove_labels(qv4_child_clean[[pvar]], labels = "Don't want to answer")

        # extract variable attributes
        pvar_attr <- attributes(qv4_child_clean[[pvar]])

        # replace 99 values
        qv4_child_clean[[pvar]] <- ifelse(is.na(qv4_child_clean[[pvar]]) | qv4_child_clean[[pvar]] == 99, NA, qv4_child_clean[[pvar]])

        # replace attributes
        attributes(qv4_child_clean[[pvar]]) <- pvar_attr
    }

    # 7) random fixes to factor level names and variable descriptions ####

    ## id label
    qv4_child_clean_labels[["id"]] <- "participant ID"

    #### 8) Format for export ####

    ## 8a) add attributes to pna data
    qv4_child_pna[2:ncol(qv4_child_pna)] <- as.data.frame(lapply(qv4_child_pna[2:ncol(qv4_child_pna)], function(x) sjlabelled::add_labels(x, labels = c(`Did not skip due to prefer not to answer` = 0, `Prefer not to answer` = 1))))

    for (v in 2:ncol(qv4_child_pna)){
        class(qv4_child_pna[[v]]) <- c("haven_labelled", "vctrs_vctr", "double")
    }

    #put data in order of participant ID for ease
    qv4_child_clean <- qv4_child_clean[order(qv4_child_clean[["id"]]), ]

    qv4_child_pna <- qv4_child_pna[order(qv4_child_pna[["id"]]), ]

    # make sure the variable labels match in the dataset
    qv4_child_clean = sjlabelled::set_label(qv4_child_clean, label = matrix(unlist(qv4_child_clean_labels, use.names = FALSE)))

    qv4_child_pna = sjlabelled::set_label(qv4_child_pna, label = matrix(unlist(qv4_child_pna_labels, use.names = FALSE)))

    ## make list of data frame and associated labels
    qv4_child <- list(data = qv4_child_clean, dict = qv4_child_clean_labels, pna_data = qv4_child_pna, pna_dict = qv4_child_pna_labels)

    ## want an export options??

    return(qv4_child)
}
