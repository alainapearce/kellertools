#' util_child_v5dat_home: Process raw qualtrics visit 5 home data for the child
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
#' @inheritParams util_parent_v1dat
#' @inheritParams util_parent_v1dat
#'
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from child visit 5 Qualtrics collected in the home; 2) dict: all variable descriptions; 3) pna_data: data.frame marking participants who 'prefered not to answer' (pna) specific questions; and 4) pna_dict: all variable descriptions for pna_data
#'
#' @examples
#' #if in same working directory as data:
#' ch_v5_dat_home <- util_child_v5dat_home('2021-10-11')
#'
#' \dontrun{
#' #date must be a string. The following will not run:
#' ch_v5_dat_home <- util_child_v5dat_home(2021-10-11)
#'
#' #date must match the file name - for file named 'Child_V5_Home_2021_09_16', the
#' following will not run:
#' ch_v5_dat_home <- util_child_v5dat_home('2021_10_11')
#' }
#'
#'
#' @export
#'
util_child_v5dat_home <- function(date_str, data_path) {

    #### 1. Set up/initial checks #####

    # check that date_str exist and is a string

    datestr_arg <- methods::hasArg(date_str)

    if (isTRUE(datestr_arg) & !is.character(date_str)) {
        stop("date_str must be enter as a string: e.g., '2021_10_11'")
    } else if (isFALSE(datestr_arg)) {
        stop("date_str must set to the data string from the child visit 5 file name: e.g., '2021_09_16'")
    }

    # check that file exists
    datapath_arg <- methods::hasArg(data_path)

    if (isTRUE(datapath_arg)) {
        if (!is.character(date_str)) {
            stop("data_path must be enter as a string: e.g., '.../Participant_Data/untouchedRaw/util_Raw/'")
        }
    }


    #### 2. Load Data #####

    if (isTRUE(datapath_arg)) {
        qv5_child_path <- paste0(data_path, "/Final_CovidAtHome/Child_V5_Home_",
            date_str, ".sav")
    } else {
        qv5_child_path <- paste0("Final_CovidAtHome/Child_V5_Home", date_str,
            ".sav")
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
    # (ID, start date), 2) DD

    qv5_child_clean <- qv5_child_clean[c(2, 1, 3:18)]

    qv5_child_clean_labels <- qv5_child_clean_labels[c(2, 1, 3:18)]

    ## manually update variable names
    names(qv5_child_clean) <- c("id", "start_date", "ctc1", "ctc2", "ctc3", "ctc4",
        "ctc5", "ctc6", "ctc7", "ctc8", "ctc9", "ctc10", "ctc11", "ctc12", "ctc13",
        "ctc14", "ctc15", "ctc16")

    ## update data labels
    names(qv5_child_clean_labels) <- names(qv5_child_clean)

    # 5) reformatting dates to be appropriate and computer readable ####
    # YYYY-MM-DD
    qv5_child_clean[["start_date"]] <- lubridate::ymd(as.Date(qv5_child_clean[["start_date"]]))
    qv5_child_clean_labels[["start_date"]] <- "start_date from qualtrics survey meta-data converted to format yyyy-mm-dd in R"

    # 6) re-calculate manual variables #### no manual variables to calculate

    # 7) fix 99's ####

    ## check for labels/99 option: 1) if 99's exist, make a 'prefer not to answer'
    ## (pna) variable to go in pna database, 2) replace 99's with NA and make
    ## variable numeric

    ## make pna database
    qv5_child_pna <- data.frame(id = qv5_child_clean[["ID"]])
    qv5_child_pna_labels <- lapply(qv5_child_pna, function(x) attributes(x)$label)
    qv5_child_pna_labels[["id"]] <- qv5_child_pna_labels[["id"]]

    pna_label <- "Note: prefer not to answer (pna) marked NA - see pna database for which were pna rather than missing NA"

    ## Fix 99/skip in ctc survey. Note, only ctc items 1-8 have skip levels
    level99_issue_catvars <- names(qv5_child_clean)[c(3:10)]

    for (v in 1:length(level99_issue_catvars)) {
        # get variable name
        pvar <- level99_issue_catvars[v]

        # if has '99' value, create new pna variable marking pna == 1
        if (is.element(99, qv5_child_clean[[pvar]])) {
            pna_dat <- ifelse(is.na(qv5_child_clean[[pvar]]), 0, ifelse(qv5_child_clean[[pvar]] ==
                99, 1, 0))

            new_pna <- length(names(qv5_child_pna)) + 1
            qv5_child_pna[[new_pna]] <- pna_dat

            names(qv5_child_pna)[new_pna] <- paste0(pvar, "_pna")

            # add label to pna database
            qv5_child_pna_labels[[paste0(pvar, "_pna")]] <- paste0("prefer not to answer marked for variable ",
                pvar, ": ", qv5_child_clean_labels[[pvar]])

            # update true data label (only want to pna label if needed)
            qv5_child_clean_labels[[pvar]] <- paste0(qv5_child_clean_labels[[pvar]],
                " -- ", pna_label)

        }

        # drop 99 level label labels only update if had 99 - done in if statement
        # above
        qv5_child_clean[[pvar]] <- sjlabelled::remove_labels(qv5_child_clean[[pvar]],
            labels = "Don't want to answer")

        # extract variable attributes
        pvar_attr <- attributes(qv5_child_clean[[pvar]])

        # replace 99 values
        qv5_child_clean[[pvar]] <- ifelse(is.na(qv5_child_clean[[pvar]]) | qv5_child_clean[[pvar]] ==
            99, NA, qv5_child_clean[[pvar]])

        # replace attributes
        attributes(qv5_child_clean[[pvar]]) <- pvar_attr
    }

    # 7b) Relevel ctc FIGURE THIS OUT. ARE SOME QUESTIONS REVERSE ORDERED?

    # 8) random fixes to factor level names and variable descriptions CHANGE
    # VARIABLE DESCRIPTION OF CTC FROM VISIT '7' to '5'

    ## id label
    qv5_child_clean_labels[["id"]] <- "participant ID"

    #### 9) Format for export #### put data in order of participant ID for ease
    qv5_child_clean <- qv5_child_clean[order(qv5_child_clean[["id"]]), ]

    qv5_child_pna <- qv5_child_pna[order(qv5_child_pna[["id"]]), ]

    # make sure the variable labels match in the dataset
    qv5_child_clean = sjlabelled::set_label(qv5_child_clean, label = matrix(unlist(qv5_child_clean_labels, use.names = FALSE)))

    qv5_child_pna = sjlabelled::set_label(qv5_child_pna, label = matrix(unlist(qv5_child_pna_labels, use.names = FALSE)))

    ## make list of data frame and associated labels
    qv5_child <- list(data = qv5_child_clean, dict = qv5_child_clean_labels)

    ## want an export options??

    return(qv5_child)
}
