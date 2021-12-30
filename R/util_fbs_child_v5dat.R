#' util_fbs_child_v5dat: Process raw qualtrics visit 5 data for the child
#'
#' This function loads the .sav raw data file for the child visit 5 data that was collected via Qualtrics and cleans the data. Cleaning the data involves:
#' 1) extracting all variable descriptions,
#' 2) selecting relevant data columns,
#' 3) removing all practice events (e.g., 999)
#' 4) re-ordering and re-name data columns
#' 5) reformatting dates to be appropriate and computer readable: YYYY-MM-DD
#' 6) re-calculate manual variables
#' 7) fix 99's / create prefer not to answer database
#' 8) random fixes to factor level names and variable descriptions
#'
#' The databases MUST follow the naming convention: Child_V5_YYYY-MM-DD.sav
#'
#' @inheritParams util_fbs_parent_v1dat
#' @inheritParams util_fbs_parent_v1dat
#'
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from child visit 5 Qualtrics; 2) dict: all variable descriptions;; 3) pna_data: data.frame marking participants who 'prefered not to answer' (pna) specific questions; and 4) pna_dict: all variable descriptions for pna_data
#'
#' @examples
#' #if in same working directory as data:
#' child_v5_dat <- util_fbs_child_v5dat('Child_V5')
#'
#' \dontrun{
#' #file_pattern must be a string. The following will not run:
#' child_v5_dat <- util_fbs_child_v5dat(Child_V5)
#'
#' #file_pattern must have the respondent ('Child') and visit number ('V5'). If just enter 'Child', the script will not run because it will return multiple files for different parent visits. The following will not run:
#' child_v5_dat <- util_fbs_child_v5dat('Child')
#' }
#'
#'
#' @export
#'
util_fbs_child_v5dat <- function(file_pattern, data_path) {

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
        qv5_child_path <- list.files(path = data_path, pattern = file_pattern, full.names = TRUE)
    } else {
        qv5_child_path <- paste0(pattern = file_pattern, full.names = TRUE)
    }

    # check number of files found
    if (length(qv5_child_path) > 1) {
        stop("More than one file matched the file_pattern. Be sure thefile_pattern specifies both the respondent (Parent/Child) and visit number (V#). If have more than 1 file matching the pattern in the directory, may need to move to enter a more specific file_pattern than is standard.")
    } else if (length(qv5_child_path) == 0) {
        stop('No files found. Be sure the data_path and file_pattern are correct and that the file exists')
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
        if (isTRUE(datapath_arg)) {
            stop("File does not exist. Check date_str and data_path entered")
        } else {
            stop("File does not exist. Check date_str and that the data exists in current working directory")
        }
    }

    #### 3. Clean Data #####

    # 1) extract variable labels/descriptions ####
    qv5_child_labels <- lapply(qv5_child_dat, function(x) attributes(x)$label)

    # 2) selecting relevant data columns ####
    qv5_child_clean <- qv5_child_dat[c(1, 18, 22:30, 35:37, 39:77, 82:89, 91:131, 133:168, 170:199, 201, 80:81)]

    ## update labels
    qv5_child_clean_labels <- qv5_child_labels[c(1, 18, 22:30, 35:37, 39:77, 82:89, 91:131, 133:168, 170:199, 201, 80:81)]


    # 3) removing all practice events (e.g., 999) ####
    qv5_child_clean <- qv5_child_clean[!is.na(qv5_child_clean[["ID"]]) & qv5_child_clean[["ID"]] < 999, ]

    # 4) re-ordering and re-name data columns ####

    # general order: 1) child information (ID. date), 2) freddies, 3) food VAS 4) intakes (meal, meal duration), 5) LOC, CtC, Interoception, Mock 6) notes

    qv5_child_clean <- qv5_child_clean[c(2, 1, 139:140, 3:14, 141:165, 15:53, 170:171, 54:60, 62:68, 70:76, 78:84, 86:92, 94:101, 103:110, 112:119, 121:128, 130:137, 61, 69, 77, 85, 93, 102, 111, 120, 129, 138, 166:169)]

    qv5_child_clean_labels <- qv5_child_clean_labels[c(2, 1, 139:140, 3:14, 141:165, 15:53, 170:171, 54:60, 62:68, 70:76, 78:84, 86:92, 94:101, 103:110, 112:119, 121:128, 130:137, 61, 69, 77, 85, 93, 102, 111, 120, 129, 138, 166:169)]

    ## make lower case
    names(qv5_child_clean) <- tolower(names(qv5_child_clean))

    ## re-name variables -- remove v5 prefix
    for (var in 1:length(names(qv5_child_clean))) {
        var_name <- as.character(names(qv5_child_clean)[var])

        # remove v5 prefix from labels
        if (grepl("v5_", var_name, fixed = TRUE)) {
            names(qv5_child_clean)[var] <- gsub("v5_", "", names(qv5_child_clean)[var])
        }

        # remove v5 prefix from labels
        if (grepl("v5", var_name, fixed = TRUE)) {
            names(qv5_child_clean)[var] <- gsub("v5", "", names(qv5_child_clean)[var])
        }
    }

    ## re-name variables
    names(qv5_child_clean)[c(2:41)] <- c("start_date", "freddy_pre_meal", "freddy_post_meal", "vas_mac_cheese", "vas_chkn_nug", "vas_broccoli", "vas_grape", "vas_water", "rank_mac_cheese", "rank_chkn_nug", "rank_broccoli", "rank_grape",  "meal_start", "meal_end", "meal_dur", "noplate_chkn_nug_g", "plate_chkn_nug_g", "post_chkn_nug_g", "consumed_chkn_nug_g", "noplate_mac_cheese_g", "plate_mac_cheese_g",  "post_mac_cheese_g", "consumed_mac_cheese_g", "noplate_grapes_g", "plate_grapes_g", "post_grapes_g", "consumed_grapes_g", "noplate_margerine_g", "noplate_broccoli_g", "plate_broccoli_g", "post_broccoli_g", "consumed_broccoli_g", "noplate_ketchup_g", "plate_ketchup_g", "post_ketchup_g", "consumed_ketchup_g", "noplate_water_g", "plate_water_g", "post_water_g", "consumed_water_g")

    names(qv5_child_clean)[c(81:171)] <- c("hrv_dur", "hrv_starttime", "intero_prac_start_target", "intero_prac_start_actual", "intero_prac_stop_target", "intero_prac_stop_actual",  "intero_prac_hbcount", "intero_prac_dur", "intero_prac_starttime", "intero_prac2_start_target", "intero_prac2_start_actual", "intero_prac2_stop_target", "intero_prac2_stop_actual", "intero_prac2_hbcount", "intero_prac2_dur", "intero_prac2_starttime", "intero_15s_start_target", "intero_15s_start_actual", "intero_15s_stop_target", "intero_15s_stop_actual", "intero_15s_hbcount", "intero_15s_dur", "intero_15s_starttime", "intero_20s_start_target", "intero_20s_start_actual", "intero_20s_stop_target", "intero_20s_stop_actual", "intero_20s_hbcount", "intero_20s_dur", "intero_20s_starttime", "intero_18s_start_target", "intero_18s_start_actual", "intero_18s_stop_target", "intero_18s_stop_actual", "intero_18s_hbcount", "intero_18s_dur", "intero_18s_starttime",  "intero_pulse_prac_start_target", "intero_pulse_prac_start_actual", "intero_pulse_prac_stop_target", "intero_pulse_prac_stop_actual", "intero_pulse_prac_hbcount", "intero_pulse_prac_dur", "intero_pulse_prac_starttime", "intero_pulse_prac_loc", "intero_pulse_prac2_start_target", "intero_pulse_prac2_start_actual", "intero_pulse_prac2_stop_target", "intero_pulse_prac2_stop_actual", "intero_pulse_prac2_hbcount", "intero_pulse_prac2_dur", "intero_pulse_prac2_starttime", "intero_pulse_prac2_loc", "intero_pulse_15s_start_target",  "intero_pulse_15s_start_actual", "intero_pulse_15s_stop_target", "intero_pulse_15s_stop_actual",  "intero_pulse_15s_hbcount", "intero_pulse_15s_dur",  "intero_pulse_15s_starttime", "intero_pulse_15s_loc", "intero_pulse_20s_start_target", "intero_pulse_20s_start_actual", "intero_pulse_20s_stop_target", "intero_pulse_20s_stop_actual", "intero_pulse_20s_hbcount", "intero_pulse_20s_dur", "intero_pulse_20s_starttime", "intero_pulse_20s_loc",  "intero_pulse_18s_start_target", "intero_pulse_18s_start_actual", "intero_pulse_18s_stop_target", "intero_pulse_18s_stop_actual", "intero_pulse_18s_hbcount", "intero_pulse_18s_dur", "intero_pulse_18s_starttime", "intero_pulse_18s_loc", "intero_prac_notes", "intero_prac2_notes", "intero_15s_notes", "intero_20s_notes", "intero_18s_notes", "intero_pulse_prac_notes", "intero_pulse_prac2_notes", "intero_pulse_15s_notes", "intero_pulse_20s_notes", "intero_pulse_18s_notes", "food_initials", "extra_mock_needed", "mockscan2_notes",  "child_notes")

    ## update data labels
    names(qv5_child_clean_labels) <- names(qv5_child_clean)

    # 5) reformatting dates to be appropriate and computer readable YYYY-MM-DD  ####
    qv5_child_clean[["start_date"]] <- lubridate::ymd(as.Date(qv5_child_clean[["start_date"]]))
    qv5_child_clean_labels[["start_date"]] <- "start_date from qualtrics survey meta-data converted to format yyyy-mm-dd in R"

    #make freaddy fullness numeric
    qv5_child_clean[c(3:4, 10:13, 16)] <- sapply(qv5_child_clean[c(3:4, 10:13, 16)], FUN = as.numeric)


    # 6) re-calculate manual variables ####

    ## re-calculate all intake values

    # get all intake variables
    intake_vars <- names(qv5_child_clean)[c(17:41)]

    # make all intake variables numeric NOTE - there is a whole row I am not
    # manually fixing as every value has ',' instead of '.'
    for (var in 1:length(intake_vars)) {
        var_name <- intake_vars[[var]]

        qv5_child_clean[[var_name]] <- ifelse(qv5_child_clean[[var_name]] == "-" |  qv5_child_clean[[var_name]] == "NA", NA, qv5_child_clean[[var_name]])

        if (is.character(qv5_child_clean[[var_name]])) {
            qv5_child_clean[[var_name]] <- as.numeric(qv5_child_clean[[var_name]])
        }
    }

    # get all foods served - extract prefix and thne postfix in name
    food_strs_g <- unique(sapply(intake_vars, function(x) gsub(".*plate_|.*post_|.*consumed_",
        "\\1", x), USE.NAMES = FALSE))
    food_strs <- unique(sapply(food_strs_g, function(x) gsub("_g.*", "\\1", x),
        USE.NAMES = FALSE))

    # loop through foods
    for (f in 1:length(food_strs)) {

        # no post weights for margerine
        if (food_strs[f] != "margerine") {
            # get variable names for plate* and post* weights
            plate_var <- paste0("plate_", food_strs[f], "_g")
            post_var <- paste0("post_", food_strs[f], "_g")
            consumed_var <- paste0("consumed_", food_strs[f], "_g")

            # calculate amount consumed
            qv5_child_clean[[consumed_var]] <- qv5_child_clean[[plate_var]] - qv5_child_clean[[post_var]]

            qv5_child_clean[[consumed_var]] <- ifelse(qv5_child_clean[[consumed_var]] < 0, 0, qv5_child_clean[[consumed_var]])

            # update labels
            qv5_child_clean_labels[[consumed_var]] <- paste0(qv5_child_clean_labels[[consumed_var]], " - recalcuated difference in R with values < 0 set to 0")
        }
    }

    # 7) fix 99's and relevel ####

    ## check for labels/99 option: 1) if 99's exist, make a 'prefer not to answer' (pna) variable to go in pna database, 2) replace 99's with NA and make variable numeric

    ## make pna database
    qv5_child_pna <- data.frame(id = qv5_child_clean[["id"]])
    qv5_child_pna_labels <- lapply(qv5_child_pna, function(x) attributes(x)$label)
    qv5_child_pna_labels[["id"]] <- qv5_child_clean_labels[["id"]]

    pna_label <- "Note: prefer not to answer (pna) marked NA - see pna database for which were pna rather than missing NA"

    ## Fix 99/skip in LOC survey. Note: CTC has allowable skips in scoring so keep.
    level99_issue_catvars <- names(qv5_child_clean)[c(42:64)]

    for (v in 1:length(level99_issue_catvars)) {
        # get variable name
        pvar <- level99_issue_catvars[v]

        # if has '99' value, create new pna variable marking pna == 1
        if (is.element(99, qv5_child_clean[[pvar]])) {
            pna_dat <- ifelse(is.na(qv5_child_clean[[pvar]]), 0, ifelse(qv5_child_clean[[pvar]] == 99, 1, 0))

            new_pna <- length(names(qv5_child_pna)) + 1
            qv5_child_pna[[new_pna]] <- pna_dat

            names(qv5_child_pna)[new_pna] <- paste0(pvar, "_pna")

            # add label to pna database
            qv5_child_pna_labels[[paste0(pvar, "_pna")]] <- paste0("prefer not to answer marked for variable ", pvar, ": ", qv5_child_clean_labels[[pvar]])

            # update true data label (only want to pna label if needed)
            qv5_child_clean_labels[[pvar]] <- paste0(qv5_child_clean_labels[[pvar]], " -- ", pna_label)

        }

        # drop 99 level label labels only update if had 99 - done in if statement above
        qv5_child_clean[[pvar]] <- sjlabelled::remove_labels(qv5_child_clean[[pvar]], labels = "Don't want to answer")

        # extract variable attributes
        pvar_attr <- attributes(qv5_child_clean[[pvar]])

        # replace 99 values
        qv5_child_clean[[pvar]] <- ifelse(is.na(qv5_child_clean[[pvar]]) | qv5_child_clean[[pvar]] == 99, NA, qv5_child_clean[[pvar]])

        # replace attributes
        attributes(qv5_child_clean[[pvar]]) <- pvar_attr
    }

    # make loc2a-loc2c numeric
    qv5_child_clean[43:45] <- sapply(qv5_child_clean[43:45], FUN = as.numeric)

    ## Fix 99 in interoception heartbeat count questions
    ### Note, both "don't remember" and 99 heartbeats were coded as 99 in qualtrics. However, experience with data collection and distribution of numbers suggests that children did not count 99 heartbeats and that 99s are indicative of "don't remember" responses. Therefore: 1) if 99's exist, make a 'prefer not to answer' (pna) variable to go in pna database, 2) replace 99's with NA and make variable numeric

    level99_issue_contvars <- names(qv5_child_clean)[c(87, 94, 101, 108, 115, 122, 130, 138, 146, 154)]

    for (v in 1:length(level99_issue_contvars)) {
        # get variable name
        pvar <- level99_issue_contvars[v]

        # if has '99' value, changes to NA
        if (is.element(99, qv5_child_clean[[pvar]])) {
            pna_dat <- ifelse(is.na(qv5_child_clean[[pvar]]), 0, ifelse(qv5_child_clean[[pvar]] == 99, 1, 0))

            if (length(names(qv5_child_pna)) == 0) {
                new_pna <- 1
                qv5_child_pna <- data.frame(pna_dat)
            } else {
                new_pna <- length(names(qv5_child_pna)) + 1
                qv5_child_pna[[new_pna]] <- pna_dat
            }

            names(qv5_child_pna)[new_pna] <- paste0(pvar, "_pna")

            # add label to pna database
            qv5_child_pna_labels[[paste0(pvar, "_pna")]] <- paste0("prefer not to answer marked for variable ", pvar, ": ", qv5_child_clean_labels[[pvar]])

            # update true data label (only want to pna label if needed)
            qv5_child_clean_labels[[pvar]] <- paste0(qv5_child_clean_labels[[pvar]], " -- ", pna_label)
        }

        # convert 99 to NA and make numeric variable labels only update if had 99 - done in if statement above
        qv5_child_clean[[pvar]] <- ifelse(qv5_child_clean[[pvar]] == 99, NA, as.numeric(qv5_child_clean[[pvar]]))
    }

    ###7b) re-level ctc
    # re-level ctc questions so that 99 - skip is changed to -99
    ctc_names <- names(qv5_child_clean)[65:80]
    for (var in 1:length(ctc_names)) {
        var_name <- as.character(ctc_names[var])

        qv5_child_clean[[var_name]] <- sjlabelled::set_labels(qv5_child_clean[[var_name]], labels = c(`Not at all` = 1, `A little` = 2, `Not sure/in the middle` = 3, `Somewhat` = 4, `A lot` = 5, `Skip` = -99))
        set_attr <- attributes(qv5_child_clean[[var_name]])

        qv5_child_clean[[var_name]] <- ifelse(is.na(qv5_child_clean[[var_name]]),  NA, ifelse(qv5_child_clean[[var_name]] == 99, -99, qv5_child_clean[[var_name]]))

        attributes(qv5_child_clean[[var_name]]) <- set_attr
        qv5_child_clean_labels[[var_name]] <- paste0(qv5_child_clean_labels[[var_name]], " - re-leveled in R so skip = -99")
    }

    # 8) random fixes to factor level names and variable descriptions ####
    qv5_child_clean_labels[["meal_start"]] <- "Meal start time"
    qv5_child_clean_labels[["meal_end"]] <- "Meal end time"

    intro_pulse_names <- names(qv5_child_clean)[c(118:157, 163:167)]
    for (var in 1:length(intro_pulse_names)) {
        var_name <- as.character(intro_pulse_names[var])
        qv5_child_clean_labels[[var_name]] <- paste0("with pulse - ", qv5_child_clean_labels[[var_name]])
    }

    for (var in 1:length(names(qv5_child_clean))) {
        var_name <- as.character(names(qv5_child_clean)[var])

        # remove v5 prefix from labels
        if (grepl("Visit 5", qv5_child_clean_labels[[var_name]], fixed = TRUE)) {
            qv5_child_clean_labels[[var_name]] <- gsub("Visit 5 ", "", qv5_child_clean_labels[[var_name]])
        }

        if (grepl("V5 -", qv5_child_clean_labels[[var_name]], fixed = TRUE)) {
            qv5_child_clean_labels[[var_name]] <- gsub("V5 - ", "", qv5_child_clean_labels[[var_name]])
        }

        if (grepl("V5", qv5_child_clean_labels[[var_name]], fixed = TRUE)) {
            qv5_child_clean_labels[[var_name]] <- gsub("V5 ", "", qv5_child_clean_labels[[var_name]])
        }

        if (grepl("Visit 7", qv5_child_clean_labels[[var_name]], fixed = TRUE)) {
            qv5_child_clean_labels[[var_name]] <- gsub("Visit 7 ", "", qv5_child_clean_labels[[var_name]])
        }
    }

    # 9) Format for export ####

    ## 9a) add attributes to pna data
    qv5_child_pna[2:ncol(qv5_child_pna)] <- as.data.frame(lapply(qv5_child_pna[2:ncol(qv5_child_pna)], function(x) sjlabelled::add_labels(x, labels = c(`Did not skip due to prefer not to answer` = 0, `Prefer not to answer` = 1))))

    for (v in 2:ncol(qv5_child_pna)){
        class(qv5_child_pna[[v]]) <- c("haven_labelled", "vctrs_vctr", "double")
    }

    #put data in order of participant ID for ease
    qv5_child_clean <- qv5_child_clean[order(qv5_child_clean[["id"]]), ]

    qv5_child_pna <- qv5_child_pna[order(qv5_child_pna[["id"]]), ]

    # make sure the variable labels match in the dataset
    qv5_child_clean = sjlabelled::set_label(qv5_child_clean, label = matrix(unlist(qv5_child_clean_labels, use.names = FALSE)))

    qv5_child_pna = sjlabelled::set_label(qv5_child_pna, label = matrix(unlist(qv5_child_pna_labels, use.names = FALSE)))

    ## make list of data frame and associated labels
    qv5_child <- list(data = qv5_child_clean, dict = qv5_child_clean_labels, pna_data = qv5_child_pna, pna_dict = qv5_child_pna_labels)

    ## want an export options??

    return(qv5_child)
}
