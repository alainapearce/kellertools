#' util_fbs_child_v5dat_lab: Process raw qualtrics visit 5 data collected in the lab for the child
#'
#' This function loads the .sav raw data file for the child visit 5 data that was collected in the via Qualtrics in the lab when the procedure was split due to covid and cleans the data. Cleaning the data involves:
#' 1) extracting all variable descriptions,
#' 2) selecting relevant data columns,
#' 3) removing all practice events (e.g., 999)
#' 4) re-ordering and re-name data columns
#' 5) reformatting dates to be appropriate and computer readable: YYYY-MM-DD
#' 6) re-calculate manual variables
#' 7) re-ordering factor levels to start with value 0
#' 8) random fixes to factor level names and variable descriptions
#'
#' The databases MUST follow the naming convention: Child_V5_Lab_YYYY-MM-DD.sav
#'
#' @inheritParams util_fbs_parent_v1dat
#' @inheritParams util_fbs_parent_v1dat
#'
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from child visit 5 Qualtrics collected in the lab and 2) dict: all variable descriptions
#'
#' @examples
#' #if in same working directory as data. Note - there is no need to add the COVID protocol (i.e., 'Home' or 'Lab') to file_pattern:
#' child_v5_dat_lab <- util_fbs_child_v5dat_lab('Child_V5')
#'
#' \dontrun{
#' #file_pattern must be a string. The following will not run:
#' child_v5_dat_lab <- util_fbs_child_v5dat_lab(Child_V5)
#'
#' #file_pattern must have the respondent ('Child') and visit number ('V5'). If just enter 'Child', the script will not run because it will return multiple files for different parent visits. The following will not run:
#' child_v5_dat_lab <- util_fbs_child_v5dat_lab('Child')
#' }
#'
#'
#' @export
#'
util_fbs_child_v5dat_lab <- function(file_pattern, data_path) {

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
        #check pattern of directories specified in Data manual
        qv5_child_pathlist <- list.files(path = paste0(data_path, '/Final_Covid/'), pattern = paste0(file_pattern, '_Lab'), full.names = TRUE)

        #if no files found, check direct data_path entered
        if (length(qv5_child_pathlist) == 0) {
            qv5_child_pathlist <- list.files(path = data_path, pattern = paste0(file_pattern, '_Lab'), full.names = TRUE)
        }
    } else {
        qv5_child_pathlist <- paste0(pattern = paste0(file_pattern, '_Lab'), full.names = TRUE)
    }

    # check number of files found
    if (length(qv5_child_pathlist) > 1) {
        stop("More than one file matched after adding '_Lab' to the file_pattern . Be sure thefile_pattern specifies both the respondent (Parent/Child) and visit number (V#). If have more than 1 file matching the pattern in the directory, may need to move to enter a more specific file_pattern than is standard.")
    } else if (length(qv5_child_pathlist) == 0) {
        stop("No files found after adding '_Lab' to file_pattern. Be sure the data_path and file_pattern are correct and that the file exists.")
    } else {
        qv5_child_path <- qv5_child_pathlist
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
            stop("File does not exist. Check file_pattern and data_path entered")
        } else {
            stop("File does not exist. Check file_pattern and that the data exists in current working directory")
        }
    }

    #### 3. Clean Data #####

    # 1) extract variable labels/descriptions #####
    qv5_child_labels <- lapply(qv5_child_dat, function(x) attributes(x)$label)

    # 2) selecting relevant data columns #####
    qv5_child_clean <- qv5_child_dat[c(1, 18, 22:30, 36:38, 40:62, 65:74, 76:116, 118:153, 155:184, 186)]

    ## update labels #####
    qv5_child_clean_labels <- qv5_child_labels[c(1, 18, 22:30, 36:38, 40:62, 65:74, 76:116, 118:153, 155:184, 186)]

    # 3) removing all practice events (e.g., 999) #####
    qv5_child_clean <- qv5_child_clean[!is.na(qv5_child_clean[["ID"]]) & qv5_child_clean[["ID"]] <
                                           999, ]

    # 4) re-ordering and re-name data columns  #####

    # general order: 1) child information (ID, date), 2) freddies, 3) food VAS 4) intakes (meal, meal duration) 5) LOC, interoception 6) notes

    qv5_child_clean <- qv5_child_clean[c(2, 1, 125:126, 3:14, 127:151, 15:37, 38:46, 48:54, 56:62, 64:70, 72:78, 80:87, 89:95, 97:105, 107:114, 116:123, 47, 55, 63, 71, 79, 88, 96, 106, 115, 124, 152:155)]

    qv5_child_clean_labels <- qv5_child_clean_labels[c(2, 1, 125:126, 3:14, 127:151, 15:37, 38:46, 48:54, 56:62, 64:70, 72:78, 80:87, 89:95, 97:105, 107:114, 116:123, 47, 55, 63, 71, 79, 88, 96, 106, 115, 124, 152:155)]

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

    ## Manually rename
    names(qv5_child_clean)[2:41] <- c("start_date", "freddy_pre_meal", "freddy_post_meal", "vas_mac_cheese", "vas_chkn_nug", "vas_broccoli", "vas_grape", "vas_water", "rank_mac_cheese", "rank_chkn_nug", "rank_broccoli", "rank_grape",  "meal_start", "meal_end", "meal_dur", "noplate_chkn_nug_g", "plate_chkn_nug_g", "post_chkn_nug_g", "consumed_chkn_nug_g", "noplate_mac_cheese_g", "plate_mac_cheese_g", "post_mac_cheese_g", "consumed_mac_cheese_g", "noplate_grapes_g", "plate_grapes_g", "post_grapes_g", "consumed_grapes_g", "noplate_margerine_g", "noplate_broccoli_g", "plate_broccoli_g", "post_broccoli_g", "consumed_broccoli_g", "noplate_ketchup_g", "plate_ketchup_g", "post_ketchup_g", "consumed_ketchup_g", "noplate_water_g", "plate_water_g", "post_water_g", "consumed_water_g")

    names(qv5_child_clean)[65:155] <- c("hrv_dur", "hrv_starttime", "intero_prac_start_target", "intero_prac_start_actual", "intero_prac_stop_target", "intero_prac_stop_actual",  "intero_prac_hbcount", "intero_prac_dur", "intero_prac_starttime", "intero_prac2_start_target", "intero_prac2_start_actual", "intero_prac2_stop_target", "intero_prac2_stop_actual", "intero_prac2_hbcount", "intero_prac2_dur", "intero_prac2_starttime", "intero_15s_start_target", "intero_15s_start_actual", "intero_15s_stop_target", "intero_15s_stop_actual", "intero_15s_hbcount", "intero_15s_dur", "intero_15s_starttime", "intero_20s_start_target", "intero_20s_start_actual", "intero_20s_stop_target", "intero_20s_stop_actual", "intero_20s_hbcount", "intero_20s_dur", "intero_20s_starttime", "intero_18s_start_target", "intero_18s_start_actual", "intero_18s_stop_target", "intero_18s_stop_actual", "intero_18s_hbcount", "intero_18s_dur", "intero_18s_starttime",  "intero_pulse_prac_start_target", "intero_pulse_prac_start_actual", "intero_pulse_prac_stop_target", "intero_pulse_prac_stop_actual", "intero_pulse_prac_hbcount", "intero_pulse_prac_dur", "intero_pulse_prac_starttime", "intero_pulse_prac_loc", "intero_pulse_prac2_start_target", "intero_pulse_prac2_start_actual", "intero_pulse_prac2_stop_target", "intero_pulse_prac2_stop_actual", "intero_pulse_prac2_hbcount", "intero_pulse_prac2_dur", "intero_pulse_prac2_starttime", "intero_pulse_prac2_loc", "intero_pulse_15s_start_target",  "intero_pulse_15s_start_actual", "intero_pulse_15s_stop_target", "intero_pulse_15s_stop_actual",  "intero_pulse_15s_hbcount", "intero_pulse_15s_dur",  "intero_pulse_15s_starttime", "intero_pulse_15s_loc", "intero_pulse_20s_start_target", "intero_pulse_20s_start_actual", "intero_pulse_20s_stop_target", "intero_pulse_20s_stop_actual", "intero_pulse_20s_hbcount", "intero_pulse_20s_dur", "intero_pulse_20s_starttime", "intero_pulse_20s_loc",  "intero_pulse_18s_start_target", "intero_pulse_18s_start_actual", "intero_pulse_18s_stop_target", "intero_pulse_18s_stop_actual", "intero_pulse_18s_hbcount", "intero_pulse_18s_dur", "intero_pulse_18s_starttime", "intero_pulse_18s_loc", "intero_prac_notes", "intero_prac2_notes", "intero_15s_notes", "intero_20s_notes", "intero_18s_notes", "intero_pulse_prac_notes", "intero_pulse_prac2_notes", "intero_pulse_15s_notes", "intero_pulse_20s_notes", "intero_pulse_18s_notes", "food_initials", "extra_mock_needed", "mockscan2_notes", "child_notes")

    ## update data labels
    names(qv5_child_clean_labels) <- names(qv5_child_clean)

    # 5) reformatting dates to be appropriate and computer readable YYYY-MM-DD ####
    #format start date
    qv5_child_clean[["start_date"]] <- lubridate::ymd(as.Date(qv5_child_clean[["start_date"]]))

    # dates are fomrated as dd-mstr-yy
    visit_dates[['RO1_V5_Date']] <- lubridate::ymd(as.Date(visit_dates[['RO1_V5_Date']], format = "%d-%b-%y"))

    # add validated dates
    names(visit_dates)[1] <- 'id'
    qv5_child_clean <- merge(qv5_child_clean, visit_dates[c('id', 'RO1_V5_Date')], by = 'id', all.x = TRUE, all.y = FALSE)

    #update start_date
    qv5_child_clean[["start_date"]] <- ifelse(!is.na(qv5_child_clean[['RO1_V5_Date']]), as.character(qv5_child_clean[['RO1_V5_Date']]), as.character(qv5_child_clean[["start_date"]]))

    #remove RO1_V date column
    qv5_child_clean <- qv5_child_clean[, names(qv5_child_clean) != "RO1_V5_Date"]

    # add label
    qv5_child_clean_labels[["start_date"]] <- "date from participant contacts databases ('verified_visit_dates*.csv) converted to format yyyy-mm-dd in R. If no date in database, uses start_date metadata from qualtrics"


    ## freddy fullness as numeric
    qv5_child_clean[c(3:4, 10:13, 16)] <- sapply(qv5_child_clean[c(3:4, 10:13, 16)], FUN = as.numeric)

    # 6) re-calculate manual variables ####

    ## re-calculate all intake values

    # get all intake variables
    intake_vars <- names(qv5_child_clean)[c(17:41)]

    # make all intake variables numeric
    for (var in 1:length(intake_vars)) {
        var_name <- intake_vars[[var]]

        qv5_child_clean[[var_name]] <- ifelse(qv5_child_clean[[var_name]] == "-" |
                                                  qv5_child_clean[[var_name]] == "NA", NA, qv5_child_clean[[var_name]])

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
            qv5_child_clean[[consumed_var]] <- qv5_child_clean[[plate_var]] -
                qv5_child_clean[[post_var]]
            qv5_child_clean[[consumed_var]] <- ifelse(qv5_child_clean[[consumed_var]] <
                                                          0, 0, qv5_child_clean[[consumed_var]])

            # update labels
            qv5_child_clean_labels[[consumed_var]] <- paste0(qv5_child_clean_labels[[consumed_var]],
                                                             " - recalcuated difference in R with values < 0 set to 0")
        }
    }

    # 7) fix 99's and relevel ####

    ## check for labels/99 option: 1) if 99's exist, make a 'prefer not to answer' (pna) variable to go in pna database, 2) replace 99's with NA and make variable numeric

    ## make pna database
    qv5_child_pna <- data.frame(id = qv5_child_clean[["id"]])
    qv5_child_pna_labels <- lapply(qv5_child_pna, function(x) attributes(x)$label)
    qv5_child_pna_labels[["id"]] <- qv5_child_clean_labels[["id"]]

    pna_label <- "Note: prefer not to answer (pna) marked NA - see pna database for which were pna rather than missing NA"

    ## Fix 99/skip in LOC survey
    level99_issue_catvars <- names(qv5_child_clean)[42:64]

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

    level99_issue_contvars <- names(qv5_child_clean)[c(71, 78, 85, 92, 99, 106, 114, 122, 130, 138)]

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

    # 8) random fixes to factor level names and variable descriptions ####
    qv5_child_clean_labels[["meal_start"]] <- "Meal start time"
    qv5_child_clean_labels[["meal_end"]] <- "Meal end time"

    intro_pulse_names <- names(qv5_child_clean)[c(102:141, 147:151)]
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
    }

    #### 9) Format for export #### put data in order of participant ID for ease

    ## 9a) add attributes to pna data
    qv5_child_pna[2:ncol(qv5_child_pna)] <- as.data.frame(lapply(qv5_child_pna[2:ncol(qv5_child_pna)], function(x) sjlabelled::add_labels(x, labels = c(`Did not skip due to prefer not to answer` = 0, `Prefer not to answer` = 1))))

    for (v in 2:ncol(qv5_child_pna)){
        class(qv5_child_pna[[v]]) <- c("haven_labelled", "vctrs_vctr", "double")
    }

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
