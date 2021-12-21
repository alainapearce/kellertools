#' util_child_v5dat: Process raw qualtrics visit 5 data for the child
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
#' @inheritParams util_parent_v1dat
#' @inheritParams util_parent_v1dat
#'
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from child visit 5 Qualtrics; 2) dict: all variable descriptions;; 3) pna_data: data.frame marking participants who 'prefered not to answer' (pna) specific questions; and 4) pna_dict: all variable descriptions for pna_data
#'
#' @examples
#' #if in same working directory as data:
#' ch_v5_dat <- util_child_v5dat('2021-10-11')
#'
#' \dontrun{
#' #date must be a string. The following will not run:
#' ch_v5_dat <- util_child_v5dat(2021-10-11)
#'
#' #date must match the file name - for file named 'Child_V5_2021_09_16', the
#' following will not run:
#' ch_v5_dat <- util_child_v5dat('2021_10_11')
#' }
#'
#'
#' @export
#'
util_child_v5dat <- function(date_str, data_path) {

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
        qv5_child_path <- paste0(data_path, "/", "Child_V5_", date_str, ".sav")
    } else {
        qv5_child_path <- paste0("Child_V5_", date_str, ".sav")
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
    qv5_child_clean <- qv5_child_dat[c(1, 18, 22:30, 35:37, 39:77, 80:89, 91:131, 133:168, 169:199, 201)]

    ## update labels
    qv5_child_clean_labels <- qv5_child_labels[c(1, 18, 22:30, 35:37, 39:77, 80:89,  91:131, 133:168, 169:199, 201)]


    # 3) removing all practice events (e.g., 999) ####
    qv5_child_clean <- qv5_child_clean[!is.na(qv5_child_clean[["ID"]]) & qv5_child_clean[["ID"]] < 999, ]

    # 4) re-ordering and re-name data columns ####

    # general order: 1) child information (ID. date), 2) freddies, 3) food VAS 4) intakes (meal, meal duration), 5) LOC, CtC, Interoception, Mock 6) notes

    qv5_child_clean <- qv5_child_clean[c(2, 1, 142:143, 3:14, 144:168, 15:141, 170:171, 169, 172)]

    qv5_child_clean_labels <- qv5_child_clean_labels[c(2, 1, 142:143, 3:14, 144:168,  15:141, 170:171, 169, 172)]

    ## re-name variables
    names(qv5_child_clean) <- c("id", "start_date", "freddy_pre_meal", "freddy_post_meal", "vas_mac_cheese", "vas_chkn_nug", "vas_broccoli", "vas_grape", "vas_water", "mealrank_mac_cheese", "mealrank_chkn_nug", "mealrank_broccoli", "mealrank_grape",  "meal_start", "meal_end", "meal_dur", "noplate_chkn_nug_g", "plate_chkn_nug_g", "post_chkn_nug_g", "consumed_chkn_nug_g", "noplate_mac_cheese_g", "plate_mac_cheese_g",  "post_mac_cheese_g", "consumed_mac_cheese_g", "noplate_grapes_g", "plate_grapes_g", "post_grapes_g", "consumed_grapes_g", "noplate_margerine_g", "noplate_broccoli_g", "plate_broccoli_g", "post_broccoli_g", "consumed_broccoli_g", "noplate_ketchup_g", "plate_ketchup_g", "post_ketchup_g", "consumed_ketchup_g", "noplate_water_g", "plate_water_g", "post_water_g", "consumed_water_g", "loc1", "loc2a", "loc2b", "loc2c", "loc3", "loc4", "loc5", "loc6", "loc7", "loc8", "loc9",  "loc10", "loc11", "loc12", "loc13", "loc14", "loc15", "loc16a", "loc17", "loc18", "loc19", "loc16b", "loc20", "ctc1", "ctc2", "ctc3", "ctc4", "ctc5", "ctc6", "ctc7", "ctc8", "ctc9", "ctc10", "ctc11", "ctc12", "ctc13", "ctc14", "ctc15", "ctc16", "hrv_dur", "hrv_starttime", "intero_prac_start_target", "intero_prac_start_actual", "intero_prac_stop_target", "intero_prac_stop_actual",  "intero_prac_hbcount", "intero_prac_dur", "intero_prac_starttime", "intero_prac_notes", "intero_prac2_start_target", "intero_prac2_start_actual", "intero_prac2_stop_target", "intero_prac2_stop_actual", "intero_prac2_hbcount", "intero_prac2_dur", "intero_prac2_starttime", "intero_prac2_notes", "intero_15s_start_target", "intero_15s_start_actual", "intero_15s_stop_target", "intero_15s_stop_actual", "intero_15s_hbcount", "intero_15s_dur", "intero_15s_starttime", "intero_15s_notes", "intero_20s_start_target", "intero_20s_start_actual", "intero_20s_stop_target", "intero_20s_stop_actual", "intero_20s_hbcount", "intero_20s_dur", "intero_20s_starttime", "intero_20s_notes", "intero_18s_start_target", "intero_18s_start_actual", "intero_18s_stop_target", "intero_18s_stop_actual", "intero_18s_hbcount", "intero_18s_dur", "intero_18s_starttime", "intero_18s_notes", "intero_pulse_prac_start_target", "intero_pulse_prac_start_actual", "intero_pulse_prac_stop_target", "intero_pulse_prac_stop_actual", "intero_pulse_prac_hbcount", "intero_pulse_prac_dur", "intero_pulse_prac_starttime", "intero_pulse_prac_loc", "intero_pulse_prac_notes", "intero_pulse_prac2_start_target", "intero_pulse_prac2_start_actual", "intero_pulse_prac2_stop_target", "intero_pulse_prac2_stop_actual", "intero_pulse_prac2_hbcount", "intero_pulse_prac2_dur", "intero_pulse_prac2_starttime", "intero_pulse_prac2_loc", "intero_pulse_prac2_notes", "intero_pulse_15s_start_target",  "intero_pulse_15s_start_actual", "intero_pulse_15s_stop_target", "intero_pulse_15s_stop_actual",  "intero_pulse_15s_hbcount", "intero_pulse_15s_dur",  "intero_pulse_15s_starttime", "intero_pulse_15s_loc", "intero_pulse_15s_notes",  "intero_pulse_20s_start_target", "intero_pulse_20s_start_actual", "intero_pulse_20s_stop_target", "intero_pulse_20s_stop_actual", "intero_pulse_20s_hbcount", "intero_pulse_20s_dur", "intero_pulse_20s_starttime", "intero_pulse_20s_loc", "intero_pulse_20s_notes", "intero_pulse_18s_start_target", "intero_pulse_18s_start_actual", "intero_pulse_18s_stop_target", "intero_pulse_18s_stop_actual", "intero_pulse_18s_hbcount", "intero_pulse_18s_dur", "intero_pulse_18s_starttime", "intero_pulse_18s_loc", "intero_pulse_18s_notes",  "mockscan2_complete", "extra_mock_needed", "mockscan2_notes", "food_initials", "child_notes")

    ## update data labels
    names(qv5_child_clean_labels) <- names(qv5_child_clean)

    # 5) reformatting dates to be appropriate and computer readable YYYY-MM-DD  ####
    qv5_child_clean[["start_date"]] <- lubridate::ymd(as.Date(qv5_child_clean[["start_date"]]))
    qv5_child_clean_labels[["start_date"]] <- "start_date from qualtrics survey meta-data converted to format yyyy-mm-dd in R"

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
    qv5_child_pna_labels[["id"]] <- qv5_child_pna_labels[["id"]]

    pna_label <- "Note: prefer not to answer (pna) marked NA - see pna database for which were pna rather than missing NA"

    ## Fix 99/skip in LOC and ctc survey. Note, only ctc items 1-8 have skip levels
    level99_issue_catvars <- names(qv5_child_clean)[c(42:63, 65:70)]

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

    ## Fix 99 in interoception
    level99_issue_cont <- names(qv5_child_clean)[c(87, 95, 103, 111, 119, 127, 136, 145, 154, 163)]

    ###7b) re-level ctc
    # re-level ctc parent questions so that higher = riskier
    ctc_parent <- names(qv5_child_clean)[77:80]
    for (var in 1:length(ctc_parent)) {
        var_name <- as.character(ctc_parent[var])

        qv5_child_clean[[var_name]] <- sjlabelled::set_labels(qv5_child_clean[[var_name]], labels = c(`Not at all` = 5, `A little` = 4, `Not sure/in the middle` = 3, Somewhat = 2, `A lot` = 1))
        set_attr <- attributes(qv5_child_clean[[var_name]])

        qv5_child_clean[[var_name]] <- ifelse(is.na(qv5_child_clean[[var_name]]),  NA,
                                              ifelse(qv5_child_clean[[var_name]] == 1, 5,
                                                     ifelse(qv5_child_clean[[var_name]] == 2, 4,
                                                            ifelse(qv5_child_clean[[var_name]] == 3, 3,
                                                                   ifelse(qv5_child_clean[[var_name]] == 4, 2, 1)))))
        attributes(qv5_child_clean[[var_name]]) <- set_attr
        qv5_child_clean_labels[[var_name]] <- paste0(qv5_child_clean_labels[[var_name]],
                                                     " - re-leveled in R so higher = riskier")
    }

    # 8) random fixes to factor level names and variable descriptions
    qv5_child_clean_labels[["meal_start"]] <- "V5 meal start time"
    qv5_child_clean_labels[["meal_end"]] <- "V5 meal end time"

    intro_pulse_names <- names(qv5_child_clean)[123:167]
    for (var in 1:length(intro_pulse_names)) {
        var_name <- as.character(intro_pulse_names[var])
        qv5_child_clean_labels[[var_name]] <- paste0("with pulse - ", qv5_child_clean_labels[[var_name]])
    }

    ### FIX INTEROCEPTION CONTINUOUS DATA CODED AS CATEGORICAL (HBCOUNT)

    # 9) Format for export ####

    #put data in order of participant ID for ease
    qv5_child_clean <- qv5_child_clean[order(qv5_child_clean[["id"]]), ]

    qv5_child_pna <- qv5_child_pna[order(qv5_child_pna[["id"]]), ]

    # make sure the variable labels match in the dataset
    qv5_child_clean = sjlabelled::set_label(qv5_child_clean, label = matrix(unlist(qv5_child_clean_labels, use.names = FALSE)))

    qv5_child_pna = sjlabelled::set_label(qv5_child_pna, label = matrix(unlist(qv5_child_pna_labels, use.names = FALSE)))

    ## make list of data frame and associated labels
    ### ADD PNA OUTPUTS TO LIST
    qv5_child <- list(data = qv5_child_clean, dict = qv5_child_clean_labels)

    ## want an export options??

    return(qv5_child)
}
