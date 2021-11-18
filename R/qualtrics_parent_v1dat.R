#' qualtrics_parent_v1dat: Process raw Qualtrics visit 1 data for the parent
#'
#' This function loads the .sav raw data file for the parent visit 1 data that was
#' collected via Qualtrics and cleans the data. Cleaning the data involves:
#' 1) extracting all variable descriptions,
#' 2) selecting relevant data columns,
#' 3) removing all practice events (e.g., 999)
#' 4) re-ordering and re-name data columns
#' 5) general fixes to variable labels (remove ' - 1')
#' 6) fix variables with 99 issue for 'prefer not to answer'
#' 7) reformatting dates to be appropriate and computer readable: YYYY-MM-DD
#' 8) re-calculate manual variables
#' 9) re-ordering factor levels to start with value 0
#' 10) random fixes to factor level names and variable descriptions
#'
#' @param date_str the date used in the name of the .sav file (e.g., for file 'Parent_V1_2021-10-11.sav',
#' the string '2021-10-11' would be entered)
#' @param data_path (optional) the full path to the V1 Parent Qualtrics database EXCLUDING the .sav file name (e.g., '.../b-childfoodlab_Shared/Active_Studies/RO1_Brain_Mechanisms_IRB_5357/Participant_Data/untouchedRaw/Qualtrics_Raw/').
#'
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from parent visit 1 Qualtrics;
#'  2) dict: all variable descriptions; 3) pna_data: data.frame marking participants who 'prefered not to answer' (pna) specific questions; and 4) pna_dict: all variable descriptions for pna_data
#'
#' @examples
#' #if in same working directory as data:
#' p_v1_dat <- qualtrics_parent_v1dat('2021-10-11')
#'
#' \dontrun{
#' #date must be a string. The following will not run:
#' p_v1_dat <- qualtrics_parent_v1dat(2021-10-11)
#'
#' #date must match the file name - for file named 'Parent_V1_2021_09_16', the
#' following will not run:
#' p_v1_dat <- qualtrics_parent_v1dat('2021_10_11')
#' }
#'
#'
#' @export
#'
qualtrics_parent_v1dat <- function(date_str, data_path) {

    #### 1. Set up/initial checks #####

    # check that date_str exist and is a string

    datastr_arg <- methods::hasArg(date_str)

    if (isTRUE(datastr_arg) & !is.character(date_str)) {
        stop("data_str must be enter as a string: e.g., '2021_10_11'")
    } else if (isFALSE(datastr_arg)) {
        stop("data_str must set to the data string from the parent visit 1 file name: e.g., '2021_09_16'")
    }

    # check that file exists
    datapath_arg <- methods::hasArg(data_path)

    if (isTRUE(datapath_arg)) {
        if (!is.character(date_str)) {
            stop("data_path must be enter as a string: e.g., '.../Participant_Data/untouchedRaw/Qualtrics_Raw/'")
        }
    }

    #### 2. Load Data #####

    if (isTRUE(datapath_arg)) {
        qv1_parent_path <- paste0(data_path, "/Parent_V1_", date_str, ".sav")
    } else {
        qv1_parent_path <- paste0("Parent_V1_", date_str, ".sav")
    }

    # check if file exists
    qv1_parent_exists <- file.exists(qv1_parent_path)

    # load data if it exists
    if (isTRUE(qv1_parent_exists)) {
        qv1_parent_dat <- as.data.frame(haven::read_spss(qv1_parent_path))

    } else {
        if (isTRUE(datapath_arg)) {
            stop("File does not exist. Check date_str and data_path entered")
        } else {
            stop("File does not exist. Check date_str and that the data exists in current working directory")
        }
    }

    #### 3. Clean Data #####

    # 3a) extract variable labels/descriptions
    qv1_parent_labels <- lapply(qv1_parent_dat, function(x) attributes(x)$label)

    # 3b) selecting relevant data columns
    qv1_parent_clean <- qv1_parent_dat[c(1, 11, 18:19, 23:388, 390:396)]

    ## update labels
    qv1_parent_clean_labels <- qv1_parent_labels[c(1, 11, 18:19, 23:388, 390:396)]

    # 3c) removing all practice events (e.g., 999)
    qv1_parent_clean <- qv1_parent_clean[!is.na(qv1_parent_clean$ID) & qv1_parent_clean$ID < 999, ]

    # 4) re-ordering and re-name data columns general order #### 1) child information (sex, dob,) 2) anthro - h/w,
    # puberty, PA, parent measured h/w, parent related info, 3) general demographics, 4) meal related Q's -
    # ranking, eat out, PSS, feeding strategies, 6) Fasting procedure

    qv1_parent_clean <- qv1_parent_clean[c(2, 1, 127, 5:15, 128:201, 16:20, 371:377, 213:218, 21:51, 125:126, 52:124,
        206:212, 202:205, 219:370, 3:4)]

    qv1_parent_clean_labels <- qv1_parent_clean_labels[c(2, 1, 127, 5:15, 128:201, 16:20, 371:377, 213:218, 21:51, 125:126,
        52:124, 206:212, 202:205, 219:370, 3:4)]

    ## re-name variables

    # make lower case
    names(qv1_parent_clean) <- tolower(names(qv1_parent_clean))

    names(qv1_parent_clean) <- c("id", "start_date", "sex", "dob", "birth_weight_lb", "birth_weight_oz", "birth_length_in",
        "premature_yn", "premature_wks", "infant_feeding", "breastfed_exclusive_mo", "birth_order", "ethnicity", "race",
        "pds_1", "pds_2", "pds_3", "pds_4m", "pds_5m", "pds_6m", "pds_4f", "pds_5fa", "pds_5fb", "pds_5fc", "pds_5fd",
        "pds_6f", "tanner_male", "tanner_female", "paq_m_wakeup", "paq_m_travel_school", "paq_m_traveltime_school", "paq_m_arriveschool",
        "paq_m_leaveschool", "paq_m_travel_home", "paq_m_traveltime_home", "paq_m_indoor_lowintensity", "paq_m_indoor_highintensity",
        "paq_m_outdoor_lowintensity", "paq_m_outdoor_highintensity", "paq_m_bedtime", "paq_t_wakeup", "paq_t_travel_school",
        "paq_t_traveltime_school", "paq_t_arriveschool", "paq_t_leaveschool", "paq_t_travel_home", "paq_t_traveltime_home",
        "paq_t_indoor_lowintensity", "paq_t_indoor_highintensity", "paq_t_outdoor_lowintensity", "paq_t_outdoor_highintensity",
        "paq_t_bedtime", "paq_w_wakeup", "paq_w_travel_school", "paq_w_traveltime_school", "paq_w_arriveschool", "paq_w_leaveschool",
        "paq_w_travel_home", "paq_w_traveltime_home", "paq_w_indoor_lowintensity", "paq_w_indoor_highintensity", "paq_w_outdoor_lowintensity",
        "paq_w_outdoor_highintensity", "paq_w_bedtime", "paq_th_wakeup", "paq_th_travel_school", "paq_th_traveltime_school",
        "paq_th_arriveschool", "paq_th_leaveschool", "paq_th_travel_home", "paq_th_traveltime_home", "paq_th_indoor_lowintensity",
        "paq_th_indoor_highintensity", "paq_th_outdoor_lowintensity", "paq_th_outdoor_highintensity", "paq_th_bedtime",
        "paq_f_wakeup", "paq_f_travel_school", "paq_f_traveltime_school", "paq_f_arriveschool", "paq_f_leaveschool",
        "paq_f_travel_home", "paq_f_traveltime_home", "paq_f_indoor_lowintensity", "paq_f_indoor_highintensity", "paq_f_outdoor_lowintensity",
        "paq_f_outdoor_highintensity", "paq_f_bedtime", "parent_respondant", "parent_respondant_other", "parent_dob",
        "parent_ethnicity", "parent_race", "parent_height1", "parent_height2", "parent_weight1", "parent_weight2", "parent_height_avg",
        "parent_weight_avg", "parent_bmi", "sr_dad_height_ft", "sr_dad_height_in", "sr_dad_weight_lb", "sr_mom_height_ft",
        "sr_mom_height_in", "sr_mom_weight_lb", "household_n_mom", "household_n_dad", "household_n_stepmom", "household_n_stepdad",
        "household_n_brother", "household_n_sister", "household_n_stepbrother", "household_n_stepsister", "household_n_grandmom",
        "household_n_grandad", "household_n_aunt", "household_n_uncle", "household_n_cousin", "household_n_other", "n_parental_separations",
        "n_fostercare_placements", "living_with_partner", "maritalstatus", "maritalstatus_other", "income", "parent_ed",
        "parent_ed_other", "partner_ed", "partner_ed_other", "parent_employed", "parent_retired", "parent_wk_workhr",
        "partner_employed", "partner_retired", "partner_wk_workhr", "mom_weightgain_10lb", "allowance", "allowance_amount",
        "feedschild", "feedschild_other", "buysfood", "buysfood_other", "feq_eatout", "freq_familydinner", "freq_homelunch",
        "family_foodcond", "snap", "wic", "tanf", "medicaid", "liheap", "partial_lunch_assistance", "full_lunch_assistance",
        "other_program", "other_program_freetxt", "foodpantry_use", "freq_foodpantry", "foodbudget_mo", "prefered_grocerystore",
        "rank_packagedbread", "rank_bakerybread", "rank_saltysnack", "rank_sweetsnack", "rank_cheese", "rank_milk", "rank_yogurt",
        "rank_butter", "rank_eggs", "rank_otherdairy", "rank_coffeetea", "rank_carbonated_drink", "rank_fruitjuice_drink",
        "rank_sports_drink", "rank_alcohol_drink", "rank_redmeat", "rank_poultry", "rank_seafood", "rank_pasta_rice",
        "rank_soup", "rank_nuts_seeds", "rank_nutjelly_spread", "rank_breakfast_cereal", "rank_breakfast_replacement",
        "rank_meal_replacement", "rank_freshprep_foods", "rank_fresh_veg", "rank_fresh_fruit", "rank_can_veg", "rank_can_fruit",
        "rank_frozen_veg", "rank_frozen_fruit", "rank_frozen_dinner", "rank_frozen_breakfast", "rank_frozen_desert",
        "rank_candygum", "rank_bakingsupplies", "rank_condiments", "rank_dips_spreads", "rank_salad_dressings", "rank_spice_seasoning",
        "rank_cooking_oil", "grow_fruits", "grow_veg", "grow_jellyspreads", "grow_nuts_seeds", "grow_milk", "grow_cheese",
        "grow_butter", "grow_eggs", "grow_redmeat", "grow_poultry", "encourage_plateclean_vas", "child_plateclean_vas",
        "percieved_child_kcal", "pcent_parent_portionchoice", "pcent_partner_portionchoice", "pcent_child_portionchoice",
        "pcent_other_portionchoice", "pss_practice1", "pss_vas_hunger", "pss_vas_couldeat", "pss_vas_fullness", "pss_practice2",
        "pss_apple_eat", "pss_apple_freq", "pss_apple_much", "pss_apple_like", "pss_apple_portion", "pss_apple_nutrition",
        "pss_broccoli_eat", "pss_broccoli_freq", "pss_broccoli_much", "pss_broccoli_like", "pss_broccoli_portion", "pss_broccoli_nutrition",
        "pss_cake_eat", "pss_cake_freq", "pss_cake_much", "pss_cake_like", "pss_cake_portion", "pss_cake_nutrition",
        "pss_candy_eat", "pss_candy_freq", "pss_candy_much", "pss_candy_like", "pss_candy_potion", "pss_candy_nutrition",
        "pss_carrot_eat", "pss_carrot_freq", "pss_carrot_much", "pss_carrot_like", "pss_carrot_portion", "pss_carrot_nutrition",
        "pss_cornflakes_eat", "pss_cornflakes_freq", "pss_ccornflakes_much", "pss_cornflakes_like", "pss_cornflakes_portion",
        "pss_cornflakes_nutrition", "pss_cheese_brgr_eat", "pss_cheese_brgr_freq", "pss_cheese_brgr_much", "pss_cheese_brgr_like",
        "pss_cheese_brgr_portion", "pss_cheese_brgr_nutrition", "pss_chkn_nug_eat", "pss_chkn_nug_freq", "pss_chkn_nug_much",
        "pss_chkn_nug_like", "pss_chkn_nug_portion", "pss_chkn_nug_nutrition", "pss_fries_eat", "pss_fries_freq", "pss_fries_much",
        "pss_fries_like", "pss_fries_portion", "pss_fries_nutrition", "pss_garlic_bread_eat", "pss_garlic_bread_freq",
        "pss_garlic_bread_much", "pss_garlic_bread_like", "pss_garlic_bread_portion", "pss_garlic_bread_nutrition", "pss_goldfish_eat",
        "pss_goldfish_freq", "pss_goldfish_much", "pss_goldfish_like", "pss_goldfish_portion", "pss_goldfish_nutrition",
        "pss_grapes_eat", "pss_grapes_freq", "pss_grapes_much", "pss_grapes_like", "pss_grapes_portion", "pss_grapes_nutrition",
        "pss_choc_icecream_eat", "pss_choc_icecream_freq", "pss_choc_icecream_much", "pss_choc_icecream_like", "pss_choc_icecream_portion",
        "pss_choc_icecream_nutrition", "pss_mac_cheese_eat", "pss_mac_cheese_freq", "pss_mac_cheese_much", "pss_mac_cheese_like",
        "pss_mac_cheese_portion", "pss_mac_cheese_nutrition", "pss_milk_eat", "pss_milk_freq", "pss_milk_much", "pss_milk_like",
        "pss_milk_portion", "pss_milk_nutrition", "pss_orangejuice_eat", "pss_orangejuice_freq", "pss_orangejuice_much",
        "pss_orangejuice_like", "pss_orangejuice_portion", "pss_orangejuice_nutrition", "pss_pbj_sndwch_eat", "pss_pbj_sndwch_freq",
        "pss_pbj_sndwch_much", "pss_pbj_sndwch_like", "pss_pbj_sndwch_portion", "pss_pbj_sndwch_nutrition", "pss_peas_eat",
        "pss_peas_freq", "pss_peas_much", "pss_peas_like", "pss_peas_portion", "pss_peas_nutrition", "pss_pizza_eat",
        "pss_pizza_freq", "pss_pizza_much", "pss_pizza_like", "pss_pizza_portion", "pss_pizza_nutrition", "pss_soda_eat",
        "pss_soda_freq", "pss_soda_much", "pss_soda_like", "pss_soda_portion", "pss_soda_nutrition", "pss_soup_eat",
        "pss_soup_freq", "pss_soup_much", "pss_soup_like", "pss_soup_portion", "pss_soup_nutrition", "pss_tomatoes_eat",
        "pss_tomatoes_freq", "pss_tomatoes_much", "pss_tomatoes_like", "pss_tomatoes_portion", "pss_tomatoes_nutrition",
        "pss_yogurt_eat", "pss_yogurt_freq", "pss_yogurt_much", "pss_yogurt_like", "pss_yogurt_portion", "pss_yogurt_nutrition",
        "fs_vas_nutritionlabel", "fs_vas_lookonplate", "fs_vas_childusualintake", "fs_vas_platesize", "fs_vas_measuringtool",
        "fs_vas_health_proffesional", "fs_vas_recipe", "fs_vas_ramilyadult_portion", "fs_vas_familychild_portion", "fs_vas_preprotioned",
        "fs_vas_child_selfregulate", "fs_helpful_strategies", "fs_wantmoreinfo", "child_broke_fast", "child_intake_premeal")

    ## update data labels
    names(qv1_parent_clean_labels) <- names(qv1_parent_clean)

    ## 5) general fixes to labels (remove '- 1') ####

    ## remove formatting errors
    for (var in 1:length(names(qv1_parent_clean))) {
        var_name <- as.character(names(qv1_parent_clean)[var])

        # remove trailing '... - 1' from labels
        if (grepl(" - 1", qv1_parent_clean_labels[[var_name]], fixed = TRUE)) {
            qv1_parent_clean_labels[[var_name]] <- gsub("\\ - 1.*", "", qv1_parent_clean_labels[[var_name]])
        }

        # remove ' \' ' from apostrophes
        if (grepl("'s", qv1_parent_clean_labels[[var_name]], fixed = TRUE)) {
            qv1_parent_clean_labels[[var_name]] <- gsub("\\'s", "", qv1_parent_clean_labels[[var_name]])
        }

        # remove trailing 'V1 ' from labels
        if (grepl("V1", qv1_parent_clean_labels[[var_name]], fixed = TRUE)) {
            qv1_parent_clean_labels[[var_name]] <- gsub("\\V1 - ", "", qv1_parent_clean_labels[[var_name]])
            qv1_parent_clean_labels[[var_name]] <- gsub("\\V1 ", "", qv1_parent_clean_labels[[var_name]])
            qv1_parent_clean_labels[[var_name]] <- gsub("\\V1", "", qv1_parent_clean_labels[[var_name]])
        }
    }

    #### 6) fix 99's and other poor categories ####

    ## check for labels/99 option: 1) if 99's exist, make a 'prefere not to answer' (pna) variable to go in pna
    ## database, 2) replace 99's with NA and make variable numeric

    ## make pna database
    qv1_parent_pna <- data.frame(id = qv1_parent_clean$id)
    qv1_parent_pna_labels <- lapply(qv1_parent_pna, function(x) attributes(x)$label)
    qv1_parent_pna_labels[["id"]] <- qv1_parent_clean_labels[["id"]]


    pna_label <- "Note: prefer not to answer (pna) marked NA - see pna database for which were pna rather than missing NA"

    ## 6a) self-reported heights/weights data ####
    parent_anthro_vars <- names(qv1_parent_clean)[101:106]

    ## duplicate self-reported weights because set 119 = <120 lb and 400 = 400+
    qv1_parent_clean$sr_dad_weight_lb_cat <- qv1_parent_clean$sr_dad_weight_lb
    qv1_parent_clean_labels[["sr_dad_weight_lb_cat"]] <- "categorical parent-reported father weight in pounds: category per pound increment with 119 = '<120 pounds' and 400 = '400+ pounds'"

    qv1_parent_clean$sr_mom_weight_lb_cat <- qv1_parent_clean$sr_mom_weight_lb
    qv1_parent_clean_labels[["sr_mom_weight_lb_cat"]] <- "categorical parent-reported father weight in pounds: category per pound increment with 119 = '<120 pounds' and 400 = '400+ pounds'"

    # update weight variable labels
    qv1_parent_clean_labels[["sr_dad_weight_lb"]] <- "parent-reported father weight in pounds - note: categorical values 119 = <120 lb and 400 = 400+ set to NA so can compute BMI continuous. These values presevered in categorical ('_cat') version of variable"
    qv1_parent_clean_labels[["sr_mom_weight_lb"]] <- "parent-reported mother weight in pounds - note: categorical values 119 = <120 lb and 400 = 400+ set to NA so can compute BMI continuous. These values presevered in categorical ('_cat') version of variable"
    qv1_parent_clean_labels[["sr_dad_height_ft"]] <- "parent-reported father height in feet."
    qv1_parent_clean_labels[["sr_dad_height_in"]] <- "parent-reported father height in inches"
    qv1_parent_clean_labels[["sr_mom_height_ft"]] <- "parent-reported mother height in feet"
    qv1_parent_clean_labels[["sr_mom_height_in"]] <- "parent-reported mother height in inches"

    # loop through to fix 99's and lb 119's and 400's
    for (v in 1:length(parent_anthro_vars)) {

        # get variable name
        pvar <- parent_anthro_vars[v]

        # if has '99' value, create new pna variable marking pna == 1
        if (is.element(99, qv1_parent_clean[[pvar]])) {
            pna_dat <- ifelse(is.na(qv1_parent_clean[[pvar]]), 0, ifelse(qv1_parent_clean[[pvar]] == 99, 1, 0))

            new_pna <- length(names(qv1_parent_pna)) + 1
            qv1_parent_pna[[new_pna]] <- pna_dat

            names(qv1_parent_pna)[new_pna] <- paste0(pvar, "_pna")

            # add label to pna database
            qv1_parent_pna_labels[[paste0(pvar, "_pna")]] <- paste0("prefer not to answer marked for variable ", pvar,
                ": ", qv1_parent_clean_labels[[pvar]])

            # update true data label (only want to pna label if needed)
            qv1_parent_clean_labels[[pvar]] <- paste0(qv1_parent_clean_labels[[pvar]], " -- ", pna_label)
        }

        if (grepl("lb", pvar, fixed = TRUE)) {
            # check for values '119' and '400' as they indicate < 119 and 400+, respectively (values are kept in
            # the categorical version of this variable - see above) labels were updated above
            qv1_parent_clean[[pvar]] <- ifelse(is.na(qv1_parent_clean[[pvar]]), NA, ifelse(qv1_parent_clean[[pvar]] ==
                119, NA, ifelse(qv1_parent_clean[[pvar]] == 400, NA, as.numeric(qv1_parent_clean[[pvar]]))))
        } else {
            # convert 99 to NA and make numeric variable labels only update if had 99 - done in if statement above
            qv1_parent_clean[[pvar]] <- ifelse(qv1_parent_clean[[pvar]] == 99, NA, as.numeric(qv1_parent_clean[[pvar]]))
        }
    }

    ## 6b) PA time data with 99's and odd anchor categories ####
    pa_vars <- names(qv1_parent_clean)[c(29:88)]

    # loop through to fix 99's
    for (v in 1:length(pa_vars)) {

        # get variable name
        pvar <- pa_vars[v]

        # if has '99' value, create new pna variable marking pna == 1
        if (is.element(99, qv1_parent_clean[[pvar]])) {
            pna_dat <- ifelse(is.na(qv1_parent_clean[[pvar]]), 0, ifelse(qv1_parent_clean[[pvar]] == 99, 1, 0))

            new_pna <- length(names(qv1_parent_pna)) + 1
            qv1_parent_pna[[new_pna]] <- pna_dat

            # make logical variable and name it
            names(qv1_parent_pna)[new_pna] <- paste0(pvar, "_pna")

            # add label to pna database
            qv1_parent_pna_labels[[paste0(pvar, "_pna")]] <- paste0("prefer not to answer marked for variable ", pvar,
                ": ", qv1_parent_clean_labels[[pvar]])

            # update true data label (only want to pna label if needed)
            qv1_parent_clean_labels[[pvar]] <- paste0(qv1_parent_clean_labels[[pvar]], " -- ", pna_label)
        }

        if (grepl("wakeup", pvar, fixed = TRUE)) {
            # duplicate to make categorical variable
            nvar <- length(names(qv1_parent_clean))
            qv1_parent_clean[[nvar + 1]] <- qv1_parent_clean[[pvar]]
            names(qv1_parent_clean)[[nvar + 1]] <- paste0(pvar, "_cat")

            # update label
            qv1_parent_clean_labels[[paste0(pvar, "_cat")]] <- paste0("categorical ", qv1_parent_clean_labels[[pvar]],
                " -note: 1 = 'Before 5 AM' and 15 = 'After 8 AM'")

            # convert to factor label values
            qv1_parent_clean[[pvar]] <- haven::as_factor(qv1_parent_clean[[pvar]])

            # check for values 'Before 5 am', 'After 8:00 am', and 'Don't want to answer' (values are kept in the
            # categorical version of this variable)
            qv1_parent_clean[[pvar]] <- ifelse(is.na(qv1_parent_clean[[pvar]]) | qv1_parent_clean[[pvar]] == "Before 5:00 AM" |
                qv1_parent_clean[[pvar]] == "After 8:00 AM" | qv1_parent_clean[[pvar]] == "Don't want to answer", NA,
                as.character(qv1_parent_clean[[pvar]]))

        } else if (grepl("arriveschool", pvar, fixed = TRUE)) {
            # duplicate to make categorical variable
            nvar <- length(names(qv1_parent_clean))
            qv1_parent_clean[[nvar + 1]] <- qv1_parent_clean[[pvar]]
            names(qv1_parent_clean)[[nvar + 1]] <- paste0(pvar, "_cat")

            # update label
            qv1_parent_clean_labels[[paste0(pvar, "_cat")]] <- paste0("categorical ", qv1_parent_clean_labels[[pvar]],
                " -note: 15 = 'After 8:45 PM'")

            # convert to factor label values
            qv1_parent_clean[[pvar]] <- haven::as_factor(qv1_parent_clean[[pvar]])

            # check for value 'After 8:45 AM' and 'Don't want to answer' (values are kept in the categorical
            # version of this variable)
            qv1_parent_clean[[pvar]] <- ifelse(is.na(qv1_parent_clean[[pvar]]) | qv1_parent_clean[[pvar]] == "After 8:45 AM" |
                qv1_parent_clean[[pvar]] == "Don't want to answer", NA, as.character(qv1_parent_clean[[pvar]]))

        } else if (grepl("leaveschool", pvar, fixed = TRUE)) {
            # duplicate to make categorical variable
            nvar <- length(names(qv1_parent_clean))
            qv1_parent_clean[[nvar + 1]] <- qv1_parent_clean[[pvar]]
            names(qv1_parent_clean)[[nvar + 1]] <- paste0(pvar, "_cat")

            # update label
            qv1_parent_clean_labels[[paste0(pvar, "_cat")]] <- paste0("categorical ", qv1_parent_clean_labels[[pvar]],
                " -note: 15 = 'After 8:45 PM'")

            # convert to factor label values
            qv1_parent_clean[[pvar]] <- haven::as_factor(qv1_parent_clean[[pvar]])

            # check for value 'Before 2 PM', 'After 6 PM', and 'Don't want to answer' (values are kept in the
            # categorical version of this variable)
            qv1_parent_clean[[pvar]] <- ifelse(is.na(qv1_parent_clean[[pvar]]) | qv1_parent_clean[[pvar]] == "Before 2:00 PM" |
                qv1_parent_clean[[pvar]] == "After 6:00 PM" | qv1_parent_clean[[pvar]] == "Don't want to answer", NA,
                as.character(qv1_parent_clean[[pvar]]))

        } else if (grepl("bedtime", pvar, fixed = TRUE)) {
            # duplicate to make categorical variable
            nvar <- length(names(qv1_parent_clean))
            qv1_parent_clean[[nvar + 1]] <- qv1_parent_clean[[pvar]]
            names(qv1_parent_clean)[[nvar + 1]] <- paste0(pvar, "_cat")

            # update label
            qv1_parent_clean_labels[[paste0(pvar, "_cat")]] <- paste0("categorical ", qv1_parent_clean_labels[[pvar]],
                " -note: 15 = 'After 10:00 PM'")

            # convert to factor label values
            qv1_parent_clean[[pvar]] <- haven::as_factor(qv1_parent_clean[[pvar]])

            # check for value 'After 10 PM' and 'Don't want to answer' (values are kept in the categorical version
            # of this variable)
            qv1_parent_clean[[pvar]] <- ifelse(is.na(qv1_parent_clean[[pvar]]) | qv1_parent_clean[[pvar]] == "After 10:00 PM" |
                qv1_parent_clean[[pvar]] == "Don't want to answer", NA, as.character(qv1_parent_clean[[pvar]]))

        } else {
            # drop 99 level label labels only update if had 99 - done in if statement above
            qv1_parent_clean[[pvar]] <- sjlabelled::remove_labels(qv1_parent_clean[[pvar]], labels = "Don't want to answer")

            # extract variable attributes
            pvar_attr <- attributes(qv1_parent_clean[[pvar]])

            # replace 99 values
            qv1_parent_clean[[pvar]] <- ifelse(is.na(qv1_parent_clean[[pvar]]) | qv1_parent_clean[[pvar]] == 99, NA,
                qv1_parent_clean[[pvar]])

            # replace attributes
            attributes(qv1_parent_clean[[pvar]]) <- pvar_attr
        }
    }

    ## 6c) continuous variables with 99's data ####

    level99_issue_contvars <- names(qv1_parent_clean)[c(5:7, 9, 11, 107:122, 145:146, 158)]

    for (v in 1:length(level99_issue_contvars)) {
        # get variable name
        pvar <- level99_issue_contvars[v]

        # if has '99' value, create new pna variable marking pna == 1
        if (is.element(99, qv1_parent_clean[[pvar]])) {
            pna_dat <- ifelse(is.na(qv1_parent_clean[[pvar]]), 0, ifelse(qv1_parent_clean[[pvar]] == 99, 1, 0))

            new_pna <- length(names(qv1_parent_pna)) + 1
            qv1_parent_pna[[new_pna]] <- pna_dat

            names(qv1_parent_pna)[new_pna] <- paste0(pvar, "_pna")

            # add label to pna database
            qv1_parent_pna_labels[[paste0(pvar, "_pna")]] <- paste0("prefer not to answer marked for variable ", pvar,
                ": ", qv1_parent_clean_labels[[pvar]])

            # update true data label (only want to pna label if needed)
            qv1_parent_clean_labels[[pvar]] <- paste0(qv1_parent_clean_labels[[pvar]], " -- ", pna_label)
        }

        # convert 99 to NA and make numeric variable labels only update if had 99 - done in if statement above
        qv1_parent_clean[[pvar]] <- ifelse(qv1_parent_clean[[pvar]] == 99, NA, as.numeric(qv1_parent_clean[[pvar]]))
    }

    ## 6d) categorical variables with 99's data ####
    level99_issue_catvars <- names(qv1_parent_clean)[c(3, 8, 10, 13:14, 89, 92:93, 123:124, 126:127, 129, 131:132, 134:135,
        137:140, 142, 144, 147:155, 157, 159:160, 203:212, 226, 232, 238, 244, 250, 256, 262, 268, 274, 280, 286, 292,
        298, 304, 310, 316, 322, 328, 334, 340, 346, 352, 358)]

    for (v in 1:length(level99_issue_catvars)) {
        # get variable name
        pvar <- level99_issue_catvars[v]

        # if has '99' value, create new pna variable marking pna == 1
        if (is.element(99, qv1_parent_clean[[pvar]])) {
            pna_dat <- ifelse(is.na(qv1_parent_clean[[pvar]]), 0, ifelse(qv1_parent_clean[[pvar]] == 99, 1, 0))

            new_pna <- length(names(qv1_parent_pna)) + 1
            qv1_parent_pna[[new_pna]] <- pna_dat

            names(qv1_parent_pna)[new_pna] <- paste0(pvar, "_pna")

            # add label to pna database
            qv1_parent_pna_labels[[paste0(pvar, "_pna")]] <- paste0("prefer not to answer marked for variable ", pvar,
                ": ", qv1_parent_clean_labels[[pvar]])

            # update true data label (only want to pna label if needed)
            qv1_parent_clean_labels[[pvar]] <- paste0(qv1_parent_clean_labels[[pvar]], " -- ", pna_label)

        }

        # drop 99 level label labels only update if had 99 - done in if statement above
        qv1_parent_clean[[pvar]] <- sjlabelled::remove_labels(qv1_parent_clean[[pvar]], labels = "Don't want to answer")

        # extract variable attributes
        pvar_attr <- attributes(qv1_parent_clean[[pvar]])

        # replace 99 values
        qv1_parent_clean[[pvar]] <- ifelse(is.na(qv1_parent_clean[[pvar]]) | qv1_parent_clean[[pvar]] == 99, NA, qv1_parent_clean[[pvar]])

        # replace attributes
        attributes(qv1_parent_clean[[pvar]]) <- pvar_attr
    }

    ## 6e) hrs work per week ####
    wkhr_vars <- names(qv1_parent_clean)[c(133, 136)]

    for (v in 1:length(wkhr_vars)) {
        # get variable name
        pvar <- wkhr_vars[v]

        # if has '99' value, create new pna variable marking pna == 1
        if (is.element(99, qv1_parent_clean[[pvar]])) {
            pna_dat <- ifelse(is.na(qv1_parent_clean[[pvar]]), 0, ifelse(qv1_parent_clean[[pvar]] == 99, 1, 0))

            new_pna <- length(names(qv1_parent_pna)) + 1
            qv1_parent_pna[[new_pna]] <- pna_dat

            names(qv1_parent_pna)[new_pna] <- paste0(pvar, "_pna")

            # add label to pna database
            qv1_parent_pna_labels[[paste0(pvar, "_pna")]] <- paste0("prefer not to answer marked for variable ", pvar,
                ": ", qv1_parent_clean_labels[[pvar]])

            # update true data label (only want to pna label if needed)
            qv1_parent_clean_labels[[pvar]] <- paste0(qv1_parent_clean_labels[[pvar]], " -- ", pna_label)
        }

        # duplicate to make categorical variable
        nvar <- length(names(qv1_parent_clean))
        qv1_parent_clean[[nvar + 1]] <- qv1_parent_clean[[pvar]]
        names(qv1_parent_clean)[[nvar + 1]] <- paste0(pvar, "_cat")

        # update label
        qv1_parent_clean_labels[[paste0(pvar, "_cat")]] <- paste0("categorical ", qv1_parent_clean_labels[[pvar]], " -note: 61 = 'More than 60'")

        # check for value 66 = 'More than 60' and 99 = 'Don't want to answer' (values are kept in the categorical
        # version of this variable)
        qv1_parent_clean[[pvar]] <- ifelse(is.na(qv1_parent_clean[[pvar]]) | qv1_parent_clean[[pvar]] == 99 | qv1_parent_clean[[pvar]] ==
            61, NA, as.numeric(qv1_parent_clean[[pvar]]))
    }

    ## birth order ####
    if (is.element(99, qv1_parent_clean$birth_order)) {
        pna_dat <- ifelse(is.na(qv1_parent_clean$birth_order), 0, ifelse(qv1_parent_clean$birth_order == 99, 1, 0))

        if (length(names(qv1_parent_pna)) == 0) {
            qv1_parent_pna$birth_order_pna <- data.frame(pna_dat)
        } else {
            qv1_parent_pna[["birth_order_pna"]] <- pna_dat
        }

        # add label to pna database
        qv1_parent_pna_labels[["birth_order_pna"]] <- paste0("prefer not to answer marked for variable ", pvar, ": ",
            qv1_parent_clean_labels[["birth_order"]])

        # update true data label (only want to pna label if needed)
        qv1_parent_clean_labels[["birth_order"]] <- paste0(qv1_parent_clean_labels[["birth_order"]], " -- ", pna_label)
    }

    # covert to factor levels
    qv1_parent_clean$birth_order <- haven::as_factor(qv1_parent_clean$birth_order)

    # replace NA and 'Don't want to answer'
    qv1_parent_clean$birth_order <- ifelse(is.na(qv1_parent_clean$birth_order) | qv1_parent_clean$birth_order == " Don't want to answer",
        NA, as.character(qv1_parent_clean$birth_order))

    # split components
    birth_split <- matrix(unlist(strsplit(qv1_parent_clean$birth_order, " of ")), ncol = 2, byrow = TRUE)

    # assign to 2 variables and make numeric
    qv1_parent_clean$birth_order <- birth_split[, 1]
    qv1_parent_clean$n_siblingsbirth <- as.numeric(birth_split[, 2])

    # replace 'st', 'nd', 'rd', and 'th' in birth order
    qv1_parent_clean$birth_order <- as.numeric(gsub("st|nd|rd|th", "", qv1_parent_clean$birth_order))

    # update labels
    qv1_parent_clean_labels[["birth_order"]] <- paste0(qv1_parent_clean_labels[["birth_order"]], " - converted to numeric by R")
    qv1_parent_clean_labels[["n_siblingsbirth"]] <- "number of siblings birthed - extracted from original birth_order variable now labeled birth_order_cat by R"

    #### 7) reformatting dates/times #### 7a) dates (start, dobs) ####
    qv1_parent_clean$start_date <- lubridate::ymd(as.Date(qv1_parent_clean$start_date))
    qv1_parent_clean_labels[["start_date"]] <- "start_date from qualtrics survey meta-data converted to format yyyy-mm-dd in R"

    qv1_parent_clean$dob <- as.Date(qv1_parent_clean$dob, format = "%m/%d/%Y")
    qv1_parent_clean_labels[["dob"]] <- "date of birth converted to format yyyy-mm-dd in R"

    qv1_parent_clean$parent_dob <- as.Date(qv1_parent_clean$parent_dob, format = "%m/%d/%Y")
    qv1_parent_clean_labels[["parent_dob"]] <- "parent date of birth converted to format yyyy-mm-dd in R"

    ## 7b) time data from PA (waketime, bedtime) ####
    time_vars <- names(qv1_parent_clean)[c(29, 32:33, 40:41, 44:45, 52:53, 56:57, 64:65, 68:69, 76:77, 80:81, 88)]

    for (v in 1:length(time_vars)) {
        pvar <- time_vars[v]

        # convert time
        qv1_parent_clean[[pvar]] <- format(as.POSIXlt(qv1_parent_clean[[pvar]], format = "%I:%M %p"), format = "%H:%M:%S")

        # update labels
        qv1_parent_clean_labels[[pvar]] <- paste0(qv1_parent_clean_labels[[pvar]], " converted to 24 hour time in R")
    }

    # 8) re-calculate manual variables ####

    ## 8a) parent anthropometrics ####
    qv1_parent_clean$parent_height_avg <- ifelse(is.na(qv1_parent_clean$parent_height1) | is.na(qv1_parent_clean$parent_height2),
        NA, rowSums(qv1_parent_clean[c("parent_height1", "parent_height2")], na.rm = TRUE)/2)
    qv1_parent_clean_labels[["parent_height1"]] <- "measured parent height 1 in lab"
    qv1_parent_clean_labels[["parent_height2"]] <- "measured parent height 2 in lab"
    qv1_parent_clean_labels[["parent_height_avg"]] <- "measured parent average height calculated in R"

    # avg parent weight, update label
    qv1_parent_clean$parent_weight_avg <- ifelse(is.na(qv1_parent_clean$parent_weight1) | is.na(qv1_parent_clean$parent_weight2),
        NA, rowSums(qv1_parent_clean[c("parent_weight1", "parent_weight2")], na.rm = TRUE)/2)
    qv1_parent_clean_labels[["parent_weight1"]] <- "measured parent weight 1 in lab"
    qv1_parent_clean_labels[["parent_weight2"]] <- "measured parent weight 2 in lab"
    qv1_parent_clean_labels[["parent_weight_avg"]] <- "measured parent average weight calculated in R"

    # parent bmi, update label
    if (class(qv1_parent_clean$parent_bmi) == "character") {
        qv1_parent_clean$parent_bmi <- as.numeric(qv1_parent_clean$parent_bmi)
    }

    qv1_parent_clean$parent_bmi <- ifelse(is.na(qv1_parent_clean$parent_height_avg) | is.na(qv1_parent_clean$parent_weight_avg),
        NA, round(qv1_parent_clean$parent_weight_avg/((qv1_parent_clean$parent_height_avg/100)^2), digits = 2))
    qv1_parent_clean_labels[["parent_bmi"]] <- "measured parent bmi calculated in R package using scripted average height and weight"

    # convert to cm and kg
    qv1_parent_clean$sr_dad_height_cm <- (qv1_parent_clean$sr_dad_height_ft * 12 + qv1_parent_clean$sr_dad_height_in) *
        2.54
    qv1_parent_clean_labels[["sr_dad_height_cm"]] <- "parent-reported father height in feet and inches converted to cm in R"

    qv1_parent_clean$sr_mom_height_cm <- (qv1_parent_clean$sr_mom_height_ft * 12 + qv1_parent_clean$sr_mom_height_in) *
        2.54
    qv1_parent_clean_labels[["sr_mom_height_cm"]] <- "parent-reported mother height in feet and inches converted to cm in R"

    qv1_parent_clean$sr_dad_weight_kg <- qv1_parent_clean$sr_dad_weight_lb/2.205
    qv1_parent_clean_labels[["sr_dad_weight_kg"]] <- "parent-reported father weight in pounds converted to kg in R"

    qv1_parent_clean$sr_mom_weight_kg <- qv1_parent_clean$sr_mom_weight_lb/2.205
    qv1_parent_clean_labels[["sr_mom_weight_kg"]] <- "parent-reported mother weight in pounds converted to kg in R"

    # parent-report bmi, update label
    qv1_parent_clean$sr_dad_bmi <- round(qv1_parent_clean$sr_dad_weight_kg/((qv1_parent_clean$sr_dad_height_cm/100)^2),
        digits = 2)
    qv1_parent_clean_labels[["sr_dad_bmi"]] <- "computed bmi from parent-reported father height and weight in R"

    qv1_parent_clean$sr_mom_bmi <- round(qv1_parent_clean$sr_mom_weight_kg/((qv1_parent_clean$sr_mom_height_cm/100)^2),
        digits = 2)
    qv1_parent_clean_labels[["sr_mom_bmi"]] <- "computed bmi from parent-reported mother height and weight in R"

    ## 8b) Inclusion/exclusion criteria, Risk Group ####



    # 9) re-ordering factor levels to start with value 0 ####

    ## 9a) variables that just need to shift values by 1 ####
    relevel_vars <- names(qv1_parent_clean)[c(10, 30:31, 34:35, 42:43, 46:47, 54:55, 58:59, 66:67, 70:71, 78:79, 82:83,
        124, 126:127, 129, 139:140, 142, 144, 159:160, 226, 232, 238, 244, 250, 256, 262, 268, 274, 280, 286, 292, 298,
        304, 310, 316, 322, 328, 334, 340, 346, 352, 358, 380:399)]

    for (v in 1:length(relevel_vars)) {
        pvar <- relevel_vars[v]

        # subtract 1 from level values so start at 0 rather than 1
        attributes(qv1_parent_clean[[pvar]])$labels <- attributes(qv1_parent_clean[[pvar]])$labels - 1

        # save attributes
        set_attr <- attributes(qv1_parent_clean[[pvar]])

        # subtract 1 from each data value to match new level values
        qv1_parent_clean[[pvar]] <- ifelse(is.na(qv1_parent_clean[[pvar]]), NA, qv1_parent_clean[[pvar]] - 1)

        # reset attributes
        attributes(qv1_parent_clean[[pvar]]) <- set_attr

        # update varaible lables
        qv1_parent_clean_labels[[pvar]] <- paste0(qv1_parent_clean_labels[[pvar]], " re-leveled in R to start with 0")
    }

    ## 9b) variables that need manual shift ####

    ## sex - make sure always matches across parent/child and visits
    qv1_parent_clean$sex <- sjlabelled::set_labels(qv1_parent_clean$sex, labels = c(Male = 0, Female = 1))
    set_attr <- attributes(qv1_parent_clean$sex)
    qv1_parent_clean$sex <- ifelse(is.na(qv1_parent_clean$sex), NA, ifelse(qv1_parent_clean$sex == 1, 0, 1))
    attributes(qv1_parent_clean$sex) <- set_attr
    qv1_parent_clean_labels[["sex"]] <- paste0(qv1_parent_clean_labels[["sex"]], " re-leveled in R to start with 0")

    ## race - make sure always matches across parent/child and visits
    race_vars <- names(qv1_parent_clean)[c(14, 93)]

    for (v in 1:length(race_vars)) {
        pvar <- race_vars[v]

        qv1_parent_clean[[pvar]] <- sjlabelled::set_labels(qv1_parent_clean[[pvar]], labels = c(`White/Caucasian` = 0,
            `American Indian/Alaskan Native` = 1, Asian = 2, `Black/African American` = 3, `Hawaiian/Pacific Islander` = 4))
        set_attr <- attributes(qv1_parent_clean[[pvar]])
        qv1_parent_clean[[pvar]] <- ifelse(is.na(qv1_parent_clean[[pvar]]), NA, ifelse(qv1_parent_clean[[pvar]] == 5,
            0, qv1_parent_clean[[pvar]]))
        attributes(qv1_parent_clean[[pvar]]) <- set_attr
        qv1_parent_clean_labels[[pvar]] <- paste0(qv1_parent_clean_labels[[pvar]], " re-leveled in R to start with 0 and set 'White/Caucasion' = 0")
    }

    # 10) specific fixes to factor level names and variable descriptions ####

    # 10a) make rankings numeric
    qv1_parent_clean[161:202] <- sapply(qv1_parent_clean[161:202], function(x) as.numeric(x))

    ## 10b) fix ever eat yogurt variable label
    qv1_parent_clean_labels[["pss_yogurt_eat"]] <- "PSS Strawberry Yogurt - Ever Eat"

    #### 11) Format for export ####

    ## 11a) final re-order/clean of data
    qv1_parent_clean <- qv1_parent_clean[c(1:12, 402, 13:28, 380, 29:31, 381, 32, 382, 33:39, 383, 40, 384, 41:43, 385,
        44, 386, 45:51, 387, 52, 388, 53:55, 389, 56, 390, 57:63, 391, 64, 392, 65:67, 393, 68, 394, 69:75, 395, 76,
        396, 77:79, 397, 80, 398, 81:87, 399, 88, 89:103, 378, 104:106, 379, 403:408, 107:133, 400, 134:136, 401, 137:377)]

    qv1_parent_clean_labels <- qv1_parent_clean_labels[c(1:12, 402, 13:28, 380, 29:31, 381, 32, 382, 33:39, 383, 40,
        384, 41:43, 385, 44, 386, 45:51, 387, 52, 388, 53:55, 389, 56, 390, 57:63, 391, 64, 392, 65:67, 393, 68, 394,
        69:75, 395, 76, 396, 77:79, 397, 80, 398, 81:87, 399, 88, 89:103, 378, 104:106, 379, 403:408, 107:133, 400, 134:136,
        401, 137:377)]


    ## 11b) add attributes to pna data
    n_pna_cols <- length(names(qv1_parent_pna))
    qv1_parent_pna[2:n_pna_cols] <- as.data.frame(lapply(qv1_parent_pna[2:n_pna_cols], function(x) sjlabelled::add_labels(x,
        labels = c(`Did not skip due to prefer not to answer` = 0, `Prefer not to answer` = 1))))

    # 11c) put data in order of participant ID for ease
    qv1_parent_clean <- qv1_parent_clean[order(qv1_parent_clean$id), ]
    qv1_parent_pna <- qv1_parent_pna[order(qv1_parent_pna$id), ]

    # 11d) make sure the variable labels match in the dataset
    qv1_parent_clean = sjlabelled::set_label(qv1_parent_clean, label = matrix(unlist(qv1_parent_clean_labels, use.names = FALSE)))
    qv1_parent_pna = sjlabelled::set_label(qv1_parent_pna, label = matrix(unlist(qv1_parent_pna_labels, use.names = FALSE)))

    # make list of data frame and associated labels
    qv1_parent <- list(data = qv1_parent_clean, dict = qv1_parent_clean_labels, pna_data = qv1_parent_pna, pna_dict = qv1_parent_pna_labels)

    ## want an export options??

    return(qv1_parent)
}
