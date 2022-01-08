#' util_fbs_parent_v7dat_home: Process raw Qualtrics visit 7 data collected at home for the parent (1-year follow-up)
#'
#' This function loads the .sav raw data file for the parent visit 7 data that was collected via Qualtrics at 1-year follow-up at home when the protocol was split due to covid and cleans the data. Cleaning the data involves:
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
#' The databases MUST follow the naming convention: Child_V7_Home_YYYY-MM-DD.sav
#'
#' @inheritParams util_fbs_parent_v1dat
#' @inheritParams util_fbs_parent_v1dat
#'
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from parent visit 7 Qualtrics; 2) dict: all variable descriptions; 3) pna_data: data.frame marking participants who 'prefered not to answer' (pna) specific questions; and 4) pna_dict: all variable descriptions for pna_data
#'
#' @examples
#' #if in same working directory as data. Note - there is no need to add the COVID protocol (i.e., 'Home' or 'Lab') to file_pattern:
#' p_v7_dat_home <- util_fbs_parent_v7dat_home('Parent_V7')
#'
#' \dontrun{
#' #file_pattern must be a string. The following will not run:
#' p_v7_dat_home <- util_fbs_parent_v7dat_home(Parent_V7)
#'
#' #file_pattern must have the respondent ('Parent') and visit number ('V1'). If just enter 'Parent', the script will not run because it will return multiple files for different parent visits. The following will not run:
#' p_v7_dat_home <- util_fbs_parent_v7dat_home('Parent')
#' }
#'
#'
#' @export
#'
util_fbs_parent_v7dat_home <- function(file_pattern, data_path) {

    #### 1. Set up/initial checks #####

    # check that file_pattern exist and is a string

    filepat_arg <- methods::hasArg(file_pattern)

    if (isTRUE(filepat_arg) & !is.character(file_pattern)) {
        stop("file_pattern must be entered as a string: e.g., 'Parent_V7'")
    } else if (isFALSE(filepat_arg)) {
        stop("file_pattern must set to the a string matching the name of the raw data file for parent visit: e.g., 'Parent_V7'")
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
        qv7_parent_path <- list.files(path = paste0(data_path, '/Final_Covid/'), pattern = paste0(file_pattern, '_Home'), full.names = TRUE)

        #if no files found, check direct data_path entered
        if (length(qv7_parent_path) == 0) {
            qv7_parent_path <- list.files(path = data_path, pattern = paste0(file_pattern, '_Home'), full.names = TRUE)
        }
    } else {
        qv7_parent_path <- paste0(pattern = paste0(file_pattern, '_Home'), full.names = TRUE)
    }

    # check number of files found
    if (length(qv7_parent_path) > 1) {
        stop("More than one file matched after adding '_Home' to the file_pattern . Be sure thefile_pattern specifies both the respondent (Parent/Child) and visit number (V#). If have more than 1 file matching the pattern in the directory, may need to move to enter a more specific file_pattern than is standard.")
    } else if (length(qv7_parent_path) == 0) {
        stop("No files found after adding '_Home' to file_pattern. Be sure the data_path and file_pattern are correct and that the file exists.")
    }

    # check that file is of type '.sav'
    if (!grepl('.sav', qv7_parent_path, fixed = TRUE)){
        stop("The file found is not an SPSS database (.sav)")
    }

    # check if file exists
    qv7_parent_exists <- file.exists(qv7_parent_path)

    # load data if it exists
    if (isTRUE(qv7_parent_exists)) {
        qv7_parent_dat <- as.data.frame(haven::read_spss(qv7_parent_path))

    } else {
        if (isTRUE(datapath_arg)) {
            stop("File does not exist. Check date_str and data_path entered")
        } else {
            stop("File does not exist. Check date_str and that the data exists in current working directory")
        }
    }


    #### 3. Clean Data #####

    # 3a) extract variable labels/descriptions
    qv7_parent_labels <- lapply(qv7_parent_dat, function(x) attributes(x)$label)

    # 3b) selecting relevant data columns
    qv7_parent_clean <- qv7_parent_dat[c(1, 11:132, 134:449, 451:523)]

    ## update labels
    qv7_parent_clean_labels <- qv7_parent_labels[c(1, 11:132, 134:449, 451:523)]

    # 3c) removing all practice events (e.g., 999)
    qv7_parent_clean <- qv7_parent_clean[!is.na(qv7_parent_clean[["ID"]]) & qv7_parent_clean[["ID"]] < 999, ]

    # 4) re-ordering and re-name data columns  ####
    # general order: 1) child information 2) puberty, PA, sleep, 3) general demographics, 4) Fasting procedure, 5) meal related Q's - ranking, eat out, PSS, feeding strategies, 6) child function - BRIEF; 7) notes

    qv7_parent_clean <- qv7_parent_clean[c(2, 1, 437:439, 124:197, 419:436, 30:31, 209:214, 16:29, 32:33,  34:48, 122:123, 503:512, 49:121, 202:208, 198:201, 215:418, 440:502, 3:15)]


    qv7_parent_clean_labels <- qv7_parent_clean_labels[c(2, 1, 437:439, 124:197, 419:436, 30:31, 209:214, 16:29, 32:33,  34:48, 122:123, 503:512, 49:121, 202:208, 198:201, 215:418, 440:502, 3:15)]

    ## re-name variables make lower case
    names(qv7_parent_clean) <- tolower(names(qv7_parent_clean))

    # fix cshq so reflects that it is the adapted version
    for (var in 1:length(names(qv7_parent_clean))){
        var_name <- as.character(names(qv7_parent_clean)[var])

        # add '-a' to cshq names
        if (grepl("cshq", var_name, fixed = TRUE)) {
            names(qv7_parent_clean)[var] <- gsub("cshq", "cshq_a", var_name)
        }
    }

    # remove 'v7'
    for (var in 1:length(names(qv7_parent_clean))) {
        var_name <- as.character(names(qv7_parent_clean)[var])

        # remove trailing 'v7' from names
        if (grepl("V7", var_name, fixed = TRUE)) {
            names(qv7_parent_clean)[var] <- gsub("V7", "", var_name)
        }

        if (grepl("v7", var_name, fixed = TRUE)) {
            names(qv7_parent_clean)[var] <- gsub("v7", "", var_name)
        }

        # remove trailing 'v' from names
        if (grepl("vcebq", var_name, fixed = TRUE)) {
            names(qv7_parent_clean)[var] <- gsub("vcebq", "cebq", var_name)
        }

        # remove trailing '_7' from names
        if (grepl("cfq", var_name, fixed = TRUE)) {
            names(qv7_parent_clean)[var] <- gsub("_7", "", var_name)
        }

        if (grepl("brief", var_name, fixed = TRUE)) {
            names(qv7_parent_clean)[var] <- gsub("_7", "", var_name)
        }

    }

    names(qv7_parent_clean)[c(2, 6:79)] <- c("start_date", "pds_1", "pds_2", "pds_3", "pds_4m", "pds_5m", "pds_6m", "pds_4f",  "pds_5fa", "pds_5fb", "pds_5fc", "pds_5fd", "pds_6f", "tanner_male", "tanner_female", "paq_m_wakeup", "paq_m_travel_school", "paq_m_traveltime_school", "paq_m_arriveschool", "paq_m_leaveschool", "paq_m_travel_home", "paq_m_traveltime_home", "paq_m_indoor_lowintensity", "paq_m_indoor_highintensity", "paq_m_outdoor_lowintensity", "paq_m_outdoor_highintensity", "paq_m_bedtime", "paq_t_wakeup", "paq_t_travel_school", "paq_t_traveltime_school", "paq_t_arriveschool", "paq_t_leaveschool", "paq_t_travel_home", "paq_t_traveltime_home", "paq_t_indoor_lowintensity", "paq_t_indoor_highintensity", "paq_t_outdoor_lowintensity", "paq_t_outdoor_highintensity", "paq_t_bedtime", "paq_w_wakeup", "paq_w_travel_school", "paq_w_traveltime_school", "paq_w_arriveschool", "paq_w_leaveschool", "paq_w_travel_home", "paq_w_traveltime_home", "paq_w_indoor_lowintensity", "paq_w_indoor_highintensity", "paq_w_outdoor_lowintensity", "paq_w_outdoor_highintensity", "paq_w_bedtime", "paq_th_wakeup", "paq_th_travel_school", "paq_th_traveltime_school", "paq_th_arriveschool", "paq_th_leaveschool", "paq_th_travel_home", "paq_th_traveltime_home", "paq_th_indoor_lowintensity", "paq_th_indoor_highintensity", "paq_th_outdoor_lowintensity",  "paq_th_outdoor_highintensity", "paq_th_bedtime", "paq_f_wakeup", "paq_f_travel_school", "paq_f_traveltime_school", "paq_f_arriveschool", "paq_f_leaveschool", "paq_f_travel_home", "paq_f_traveltime_home", "paq_f_indoor_lowintensity", "paq_f_indoor_highintensity", "paq_f_outdoor_lowintensity", "paq_f_outdoor_highintensity", "paq_f_bedtime")

    names(qv7_parent_clean)[98:138] <- c("parent_respondent", "parent_dob", "sr_dad_height_ft", "sr_dad_height_in", "sr_dad_weight_lb", "sr_mom_height_ft", "sr_mom_height_in", "sr_mom_weight_lb", "household_n_mom", "household_n_dad", "household_n_stepmom", "household_n_stepdad", "household_n_brother", "household_n_sister", "household_n_stepbrother", "household_n_stepsister", "household_n_grandmom", "household_n_grandad", "household_n_aunt", "household_n_uncle", "household_n_cousin", "household_n_other", "n_parental_separations", "n_fostercare_placements", "living_with_partner", "maritalstatus", "maritalstatus_other", "income", "parent_ed", "parent_ed_other", "partner_ed", "partner_ed_other", "parent_employed", "parent_retired", "parent_wk_workhr", "partner_employed", "partner_retired", "partner_wk_workhr", "mom_weightgain_10lb", "allowance", "allowance_amount")

    names(qv7_parent_clean)[149:228] <- c("feedschild", "feedschild_other", "buysfood", "buysfood_other", "feq_eatout", "freq_familydinner", "freq_homelunch", "family_foodcond", "snap", "wic", "tanf", "medicaid", "liheap", "partial_lunch_assistance", "full_lunch_assistance", "other_program", "other_program_freetxt", "foodpantry_use", "freq_foodpantry", "foodbudget_mo", "prefered_grocerystore", "rank_packagedbread", "rank_bakerybread", "rank_saltysnack", "rank_sweetsnack", "rank_cheese", "rank_milk", "rank_yogurt", "rank_butter", "rank_eggs", "rank_otherdairy", "rank_coffeetea", "rank_carbonated_drink", "rank_fruitjuice_drink", "rank_sports_drink", "rank_alcohol_drink", "rank_redmeat", "rank_poultry", "rank_seafood", "rank_pasta_rice", "rank_soup", "rank_nuts_seeds", "rank_nutjelly_spread", "rank_breakfast_cereal", "rank_breakfast_replacement", "rank_meal_replacement", "rank_freshprep_foods", "rank_fresh_veg", "rank_fresh_fruit", "rank_can_veg", "rank_can_fruit",  "rank_frozen_veg", "rank_frozen_fruit", "rank_frozen_dinner", "rank_frozen_breakfast", "rank_frozen_desert", "rank_candygum", "rank_bakingsupplies", "rank_condiments", "rank_dips_spreads", "rank_salad_dressings", "rank_spice_seasoning",  "rank_cooking_oil", "grow_fruits", "grow_veg", "grow_jellyspreads", "grow_nuts_seeds", "grow_milk", "grow_cheese", "grow_butter", "grow_eggs", "grow_redmeat", "grow_poultry", "encourage_plateclean_vas", "child_plateclean_vas",  "percieved_child_kcal", "pcent_parent_portionchoice", "pcent_partner_portionchoice", "pcent_child_portionchoice",  "pcent_other_portionchoice")

    names(qv7_parent_clean)[229:371] <- c("pss_practice1", "pss_vas_hunger", "pss_vas_couldeat", "pss_vas_fullness", "pss_practice2",  "pss_apple_eat", "pss_apple_freq", "pss_apple_much", "pss_apple_like", "pss_apple_portion", "pss_apple_nutrition",  "pss_broccoli_eat", "pss_broccoli_freq", "pss_broccoli_much", "pss_broccoli_like", "pss_broccoli_portion", "pss_broccoli_nutrition",  "pss_cake_eat", "pss_cake_freq", "pss_cake_much", "pss_cake_like", "pss_cake_portion", "pss_cake_nutrition",  "pss_candy_eat", "pss_candy_freq", "pss_candy_much", "pss_candy_like", "pss_candy_potion", "pss_candy_nutrition",  "pss_carrot_eat", "pss_carrot_freq", "pss_carrot_much", "pss_carrot_like", "pss_carrot_portion", "pss_carrot_nutrition", "pss_cornflakes_eat", "pss_cornflakes_freq", "pss_ccornflakes_much", "pss_cornflakes_like", "pss_cornflakes_portion", "pss_cornflakes_nutrition", "pss_cheese_brgr_eat", "pss_cheese_brgr_freq", "pss_cheese_brgr_much", "pss_cheese_brgr_like", "pss_cheese_brgr_portion", "pss_cheese_brgr_nutrition", "pss_chkn_nug_eat", "pss_chkn_nug_freq", "pss_chkn_nug_much", "pss_chkn_nug_like", "pss_chkn_nug_portion", "pss_chkn_nug_nutrition", "pss_fries_eat", "pss_fries_freq", "pss_fries_much", "pss_fries_like", "pss_fries_portion", "pss_fries_nutrition", "pss_garlic_bread_eat", "pss_garlic_bread_freq", "pss_garlic_bread_much", "pss_garlic_bread_like", "pss_garlic_bread_portion", "pss_garlic_bread_nutrition", "pss_goldfish_eat", "pss_goldfish_freq", "pss_goldfish_much", "pss_goldfish_like", "pss_goldfish_portion", "pss_goldfish_nutrition", "pss_grapes_eat", "pss_grapes_freq", "pss_grapes_much", "pss_grapes_like", "pss_grapes_portion", "pss_grapes_nutrition",  "pss_choc_icecream_eat", "pss_choc_icecream_freq", "pss_choc_icecream_much", "pss_choc_icecream_like", "pss_choc_icecream_portion", "pss_choc_icecream_nutrition", "pss_mac_cheese_eat", "pss_mac_cheese_freq", "pss_mac_cheese_much", "pss_mac_cheese_like",  "pss_mac_cheese_portion", "pss_mac_cheese_nutrition", "pss_milk_eat", "pss_milk_freq", "pss_milk_much", "pss_milk_like",  "pss_milk_portion", "pss_milk_nutrition", "pss_orangejuice_eat", "pss_orangejuice_freq", "pss_orangejuice_much",  "pss_orangejuice_like", "pss_orangejuice_portion", "pss_orangejuice_nutrition", "pss_pbj_sndwch_eat", "pss_pbj_sndwch_freq", "pss_pbj_sndwch_much", "pss_pbj_sndwch_like", "pss_pbj_sndwch_portion", "pss_pbj_sndwch_nutrition", "pss_peas_eat", "pss_peas_freq", "pss_peas_much", "pss_peas_like", "pss_peas_portion", "pss_peas_nutrition", "pss_pizza_eat",  "pss_pizza_freq", "pss_pizza_much", "pss_pizza_like", "pss_pizza_portion", "pss_pizza_nutrition", "pss_soda_eat", "pss_soda_freq", "pss_soda_much", "pss_soda_like", "pss_soda_portion", "pss_soda_nutrition", "pss_soup_eat",  "pss_soup_freq", "pss_soup_much", "pss_soup_like", "pss_soup_portion", "pss_soup_nutrition", "pss_tomatoes_eat",  "pss_tomatoes_freq", "pss_tomatoes_much", "pss_tomatoes_like", "pss_tomatoes_portion", "pss_tomatoes_nutrition",  "pss_yogurt_eat", "pss_yogurt_freq", "pss_yogurt_much", "pss_yogurt_like", "pss_yogurt_portion", "pss_yogurt_nutrition")


    ## update data labels
    names(qv7_parent_clean_labels) <- names(qv7_parent_clean)

    ## 5) general fixes to labels (remove '- 1') ####

    ## remove formatting errors
    for (var in 1:length(names(qv7_parent_clean))) {
        var_name <- as.character(names(qv7_parent_clean)[var])

        # remove trailing '... - 1' from labels
        if (grepl(" - 1", qv7_parent_clean_labels[[var_name]], fixed = TRUE)) {
            qv7_parent_clean_labels[[var_name]] <- gsub("\\ - 1.*", "", qv7_parent_clean_labels[[var_name]])
        }

        if (grepl(" - 3", qv7_parent_clean_labels[[var_name]], fixed = TRUE)) {
            qv7_parent_clean_labels[[var_name]] <- gsub("\\ - 3.*", "", qv7_parent_clean_labels[[var_name]])
        }

        # remove ' \' ' from apostrophes
        if (grepl("'s", qv7_parent_clean_labels[[var_name]], fixed = TRUE)) {
            qv7_parent_clean_labels[[var_name]] <- gsub("\\'s", "", qv7_parent_clean_labels[[var_name]])
        }

        # remove trailing 'V7 ' from labels
        if (grepl("V7", qv7_parent_clean_labels[[var_name]], fixed = TRUE)) {
            qv7_parent_clean_labels[[var_name]] <- gsub("\\V7 - ", "", qv7_parent_clean_labels[[var_name]])
            qv7_parent_clean_labels[[var_name]] <- gsub("\\V7 ", "", qv7_parent_clean_labels[[var_name]])
            qv7_parent_clean_labels[[var_name]] <- gsub("\\V7", "", qv7_parent_clean_labels[[var_name]])
        }
    }

    ## 5a) fix CSHQ labels ####
    qv7_parent_clean_labels[["cshq_a1"]] <- "1. Child goes to bed at the same time at night."
    qv7_parent_clean_labels[["cshq_a10"]] <- "10. Child sleeps about the same amount each day."
    qv7_parent_clean_labels[["cshq_a11"]] <- "11. Child is restless and moves a lot during sleep."
    qv7_parent_clean_labels[["cshq_a12"]] <- "12. Child moves to someone else’s bed during the night (parent, sibling, etc.)."
    qv7_parent_clean_labels[["cshq_a13"]] <- "13. Child grinds teeth during sleep (your dentist may have told you this)."
    qv7_parent_clean_labels[["cshq_a14"]] <- "14. Child snores loudly."
    qv7_parent_clean_labels[["cshq_a15"]] <- "15. Child awakens during the night and is sweating, screaming, and inconsolable."
    qv7_parent_clean_labels[["cshq_a16"]] <- "16. Child naps during the day."
    qv7_parent_clean_labels[["cshq_a17"]] <- "17. Child wakes up once during the night."
    qv7_parent_clean_labels[["cshq_a18"]] <- "18. Child wakes up more than once during the night."

    ## 5b) fix weight/height labels ####
    qv7_parent_clean_labels[["sr_mom_height_ft"]] <- "Biological Mother current height, in FEET"

    ## 5c) fix CEBQ labels ####
    qv7_parent_clean_labels[["cebq1"]] <- "CEBQ - 1. My child loves food."
    qv7_parent_clean_labels[["cebq10"]] <- "CEBQ - 10. My child enjoys tasting new foods."
    qv7_parent_clean_labels[["cebq11"]] <- "CEBQ - 11. My child eats less when s/he is tired."
    qv7_parent_clean_labels[["cebq12"]] <- "CEBQ - 12. My child is always asking for food."
    qv7_parent_clean_labels[["cebq13"]] <- "CEBQ - 13. My child eats more when annoyed."
    qv7_parent_clean_labels[["cebq14"]] <- "CEBQ - 14. If allowed to, my child would eat too much."
    qv7_parent_clean_labels[["cebq15"]] <- "CEBQ - 15. My child eats more when anxious."
    qv7_parent_clean_labels[["cebq16"]] <- "CEBQ - 16. My child enjoys a wide variety of foods."
    qv7_parent_clean_labels[["cebq17"]] <- "CEBQ - 17. My child leaves food on his/her plate at the end of a meal."
    qv7_parent_clean_labels[["cebq18"]] <- "CEBQ - 18. My child takes more than 30 minutes to finish a meal."
    qv7_parent_clean_labels[["cebq19"]] <- "CEBQ - 19. Given the choice, my child would eat most of the time."

    ## 5d) fix ever eat yogurt variable label
    qv7_parent_clean_labels[["pss_yogurt_eat"]] <- "PSS Strawberry Yogurt - Ever Eat"


    #### 6) fix 99's and other poor categories ####

    ## check for labels/99 option: 1) if 99's exist, make a 'prefere not to answer' (pna) variable to go in pna database, 2) replace 99's with NA and make variable numeric

    ## make pna database
    qv7_parent_pna <- data.frame(id = qv7_parent_clean[["id"]])
    qv7_parent_pna_labels <- lapply(qv7_parent_pna, function(x) attributes(x)$label)
    qv7_parent_pna_labels[["id"]] <- qv7_parent_clean_labels[["id"]]


    pna_label <- "Note: prefer not to answer (pna) marked NA - see pna database for which were pna rather than missing NA"

    ## 6a) self-reported heights/weights data ####
    parent_anthro_vars <- names(qv7_parent_clean)[100:105]

    ## duplicate self-reported weights because set 119 = <120 lb and 400 = 400+
    qv7_parent_clean[["sr_dad_weight_lb_cat"]] <- qv7_parent_clean[["sr_dad_weight_lb"]]
    qv7_parent_clean_labels[["sr_dad_weight_lb_cat"]] <- "categorical parent-reported father weight in pounds: category per pound increment with 119 = '<120 pounds' and 400 = '400+ pounds'"

    qv7_parent_clean[["sr_mom_weight_lb_cat"]] <- qv7_parent_clean[["sr_mom_weight_lb"]]
    qv7_parent_clean_labels[["sr_mom_weight_lb_cat"]] <- "categorical parent-reported father weight in pounds: category per pound increment with 119 = '<120 pounds' and 400 = '400+ pounds'"

    # update weight variable labels
    qv7_parent_clean_labels[["sr_dad_weight_lb"]] <- "parent-reported father weight in pounds - note: categorical values 119 = <120 lb and 400 = 400+ set to NA so can compute BMI continuous. These values presevered in categorical ('_cat') version of variable"
    qv7_parent_clean_labels[["sr_mom_weight_lb"]] <- "parent-reported mother weight in pounds - note: categorical values 119 = <120 lb and 400 = 400+ set to NA so can compute BMI continuous. These values presevered in categorical ('_cat') version of variable"
    qv7_parent_clean_labels[["sr_dad_height_ft"]] <- "parent-reported father height in feet."
    qv7_parent_clean_labels[["sr_dad_height_in"]] <- "parent-reported father height in inches"
    qv7_parent_clean_labels[["sr_mom_height_ft"]] <- "parent-reported mother height in feet"
    qv7_parent_clean_labels[["sr_mom_height_in"]] <- "parent-reported mother height in inches"

    # loop throgh to fix 99's and lb 119's and 400's
    for (v in 1:length(parent_anthro_vars)) {

        # get variable name
        pvar <- parent_anthro_vars[v]

        # if has '99' value, create new pna variable marking pna == 1
        if (is.element(99, qv7_parent_clean[[pvar]])) {
            pna_dat <- ifelse(is.na(qv7_parent_clean[[pvar]]), 0, ifelse(qv7_parent_clean[[pvar]] == 99, 1, 0))

            new_pna <- length(names(qv7_parent_pna)) + 1
            qv7_parent_pna[[new_pna]] <- pna_dat

            names(qv7_parent_pna)[new_pna] <- paste0(pvar, "_pna")

            # add label to pna database
            qv7_parent_pna_labels[[paste0(pvar, "_pna")]] <- paste0("prefer not to answer marked for variable ", pvar,
                                                                    ": ", qv7_parent_clean_labels[[pvar]])

            # update true data label (only want to pna label if needed)
            qv7_parent_clean_labels[[pvar]] <- paste0(qv7_parent_clean_labels[[pvar]], " -- ", pna_label)
        }

        if (grepl("lb", pvar, fixed = TRUE)) {
            # check for values '119' and '400' as they indicate < 119 and 400+, respectively (values are kept in
            # the categorical version of this variable - see above) labels were updated above
            qv7_parent_clean[[pvar]] <- ifelse(is.na(qv7_parent_clean[[pvar]]), NA, ifelse(qv7_parent_clean[[pvar]] ==
                                                                                               119, NA, ifelse(qv7_parent_clean[[pvar]] == 400, NA, as.numeric(qv7_parent_clean[[pvar]]))))
        } else {
            # convert 99 to NA and make numeric variable labels only update if had 99 - done in if statement above
            qv7_parent_clean[[pvar]] <- ifelse(qv7_parent_clean[[pvar]] == 99, NA, as.numeric(qv7_parent_clean[[pvar]]))
        }
    }

    ## 6b) PA time data with 99's and odd anchor categories ####
    paq_vars <- names(qv7_parent_clean)[c(20:79)]

    # loop through to fix 99's
    for (v in 1:length(paq_vars)) {

        # get variable name
        pvar <- paq_vars[v]

        # if has '99' value, create new pna variable marking pna == 1
        if (is.element(99, qv7_parent_clean[[pvar]])) {
            pna_dat <- ifelse(is.na(qv7_parent_clean[[pvar]]), 0, ifelse(qv7_parent_clean[[pvar]] == 99, 1, 0))

            new_pna <- length(names(qv7_parent_pna)) + 1
            qv7_parent_pna[[new_pna]] <- pna_dat

            # make logical variable and name it
            names(qv7_parent_pna)[new_pna] <- paste0(pvar, "_pna")

            # add label to pna database
            qv7_parent_pna_labels[[paste0(pvar, "_pna")]] <- paste0("prefer not to answer marked for variable ", pvar,
                                                                    ": ", qv7_parent_clean_labels[[pvar]])

            # update true data label (only want to pna label if needed)
            qv7_parent_clean_labels[[pvar]] <- paste0(qv7_parent_clean_labels[[pvar]], " -- ", pna_label)
        }

        if (grepl("wakeup", pvar, fixed = TRUE)) {
            # duplicate to make categorical variable
            nvar <- length(names(qv7_parent_clean))
            qv7_parent_clean[[nvar + 1]] <- qv7_parent_clean[[pvar]]
            names(qv7_parent_clean)[[nvar + 1]] <- paste0(pvar, "_cat")

            # update label
            qv7_parent_clean_labels[[paste0(pvar, "_cat")]] <- paste0("categorical ", qv7_parent_clean_labels[[pvar]],
                                                                      " -note: 1 = 'Before 5 AM' and 15 = 'After 8 AM'")

            # convert to factor label values
            qv7_parent_clean[[pvar]] <- haven::as_factor(qv7_parent_clean[[pvar]])

            # check for values 'Before 5 am', 'After 8:00 am', and 'Don't want to answer' (values are kept in the
            # categorical version of this variable)
            qv7_parent_clean[[pvar]] <- ifelse(is.na(qv7_parent_clean[[pvar]]) | qv7_parent_clean[[pvar]] == "Before 5:00 AM" |
                                                   qv7_parent_clean[[pvar]] == "After 8:00 AM" | qv7_parent_clean[[pvar]] == "Don't want to answer", NA,
                                               as.character(qv7_parent_clean[[pvar]]))

        } else if (grepl("arriveschool", pvar, fixed = TRUE)) {
            # duplicate to make categorical variable
            nvar <- length(names(qv7_parent_clean))
            qv7_parent_clean[[nvar + 1]] <- qv7_parent_clean[[pvar]]
            names(qv7_parent_clean)[[nvar + 1]] <- paste0(pvar, "_cat")

            # update label
            qv7_parent_clean_labels[[paste0(pvar, "_cat")]] <- paste0("categorical ", qv7_parent_clean_labels[[pvar]],
                                                                      " -note: 15 = 'After 8:45 PM'")

            # convert to factor label values
            qv7_parent_clean[[pvar]] <- haven::as_factor(qv7_parent_clean[[pvar]])

            # check for value 'After 8:45 AM' and 'Don't want to answer' (values are kept in the categorical
            # version of this variable)
            qv7_parent_clean[[pvar]] <- ifelse(is.na(qv7_parent_clean[[pvar]]) | qv7_parent_clean[[pvar]] == "After 8:45 AM" |
                                                   qv7_parent_clean[[pvar]] == "Don't want to answer", NA, as.character(qv7_parent_clean[[pvar]]))

        } else if (grepl("leaveschool", pvar, fixed = TRUE)) {
            # duplicate to make categorical variable
            nvar <- length(names(qv7_parent_clean))
            qv7_parent_clean[[nvar + 1]] <- qv7_parent_clean[[pvar]]
            names(qv7_parent_clean)[[nvar + 1]] <- paste0(pvar, "_cat")

            # update label
            qv7_parent_clean_labels[[paste0(pvar, "_cat")]] <- paste0("categorical ", qv7_parent_clean_labels[[pvar]],
                                                                      " -note: 15 = 'After 8:45 PM'")

            # convert to factor label values
            qv7_parent_clean[[pvar]] <- haven::as_factor(qv7_parent_clean[[pvar]])

            # check for value 'Before 2 PM', 'After 6 PM', and 'Don't want to answer' (values are kept in the
            # categorical version of this variable)
            qv7_parent_clean[[pvar]] <- ifelse(is.na(qv7_parent_clean[[pvar]]) | qv7_parent_clean[[pvar]] == "Before 2:00 PM" |
                                                   qv7_parent_clean[[pvar]] == "After 6:00 PM" | qv7_parent_clean[[pvar]] == "Don't want to answer", NA,
                                               as.character(qv7_parent_clean[[pvar]]))

        } else if (grepl("bedtime", pvar, fixed = TRUE)) {
            # duplicate to make categorical variable
            nvar <- length(names(qv7_parent_clean))
            qv7_parent_clean[[nvar + 1]] <- qv7_parent_clean[[pvar]]
            names(qv7_parent_clean)[[nvar + 1]] <- paste0(pvar, "_cat")

            # update label
            qv7_parent_clean_labels[[paste0(pvar, "_cat")]] <- paste0("categorical ", qv7_parent_clean_labels[[pvar]],
                                                                      " -note: 15 = 'After 10:00 PM'")

            # convert to factor label values
            qv7_parent_clean[[pvar]] <- haven::as_factor(qv7_parent_clean[[pvar]])

            # check for value 'After 10 PM' and 'Don't want to answer' (values are kept in the categorical version
            # of this variable)
            qv7_parent_clean[[pvar]] <- ifelse(is.na(qv7_parent_clean[[pvar]]) | qv7_parent_clean[[pvar]] == "After 10:00 PM" |
                                                   qv7_parent_clean[[pvar]] == "Don't want to answer", NA, as.character(qv7_parent_clean[[pvar]]))

        } else {
            # drop 99 level label labels only update if had 99 - done in if statement above
            qv7_parent_clean[[pvar]] <- sjlabelled::remove_labels(qv7_parent_clean[[pvar]], labels = "Don't want to answer")

            # extract variable attributes
            pvar_attr <- attributes(qv7_parent_clean[[pvar]])

            # replace 99 values
            qv7_parent_clean[[pvar]] <- ifelse(is.na(qv7_parent_clean[[pvar]]) | qv7_parent_clean[[pvar]] == 99, NA,
                                               qv7_parent_clean[[pvar]])

            # replace attributes
            attributes(qv7_parent_clean[[pvar]]) <- pvar_attr
        }
    }

    ## 6c) continuous variables with 99's data ####

    level99_issue_contvars <- names(qv7_parent_clean)[c(5, 14, 16, 106:121, 153:154, 167)]

    #update 1 label
    qv7_parent_clean_labels[['pds_5fd']] <- paste0(qv7_parent_clean_labels[['pds_5fd']], 'in weeks')

    for (v in 1:length(level99_issue_contvars)) {
        # get variable name
        pvar <- level99_issue_contvars[v]

        # if has '99' value, create new pna variable marking pna == 1
        if (is.element(99, qv7_parent_clean[[pvar]])) {
            pna_dat <- ifelse(is.na(qv7_parent_clean[[pvar]]), 0, ifelse(qv7_parent_clean[[pvar]] == 99, 1, 0))

            new_pna <- length(names(qv7_parent_pna)) + 1
            qv7_parent_pna[[new_pna]] <- pna_dat

            names(qv7_parent_pna)[new_pna] <- paste0(pvar, "_pna")

            # add label to pna database
            qv7_parent_pna_labels[[paste0(pvar, "_pna")]] <- paste0("prefer not to answer marked for variable ", pvar, ": ", qv7_parent_clean_labels[[pvar]])

            # update true data label (only want to pna label if needed)
            qv7_parent_clean_labels[[pvar]] <- paste0(qv7_parent_clean_labels[[pvar]], " -- ", pna_label)
        }

        # convert 99 to NA and make numeric variable labels only update if had 99 - done in if statement above
        qv7_parent_clean[[pvar]] <- ifelse(qv7_parent_clean[[pvar]] == 99, NA, as.numeric(qv7_parent_clean[[pvar]]))
    }

    ## 6d) categorical variables with 99's data ####
    level99_issue_catvars <- names(qv7_parent_clean)[c(3, 98, 122:123, 125:126, 128, 130:131, 133:134, 136:148, 149, 151, 156:164, 166, 168:169, 212:221, 235, 241, 247, 253, 259, 265, 271, 277, 283, 289, 295, 301, 307, 313, 319, 325, 331, 337, 343, 349, 355, 361, 367, 372:506)]

    for (v in 1:length(level99_issue_catvars)) {
        # get variable name
        pvar <- level99_issue_catvars[v]

        # if has '99' value, create new pna variable marking pna == 1
        if (is.element(99, qv7_parent_clean[[pvar]])) {
            pna_dat <- ifelse(is.na(qv7_parent_clean[[pvar]]), 0, ifelse(qv7_parent_clean[[pvar]] == 99, 1, 0))

            new_pna <- length(names(qv7_parent_pna)) + 1
            qv7_parent_pna[[new_pna]] <- pna_dat

            names(qv7_parent_pna)[new_pna] <- paste0(pvar, "_pna")

            # add label to pna database
            qv7_parent_pna_labels[[paste0(pvar, "_pna")]] <- paste0("prefer not to answer marked for variable ", pvar,  ": ", qv7_parent_clean_labels[[pvar]])

            # update true data label (only want to pna label if needed)
            qv7_parent_clean_labels[[pvar]] <- paste0(qv7_parent_clean_labels[[pvar]], " -- ", pna_label)

        }

        # drop 99 level label labels only update if had 99 - done in if statement above
        qv7_parent_clean[[pvar]] <- sjlabelled::remove_labels(qv7_parent_clean[[pvar]], labels = "Don't want to answer")

        # extract variable attributes
        pvar_attr <- attributes(qv7_parent_clean[[pvar]])

        # replace 99 values
        qv7_parent_clean[[pvar]] <- ifelse(is.na(qv7_parent_clean[[pvar]]) | qv7_parent_clean[[pvar]] == 99, NA, qv7_parent_clean[[pvar]])

        # replace attributes
        attributes(qv7_parent_clean[[pvar]]) <- pvar_attr
    }

    ## 6e) hrs work per week ####
    wkhr_vars <- names(qv7_parent_clean)[c(132, 135)]

    for (v in 1:length(wkhr_vars)) {
        # get variable name
        pvar <- wkhr_vars[v]

        # if has '99' value, create new pna variable marking pna == 1
        if (is.element(99, qv7_parent_clean[[pvar]])) {
            pna_dat <- ifelse(is.na(qv7_parent_clean[[pvar]]), 0, ifelse(qv7_parent_clean[[pvar]] == 99, 1, 0))

            new_pna <- length(names(qv7_parent_pna)) + 1
            qv7_parent_pna[[new_pna]] <- pna_dat

            names(qv7_parent_pna)[new_pna] <- paste0(pvar, "_pna")

            # add label to pna database
            qv7_parent_pna_labels[[paste0(pvar, "_pna")]] <- paste0("prefer not to answer marked for variable ", pvar,
                                                                    ": ", qv7_parent_clean_labels[[pvar]])

            # update true data label (only want to pna label if needed)
            qv7_parent_clean_labels[[pvar]] <- paste0(qv7_parent_clean_labels[[pvar]], " -- ", pna_label)
        }

        # duplicate to make categorical variable
        nvar <- length(names(qv7_parent_clean))
        qv7_parent_clean[[nvar + 1]] <- qv7_parent_clean[[pvar]]
        names(qv7_parent_clean)[[nvar + 1]] <- paste0(pvar, "_cat")

        # update label
        qv7_parent_clean_labels[[paste0(pvar, "_cat")]] <- paste0("categorical ", qv7_parent_clean_labels[[pvar]], " -note: 61 = 'More than 60'")

        # check for value 66 = 'More than 60' and 99 = 'Don't want to answer' (values are kept in the categorical
        # version of this variable)
        qv7_parent_clean[[pvar]] <- ifelse(is.na(qv7_parent_clean[[pvar]]) | qv7_parent_clean[[pvar]] == 99 | qv7_parent_clean[[pvar]] ==
                                               61, NA, as.numeric(qv7_parent_clean[[pvar]]))
    }

    #### 7) reformatting dates/times ####

    ## 7a) dates (start, dobs) ####
    qv7_parent_clean[["start_date"]] <- lubridate::ymd(as.Date(qv7_parent_clean[["start_date"]]))
    qv7_parent_clean_labels[["start_date"]] <- "start_date from qualtrics survey meta-data converted to format yyyy-mm-dd in R"

    qv7_parent_clean[["parent_dob"]] <- as.Date(qv7_parent_clean[["parent_dob"]], format = "%m/%d/%Y")
    qv7_parent_clean_labels[["parent_dob"]] <- "parent date of birth converted to format yyyy-mm-dd in R"

    ## 7b) time data from PA (waketime, bedtime) ####
    time_vars <- names(qv7_parent_clean)[c(20, 23:24, 31:32, 35:36, 43:44, 47:48, 55:56, 59:60, 67:68, 71:72, 79)]

    for (v in 1:length(time_vars)) {
        pvar <- time_vars[v]

        # convert time
        qv7_parent_clean[[pvar]] <- format(as.POSIXlt(qv7_parent_clean[[pvar]], format = "%I:%M %p"), format = "%H:%M:%S")

        # update labels
        qv7_parent_clean_labels[[pvar]] <- paste0(qv7_parent_clean_labels[[pvar]], " converted to 24 hour time in R")
    }

    # 8) re-calculate manual variables ####

    ## 8a) parent anthropometrics ####

    # convert to cm and kg
    qv7_parent_clean[["sr_dad_height_cm"]] <- (qv7_parent_clean[["sr_dad_height_ft"]] * 12 + qv7_parent_clean[["sr_dad_height_in"]]) *
        2.54
    qv7_parent_clean_labels[["sr_dad_height_cm"]] <- "parent-reported father height in feet and inches converted to cm in R"

    qv7_parent_clean[["sr_mom_height_cm"]] <- (qv7_parent_clean[["sr_mom_height_ft"]] * 12 + qv7_parent_clean[["sr_mom_height_in"]]) *
        2.54
    qv7_parent_clean_labels[["sr_mom_height_cm"]] <- "parent-reported mother height in feet and inches converted to cm in R"

    qv7_parent_clean[["sr_dad_weight_kg"]] <- qv7_parent_clean[["sr_dad_weight_lb"]]/2.205
    qv7_parent_clean_labels[["sr_dad_weight_kg"]] <- "parent-reported father weight in pounds converted to kg in R"

    qv7_parent_clean[["sr_mom_weight_kg"]] <- qv7_parent_clean[["sr_mom_weight_lb"]]/2.205
    qv7_parent_clean_labels[["sr_mom_weight_kg"]] <- "parent-reported mother weight in pounds converted to kg in R"

    # parent-report bmi, update label
    qv7_parent_clean[["sr_dad_bmi"]] <- round(qv7_parent_clean[["sr_dad_weight_kg"]]/((qv7_parent_clean[["sr_dad_height_cm"]]/100)^2),
                                              digits = 2)
    qv7_parent_clean_labels[["sr_dad_bmi"]] <- "computed bmi from parent-reported father height and weight in R"

    qv7_parent_clean[["sr_mom_bmi"]] <- round(qv7_parent_clean[["sr_mom_weight_kg"]]/((qv7_parent_clean[["sr_mom_height_cm"]]/100)^2),
                                              digits = 2)
    qv7_parent_clean_labels[["sr_mom_bmi"]] <- "computed bmi from parent-reported mother height and weight in R"

    # 9) re-ordering factor levels to start with value 0 ####

    ## 9a) variables that just need to shift values by 1 ####
    relevel_vars <- names(qv7_parent_clean)[c(21:22, 25:26, 33:34, 37:38, 45:46, 49:50, 57:58, 61:62, 69:70, 73:74, 123, 125:126, 128, 138, 149, 168, 169, 235, 241, 247, 253, 259, 265, 271, 277, 283, 289, 295, 301, 307, 313, 319, 325, 331, 337, 343, 349, 355, 361, 367)]

    for (v in 1:length(relevel_vars)) {
        pvar <- relevel_vars[v]

        # subtract 1 from level values so start at 0 rather than 1
        attributes(qv7_parent_clean[[pvar]])$labels <- attributes(qv7_parent_clean[[pvar]])$labels - 1

        # save attributes
        set_attr <- attributes(qv7_parent_clean[[pvar]])

        # subtract 1 from each data value to match new level values
        qv7_parent_clean[[pvar]] <- ifelse(is.na(qv7_parent_clean[[pvar]]), NA, qv7_parent_clean[[pvar]] - 1)

        # reset attributes
        attributes(qv7_parent_clean[[pvar]]) <- set_attr

        # update varaible lables
        qv7_parent_clean_labels[[pvar]] <- paste0(qv7_parent_clean_labels[[pvar]], " re-leveled in R to start with 0")
    }

    # 10) specific fixes to factor level names and variable descriptions ####

    # 10a) make rankings numeric
    qv7_parent_clean[170:211] <- sapply(qv7_parent_clean[170:211], function(x) as.numeric(x))

    #### 11) Format for export ####

    ## 11a ) final re-order/clean of data
    qv7_parent_clean <- qv7_parent_clean[c(1:19, 515, 20:22, 516, 23, 517, 24:30, 518, 31, 519, 32:34, 520, 35, 521, 36:42, 522, 43, 523, 44:46, 524, 47, 525, 48:54, 526, 55, 527, 56:58, 528, 59, 529, 60:66, 530, 67, 531, 68:70, 532, 71, 533, 72:79, 534, 80:102, 513, 103:105, 514, 537:542, 106:132, 535, 133:135, 536, 136:512)]

    qv7_parent_clean_labels <- qv7_parent_clean_labels[c(1:19, 515, 20:22, 516, 23, 517, 24:30, 518, 31, 519, 32:34, 520, 35, 521, 36:42, 522, 43, 523, 44:46, 524, 47, 525, 48:54, 526, 55, 527, 56:58, 528, 59, 529, 60:66, 530, 67, 531, 68:70, 532, 71, 533, 72:79, 534, 80:102, 513, 103:105, 514, 537:542, 106:132, 535, 133:135, 536, 136:512)]

    ## 11b) add attributes to pna data
    n_pna_cols <- length(names(qv7_parent_pna))

    qv7_parent_pna[2:n_pna_cols] <- as.data.frame(lapply(qv7_parent_pna[2:n_pna_cols], function(x) sjlabelled::add_labels(x, labels = c(`Did not skip due to prefer not to answer` = 0, `Prefer not to answer` = 1))))

    for (v in 2:ncol(qv7_parent_pna)){
        class(qv7_parent_pna[[v]]) <- c("haven_labelled", "vctrs_vctr", "double")
    }


    ## 11c) put data in order of participant ID for ease
    qv7_parent_clean <- qv7_parent_clean[order(qv7_parent_clean[["id"]]), ]
    qv7_parent_pna <- qv7_parent_pna[order(qv7_parent_pna[["id"]]), ]

    ## 11d) make sure the variable labels match in the dataset
    qv7_parent_clean = sjlabelled::set_label(qv7_parent_clean, label = matrix(unlist(qv7_parent_clean_labels, use.names = FALSE)))
    qv7_parent_pna = sjlabelled::set_label(qv7_parent_pna, label = matrix(unlist(qv7_parent_pna_labels, use.names = FALSE)))

    # make list of data frame and associated labels
    qv7_parent <- list(data = qv7_parent_clean, dict = qv7_parent_clean_labels, pna_data = qv7_parent_pna, pna_dict = qv7_parent_pna_labels)

    return(qv7_parent)
}
