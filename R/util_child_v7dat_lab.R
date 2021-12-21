#' util_child_v7dat_lab: Process raw qualtrics visit 7 data collected in the lab for the child
#'
#' This function loads the .sav raw data file for the child visit 7 data that was collected via Qualtrics in the lab when the protocol was split due to covid and cleans the data. Cleaning the data involves:
#' 1) extracting all variable descriptions,
#' 2) selecting relevant data columns,
#' 3) removing all practice events (e.g., 999)
#' 4) re-ordering and re-name data columns
#' 5) reformatting dates to be appropriate and computer readable: YYYY-MM-DD
#' 6) re-calculate manual variables
#' 7) re-ordering factor levels
#' 8) random fixes to factor level names and variable descriptions
#'
#' The databases MUST follow the naming convention: Child_V7_Lab_YYYY-MM-DD.sav
#'
#' @inheritParams util_parent_v1dat
#' @inheritParams util_parent_v1dat
#'
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from child visit 7 Qualtrics; 2) dict: all variable descriptions; 3) pna_data: data.frame marking participants who 'prefered not to answer' (pna) specific questions; and 4) pna_dict: all variable descriptions for pna_data
#'
#' @examples
#' #if in same working directory as data:
#' ch_v7_dat_lab <- util_child_v7dat_lab('2021-10-11')
#'
#' \dontrun{
#' #date must be a string. The following will not run:
#' ch_v7_dat_lab <- util_child_v7dat_lab(2021-10-11)
#'
#' #date must match the file name - for file named 'Child_V7_Lab_2021_09_16', the
#' following will not run:
#' ch_v7_dat_lab <- util_child_v7dat_lab('2021_10_11')
#' }
#'
#'
#' @export
#'
util_child_v7dat_lab <- function(date_str, data_path) {

    #### 1. Set up/initial checks #####

    # check that date_str exist and is a string

    datestr_arg <- methods::hasArg(date_str)

    if (isTRUE(datestr_arg) & !is.character(date_str)) {
        stop("data_str must be enter as a string: e.g., '2021_10_11'")
    } else if (isFALSE(datestr_arg)) {
        stop("data_str must set to the data string from the child visit 1 file name: e.g., '2021_09_16'")
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
        qv7_child_path <- paste0(data_path, "/Final_CovidAtHome/Child_V7_Lab_", date_str, ".sav")
    } else {
        qv7_child_path <- paste0("/Final_CovidAtHome/Child_V7_Lab_", date_str, ".sav")
    }

    # check if file exists
    qv7_child_exists <- file.exists(qv7_child_path)

    # load data if it exists
    if (isTRUE(qv7_child_exists)) {
        qv7_child_dat <- as.data.frame(haven::read_spss(qv7_child_path))

    } else {
        if (isTRUE(datapath_arg)) {
            stop("File does not exist. Check date_str and data_path entered")
        } else {
            stop("File does not exist. Check date_str and that the data exists in current working directory")
        }
    }

    #### 3. Clean Data #####

    # 1) extract variable labels/descriptions  ####
    qv7_child_labels <- lapply(qv7_child_dat, function(x) attributes(x)$label)

    # 2) selecting relevant data columns ####
    qv7_child_clean <- qv7_child_dat[c(1, 18:20, 28:51, 58:60, 68:95, 107, 109:233, 238)]

    ## update labels
    qv7_child_clean_labels <- qv7_child_labels[c(1, 18:20, 28:51, 58:60, 68:95, 107, 109:233, 238)]


    # 3) removing all practice events (e.g., 999) ####
    qv7_child_clean <- qv7_child_clean[!is.na(qv7_child_clean[["ID"]]) & qv7_child_clean[["ID"]] < 999, ]

    # 4) re-ordering and re-name data columns general order ####

    #1) child information (sex, dob, h/w, bmi, puberty), 2) freddies, 3) food VAS 4) intakes (preMeal, EAH, meal duration), 5) wanting, LOC, 6) CTC, CWC, etc 7) notes

    qv7_child_clean <- qv7_child_clean[c(2, 1, 3:4, 84:91, 92:93, 96, 94:95, 5:24, 29:31, 97:184, 32:59, 25:28, 61:83, 60, 185:186)]

    qv7_child_clean_labels <- qv7_child_clean_labels[c(2, 1, 3:4, 84:91, 92:93, 96, 94:95, 5:24, 29:31, 97:184, 32:59, 25:28, 61:83, 60, 185:186)]

    ## re-name variables
    names(qv7_child_clean) <- c("id", "start_date", "sex", "dob", "height1", "height2", "weight1", "weight2", "height_avg", "weight_avg", "bmi", "bmi_percentile", "freddy_pre_meal", "freddy_post_meal", "freddy_pre_want", "freddy_pre_eah", "freddy_post_eah", "vas_practice", "vas_popcorn", "vas_pretzle", "vas_cornchip", "vas_cookie", "vas_brownie", "vas_starburst", "vas_skittle", "vas_hershey", "vas_icecream", "vas_pbj_sndwch", "vas_ham_sndwch", "vas_turkey_sndwch", "vas_cheese_sndwch", "vas_applesauce", "vas_potatoechip", "vas_babycarrot", "vas_oreo", "vas_milk", "vas_water", "meal_start", "meal_end", "meal_dur", "noplate_applesauce_g", "plate_applesauce_g", "post_applesauce_g", "consumed_applesauce_g", "noplate_carrot_g", "plate_carrot_g", "post_carrot_g", "consumed_carrot_g", "noplate_cheese_sndwch_g", "plate_cheese_sndwch_g", "post_cheese_sndwch_g", "consumed_cheese_sndwch_g", "noplate_cookies_g", "plate_cookies_g", "post_cookies_g", "consumed_cookies_g", "noplate_ham_sndwch_g", "plate_ham_sndwch_g", "post_ham_sndwch_g", "consumed_ham_sndwch_g", "noplate_milk_g", "plate_milk_g", "post_milk_g", "consumed_milk_g", "noplate_pbj_sndwch_g", "plate_pbj_sndwch_g", "post_pbj_sndwch_g", "consumed_pbj_sndwch_g", "noplate_potatochip_g", "plate_potatochip_g", "post_potatochip_g", "consumed_potatochip_g", "noplate_turkey_sndwch_g", "plate_turkey_sndwch_g", "post_turkey_sndwch_g", "consumed_turkey_sndwch_g", "noplate_ketchup_g", "plate_ketchup_g", "post_ketchup_g", "consumed_ketchup_g", "noplate_mayo_g", "plate_mayo_g", "post_mayo_g", "consumed_mayo_g", "noplate_mustard_g", "plate_mustard_g", "post_mustard_g", "consumed_mustard_g", "noplate_brownies_g", "plate_brownies_g", "post_brownies_g", "consumed_brownies_g", "noplate_cornchips_g", "plate_cornchips_g", "post_cornchips_g", "consumed_cornchips_g", "noplate_hersheys_g", "plate_hersheys_g", "post_hersheys_g", "consumed_hersheys_g", "noplate_icecream_g", "plate_icecream_g", "post_icecream_g", "consumed_icecream_g", "noplate_oreos_g", "plate_oreos_g", "post_oreos_g", "consumed_oreos_g", "noplate_popcorn_g", "plate_popcorn_g", "post_popcorn_g", "consumed_popcorn_g", "noplate_pretzels_g", "plate_pretzels_g", "post_pretzels_g", "consumed_pretzels_g", "noplate_skittles_g", "plate_skittles_g", "post_skittles_g", "consumed_skittles_g", "noplate_starbursts_g", "plate_starbursts_g", "post_starbursts_g", "consumed_starbursts_g", "noplate_water_g", "plate_water_g", "post_water_g", "consumed_water_g", "want_water", "want_brownies", "want_applesauce", "want_carrots", "want_cars", "want_cheese", "want_cookies", "want_cornchip", "want_toy", "want_connect4", "want_crayons", "want_ham", "want_dino", "want_hershey", "want_icecream", "want_jenga", "want_legos", "want_elephant", "want_oreos", "want_pbj_sndwch", "want_popcorn", "want_chips", "want_pretzels", "want_skittles", "want_trains", "want_trucks", "want_starbursts", "want_turkey_sndwch", "rank_mac_cheese", "rank_chkn_nug", "rank_broccoli", "rank_grape", "loc1", "loc2a", "loc2b", "loc2c", "loc3", "loc4", "loc5", "loc6", "loc7", "loc8", "loc9",  "loc10", "loc11", "loc12", "loc13", "loc14", "loc15", "loc16a", "loc17", "loc18", "loc19", "loc16b", "loc20", "spacegame_reward", "food_initials", "child_notes")

    ## update data labels
    names(qv7_child_clean_labels) <- names(qv7_child_clean)

    # 5) reformatting dates to be appropriate and computer readable YYYY-MM-DD ####
    qv7_child_clean[["start_date"]] <- lubridate::ymd(as.Date(qv7_child_clean[["start_date"]]))
    qv7_child_clean_labels[["start_date"]] <- "start_date from qualtrics survey meta-data converted to format yyyy-mm-dd in R"

    qv7_child_clean[["dob"]] <- as.Date(qv7_child_clean[["dob"]], format = "%m/%d/%Y")
    qv7_child_clean_labels[["dob"]] <- "date of birth converted to format yyyy-mm-dd in R"

    # 6) re-calculate manual variables ####

    # avg child height, update label
    qv7_child_clean[["height_avg"]] <- ifelse(is.na(qv7_child_clean[["height1"]]) | is.na(qv7_child_clean[["height2"]]), NA, rowSums(qv7_child_clean[c("height1",
        "height2")], na.rm = TRUE)/2)
    qv7_child_clean_labels[["height_avg"]] <- "average height calculated in R"

    # avg child weight, update label
    qv7_child_clean[["weight_avg"]] <- ifelse(is.na(qv7_child_clean[["weight1"]]) | is.na(qv7_child_clean[["weight2"]]), NA, rowSums(qv7_child_clean[c("weight1",
        "weight2")], na.rm = TRUE)/2)
    qv7_child_clean_labels[["weight_avg"]] <- "average weight calculated in R"

    # child bmi, update label
    if (class(qv7_child_clean[["bmi"]]) == "character") {
        qv7_child_clean[["bmi"]] <- as.numeric(qv7_child_clean[["bmi"]])
    }

    qv7_child_clean[["bmi"]] <- ifelse(is.na(qv7_child_clean[["height_avg"]]) | is.na(qv7_child_clean[["weight_avg"]]), NA, round(qv7_child_clean[["weight_avg"]]/((qv7_child_clean[["height_avg"]]/100)^2),
        digits = 2))
    qv7_child_clean_labels[["bmi"]] <- "bmi calculated in R package using scripted average height and weight"

    # child age - new variables so need to add to labels
    qv7_child_clean[["age_yr"]] <- round(lubridate::`%--%`(qv7_child_clean[["dob"]], qv7_child_clean[["start_date"]])/lubridate::years(1), digits = 2)
    qv7_child_clean_labels[["age_yr"]] <- "Age in years calculated from dob and start_date"

    qv7_child_clean[["age_mo"]] <- round(lubridate::`%--%`(qv7_child_clean[["dob"]], qv7_child_clean[["start_date"]])/lubridate::dmonths(1), digits = 1)
    qv7_child_clean_labels[["age_mo"]] <- "Age in months calculated from dob and start_date"

    # child bmi percentile, update label
    qv7_child_clean[["bmi_percentile"]] <- round((childsds::sds(value = qv7_child_clean[["bmi"]], age = qv7_child_clean[["age_yr"]], sex = qv7_child_clean[['sex']], item = "bmi", ref = childsds::cdc.ref, type = "perc", male = 1, female = 2)) * 100, digits = 2)
    qv7_child_clean_labels[["bmi_percentile"]] <- "BMI percentile updated: calculated using childsds R package and scripted average height and weight"

    # child bmi z score : sds (standard deviations away from center/50th centile) - new variable so need to add to labels
    qv7_child_clean[["bmi_z"]] <- round(childsds::sds(value = qv7_child_clean[["bmi"]], age = qv7_child_clean[["age_yr"]], sex = qv7_child_clean[['sex']], item = "bmi", ref = childsds::cdc.ref, type = "SDS", male = 1, female = 2), digits = 2)
    qv7_child_clean_labels[["bmi_z"]] <- "BMI-z/sds calculated using childsds R package"

    # re-organize variables and labels with newly added variables
    qv7_child_clean <- qv7_child_clean[c(1:4, 187:188, 5:12, 189, 13:186)]
    qv7_child_clean_labels <- qv7_child_clean_labels[c(1:4, 187:188, 5:12, 189, 13:186)]

    ## re-calculate all intake values

    # get all intake variables
    intake_vars <- names(qv7_child_clean)[c(44:127)]

    # make all intake variables numeric
    for (var in 1:length(intake_vars)) {
        var_name <- intake_vars[[var]]

        qv7_child_clean[[var_name]] <- ifelse(qv7_child_clean[[var_name]] == "-", NA, ifelse(qv7_child_clean[[var_name]] == "na", NA, qv7_child_clean[[var_name]]))

        if (is.character(qv7_child_clean[[var_name]])) {
            qv7_child_clean[[var_name]] <- as.numeric(qv7_child_clean[[var_name]])
        }
    }

    # get all foods served - extract prefix and thne postfix in name
    food_strs_g <- unique(sapply(intake_vars, function(x) gsub(".*plate_|.*post_|.*consumed_", "\\1", x), USE.NAMES = FALSE))
    food_strs <- unique(sapply(food_strs_g, function(x) gsub("_g.*", "\\1", x), USE.NAMES = FALSE))

    # loop through foods
    for (f in 1:length(food_strs)) {
        # get variable names for plate* and post* weights
        plate_var <- paste0("plate_", food_strs[f], "_g")
        post_var <- paste0("post_", food_strs[f], "_g")
        consumed_var <- paste0("consumed_", food_strs[f], "_g")

        # calculate amount consumed
        qv7_child_clean[[consumed_var]] <- qv7_child_clean[[plate_var]] - qv7_child_clean[[post_var]]
        qv7_child_clean[[consumed_var]] <- ifelse(qv7_child_clean[[consumed_var]] < 0, 0, qv7_child_clean[[consumed_var]])

        # update labels
        qv7_child_clean_labels[[consumed_var]] <- paste0(qv7_child_clean_labels[[consumed_var]], " - recalcuated difference in R with values < 0 set to 0")
    }

    # 7) re-ordering factor levels ####

    ## sex - make sure always matches across parent/child and visits
    qv7_child_clean[['sex']]<- sjlabelled::set_labels(qv7_child_clean[['sex']], labels = c(Male = 0, Female = 1))
    set_attr <- attributes(qv7_child_clean[['sex']])
    qv7_child_clean[['sex']] <- ifelse(is.na(qv7_child_clean[['sex']]), NA, ifelse(qv7_child_clean[['sex']] == 1, 0, 1))
    attributes(qv7_child_clean[['sex']]) <- set_attr
    qv7_child_clean_labels[["sex"]] <- paste0(qv7_child_clean_labels[["sex"]], " re-leveled in R to start with 0")

    # make pna database
    qv7_child_pna <- data.frame(id = qv7_child_clean[["id"]])
    qv7_child_pna_labels <- lapply(qv7_child_pna, function(x) attributes(x)$label)
    qv7_child_pna_labels[["id"]] <- qv7_child_pna_labels[["id"]]

    pna_label <- "Note: prefer not to answer (pna) marked NA - see pna database for which were pna rather than missing NA"

    ## Fix 99/skip in LOC
    level99_issue_catvars <- names(qv7_child_clean)[c(164:186)]

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

    # 8) fix labels ####

    ## remove 'V7', 'V6', and 'V1' in labels
    for (var in 1:length(names(qv7_child_clean))) {
        var_name <- as.character(names(qv7_child_clean)[var])

        if (grepl("V7", qv7_child_clean_labels[[var_name]], fixed = TRUE)) {
            qv7_child_clean_labels[[var_name]] <- gsub("V7 ", "", qv7_child_clean_labels[[var_name]])
        }

        if (grepl("V6", qv7_child_clean_labels[[var_name]], fixed = TRUE)) {
            qv7_child_clean_labels[[var_name]] <- gsub("V6 ", "", qv7_child_clean_labels[[var_name]])
        }

        if (grepl("V1", qv7_child_clean_labels[[var_name]], fixed = TRUE)) {
            qv7_child_clean_labels[[var_name]] <- gsub("V1 ", "", qv7_child_clean_labels[[var_name]])
        }
    }

    ## add meal/EAH to intake descriptions
    intake_vars <- names(qv7_child_clean)[44:131]
    for (var in 1:length(intake_vars)) {
        var_name <- as.character(intake_vars[var])
        if (var < 49) {
            qv7_child_clean_labels[[var_name]] <- paste0("Meal ", qv7_child_clean_labels[[var_name]])
        } else {
            qv7_child_clean_labels[[var_name]] <- paste0("EAH ", qv7_child_clean_labels[[var_name]])
        }
    }

    ## remove trailing '... - 1' from labels
    for (var in 1:length(names(qv7_child_clean))) {
        var_name <- as.character(names(qv7_child_clean)[var])
        if (grepl(" - 1", qv7_child_clean_labels[[var_name]], fixed = TRUE)) {
            qv7_child_clean_labels[[var_name]] <- gsub("\\ - 1.*", "", qv7_child_clean_labels[[var_name]])
        }
    }

    qv7_child_clean_labels[["spacegame_reward"]] <- "Type of candy selected for Space Game reward"

    #### 9) Format for export ####

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
