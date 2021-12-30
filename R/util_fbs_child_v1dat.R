#' util_fbs_child_v1dat: Process raw qualtrics visit 1 data for the child
#'
#' This function loads the .sav raw data file for the child visit 1 data that was
#' collected via Qualtrics and cleans the data. Cleaning the data involves:
#' 1) extracting all variable descriptions,
#' 2) selecting relevant data columns,
#' 3) removing all practice events (e.g., 999)
#' 4) re-ordering and re-name data columns
#' 5) reformatting dates to be appropriate and computer readable: YYYY-MM-DD
#' 6) re-calculate manual variables
#' 7) re-ordering factor levels to start with value 0
#' 8) random fixes to factor level names and variable descriptions
#'
#' The databases MUST follow the naming convention: Child_V1_YYYY-MM-DD.sav
#'
#' @inheritParams util_fbs_child_v1dat
#' @inheritParams util_fbs_child_v1dat
#'
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from child visit 1 Qualtrics
#' and 2) dict: all variable descriptions
#'
#' @examples
#' #if in same working directory as data:
#' child_v1_dat <- util_fbs_child_v1dat('Child_V1')
#'
#' \dontrun{
#' #file_pattern must be a string. The following will not run:
#' child_v1_dat <- util_fbs_child_v1dat(Child_V1)
#'
#' #file_pattern must have the respondent ('Child') and visit number ('V1'). If just enter 'Child', the script will not run because it will return multiple files for different child visits. The following will not run:
#' child_v1_dat <- util_fbs_child_v1dat('Child')
#' }
#'
#'
#' @export
#'
util_fbs_child_v1dat <- function(file_pattern, data_path) {

    #### 1. Set up/initial checks #####

    # check that file_pattern exist and is a string

    filepat_arg <- methods::hasArg(file_pattern)

    if (isTRUE(filepat_arg) & !is.character(file_pattern)) {
        stop("file_pattern must be entered as a string: e.g., 'Child_V1'")
    } else if (isFALSE(filepat_arg)) {
        stop("file_pattern must set to the a string matching the name of the raw data file for child visit: e.g., 'Child_V1'")
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
        qv1_child_path <- list.files(path = data_path, pattern = file_pattern, full.names = TRUE)
    } else {
        qv1_child_path <- paste0(pattern = file_pattern, full.names = TRUE)
    }

    # check number of files found
    if (length(qv1_child_path) > 1) {
        stop("More than one file matched the file_pattern. Be sure thefile_pattern specifies both the respondent (Parent/Child) and visit number (V#). If have more than 1 file matching the pattern in the directory, may need to move to enter a more specific file_pattern than is standard.")
    } else if (length(qv1_child_path) == 0) {
        stop('No files found. Be sure the data_path and file_pattern are correct and that the file exists')
    }

    # check that file is of type '.sav'
    if (!grepl('.sav', qv1_child_path, fixed = TRUE)){
        stop("The file found is not an SPSS database (.sav)")
    }

    # check if file exists
    qv1_child_exists <- file.exists(qv1_child_path)

    # load data if it exists
    if (isTRUE(qv1_child_exists)) {
        qv1_child_dat <- as.data.frame(haven::read_spss(qv1_child_path))

    } else {
        if (isTRUE(datapath_arg)) {
            stop("File does not exist. Check date_str and data_path entered")
        } else {
            stop("File does not exist. Check date_str and that the data exists in current working directory")
        }
    }

    #### 3. Clean Data #####

    # 1) extract variable labels/descriptions  ####
    qv1_child_labels <- lapply(qv1_child_dat, function(x) attributes(x)$label)

    # 2) selecting relevant data columns ####
    qv1_child_clean <- qv1_child_dat[c(1, 11:13, 20:39, 44:46, 54:81, 92:103, 112, 121, 130, 139, 148, 157:335)]

    ## update labels
    qv1_child_clean_labels <- qv1_child_labels[c(1, 11:13, 20:39, 44:46, 54:81, 92:103, 112, 121, 130, 139, 148, 157:335)]


    # 3) removing all practice events (e.g., 999) ####
    qv1_child_clean <- qv1_child_clean[!is.na(qv1_child_clean[["ID"]]) & qv1_child_clean[["ID"]] < 999, ]

    # 4) re-ordering and re-name data columns general order ####

    #1) child information (sex, dob, h/w, bmi, screen out), 2) freddies, 3) food VAS 4) intakes (preMeal, EAH, meal duration), 5) wanting, PDS, PSS, etc 6) notes

    qv1_child_clean <- qv1_child_clean[c(2, 1, 3:4, 148:156, 157:158, 161, 159:160, 5:24, 25:27, 162:249, 28:147, 250:251)]

    qv1_child_clean_labels <- qv1_child_clean_labels[c(2, 1, 3:4, 148:156, 157:158, 161, 159:160, 5:24, 25:27, 162:249, 28:147, 250:251)]

    ## re-name variables
    names(qv1_child_clean)[1:157] <- c("id", "start_date", "sex", "dob", "height1", "height2", "weight1", "weight2", "height_avg", "weight_avg", "bmi", "bmi_percentile", "bmi_screenout", "freddy_pre_meal", "freddy_post_meal", "freddy_pre_want", "freddy_pre_eah", "freddy_post_eah", "vas_practice", "vas_popcorn", "vas_pretzle", "vas_cornchip", "vas_cookie", "vas_brownie", "vas_starburst", "vas_skittle", "vas_hershey", "vas_icecream", "vas_pbj_sndwch", "vas_ham_sndwch", "vas_turkey_sndwch", "vas_cheese_sndwch", "vas_applesauce", "vas_potatoechip", "vas_babycarrot", "vash_oreo", "vas_milk", "vas_water", "meal_start", "meal_end", "meal_dur", "noplate_applesauce_g", "plate_applesauce_g", "post_applesauce_g", "consumed_applesauce_g", "noplate_carrot_g", "plate_carrot_g", "post_carrot_g", "consumed_carrot_g", "noplate_cheese_sndwch_g", "plate_cheese_sndwch_g", "post_cheese_sndwch_g", "consumed_cheese_sndwch_g", "noplate_cookies_g", "plate_cookies_g", "post_cookies_g", "consumed_cookies_g", "noplate_ham_sndwch_g", "plate_ham_sndwch_g", "post_ham_sndwch_g", "consumed_ham_sndwch_g", "noplate_milk_g", "plate_milk_g", "post_milk_g", "consumed_milk_g", "noplate_pbj_sndwch_g", "plate_pbj_sndwch_g", "post_pbj_sndwch_g", "consumed_pbj_sndwch_g", "noplate_potatochip_g", "plate_potatochip_g", "post_potatochip_g", "consumed_potatochip_g", "noplate_turkey_sndwch_g", "plate_turkey_sndwch_g", "post_turkey_sndwch_g", "consumed_turkey_sndwch_g", "noplate_ketchup_g", "plate_ketchup_g", "post_ketchup_g", "consumed_ketchup_g", "noplate_mayo_g", "plate_mayo_g", "post_mayo_g", "consumed_mayo_g", "noplate_mustard_g", "plate_mustard_g", "post_mustard_g", "consumed_mustard_g", "noplate_brownies_g", "plate_brownies_g", "post_brownies_g", "consumed_brownies_g", "noplate_cornchips_g", "plate_cornchips_g", "post_cornchips_g", "consumed_cornchips_g", "noplate_hersheys_g", "plate_hersheys_g", "post_hersheys_g", "consumed_hersheys_g", "noplate_icecream_g", "plate_icecream_g", "post_icecream_g", "consumed_icecream_g", "noplate_oreos_g", "plate_oreos_g", "post_oreos_g", "consumed_oreos_g", "noplate_popcorn_g", "plate_popcorn_g", "post_popcorn_g", "consumed_popcorn_g", "noplate_pretzels_g", "plate_pretzels_g", "post_pretzels_g", "consumed_pretzels_g", "noplate_skittles_g", "plate_skittles_g", "post_skittles_g", "consumed_skittles_g", "noplate_starbursts_g", "plate_starbursts_g", "post_starbursts_g", "consumed_starbursts_g", "noplate_water_g", "plate_water_g", "post_water_g", "consumed_water_g", "want_water", "want_brownies", "want_applesauce", "want_carrots", "want_cars", "want_cheese", "want_cookies", "want_cornchip", "want_toy", "want_connect4", "want_crayons", "want_ham", "want_dino", "want_hershey", "want_icecream", "want_jenga", "want_legos", "want_elephant", "want_oreos", "want_pbj_sndwch", "want_popcorn", "want_chips", "want_pretzels", "want_skittles", "want_trains", "want_trucks", "want_starbursts", "want_turkey_sndwch")

    names(qv1_child_clean)[158:251] <- c("pss_psd_practice1", "pss_psd_practice2", "pss_psd1", "pss_psd2", "pss_psd3", "pss_psd4", "pss_psd5", "pss_psd6", "pss_psd7", "pss_psd8", "pss_psd9", "pss_psd10", "pss_psd_practice8", "pss_psd11", "pss_psd12", "pss_psd13", "pss_psd14", "pss_psd15", "pss_practice1", "pss_vas_hunger", "pss_vas_couldeat", "pss_vas_fullness", "pss_practice2", "pss_apple_eat", "pss_apple_much", "pss_apple_like", "pss_broccoli_eat", "pss_broccoli_much", "pss_broccoli_like", "pss_cake_eat", "pss_cake_much", "pss_cake_like", "pss_candy_eat", "pss_candy_much", "pss_candy_like", "pss_carrot_eat", "pss_carrot_much", "pss_carrot_like", "pss_cornflakes_eat", "pss_ccornflakes_much", "pss_cornflakes_like", "pss_cheese_brgr_eat", "pss_cheese_brgr_much", "pss_cheese_brgr_like", "pss_chkn_nug_eat", "pss_chkn_nug_much", "pss_chkn_nug_like", "pss_fries_eat", "pss_fries_much", "pss_fries_like", "pss_garlic_bread_eat", "pss_garlic_bread_much", "pss_garlic_bread_like", "pss_goldfish_eat", "pss_goldfish_much", "pss_goldfish_like", "pss_grapes_eat", "pss_grapes_much", "pss_grapes_like", "pss_choc_icecream_eat", "pss_choc_icecream_much", "pss_choc_icecream_like", "pss_mac_cheese_eat", "pss_mac_cheese_much", "pss_mac_cheese_like", "pss_milk_eat", "pss_milk_much", "pss_milk_like", "pss_orangejuice_eat", "pss_orangejuice_much", "pss_orangejuice_like", "pss_pbj_sndwch_eat", "pss_pbj_sndwch_much", "pss_pbj_sndwch_like", "pss_peas_eat", "pss_peas_much", "pss_peas_like", "pss_pizza_eat", "pss_pizza_much", "pss_pizza_like", "pss_soda_eat", "pss_soda_much", "pss_soda_like", "pss_soup_eat", "pss_soup_much", "pss_soup_like", "pss_tomatoes_eat", "pss_tomatoes_much", "pss_tomatoes_like", "pss_yogurt_eat", "pss_yogurt_much", "pss_yogurt_like", "food_initials", "child_notes")

    ## update data labels
    names(qv1_child_clean_labels) <- names(qv1_child_clean)

    # 5) reformatting dates to be appropriate and computer readable YYYY-MM-DD ####

    qv1_child_clean[["start_date"]] <- lubridate::ymd(as.Date(qv1_child_clean[["start_date"]]))
    qv1_child_clean_labels[["start_date"]] <- "start_date from qualtrics survey meta-data converted to format yyyy-mm-dd in R"

    qv1_child_clean[["dob"]] <- as.Date(qv1_child_clean[["dob"]], format = "%m/%d/%Y")
    qv1_child_clean_labels[["dob"]] <- "date of birth converted to format yyyy-mm-dd in R"

    ## freddy fullness as numeric
    qv1_child_clean[c(14:18, 41)] <- sapply(qv1_child_clean[c(14:18, 41)], FUN = as.numeric)

    # 6) re-calculate manual variables ####

    # avg child height, update label
    qv1_child_clean[["height_avg"]] <- ifelse(is.na(qv1_child_clean[["height1"]]) | is.na(qv1_child_clean[["height2"]]), NA, rowSums(qv1_child_clean[c("height1",
        "height2")], na.rm = TRUE)/2)
    qv1_child_clean_labels[["height_avg"]] <- "average height calculated in R"

    # avg child weight, update label
    qv1_child_clean[["weight_avg"]] <- ifelse(is.na(qv1_child_clean[["weight1"]]) | is.na(qv1_child_clean[["weight2"]]), NA, rowSums(qv1_child_clean[c("weight1",
        "weight2")], na.rm = TRUE)/2)
    qv1_child_clean_labels[["weight_avg"]] <- "average weight calculated in R"

    # child bmi, update label
    if (class(qv1_child_clean[["bmi"]]) == "character") {
        qv1_child_clean[["bmi"]] <- as.numeric(qv1_child_clean[["bmi"]])
    }

    qv1_child_clean[["bmi"]] <- ifelse(is.na(qv1_child_clean[["height_avg"]]) | is.na(qv1_child_clean[["weight_avg"]]), NA, round(qv1_child_clean[["weight_avg"]]/((qv1_child_clean[["height_avg"]]/100)^2),
        digits = 2))
    qv1_child_clean_labels[["bmi"]] <- "bmi calculated in R package using scripted average height and weight"

    # child age - new variables so need to add to labels
    qv1_child_clean[["age_yr"]] <- round(lubridate::`%--%`(qv1_child_clean[["dob"]], qv1_child_clean[["start_date"]])/lubridate::years(1), digits = 2)
    qv1_child_clean_labels[["age_yr"]] <- "Age in years calculated from dob and start_date"

    qv1_child_clean[["age_mo"]] <- round(lubridate::`%--%`(qv1_child_clean[["dob"]], qv1_child_clean[["start_date"]])/lubridate::dmonths(1), digits = 1)
    qv1_child_clean_labels[["age_mo"]] <- "Age in months calculated from dob and start_date"

    # child bmi percentile, update label
    qv1_child_clean[["bmi_percentile"]] <- round((childsds::sds(value = qv1_child_clean[["bmi"]], age = qv1_child_clean[["age_yr"]], sex = qv1_child_clean[['sex']], item = "bmi", ref = childsds::cdc.ref, type = "perc", male = 1, female = 2)) * 100, digits = 2)
    qv1_child_clean_labels[["bmi_percentile"]] <- "BMI percentile updated: calculated using childsds R package and scripted average height and weight"

    #update/confirm child screenout - can only change for those NOT screened out as if they were screened out we wont have data for them. The initial criteria was BMI percentile < 85 but now it is < 90
    bmi_set_attr <- attributes(qv1_child_clean[["bmi_screenout"]])

    qv1_child_clean[["bmi_screenout"]] <- ifelse(qv1_child_clean[["bmi_screenout"]] == 0 & qv1_child_clean[["bmi_percentile"]] >= 90, 1, qv1_child_clean[["bmi_screenout"]])

    attributes(qv1_child_clean[["bmi_screenout"]]) <- bmi_set_attr

    # child bmi z score : sds (standard deviations away from center/50th centile) - new variable so need to add to labels
    qv1_child_clean[["bmi_z"]] <- round(childsds::sds(value = qv1_child_clean[["bmi"]], age = qv1_child_clean[["age_yr"]], sex = qv1_child_clean[['sex']], item = "bmi", ref = childsds::cdc.ref, type = "SDS", male = 1, female = 2), digits = 2)
    qv1_child_clean_labels[["bmi_z"]] <- "BMI-z/sds calculated using childsds R package"

    # re-organize variables and labels with newly added variables
    qv1_child_clean <- qv1_child_clean[c(1:4, 252:253, 5:12, 254, 13:251)]
    qv1_child_clean_labels <- qv1_child_clean_labels[c(1:4, 252:253, 5:12, 254, 13:251)]

    ## re-calculate all intake values

    # get all intake variables
    intake_vars <- names(qv1_child_clean)[c(45:132)]

    # make all intake variables numeric
    for (var in 1:length(intake_vars)) {
        var_name <- intake_vars[[var]]

        qv1_child_clean[[var_name]] <- ifelse(qv1_child_clean[[var_name]] == "-", NA, ifelse(qv1_child_clean[[var_name]] == "66.19231.35", "66.19", ifelse(qv1_child_clean[[var_name]] == "246.60q", "246.60", ifelse(qv1_child_clean[[var_name]] == "na", NA, qv1_child_clean[[var_name]]))))

        if (is.character(qv1_child_clean[[var_name]])) {
            qv1_child_clean[[var_name]] <- as.numeric(qv1_child_clean[[var_name]])
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
        qv1_child_clean[[consumed_var]] <- qv1_child_clean[[plate_var]] - qv1_child_clean[[post_var]]
        qv1_child_clean[[consumed_var]] <- ifelse(qv1_child_clean[[consumed_var]] < 0, 0, qv1_child_clean[[consumed_var]])

        # update labels
        qv1_child_clean_labels[[consumed_var]] <- paste0(qv1_child_clean_labels[[consumed_var]], " - recalcuated difference in R with values < 0 set to 0")
    }

    # 7) re-ordering factor levels to start with value 0 ####

    ## sex - make sure always matches across child/child and visits
    qv1_child_clean[['sex']]<- sjlabelled::set_labels(qv1_child_clean[['sex']], labels = c(Male = 0, Female = 1))
    set_attr <- attributes(qv1_child_clean[['sex']])
    qv1_child_clean[['sex']] <- ifelse(is.na(qv1_child_clean[['sex']]), NA, ifelse(qv1_child_clean[['sex']] == 1, 0, 1))
    attributes(qv1_child_clean[['sex']]) <- set_attr
    qv1_child_clean_labels[["sex"]] <- paste0(qv1_child_clean_labels[["sex"]], " re-leveled in R to start with 0")

    # 8) random fixes to factor level names and variable descriptions fix pss-pds value labels
    pss_psd_names <- names(qv1_child_clean)[161:178]

    for (var in 1:length(pss_psd_names)) {
        var_name <- as.character(pss_psd_names[var])
        if (attributes(qv1_child_clean[[var_name]])$labels[1] == 1) {
            names(attributes(qv1_child_clean[[var_name]])$labels) <- c("Correct", "Incorrect")
        } else {
            names(attributes(qv1_child_clean[[var_name]])$labels) <- c("Incorrect", "Correct")
        }

        ## update variable descriptions for pps to distinguish same/different from more/less
        orig_label <- qv1_child_clean_labels[[var_name]]
        if (var < 13) {
            qv1_child_clean_labels[[var_name]] <- paste0(orig_label, " : same or different?")
        } else {
            qv1_child_clean_labels[[var_name]] <- paste0(orig_label, " : which has more, first or second?")
        }
    }

    ## add meal/EAH to intake descriptions
    intake_vars <- names(qv1_child_clean)[45:132]
    for (var in 1:length(intake_vars)) {
        var_name <- as.character(intake_vars[var])
        if (var < 49) {
            qv1_child_clean_labels[[var_name]] <- paste0("Meal ", qv1_child_clean_labels[[var_name]])
        } else {
            qv1_child_clean_labels[[var_name]] <- paste0("EAH ", qv1_child_clean_labels[[var_name]])
        }
    }

    ## make ever eat yogurt formatted same as others
    qv1_child_clean_labels[["pss_yogurt_eat"]] <- "PSS Child Strawberry Yogurt - Ever Eat"

    ## remove trailing '... - 1' from labels
    for (var in 1:length(names(qv1_child_clean))) {
        var_name <- as.character(names(qv1_child_clean)[var])
        if (grepl(" - 1", qv1_child_clean_labels[[var_name]], fixed = TRUE)) {
            qv1_child_clean_labels[[var_name]] <- gsub("\\ - 1.*", "", qv1_child_clean_labels[[var_name]])
        }
    }

    #### 8) Format for export ####

    #put data in order of participant ID for ease
    qv1_child_clean <- qv1_child_clean[order(qv1_child_clean[["id"]]), ]

    # make sure the variable labels match in the dataset
    qv1_child_clean = sjlabelled::set_label(qv1_child_clean, label = matrix(unlist(qv1_child_clean_labels, use.names = FALSE)))

    # make list of data frame and associated labels
    qv1_child <- list(data = qv1_child_clean, dict = qv1_child_clean_labels)

    ## want an export options??

    return(qv1_child)
}
