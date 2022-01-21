#' util_fbs_data_qc: Process RedCap exported data quality control progress
#'
#' This function loads the .csv raw data file from RedCap (export option = 'raw data') and the RedCap exported data dictionary. Cleaning the data involves:
#' 1) extracting all variable descriptions,
#' 2) selecting relevant data columns,
#' 3) removing all practice events (e.g., 999)
#' 4) re-ordering and re-name data columns
#' 5) reformatting dates to be appropriate and computer readable: YYYY-MM-DD
#' 6) re-calculate manual variables
#' 7) re-ordering factor levels to start with value 0
#' 8) random fixes to factor level names and variable descriptions
#'
#' The databases MUST follow the naming convention: *_data_qc_YYY-MM-DD.sav. The dictionary must be saved as: dict_data_qc.csv
#'
#' @param file_pattern string with the pattern to search for to find the RedCap data_qc file without the date included. (e.g., for file all_data_qc_2022-01-11.csv, would enter 'all').
#' @param qc_path (optional) the full path to the directory where the dict_data_qc.csv and RedCap QC database is saved, EXCLUDING the file name (e.g., '.../b-childfoodlab_Shared/Active_Studies/RO1_Brain_Mechanisms_IRB_5357/Participant_Data/QC/'). If NOT entered, will assume the database is saved in the working directory.
#'
#'
#' @return A list containing: 1) data: data.frame with qc data and 2) dict: all variable descriptions
#'
#' @examples
#' #if in same working directory as data:
#' qc_dat <- util_fbs_data_qc('all')
#'
#' \dontrun{
#' #file_pattern must be a string. The following will not run:
#' qc_dat <- util_fbs_data_qc(all)
#'
#' }
#'
#'
#' @export
#'
util_fbs_data_qc <- function(file_pattern, qc_path) {

    #### 1. Set up/initial checks #####

    # check that file_pattern exist and is a string

    filepat_arg <- methods::hasArg(file_pattern)

    if (isTRUE(filepat_arg) & !is.character(file_pattern)) {
        stop("file_pattern must be entered as a string: e.g., 'all'")
    } else if (isFALSE(filepat_arg)) {
        stop("file_pattern must set to the a string matching the name of the raw data file for child visit: e.g., 'all'")
    }

    # check datapath
    qcpath_arg <- methods::hasArg(qc_path)

    if (isTRUE(qcpath_arg)) {
        if (!is.character(qc_path)) {
            stop("qc_path must be entered as a string: e.g., '.../Participant_Data/QC/'")
        }
    }

    #### 2. Load Data #####

    if (isTRUE(qcpath_arg)) {
        qc_path_files <- list.files(path = qc_path, pattern = file_pattern, full.names = TRUE)
    } else {
        qc_path_files <- paste0(pattern = file_pattern, full.names = TRUE)
    }

    # check number of files found
    if (length(qc_path_files) > 1) {
        stop("More than one file matched the file_pattern. Be sure the file_pattern specifies a unique file string before '_data_qc_YYYY-MM-DD. If have more than 1 file matching the pattern in the directory, may need to move to enter a more specific file_pattern than is standard.")
    } else if (length(qc_path_files) == 0) {
        stop('No files found. Be sure the qc_path and file_pattern are correct and that the file exists')
    }

    # check that file is of type '.sav'
    if (!grepl('.csv', qc_path_files, fixed = TRUE)){
        stop("The file found is not a .csv database")
    }

    # check if file exists
    qc_data_exists <- file.exists(qc_path_files)

    # load data if it exists
    if (isTRUE(qc_data_exists)) {
        qc_data <- read.csv(qc_path_files)

    } else {
        if (isTRUE(qcpath_arg)) {
            stop("File does not exist. Check file_pattern and qc_path entered")
        } else {
            stop("File does not exist. Check file_pattern and that the data exists in current working directory")
        }
    }

    ## load dictionary
    dict_qc <- read.csv(paste0(qc_path, '/dict_data_qc.csv'))


    #### 3. Clean Data #####

    # 1) extract variable labels/descriptions  ####
    qc_data_labels <- lapply(qc_data, function(x) attributes(x)$label)

    for (v in 1:ncol(qc_data)){
        var_name <- names(qc_data)[v]
        qc_data_labels[[var_name]] <- dict_qc[dict_qc[['Variable...Field.Name']] == var_name, 'Field.Label']
    }


    # 2) selecting relevant data columns ####
    qc_data_clean <- qc_data[c(1, 11:13, 20:39, 44:46, 54:81, 92:103, 112, 121, 130, 139, 148, 157:335)]

    ## update labels
    qc_data_clean_labels <- qc_data_labels[c(1, 11:13, 20:39, 44:46, 54:81, 92:103, 112, 121, 130, 139, 148, 157:335)]


    # 3) removing all practice events (e.g., 999) ####
    qc_data_clean <- qc_data_clean[!is.na(qc_data_clean[["ID"]]) & qc_data_clean[["ID"]] < 999, ]

    # 4) re-ordering and re-name data columns general order ####

    #1) child information (sex, dob, h/w, bmi, screen out), 2) freddies, 3) food VAS 4) intakes (preMeal, EAH, meal duration), 5) wanting, PDS, PSS, etc 6) notes

    qc_data_clean <- qc_data_clean[c(2, 1, 3:4, 148:156, 157:158, 161, 159:160, 5:24, 25:27, 162:249, 28:147, 250:251)]

    qc_data_clean_labels <- qc_data_clean_labels[c(2, 1, 3:4, 148:156, 157:158, 161, 159:160, 5:24, 25:27, 162:249, 28:147, 250:251)]

    ## re-name variables
    names(qc_data_clean)[1:157] <- c("id", "start_date", "sex", "dob", "height1", "height2", "weight1", "weight2", "height_avg", "weight_avg", "bmi", "bmi_percentile", "bmi_screenout", "freddy_pre_meal", "freddy_post_meal", "freddy_pre_want", "freddy_pre_eah", "freddy_post_eah", "vas_practice", "vas_popcorn", "vas_pretzle", "vas_cornchip", "vas_cookie", "vas_brownie", "vas_starburst", "vas_skittle", "vas_hershey", "vas_icecream", "vas_pbj_sndwch", "vas_ham_sndwch", "vas_turkey_sndwch", "vas_cheese_sndwch", "vas_applesauce", "vas_potatoechip", "vas_babycarrot", "vash_oreo", "vas_milk", "vas_water", "meal_start", "meal_end", "meal_dur", "noplate_applesauce_g", "plate_applesauce_g", "post_applesauce_g", "consumed_applesauce_g", "noplate_carrot_g", "plate_carrot_g", "post_carrot_g", "consumed_carrot_g", "noplate_cheese_sndwch_g", "plate_cheese_sndwch_g", "post_cheese_sndwch_g", "consumed_cheese_sndwch_g", "noplate_cookies_g", "plate_cookies_g", "post_cookies_g", "consumed_cookies_g", "noplate_ham_sndwch_g", "plate_ham_sndwch_g", "post_ham_sndwch_g", "consumed_ham_sndwch_g", "noplate_milk_g", "plate_milk_g", "post_milk_g", "consumed_milk_g", "noplate_pbj_sndwch_g", "plate_pbj_sndwch_g", "post_pbj_sndwch_g", "consumed_pbj_sndwch_g", "noplate_potatochip_g", "plate_potatochip_g", "post_potatochip_g", "consumed_potatochip_g", "noplate_turkey_sndwch_g", "plate_turkey_sndwch_g", "post_turkey_sndwch_g", "consumed_turkey_sndwch_g", "noplate_ketchup_g", "plate_ketchup_g", "post_ketchup_g", "consumed_ketchup_g", "noplate_mayo_g", "plate_mayo_g", "post_mayo_g", "consumed_mayo_g", "noplate_mustard_g", "plate_mustard_g", "post_mustard_g", "consumed_mustard_g", "noplate_brownies_g", "plate_brownies_g", "post_brownies_g", "consumed_brownies_g", "noplate_cornchips_g", "plate_cornchips_g", "post_cornchips_g", "consumed_cornchips_g", "noplate_hersheys_g", "plate_hersheys_g", "post_hersheys_g", "consumed_hersheys_g", "noplate_icecream_g", "plate_icecream_g", "post_icecream_g", "consumed_icecream_g", "noplate_oreos_g", "plate_oreos_g", "post_oreos_g", "consumed_oreos_g", "noplate_popcorn_g", "plate_popcorn_g", "post_popcorn_g", "consumed_popcorn_g", "noplate_pretzels_g", "plate_pretzels_g", "post_pretzels_g", "consumed_pretzels_g", "noplate_skittles_g", "plate_skittles_g", "post_skittles_g", "consumed_skittles_g", "noplate_starbursts_g", "plate_starbursts_g", "post_starbursts_g", "consumed_starbursts_g", "noplate_water_g", "plate_water_g", "post_water_g", "consumed_water_g", "want_water", "want_brownies", "want_applesauce", "want_carrots", "want_cars", "want_cheese", "want_cookies", "want_cornchip", "want_toy", "want_connect4", "want_crayons", "want_ham", "want_dino", "want_hershey", "want_icecream", "want_jenga", "want_legos", "want_elephant", "want_oreos", "want_pbj_sndwch", "want_popcorn", "want_chips", "want_pretzels", "want_skittles", "want_trains", "want_trucks", "want_starbursts", "want_turkey_sndwch")

    names(qc_data_clean)[158:251] <- c("pss_psd_practice1", "pss_psd_practice2", "pss_psd1", "pss_psd2", "pss_psd3", "pss_psd4", "pss_psd5", "pss_psd6", "pss_psd7", "pss_psd8", "pss_psd9", "pss_psd10", "pss_psd_practice8", "pss_psd11", "pss_psd12", "pss_psd13", "pss_psd14", "pss_psd15", "pss_practice1", "pss_vas_hunger", "pss_vas_couldeat", "pss_vas_fullness", "pss_practice2", "pss_apple_eat", "pss_apple_much", "pss_apple_like", "pss_broccoli_eat", "pss_broccoli_much", "pss_broccoli_like", "pss_cake_eat", "pss_cake_much", "pss_cake_like", "pss_candy_eat", "pss_candy_much", "pss_candy_like", "pss_carrot_eat", "pss_carrot_much", "pss_carrot_like", "pss_cornflakes_eat", "pss_ccornflakes_much", "pss_cornflakes_like", "pss_cheese_brgr_eat", "pss_cheese_brgr_much", "pss_cheese_brgr_like", "pss_chkn_nug_eat", "pss_chkn_nug_much", "pss_chkn_nug_like", "pss_fries_eat", "pss_fries_much", "pss_fries_like", "pss_garlic_bread_eat", "pss_garlic_bread_much", "pss_garlic_bread_like", "pss_goldfish_eat", "pss_goldfish_much", "pss_goldfish_like", "pss_grapes_eat", "pss_grapes_much", "pss_grapes_like", "pss_choc_icecream_eat", "pss_choc_icecream_much", "pss_choc_icecream_like", "pss_mac_cheese_eat", "pss_mac_cheese_much", "pss_mac_cheese_like", "pss_milk_eat", "pss_milk_much", "pss_milk_like", "pss_orangejuice_eat", "pss_orangejuice_much", "pss_orangejuice_like", "pss_pbj_sndwch_eat", "pss_pbj_sndwch_much", "pss_pbj_sndwch_like", "pss_peas_eat", "pss_peas_much", "pss_peas_like", "pss_pizza_eat", "pss_pizza_much", "pss_pizza_like", "pss_soda_eat", "pss_soda_much", "pss_soda_like", "pss_soup_eat", "pss_soup_much", "pss_soup_like", "pss_tomatoes_eat", "pss_tomatoes_much", "pss_tomatoes_like", "pss_yogurt_eat", "pss_yogurt_much", "pss_yogurt_like", "food_initials", "child_notes")

    ## update data labels
    names(qc_data_clean_labels) <- names(qc_data_clean)

    # 5) reformatting dates to be appropriate and computer readable YYYY-MM-DD ####

    qc_data_clean[["start_date"]] <- lubridate::ymd(as.Date(qc_data_clean[["start_date"]]))
    qc_data_clean_labels[["start_date"]] <- "start_date from qualtrics survey meta-data converted to format yyyy-mm-dd in R"

    qc_data_clean[["dob"]] <- as.Date(qc_data_clean[["dob"]], format = "%m/%d/%Y")
    qc_data_clean_labels[["dob"]] <- "date of birth converted to format yyyy-mm-dd in R"

    ## freddy fullness as numeric
    qc_data_clean[c(14:18, 41)] <- sapply(qc_data_clean[c(14:18, 41)], FUN = as.numeric)

    # 6) re-calculate manual variables ####

    # avg child height, update label
    qc_data_clean[["height_avg"]] <- ifelse(is.na(qc_data_clean[["height1"]]) | is.na(qc_data_clean[["height2"]]), NA, rowSums(qc_data_clean[c("height1",
        "height2")], na.rm = TRUE)/2)
    qc_data_clean_labels[["height_avg"]] <- "average height calculated in R"

    # avg child weight, update label
    qc_data_clean[["weight_avg"]] <- ifelse(is.na(qc_data_clean[["weight1"]]) | is.na(qc_data_clean[["weight2"]]), NA, rowSums(qc_data_clean[c("weight1",
        "weight2")], na.rm = TRUE)/2)
    qc_data_clean_labels[["weight_avg"]] <- "average weight calculated in R"

    # child bmi, update label
    if (class(qc_data_clean[["bmi"]]) == "character") {
        qc_data_clean[["bmi"]] <- as.numeric(qc_data_clean[["bmi"]])
    }

    qc_data_clean[["bmi"]] <- ifelse(is.na(qc_data_clean[["height_avg"]]) | is.na(qc_data_clean[["weight_avg"]]), NA, round(qc_data_clean[["weight_avg"]]/((qc_data_clean[["height_avg"]]/100)^2),
        digits = 2))
    qc_data_clean_labels[["bmi"]] <- "bmi calculated in R package using scripted average height and weight"

    # child age - new variables so need to add to labels
    qc_data_clean[["age_yr"]] <- round(lubridate::`%--%`(qc_data_clean[["dob"]], qc_data_clean[["start_date"]])/lubridate::years(1), digits = 2)
    qc_data_clean_labels[["age_yr"]] <- "Age in years calculated from dob and start_date"

    qc_data_clean[["age_mo"]] <- round(lubridate::`%--%`(qc_data_clean[["dob"]], qc_data_clean[["start_date"]])/lubridate::dmonths(1), digits = 1)
    qc_data_clean_labels[["age_mo"]] <- "Age in months calculated from dob and start_date"

    # child bmi percentile, update label
    qc_data_clean[["bmi_percentile"]] <- round((childsds::sds(value = qc_data_clean[["bmi"]], age = qc_data_clean[["age_yr"]], sex = qc_data_clean[['sex']], item = "bmi", ref = childsds::cdc.ref, type = "perc", male = 1, female = 2)) * 100, digits = 2)
    qc_data_clean_labels[["bmi_percentile"]] <- "BMI percentile updated: calculated using childsds R package and scripted average height and weight"

    #update/confirm child screenout - can only change for those NOT screened out as if they were screened out we wont have data for them. The initial criteria was BMI percentile < 85 but now it is < 90
    bmi_set_attr <- attributes(qc_data_clean[["bmi_screenout"]])

    qc_data_clean[["bmi_screenout"]] <- ifelse(qc_data_clean[["bmi_screenout"]] == 0 & qc_data_clean[["bmi_percentile"]] >= 90, 1, qc_data_clean[["bmi_screenout"]])

    attributes(qc_data_clean[["bmi_screenout"]]) <- bmi_set_attr

    # child bmi z score : sds (standard deviations away from center/50th centile) - new variable so need to add to labels
    qc_data_clean[["bmi_z"]] <- round(childsds::sds(value = qc_data_clean[["bmi"]], age = qc_data_clean[["age_yr"]], sex = qc_data_clean[['sex']], item = "bmi", ref = childsds::cdc.ref, type = "SDS", male = 1, female = 2), digits = 2)
    qc_data_clean_labels[["bmi_z"]] <- "BMI-z/sds calculated using childsds R package"

    # re-organize variables and labels with newly added variables
    qc_data_clean <- qc_data_clean[c(1:4, 252:253, 5:12, 254, 13:251)]
    qc_data_clean_labels <- qc_data_clean_labels[c(1:4, 252:253, 5:12, 254, 13:251)]

    ## re-calculate all intake values

    # get all intake variables
    intake_vars <- names(qc_data_clean)[c(45:132)]

    # make all intake variables numeric
    for (var in 1:length(intake_vars)) {
        var_name <- intake_vars[[var]]

        qc_data_clean[[var_name]] <- ifelse(qc_data_clean[[var_name]] == "-", NA, ifelse(qc_data_clean[[var_name]] == "66.19231.35", "66.19", ifelse(qc_data_clean[[var_name]] == "246.60q", "246.60", ifelse(qc_data_clean[[var_name]] == "na", NA, qc_data_clean[[var_name]]))))

        if (is.character(qc_data_clean[[var_name]])) {
            qc_data_clean[[var_name]] <- as.numeric(qc_data_clean[[var_name]])
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
        qc_data_clean[[consumed_var]] <- qc_data_clean[[plate_var]] - qc_data_clean[[post_var]]
        qc_data_clean[[consumed_var]] <- ifelse(qc_data_clean[[consumed_var]] < 0, 0, qc_data_clean[[consumed_var]])

        # update labels
        qc_data_clean_labels[[consumed_var]] <- paste0(qc_data_clean_labels[[consumed_var]], " - recalcuated difference in R with values < 0 set to 0")
    }

    # 7) re-ordering factor levels to start with value 0 ####

    ## sex - make sure always matches across child/child and visits
    qc_data_clean[['sex']]<- sjlabelled::set_labels(qc_data_clean[['sex']], labels = c(Male = 0, Female = 1))
    set_attr <- attributes(qc_data_clean[['sex']])
    qc_data_clean[['sex']] <- ifelse(is.na(qc_data_clean[['sex']]), NA, ifelse(qc_data_clean[['sex']] == 1, 0, 1))
    attributes(qc_data_clean[['sex']]) <- set_attr
    qc_data_clean_labels[["sex"]] <- paste0(qc_data_clean_labels[["sex"]], " re-leveled in R to start with 0")

    # 8) random fixes to factor level names and variable descriptions fix pss-pds value labels
    pss_psd_names <- names(qc_data_clean)[161:178]

    for (var in 1:length(pss_psd_names)) {
        var_name <- as.character(pss_psd_names[var])
        if (attributes(qc_data_clean[[var_name]])$labels[1] == 1) {
            names(attributes(qc_data_clean[[var_name]])$labels) <- c("Correct", "Incorrect")
        } else {
            names(attributes(qc_data_clean[[var_name]])$labels) <- c("Incorrect", "Correct")
        }

        ## update variable descriptions for pps to distinguish same/different from more/less
        orig_label <- qc_data_clean_labels[[var_name]]
        if (var < 13) {
            qc_data_clean_labels[[var_name]] <- paste0(orig_label, " : same or different?")
        } else {
            qc_data_clean_labels[[var_name]] <- paste0(orig_label, " : which has more, first or second?")
        }
    }

    ## add meal/EAH to intake descriptions
    intake_vars <- names(qc_data_clean)[45:132]
    for (var in 1:length(intake_vars)) {
        var_name <- as.character(intake_vars[var])
        if (var < 49) {
            qc_data_clean_labels[[var_name]] <- paste0("Meal ", qc_data_clean_labels[[var_name]])
        } else {
            qc_data_clean_labels[[var_name]] <- paste0("EAH ", qc_data_clean_labels[[var_name]])
        }
    }

    ## make ever eat yogurt formatted same as others
    qc_data_clean_labels[["pss_yogurt_eat"]] <- "PSS Child Strawberry Yogurt - Ever Eat"

    ## remove trailing '... - 1' from labels
    for (var in 1:length(names(qc_data_clean))) {
        var_name <- as.character(names(qc_data_clean)[var])
        if (grepl(" - 1", qc_data_clean_labels[[var_name]], fixed = TRUE)) {
            qc_data_clean_labels[[var_name]] <- gsub("\\ - 1.*", "", qc_data_clean_labels[[var_name]])
        }
    }

    #### 8) Format for export ####

    #put data in order of participant ID for ease
    qc_data_clean <- qc_data_clean[order(qc_data_clean[["id"]]), ]

    # make sure the variable labels match in the dataset
    qc_data_clean = sjlabelled::set_label(qc_data_clean, label = matrix(unlist(qc_data_clean_labels, use.names = FALSE)))

    # make list of data frame and associated labels
    qc_data <- list(data = qc_data_clean, dict = qc_data_clean_labels)

    ## want an export options??

    return(qc_data)
}
