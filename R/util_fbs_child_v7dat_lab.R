#' util_fbs_child_v7dat_lab: Process raw qualtrics visit 7 data collected in the lab for the child
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
#' @inheritParams util_fbs_parent_v7dat
#' @inheritParams util_fbs_parent_v7dat
#'
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from child visit 7 Qualtrics; 2) dict: all variable descriptions; 3) pna_data: data.frame marking participants who 'prefered not to answer' (pna) specific questions; and 4) pna_dict: all variable descriptions for pna_data
#'
#' @examples
#' #if in same working directory as data. Note - there is no need to add the COVID protocol (i.e., 'Home' or 'Lab') to file_pattern:
#' child_v7_dat_lab <- util_fbs_child_v7dat_lab('Child_V7')
#'
#' \dontrun{
#' #file_pattern must be a string. The following will not run:
#' child_v7_dat_lab <- util_fbs_child_v7dat_lab(Child_V7)
#'
#' #file_pattern must have the respondent ('Child') and visit number ('V7'). If just enter 'Child', the script will not run because it will return multiple files for different parent visits. The following will not run:
#' child_v7_dat_lab <- util_fbs_child_v7dat_lab('Child')
#' }
#'
#'
#' @export
#'
util_fbs_child_v7dat_lab <- function(file_pattern, data_path) {

    #### 1. Set up/initial checks #####

    # check that file_pattern exist and is a string

    filepat_arg <- methods::hasArg(file_pattern)

    if (isTRUE(filepat_arg) & !is.character(file_pattern)) {
        stop("file_pattern must be entered as a string: e.g., 'Child_V7'")
    } else if (isFALSE(filepat_arg)) {
        stop("file_pattern must set to the a string matching the name of the raw data file for child visit: e.g., 'Child_V7'")
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
        qv7_child_pathlist <- list.files(path = paste0(data_path, '/Final_Covid/'), pattern = paste0(file_pattern, '_Lab'), full.names = TRUE)

        #if no files found, check direct data_path entered
        if (length(qv7_child_pathlist) == 0) {
            qv7_child_pathlist <- list.files(path = data_path, pattern = paste0(file_pattern, '_Lab'), full.names = TRUE)
        }

        #check for DXA data
        qv7_child_DXApathlist <- list.files(path = data_path, pattern = paste0(file_pattern, '_DXA'), full.names = TRUE)

        if (length(qv7_child_DXApathlist) > 0){
            qv7_child_pathlist <- c(qv7_child_pathlist, qv7_child_DXApathlist)
        }

    } else {
        qv7_child_pathlist <- paste0(pattern = paste0(file_pattern, '_Lab'), full.names = TRUE)
    }

    # check for DXA files
    DXA_file <- grepl('DXA', qv7_child_pathlist, fixed = TRUE)

    # check number of files found
    if (length(qv7_child_pathlist) - sum(DXA_file) > 1) {
        stop("More than one file matched after adding '_Lab' to the file_pattern . Be sure thefile_pattern specifies both the respondent (Parent/Child) and visit number (V#). If have more than 1 file matching the pattern in the directory, may need to move to enter a more specific file_pattern than is standard.")
    } else if (length(qv7_child_pathlist) == 0) {
        stop("No files found after adding '_Lab' to file_pattern. Be sure the data_path and file_pattern are correct and that the file exists.")
    } else {
        #get child qualtrics path
        if (sum(DXA_file) > 0) {
            qv7_child_path <- qv7_child_pathlist[DXA_file == FALSE]
        } else {
            qv7_child_path <- qv7_child_pathlist
        }
    }

    # check that file is of type '.sav'
    if (!grepl('.sav', qv7_child_path, fixed = TRUE)){
        stop("The file found is not an SPSS database (.sav)")
    }

    # check if file exists
    qv7_child_exists <- file.exists(qv7_child_path)

    # load data if it exists
    if (isTRUE(qv7_child_exists)) {
        qv7_child_dat <- as.data.frame(haven::read_spss(qv7_child_path))

    } else {
        if (isTRUE(datapath_arg)) {
            stop("File does not exist. Check file_pattern and data_path entered")
        } else {
            stop("File does not exist. Check file_pattern and that the data exists in current working directory")
        }
    }

    # check for and load DXA data
    if (sum(DXA_file) == 1){
        qv7_child_DXApath <- qv7_child_pathlist[DXA_file]

        # check that file is of type '.sav'
        if (!grepl('.sav', qv7_child_DXApath, fixed = TRUE)){
            stop("The DXA file found is not an SPSS database (.sav)")
        }

        # check if DXA exists
        qv7_child_DXA_exists <- file.exists(qv7_child_DXApath)

        # load data if it exists
        if (isTRUE(qv7_child_DXA_exists)) {
            qv7_child_DXAdat <- as.data.frame(haven::read_spss(qv7_child_DXApath))

        } else {
            if (isTRUE(datapath_arg)) {
                stop("DXA file does not exist. Check file_pattern and data_path entered")
            } else {
                stop("DXA file does not exist. Check file_pattern and that the data exists in current working directory")
            }
        }
    } else if (sum(DXA_file) > 1){
        stop("More than one file matched the DXA file_pattern. If have more than 1 file matching the pattern in the directory, may need to move to enter a more specific file_pattern than is standard.")
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

    qv7_child_clean <- qv7_child_clean[c(2, 1, 3:4, 84:91, 92:93, 96, 94:95, 5:24, 29, 31, 30, 97:184, 32:59, 25:28, 61:83, 60, 185:186)]

    qv7_child_clean_labels <- qv7_child_clean_labels[c(2, 1, 3:4, 84:91, 92:93, 96, 94:95, 5:24, 29, 31, 30, 97:184, 32:59, 25:28, 61:83, 60, 185:186)]

    ## make lower case
    names(qv7_child_clean) <- tolower(names(qv7_child_clean))

    ## re-name variables
    for (var in 1:length(names(qv7_child_clean))) {
        var_name <- as.character(names(qv7_child_clean)[var])

        # remove v7 prefix from labels
        if (grepl("v7_", var_name, fixed = TRUE)) {
            names(qv7_child_clean)[var] <- gsub("v7_", "", names(qv7_child_clean)[var])
        }

        # remove v7 prefix from labels
        if (grepl("v7", var_name, fixed = TRUE)) {
            names(qv7_child_clean)[var] <- gsub("v7", "", names(qv7_child_clean)[var])
        }

        # remove v6 prefix from labels
        if (grepl("v6", var_name, fixed = TRUE)) {
            names(qv7_child_clean)[var] <- gsub("v6", "", names(qv7_child_clean)[var])
        }

        # remove trailing _1 from labels
        if (grepl("_1", var_name, fixed = TRUE)) {
            names(qv7_child_clean)[var] <- gsub("_1", "", names(qv7_child_clean)[var])
        }
    }

    ## re-name variables
    names(qv7_child_clean)[2:160] <- c("start_date", "sex", "dob", "height1", "height2", "weight1", "weight2", "height_avg", "weight_avg", "bmi", "bmi_percentile", "freddy_pre_meal", "freddy_post_meal", "freddy_pre_want", "freddy_pre_eah", "freddy_post_eah", "vas_practice", "vas_popcorn", "vas_pretzle", "vas_cornchip", "vas_cookie", "vas_brownie", "vas_starburst", "vas_skittle", "vas_hershey", "vas_icecream", "vas_pbj_sndwch", "vas_ham_sndwch", "vas_turkey_sndwch", "vas_cheese_sndwch", "vas_applesauce", "vas_potatoechip", "vas_babycarrot", "vas_oreo", "vas_milk", "vas_water", "meal_start", "meal_end", "meal_dur", "noplate_applesauce_g", "plate_applesauce_g", "post_applesauce_g", "consumed_applesauce_g", "noplate_carrot_g", "plate_carrot_g", "post_carrot_g", "consumed_carrot_g", "noplate_cheese_sndwch_g", "plate_cheese_sndwch_g", "post_cheese_sndwch_g", "consumed_cheese_sndwch_g", "noplate_cookies_g", "plate_cookies_g", "post_cookies_g", "consumed_cookies_g", "noplate_ham_sndwch_g", "plate_ham_sndwch_g", "post_ham_sndwch_g", "consumed_ham_sndwch_g", "noplate_milk_g", "plate_milk_g", "post_milk_g", "consumed_milk_g", "noplate_pbj_sndwch_g", "plate_pbj_sndwch_g", "post_pbj_sndwch_g", "consumed_pbj_sndwch_g", "noplate_potatochip_g", "plate_potatochip_g", "post_potatochip_g", "consumed_potatochip_g", "noplate_turkey_sndwch_g", "plate_turkey_sndwch_g", "post_turkey_sndwch_g", "consumed_turkey_sndwch_g", "noplate_ketchup_g", "plate_ketchup_g", "post_ketchup_g", "consumed_ketchup_g", "noplate_mayo_g", "plate_mayo_g", "post_mayo_g", "consumed_mayo_g", "noplate_mustard_g", "plate_mustard_g", "post_mustard_g", "consumed_mustard_g", "noplate_brownies_g", "plate_brownies_g", "post_brownies_g", "consumed_brownies_g", "noplate_cornchips_g", "plate_cornchips_g", "post_cornchips_g", "consumed_cornchips_g", "noplate_hersheys_g", "plate_hersheys_g", "post_hersheys_g", "consumed_hersheys_g", "noplate_icecream_g", "plate_icecream_g", "post_icecream_g", "consumed_icecream_g", "noplate_oreos_g", "plate_oreos_g", "post_oreos_g", "consumed_oreos_g", "noplate_popcorn_g", "plate_popcorn_g", "post_popcorn_g", "consumed_popcorn_g", "noplate_pretzels_g", "plate_pretzels_g", "post_pretzels_g", "consumed_pretzels_g", "noplate_skittles_g", "plate_skittles_g", "post_skittles_g", "consumed_skittles_g", "noplate_starbursts_g", "plate_starbursts_g", "post_starbursts_g", "consumed_starbursts_g", "noplate_water_g", "plate_water_g", "post_water_g", "consumed_water_g", "want_water", "want_brownies", "want_applesauce", "want_carrots", "want_cars", "want_cheese", "want_cookies", "want_cornchip", "want_toy", "want_connect4", "want_crayons", "want_ham", "want_dino", "want_hershey", "want_icecream", "want_jenga", "want_legos", "want_elephant", "want_oreos", "want_pbj_sndwch", "want_popcorn", "want_chips", "want_pretzels", "want_skittles", "want_trains", "want_trucks", "want_starbursts", "want_turkey_sndwch", "rank_mac_cheese", "rank_chkn_nug", "rank_broccoli", "rank_grape")

    names(qv7_child_clean)[184] <- "spacegame_reward"

    ## update data labels
    names(qv7_child_clean_labels) <- names(qv7_child_clean)

    # 5) reformatting dates to be appropriate and computer readable YYYY-MM-DD ####
    #format start date
    qv7_child_clean[["start_date"]] <- lubridate::ymd(as.Date(qv7_child_clean[["start_date"]]))

    # dates are fomrated as dd-mstr-yy
    visit_dates[['RO1_V7_Date']] <- lubridate::ymd(as.Date(visit_dates[['RO1_V7_Date']], format = "%d-%b-%y"))

    # add validated dates
    names(visit_dates)[1] <- 'id'
    qv7_child_clean <- merge(qv7_child_clean, visit_dates[c('id', 'RO1_V7_Date')], by = 'id', all.x = TRUE, all.y = FALSE)

    #update start_date
    qv7_child_clean[["start_date"]] <- ifelse(!is.na(qv7_child_clean[['RO1_V7_Date']]), as.character(qv7_child_clean[['RO1_V7_Date']]), as.character(qv7_child_clean[["start_date"]]))

    #remove RO1_V date column
    qv7_child_clean <- qv7_child_clean[, names(qv7_child_clean) != "RO1_V7_Date"]

    # add label
    qv7_child_clean_labels[["start_date"]] <- "date from participant contacts databases ('verified_visit_dates*.csv) converted to format yyyy-mm-dd in R. If no date in database, uses start_date metadata from qualtrics"

    #dob
    qv7_child_clean[["dob"]] <- as.Date(qv7_child_clean[["dob"]], format = "%m/%d/%Y")
    qv7_child_clean_labels[["dob"]] <- "date of birth converted to format yyyy-mm-dd in R"

    #make freaddy fullness numeric
    qv7_child_clean[c(13:17, 40, 157:160)] <- sapply(qv7_child_clean[c(13:17, 40, 157:160)], FUN = as.numeric)


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
    intake_vars <- names(qv7_child_clean)[c(44:131)]

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
    qv7_child_pna_labels[["id"]] <- qv7_child_clean_labels[["id"]]

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

    # make loc2a-loc2c numeric
    qv7_child_clean[165:167] <- sapply(qv7_child_clean[165:167], FUN = as.numeric)

    # 8) fix labels ####

    ## remove 'V7', 'V6', and 'v7' in labels
    for (var in 1:length(names(qv7_child_clean))) {
        var_name <- as.character(names(qv7_child_clean)[var])

        if (grepl("V7", qv7_child_clean_labels[[var_name]], fixed = TRUE)) {
            qv7_child_clean_labels[[var_name]] <- gsub("V7 ", "", qv7_child_clean_labels[[var_name]])
        }

        if (grepl("V6", qv7_child_clean_labels[[var_name]], fixed = TRUE)) {
            qv7_child_clean_labels[[var_name]] <- gsub("V6 ", "", qv7_child_clean_labels[[var_name]])
        }

        if (grepl("v7", qv7_child_clean_labels[[var_name]], fixed = TRUE)) {
            qv7_child_clean_labels[[var_name]] <- gsub("v7 ", "", qv7_child_clean_labels[[var_name]])
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

    #### 9) Add DXA data ####
    if (sum(DXA_file) > 0){
        if (isTRUE(qv7_child_DXA_exists)){

            ## extract variable labels/descriptions
            qv7_child_DXAlabels <- lapply(qv7_child_DXAdat, function(x) attributes(x)$label)

            ## make lowercase
            names(qv7_child_DXAdat) <- tolower(names(qv7_child_DXAdat))

            ## fix naming
            names(qv7_child_DXAdat)[c(65, 68, 70, 73, 75, 78, 80, 83, 85, 88, 90, 93, 95, 99, 102, 104, 108, 111:119, 123:126)] <- c('l_arm_lean_bmc_comb', 'l_arm_perc_fat_ptile', 'r_arm_lean_bmc_comb', 'r_arm_perc_fat_ptile', 'trunk_lean_bmc_comb', 'trunk_perc_fat_ptile', 'l_leg_lean_bmc_comb', 'l_leg_perc_fat_ptile', 'r_leg_lean_bmc_comb', 'r_leg_perc_fat_ptile', 'subtotal_lean_bmc_comb', 'subtotal_perc_fat_ptile', 'head_lean_bmc_comb', 'total_lean_bmc_comb', 'total_perc_fat_ptile', 'android_lean_bmc_comb', 'gynoid_lean_bmc_comb', 'total_body_perc_fat', 'bodyfat_ptile', 'fatmass_height_ratio', 'fatmass_height_ratio_ptile', 'android_gynoid_ratio', 'percfat_trunk_legs_ratio', 'percfat_trunk_legs_ratio_ptile', 'fatmass_trunk_legs_ratio', 'fatmass_trunk_legs_ratio_ptile', 'lean_height_ratio', 'lean_height_ratio_ptile', 'appen_lean_height', 'appen_lean_height_ptile')

            ## add 'dxa' to names and shorten 'percent'
            for (var in 1:length(names(qv7_child_DXAdat))) {
                var_name <- names(qv7_child_DXAdat)[var]

                #add DXA
                #add DXA
                if (var_name != 'id'){
                    names(qv7_child_DXAdat)[var] <- paste0('dxa_', var_name)
                }

                #update var_name
                var_name <- names(qv7_child_DXAdat)[var]

                #shorten 'percent'
                if (grepl("percent", var_name, fixed = TRUE)) {
                    names(qv7_child_DXAdat)[var] <- gsub("percent", "perc", var_name)
                }

            }

            #update labels
            names(qv7_child_DXAlabels) <- names(qv7_child_DXAdat)

            for (var in 1:length(names(qv7_child_DXAlabels))) {
                var_name <- names(qv7_child_DXAlabels)[var]

                if (grepl("VAT", qv7_child_DXAlabels[[var_name]], fixed = TRUE) | grepl("vat", qv7_child_DXAlabels[[var_name]], fixed = TRUE)) {
                    qv7_child_DXAlabels[[var_name]] <- gsub("VAT", "viseral adipose tissue (VAT)", qv7_child_DXAlabels[[var_name]])
                }

                if (grepl("BMC", qv7_child_DXAlabels[[var_name]], fixed = TRUE)) {
                    qv7_child_DXAlabels[[var_name]] <- gsub("BMC", "bone mineral content (BMC)", qv7_child_DXAlabels[[var_name]])
                }

                if (grepl("BMD", qv7_child_DXAlabels[[var_name]], fixed = TRUE)) {
                    qv7_child_DXAlabels[[var_name]] <- gsub("BMD", "bone mineral density (BMD)", qv7_child_DXAlabels[[var_name]])
                }

                if (grepl("Percentile AM", qv7_child_DXAlabels[[var_name]], fixed = TRUE)) {
                    qv7_child_DXAlabels[[var_name]] <- gsub("Percentile AM", "aged matched percentile", qv7_child_DXAlabels[[var_name]])
                }
            }

            #merge
            qv7_child_clean <- merge(qv7_child_clean, qv7_child_DXAdat[c(18, 26:135)], by = 'id', all.x = TRUE)
            qv7_child_clean_labels <- c(qv7_child_clean_labels, qv7_child_DXAlabels[c(26:135)])
        }
    }

    #### 10) Format for export ####

    ## 10a) add attributes to pna data
    qv7_child_pna[2:ncol(qv7_child_pna)] <- as.data.frame(lapply(qv7_child_pna[2:ncol(qv7_child_pna)], function(x) sjlabelled::add_labels(x, labels = c(`Did not skip due to prefer not to answer` = 0, `Prefer not to answer` = 1))))

    for (v in 2:ncol(qv7_child_pna)){
        class(qv7_child_pna[[v]]) <- c("haven_labelled", "vctrs_vctr", "double")
    }

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
