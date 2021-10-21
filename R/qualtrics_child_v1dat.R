#' qualtrics_child_v1dat: Process raw qualtrics visit 1 data for the child
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
#' 8) random fixes to factor level base::names and variable descriptions
#'
#' @param date_str the date used in the name of the .sav file (e.g., for file 'Child_V1_2021-10-11.sav',
#' the string '2021-10-11' would be entered)
#' @param data_path (optional) the full path to the V1 Child Qualtircs database EXCLUDING the .sav file name (e.g., '.../b-childfoodlab_Shared/Active_Studies/RO1_Brain_Mechanisms_IRB_5357/Participant_Data/untouchedRaw/Qualtrics_Raw/').
#'
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from child visit 1 Qualtrics
#' and 2) dict: all variable descriptions
#'
#' @examples
#' #if in same working directory as data:
#' ch_v1_dat <- qualtrics_child_v1dat('2021-10-11')
#'
#' \dontrun{
#' #date must be a string. The following will not run:
#' ch_v1_dat <- qualtrics_child_v1dat(2021-10-11)
#'
#' #date must match the file name - for file named 'Child_V1_2021_09_16', the
#' following will not run:
#' ch_v1_dat <- qualtrics_child_v1dat('2021_10_11')
#' }
#'
#'
#' @export
#'
qualtrics_child_v1dat <- function(date_str, data_path) {

    #### 1. Set up/initial checks #####

    # check that date_str exist and is a string

    datastr_arg <- methods::hasArg(date_str)

    if (isTRUE(datastr_arg) & !is.character(date_str)) {
        stop("data_str must be enter as a string: e.g., '2021_10_11'")
    } else if (isFALSE(datastr_arg)) {
        stop("data_str must set to the data string from the child visit 1 file name: e.g., '2021_09_16'")
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
        qv1_child_path <- base::paste0(data_path, "/", "Child_V1_", date_str,
            ".sav")
    } else {
        qv1_child_path <- base::paste0("Child_V1_", date_str, ".sav")
    }

    # check if file exists
    qv1_child_exists <- base::file.exists(qv1_child_path)

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

    # 1) extract variable labels/descriptions
    qv1_child_labels <- lapply(qv1_child_dat, function(x) base::attributes(x)$label)

    # 2) selecting relevant data columns
    qv1_child_clean <- qv1_child_dat[c(1, 11:13, 20:39, 44:46, 54:81, 92:103,
        112, 121, 130, 139, 148, 157:335)]

    ## update labels
    qv1_child_clean_labels <- qv1_child_labels[c(1, 11:13, 20:39, 44:46, 54:81, 92:103,
        112, 121, 130, 139, 148, 157:335)]


    # 3) removing all practice events (e.g., 999)
    qv1_child_clean <- qv1_child_clean[!is.na(qv1_child_clean$ID) & qv1_child_clean$ID <
        999, ]

    # 4) re-ordering and re-name data columns general order: 1) child
    # information (sex, dob, h/w, bmi, screen out), 2) freddies, 3) food
    # VAS 4) intakes (preMeal, EAH, meal duration), 5) wanting, PSD, PSS,
    # etc 6) notes

    qv1_child_clean <- qv1_child_clean[c(2, 1, 3:4, 148:156, 157:158, 161,
        159:160, 5:24, 25:27, 162:249, 28:147, 250:251)]

    qv1_child_clean_labels <- qv1_child_clean_labels[c(2, 1, 3:4, 148:156, 157:158, 161,
        159:160, 5:24, 25:27, 162:249, 28:147, 250:251)]

    ## re-name variables
    base::names(qv1_child_clean) <- c("id", "start_date", "sex", "dob",
        "height1", "height2", "weight1", "weight2", "height_avg", "weight_avg",
        "bmi", "bmi_percentile", "bmi_screenout", "freddy_pre_meal", "freddy_post_meal",
        "freddy_pre_want", "freddy_pre_eah", "freddy_post_eah", "vas_practice",
        "vas_popcorn", "vas_pretzle", "vas_cornchip", "vas_cookie", "vas_brownie",
        "vas_starburst", "vas_skittle", "vas_hershey", "vas_icecream",
        "vas_pbj_sndwch", "vas_ham_sndwch", "vas_turkey_sndwch", "vas_cheese_sndwch",
        "vas_applesauce", "vas_potatoechip", "vas_babycarrot", "vash_oreo",
        "vas_milk", "vas_water", "meal_start", "meal_end", "meal_dur",
        "noplate_applesauce_g", "plate_applesauce_g", "post_applesauce_g",
        "consumed_applesauce_g", "noplate_carrot_g", "plate_carrot_g",
        "post_carrot_g", "consumed_carrot_sndwch_g", "noplate_cheese_sndwch_g",
        "plate_cheese_sndwch_g", "post_cheese_sndwch_g", "consumed_cheese_sndwch_g",
        "noplate_cookies_g", "plate_cookies_g", "post_cookies_g", "consumed_cookies_g",
        "noplate_ham_sndwch_g", "plate_ham_sndwch_g", "post_ham_sndwch_g",
        "consumed_ham_sndwch_g", "noplate_milk_g", "plate_milk_g", "post_milk_g",
        "consumed_milk_g", "noplate_pbj_sndwch_g", "plate_pbj_sndwch_g",
        "post_pbj_sndwch_g", "consumed_pbj_sndwch_g", "noplate_potatochip_g",
        "plate_potatochip_g", "post_potatochip_g", "consumed_potatochip_g",
        "noplate_turkey_sndwch_g", "plate_turkey_sndwch_g", "post_turkey_sndwch_g",
        "consumed_turkey_sndwch_g", "noplate_ketchup_g", "plate_ketchup_g",
        "post_ketchup_g", "consumed_ketchup_g", "noplate_mayo_g", "plate_mayo_g",
        "post_mayo_g", "consumed_mayo_g", "noplate_mustard_g", "plate_mustard_g",
        "post_mustard_g", "consumed_mustard_g", "noplate_brownies_g", "plate_brownies_g",
        "post_brownies_g", "consumed_brownies_g", "noplate_cornchips_g",
        "plate_cornchips_g", "post_cornchips_g", "consumed_cornchips_g",
        "noplate_hersheys_g", "plate_hersheys_g", "post_hersheys_g", "consumed_hersheys_g",
        "noplate_icecream_g", "plate_icecream_g", "post_icecream_g", "consumed_icecream_g",
        "noplate_oreos_g", "plate_oreos_g", "post_oreos_g", "consumed_oreos_g",
        "noplate_popcorn_g", "plate_popcorn_g", "post_popcorn_g", "consumed_popcorn_g",
        "noplate_pretzels_g", "plate_pretzels_g", "post_pretzels_g", "consumed_pretzels_g",
        "noplate_skittles_g", "plate_skittles_g", "post_skittles_g", "consumed_skittles_g",
        "noplate_starbursts_g", "plate_starbursts_g", "post_starbursts_g",
        "consumed_starbursts_g", "noplate_water_g", "plate_water_g", "post_water_g",
        "consumed_water_g", "want_water","want_brownies", "want_applesauce", "want_carrots",
        "want_cars", "want_cheese", "want_cookies", "want_cornchip", "want_toy",
        "want_connect4", "want_crayons", "want_ham", "want_dino", "want_hershey",
        "want_icecream", "want_jenga", "want_legos", "want_elephant", "want_oreos",
        "want_pbj_sndwch", "want_popcorn", "want_chips", "want_pretzels",
        "want_skittles", "want_trains", "want_trucks", "want_starbursts",
        "want_turkey_sndwch", "psd_practice1", "psd_practice2", "psd_1",
        "psd_2", "psd_3", "psd_4", "psd_5", "psd_6", "psd_7", "psd_8",
        "psd_9", "psd_10", "psd_practice8", "psd_11", "psd_12", "psd_13",
        "psd_14", "psd_15", "pss_practice1", "pss_vas_hunger", "pss_vas_couldeat",
        "pss_vas_fullness", "pss_practice2", "pss_apple_eat", "pss_apple_much",
        "pss_apple_like", "pss_broccoli_eat", "pss_broccoli_much", "pss_broccoli_like",
        "pss_cake_eat", "pss_cake_much", "pss_cake_like", "pss_candy_eat",
        "pss_candy_much", "pss_candy_like", "pss_carrot_eat", "pss_carrot_much",
        "pss_carrot_like", "pss_cornflakes_eat", "pss_ccornflakes_much",
        "pss_cornflakes_like", "pss_cheese_brgr_eat", "pss_cheese_brgr_much",
        "pss_cheese_brgr_like", "pss_chkn_nug_eat", "pss_chkn_nug_much",
        "pss_chkn_nug_like", "pss_fries_eat", "pss_fries_much", "pss_fries_like",
        "pss_garlic_bread_eat", "pss_garlic_bread_much", "pss_garlic_bread_like",
        "pss_goldfish_eat", "pss_goldfish_much", "pss_goldfish_like", "pss_grapes_eat",
        "pss_grapes_much", "pss_grapes_like", "pss_choc_icecream_eat",
        "pss_choc_icecream_much", "pss_choc_icecream_like", "pss_mac_cheese_eat",
        "pss_mac_cheese_much", "pss_mac_cheese_like", "pss_milk_eat", "pss_milk_much",
        "pss_milk_like", "pss_orangejuice_eat", "pss_orangejuice_much",
        "pss_orangejuice_like", "pss_pbj_sndwch_eat", "pss_pbj_sndwch_much",
        "pss_pbj_sndwch_like", "pss_peas_eat", "pss_peas_much", "pss_peas_like",
        "pss_pizza_eat", "pss_pizza_much", "pss_pizza_like", "pss_soda_eat",
        "pss_soda_much", "pss_soda_like", "pss_soup_eat", "pss_soup_much",
        "pss_soup_like", "pss_tomatoes_eat", "pss_tomatoes_much", "pss_tomatoes_like",
        "pss_yogurt_eat", "pss_yogurt_much", "pss_yogurt_like", "food_initials",
        "child_notes")

    ## update data labels
    base::names(qv1_child_clean_labels) <- base::names(qv1_child_clean)

    # 5) reformatting dates to be appropriate and computer readable:
    # YYYY-MM-DD
    qv1_child_clean$start_date <- ymd(as.Date(qv1_child_clean$start_date))
    qv1_child_clean_labels[["start_date"]] <- "start_date from qualtrics survey meta-data converted to format yyyy-mm-dd in R"

    qv1_child_clean$dob <- as.Date(qv1_child_clean$dob, format = "%m/%d/%Y")
    qv1_child_clean_labels[["dob"]] <- "date of birth converted to format yyyy-mm-dd in R"

    # 6) re-calculate manual variables

    # avg child height, update label
    qv1_child_clean$height_avg <- ifelse(is.na(qv1_child_clean$height1) |
        is.na(qv1_child_clean$height2), NA, rowSums(qv1_child_clean[c("height1",
        "height2")], na.rm = TRUE)/2)
    qv1_child_clean_labels[["height_avg"]] <- "average height calculated in R"

    # avg child weight, update label
    qv1_child_clean$weight_avg <- ifelse(is.na(qv1_child_clean$weight1) |
        is.na(qv1_child_clean$weight2), NA, rowSums(qv1_child_clean[c("weight1",
        "weight2")], na.rm = TRUE)/2)
    qv1_child_clean_labels[["weight_avg"]] <- "average weight calculated in R"

    # child bmi, update label
    if (class(qv1_child_clean$bmi) == "character") {
        qv1_child_clean$bmi <- as.numeric(qv1_child_clean$bmi)
    }

    qv1_child_clean$bmi <- ifelse(is.na(qv1_child_clean$height_avg) | is.na(qv1_child_clean$weight_avg),
        NA, round(qv1_child_clean$weight_avg/((qv1_child_clean$height_avg/100)^2),
            digits = 2))
    qv1_child_clean_labels[["bmi"]] <- "bmi calculated in R package using scripted average height and weight"


    # child age - new variables so need to add to labels
    qv1_child_clean$age_yr <- round((qv1_child_clean$dob %--% qv1_child_clean$start_date)/years(1),
        digits = 2)
    qv1_child_clean_labels[["age_yr"]] <- "Age in years calculated from dob and start_date"

    qv1_child_clean$age_mo <- round((qv1_child_clean$dob %--% qv1_child_clean$start_date)/months(1),
        digits = 1)
    qv1_child_clean_labels[["age_mo"]] <- "Age in months calculated from dob and start_date"

    # child bmi percentile, update label
    qv1_child_clean$bmi_percentile <- round((childsds::sds(value = qv1_child_clean$bmi,
        age = qv1_child_clean$age_yr, sex = qv1_child_clean$sex, item = "bmi",
        ref = cdc.ref, type = "perc", male = 1, female = 2)) * 100, digits = 2)
    qv1_child_clean_labels[["bmi_percentile"]] <- "BMI percentile updated: calculated using childsds R package and scripted average height and weight"

    # child bmi z score : sds (standard deviations away from center/50th
    # centile) - new variable so need to add to labels
    qv1_child_clean$bmi_z <- round(childsds::sds(value = qv1_child_clean$bmi,
        age = qv1_child_clean$age_yr, sex = qv1_child_clean$sex, item = "bmi",
        ref = cdc.ref, type = "SDS", male = 1, female = 2), digits = 2)
    qv1_child_clean_labels[["bmi_z"]] <- "BMI-z/sds calculated using childsds R package"

    # re-organize variables and labels with newly added variables
    qv1_child_clean <- qv1_child_clean[c(1:4, 251:252, 5:12, 253, 13:250)]
    qv1_child_clean_labels <- qv1_child_clean_labels[c(1:4, 251:252, 5:12,
        253, 13:250)]

    # re-calculate all intake values (should we write subfunction to call??)

    # 7) re-ordering factor levels to start with value 0

    ## sex
    base::attributes(qv1_child_clean$sex)$labels <- c(0, 1)

    # 8) random fixes to factor level base::names and variable descriptions
    # fix psd value labes
    psd_names <- base::names(qv1_child_clean)[160:177]

    for (var in 1:length(psd_names)) {
        var_name <- as.character(psd_names[var])
        if (base::attributes(qv1_child_clean[[var_name]])$labels[1] ==
            1) {
            base::names(base::attributes(qv1_child_clean[[var_name]])$labels) <- c("Correct",
                "Incorrect")
        } else {
            base::names(base::attributes(qv1_child_clean[[var_name]])$labels) <- c("Incorrect",
                "Correct")
        }

        ## update variable descriptions for pps to distinguish same/different
        ## from more/less
        orig_label <- qv1_child_clean_labels[[var_name]]
        if (var < 13) {
            qv1_child_clean_labels[[var_name]] <- base::paste0(orig_label,
                " : same or different?")
        } else {
            qv1_child_clean_labels[[var_name]] <- base::paste0(orig_label,
                " : which has more, first or second?")
        }
    }

    ## add meal/EAH to intake descriptions
    intake_vars <- base::names(qv1_child_clean)[45:132]
    for (var in 1:length(intake_vars)) {
        var_name <- as.character(intake_vars[var])
        if (var < 49) {
            qv1_child_clean_labels[[var_name]] <- base::paste0("Meal ",
                qv1_child_clean_labels[[var_name]])
        } else {
            qv1_child_clean_labels[[var_name]] <- base::paste0("EAH ",
                qv1_child_clean_labels[[var_name]])
        }
    }

    ## make ever eat yogurt formatted same as others
    qv1_child_clean_labels[["pss_yogurt_eat"]] <- "PSS Child Strawberry Yogurt - Ever Eat"

    ## remove trailing '... - 1' from labels
    for (var in 1:length(base::names(qv1_child_clean))) {
        var_name <- as.character(base::names(qv1_child_clean)[var])
        if (grepl(" - 1", qv1_child_clean_labels[[var_name]], fixed = TRUE)) {
            qv1_child_clean_labels[[var_name]] <- gsub("\\ - 1.*", "",
                qv1_child_clean_labels[[var_name]])
        }
    }

    # make list of data frame and associated labels
    qv1_child <- list(data = qv1_child_clean, dict = qv1_child_clean_labels)

    ## want an export options??

    return(qv1_child)
}
