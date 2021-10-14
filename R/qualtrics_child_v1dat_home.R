#' qualtrics_child_v1dat_home: Process raw qualtrics visit 1_home data for the child
#'
#' This function loads the .sav raw data file for the child visit 1_home data that was
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
qualtrics_child_v1dat_home <- function(date_str, data_path) {

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
        qv1_child_path <- base::paste0(data_path, "/Final_CovidAtHome/Child_V1_Home_", date_str,
            ".sav")
    } else {
        qv1_child_path <- base::paste0("Final_CovidAtHome/Child_V1_Home", date_str, ".sav")
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

    qv1_child_clean <- qv1_child_dat[c(1, 18:32, 41, 50, 59, 68, 77, 86:157)]

    ## update labels
    qv1_child_clean_labels <- qv1_child_labels[c(1, 18:32, 41, 50, 59, 68, 77, 86:157)]


    # 3) removing all practice events (e.g., 999)
    qv1_child_clean <- qv1_child_clean[!is.na(qv1_child_clean$ID) & qv1_child_clean$ID <
        999, ]

    # 4) re-ordering and re-name data columns general order: 1) child id,
    # (2) start date, (3) child information (sex, dob, h/w, bmi, screen out),
    # (4)PSD, PSS

    qv1_child_clean <- qv1_child_clean[c(2, 1, 3:93)]

    qv1_child_clean_labels <- qv1_child_clean_labels[c(2, 1, 3:93)]

    ## re-name variables
    base::names(qv1_child_clean) <- c("id", "start_date", "sex", "dob",
        "psd_practice1", "psd_practice2", "psd_1",
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
        "pss_goldfish_eat", "pss_goldfish_much", "pss_goldfish_like", "pss_choc_icecream_eat",
        "pss_choc_icecream_much", "pss_choc_icecream_like", "pss_mac_cheese_eat",
        "pss_mac_cheese_much", "pss_mac_cheese_like", "pss_milk_eat", "pss_milk_much",
        "pss_milk_like", "pss_orangejuice_eat", "pss_orangejuice_much",
        "pss_orangejuice_like", "pss_pbj_sndwch_eat", "pss_pbj_sndwch_much",
        "pss_pbj_sndwch_like", "pss_peas_eat", "pss_peas_much", "pss_peas_like",
        "pss_pizza_eat", "pss_pizza_much", "pss_pizza_like", "pss_soda_eat",
        "pss_soda_much", "pss_soda_like", "pss_tomatoes_eat", "pss_tomatoes_much",
        "pss_tomatoes_like","pss_grapes_eat","pss_grapes_much", "pss_grapes_like",
        "pss_yogurt_eat", "pss_yogurt_much", "pss_yogurt_like")

    ## update data labels
    base::names(qv1_child_clean_labels) <- base::names(qv1_child_clean)

    # 5) reformatting dates to be appropriate and computer readable:
    # YYYY-MM-DD
    qv1_child_clean$start_date <- lubridate::ymd(as.Date(qv1_child_clean$start_date))
    qv1_child_clean_labels[["start_date"]] <- "start_date from qualtrics survey meta-data converted to format yyyy-mm-dd in R"

    qv1_child_clean$dob <- as.Date(qv1_child_clean$dob, format = "%m/%d/%Y")
    qv1_child_clean_labels[["dob"]] <- "date of birth converted to format yyyy-mm-dd in R"

    # 6) re-calculate manual variables

    # child age - new variables so need to add to labels
    qv1_child_clean$age_yr <- round((qv1_child_clean$dob %--% qv1_child_clean$start_date)/years(1),
        digits = 2)
    qv1_child_clean_labels[["age_yr"]] <- "Age in years calculated from dob and start_date"

    qv1_child_clean$age_mo <- round((qv1_child_clean$dob %--% qv1_child_clean$start_date)/months(1),
        digits = 1)
    qv1_child_clean_labels[["age_mo"]] <- "Age in months calculated from dob and start_date"

    # re-organize variables and labels with newly added variables
    qv1_child_clean <- qv1_child_clean[c(1:4, 94:95, 5:93)]
    qv1_child_clean_labels <- qv1_child_clean_labels[c(1:4, 94:95, 5:93)]

    # 7) re-ordering factor levels to start with value 0

    ## sex
    base::attributes(qv1_child_clean$sex)$labels <- c(0, 1)

    # 8) random fixes to factor level base::names and variable descriptions
    # fix psd value labes
    psd_names <- base::names(qv1_child_clean)[7:24]

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
