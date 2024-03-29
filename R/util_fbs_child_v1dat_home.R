#' util_fbs_child_v1dat_home: Process raw qualtrics visit 1 home data for the child
#'
#' This function loads the .sav raw data file for the child visit 1 that was
#' collected via Qualtrics at the child's home when the procedure was split due to covid
#' and cleans the data. Cleaning the data involves:
#' 1) extracting all variable descriptions,
#' 2) selecting relevant data columns,
#' 3) removing all practice events (e.g., 999)
#' 4) re-ordering and re-name data columns
#' 5) reformatting dates to be appropriate and computer readable: YYYY-MM-DD
#' 6) re-calculate manual variables
#' 7) re-ordering factor levels to start with value 0
#' 8) random fixes to factor level names and variable descriptions
#'
#' The databases MUST follow the naming convention: Child_V1_Home_YYYY-MM-DD.sav
#'
#' @inheritParams util_fbs_parent_v1dat
#' @inheritParams util_fbs_parent_v1dat
#'
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from child visit 1 Qualtrics collected in the child's home and 2) dict: all variable descriptions
#'
#' @examples
#' #if in same working directory as data. Note - there is no need to add the COVID protocol (i.e., 'Home' or 'Lab') to file_pattern:
#' child_v1_dat_home <- util_fbs_child_v1dat_home('Child_V1')
#'
#' \dontrun{
#' #file_pattern must be a string. The following will not run:
#' child_v1_dat_home <- util_fbs_child_v1dat_home(Child_V1)
#'
#' #file_pattern must have the respondent ('Child') and visit number ('V1'). If just enter 'Child', the script will not run because it will return multiple files for different parent visits. The following will not run:
#' child_v1_dat_home <- util_fbs_child_v1dat_home('Child')
#' }
#'
#'
#' @export
#'
util_fbs_child_v1dat_home <- function(file_pattern, data_path) {

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
        #check pattern of directories specified in Data manual
        qv1_child_pathlist <- list.files(path = paste0(data_path, '/Final_Covid/'), pattern = paste0(file_pattern, '_Home'), full.names = TRUE)

        #if no files found, check direct data_path entered
        if (length(qv1_child_pathlist) == 0) {
            qv1_child_pathlist <- list.files(path = data_path, pattern = paste0(file_pattern, '_Home'), full.names = TRUE)
        }
    } else {
        qv1_child_pathlist <- paste0(pattern = paste0(file_pattern, '_Home'), full.names = TRUE)
    }

    # check number of files found
    if (length(qv1_child_pathlist) > 1) {
        stop("More than one file matched after adding '_Home' to the file_pattern . Be sure thefile_pattern specifies both the respondent (Parent/Child) and visit number (V#). If have more than 1 file matching the pattern in the directory, may need to move to enter a more specific file_pattern than is standard.")
    } else if (length(qv1_child_pathlist) == 0) {
        stop("No files found after adding '_Home' to file_pattern. Be sure the data_path and file_pattern are correct and that the file exists.")
    }

    # check that file is of type '.sav'
    if (!grepl('.sav', qv1_child_pathlist, fixed = TRUE)){
        stop("The file found is not an SPSS database (.sav)")
    }

    # check if file exists
    qv1_child_exists <- file.exists(qv1_child_pathlist)

    # load data if it exists
    if (isTRUE(qv1_child_exists)) {
        qv1_child_dat <- as.data.frame(haven::read_spss(qv1_child_pathlist))

    } else {

        if (isTRUE(datapath_arg)) {
            stop("File does not exist. Check file_pattern and data_path entered")
        } else {
            stop("File does not exist. Check file_pattern and that the data exists in current working directory")
        }
    }

    #### 3. Clean Data #####

    # 1) extract variable labels/descriptions ####
    qv1_child_labels <- lapply(qv1_child_dat, function(x) attributes(x)$label)

    # 2) selecting relevant data columns ####

    qv1_child_clean <- qv1_child_dat[c(1, 18:32, 41, 50, 59, 68, 77, 86:157)]

    ## update labels
    qv1_child_clean_labels <- qv1_child_labels[c(1, 18:32, 41, 50, 59, 68, 77, 86:157)]


    # 3) removing all practice events (e.g., 999) ####
    qv1_child_clean <- qv1_child_clean[!is.na(qv1_child_clean[["ID"]]) & qv1_child_clean[["ID"]] < 999, ]

    # 4) re-ordering and re-name data columns general order ####

    # 1) child id, 2) start date, (3) child information (sex, dob, h/w, bmi, screen out), (4)PSD, PSS

    qv1_child_clean <- qv1_child_clean[c(2, 1, 3:60, 88:90, 61:87, 91:93)]

    qv1_child_clean_labels <- qv1_child_clean_labels[c(2, 1, 3:60, 88:90, 61:87, 91:93)]

    ## re-name variables
    names(qv1_child_clean) <- c("id", "start_date", "sex", "dob", "pss_psd_practice1", "pss_psd_practice2", "pss_psd1", "pss_psd2", "pss_psd3", "pss_psd4", "pss_psd5", "pss_psd6", "pss_psd7", "pss_psd8", "pss_psd9", "pss_psd10", "pss_psd_practice8", "pss_psd11", "pss_psd12", "pss_psd13", "pss_psd14", "pss_psd15", "pss_practice1", "pss_vas_hunger", "pss_vas_couldeat", "pss_vas_fullness", "pss_practice2", "pss_apple_eat", "pss_apple_much", "pss_apple_like", "pss_broccoli_eat", "pss_broccoli_much", "pss_broccoli_like", "pss_cake_eat", "pss_cake_much", "pss_cake_like", "pss_candy_eat", "pss_candy_much", "pss_candy_like", "pss_carrot_eat", "pss_carrot_much", "pss_carrot_like", "pss_cornflakes_eat", "pss_ccornflakes_much", "pss_cornflakes_like", "pss_cheese_brgr_eat", "pss_cheese_brgr_much", "pss_cheese_brgr_like", "pss_chkn_nug_eat", "pss_chkn_nug_much", "pss_chkn_nug_like", "pss_fries_eat", "pss_fries_much", "pss_fries_like", "pss_garlic_bread_eat", "pss_garlic_bread_much", "pss_garlic_bread_like", "pss_goldfish_eat", "pss_goldfish_much", "pss_goldfish_like", "pss_grapes_eat", "pss_grapes_much", "pss_grapes_like", "pss_choc_icecream_eat", "pss_choc_icecream_much", "pss_choc_icecream_like", "pss_mac_cheese_eat", "pss_mac_cheese_much", "pss_mac_cheese_like", "pss_milk_eat", "pss_milk_much", "pss_milk_like", "pss_orangejuice_eat", "pss_orangejuice_much", "pss_orangejuice_like", "pss_pbj_sndwch_eat", "pss_pbj_sndwch_much", "pss_pbj_sndwch_like", "pss_peas_eat", "pss_peas_much", "pss_peas_like", "pss_pizza_eat", "pss_pizza_much", "pss_pizza_like", "pss_soda_eat", "pss_soda_much", "pss_soda_like", "pss_tomatoes_eat", "pss_tomatoes_much", "pss_tomatoes_like", "pss_yogurt_eat", "pss_yogurt_much", "pss_yogurt_like")

    ## update data labels
    names(qv1_child_clean_labels) <- names(qv1_child_clean)

    # 5) reformatting dates to be appropriate and computer readable YYYY-MM-DD ####
    qv1_child_clean[["start_date"]] <- lubridate::ymd(as.Date(qv1_child_clean[["start_date"]]))
    qv1_child_clean_labels[["start_date"]] <- "start_date from qualtrics survey meta-data converted to format yyyy-mm-dd in R"

    qv1_child_clean[["dob"]] <- as.Date(qv1_child_clean[["dob"]], format = "%m/%d/%Y")
    qv1_child_clean_labels[["dob"]] <- "date of birth converted to format yyyy-mm-dd in R"

    # 6) re-calculate manual variables ####

    # child age - new variables so need to add to labels
    qv1_child_clean[["age_yr"]] <- round(lubridate::`%--%`(qv1_child_clean[["dob"]], qv1_child_clean[["start_date"]])/lubridate::years(1), digits = 2)
    qv1_child_clean_labels[["age_yr"]] <- "Age in years calculated from dob and start_date"

    qv1_child_clean[["age_mo"]] <- round(lubridate::`%--%`(qv1_child_clean[["dob"]], qv1_child_clean[["start_date"]])/lubridate::dmonths(1), digits = 1)
    qv1_child_clean_labels[["age_mo"]] <- "Age in months calculated from dob and start_date"

    # re-organize variables and labels with newly added variables
    qv1_child_clean <- qv1_child_clean[c(1:4, 94:95, 5:93)]
    qv1_child_clean_labels <- qv1_child_clean_labels[c(1:4, 94:95, 5:93)]

    # 7) re-ordering factor levels to start with value 0 ####

    ## sex - make sure always matches across parent/child and visits
    qv1_child_clean[["sex"]] <- sjlabelled::set_labels(qv1_child_clean[["sex"]], labels = c(Male = 0, Female = 1))
    set_attr <- attributes(qv1_child_clean[["sex"]])
    qv1_child_clean[["sex"]] <- ifelse(is.na(qv1_child_clean[["sex"]]), NA, ifelse(qv1_child_clean[["sex"]] == 1, 0, 1))
    attributes(qv1_child_clean[["sex"]]) <- set_attr
    qv1_child_clean_labels[["sex"]] <- paste0(qv1_child_clean_labels[["sex"]], " re-leveled in R to start with 0")

    # 8) random fixes to factor level names and variable descriptions ####
    # fix pss-pds value labes
    pss_psd_names <- names(qv1_child_clean)[7:24]

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

    ## make ever eat yogurt formatted same as others
    qv1_child_clean_labels[["pss_yogurt_eat"]] <- "PSS Child Strawberry Yogurt - Ever Eat"

    ## remove trailing '... - 1' from labels
    for (var in 1:length(names(qv1_child_clean))) {
        var_name <- as.character(names(qv1_child_clean)[var])
        if (grepl(" - 1", qv1_child_clean_labels[[var_name]], fixed = TRUE)) {
            qv1_child_clean_labels[[var_name]] <- gsub("\\ - 1.*", "", qv1_child_clean_labels[[var_name]])
        }
    }

    #### 9) Format for export ####

    # put data in order of participant ID for ease
    qv1_child_clean <- qv1_child_clean[order(qv1_child_clean[["id"]]), ]

    # make sure the variable labels match in the dataset
    qv1_child_clean = sjlabelled::set_label(qv1_child_clean, label = matrix(unlist(qv1_child_clean_labels, use.names = FALSE)))

    # make list of data frame and associated labels
    qv1_child <- list(data = qv1_child_clean, dict = qv1_child_clean_labels)

    ## want an export options??

    return(qv1_child)
}
