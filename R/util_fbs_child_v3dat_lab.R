#' util_fbs_child_v3dat_lab: Process raw qualtrics visit 3 data for the child
#'
#' This function loads the .sav raw data file for the child visit 3 data that was collected via Qualtrics in the lab when the procedure was split due to covid and cleans the data. Cleaning the data involves:
#' 1) extracting all variable descriptions,
#' 2) selecting relevant data columns,
#' 3) removing all practice events (e.g., 999)
#' 4) re-ordering and re-name data columns
#' 5) reformatting dates to be appropriate and computer readable: YYYY-MM-DD
#' 6) re-calculate manual variables
#' 7) re-ordering factor levels to start with value 0
#' 8) random fixes to factor level names and variable descriptions
#'
#' The databases MUST follow the naming convention: Child_V3_Lab_YYYY-MM-DD.sav
#'
#' @inheritParams qutil_fbs_parent_v1dat
#' @inheritParams qutil_fbs_parent_v1dat
#'
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from child visit 3 Qualtrics and 2) dict: all variable descriptions
#'
#' @examples
#' #if in same working directory as data. Note - there is no need to add the COVID protocol (i.e., 'Home' or 'Lab') to file_pattern:
#' child_v3_dat_lab <- util_fbs_child_v3dat_lab('Child_V3')
#'
#' \dontrun{
#' #file_pattern must be a string. The following will not run:
#' child_v3_dat_lab <- util_fbs_child_v3dat_lab(Child_V3)
#'
#' #file_pattern must have the respondent ('Child') and visit number ('V3'). If just enter 'Child', the script will not run because it will return multiple files for different parent visits. The following will not run:
#' child_v3_dat_lab <- util_fbs_child_v3dat_lab('Child')
#' }
#'
#'
#' @export
#'
util_fbs_child_v3dat_lab <- function(file_pattern, data_path) {

    #### 1. Set up/initial checks #####

    # check that file_pattern exist and is a string

    filepat_arg <- methods::hasArg(file_pattern)

    if (isTRUE(filepat_arg) & !is.character(file_pattern)) {
        stop("file_pattern must be entered as a string: e.g., 'Child_V3'")
    } else if (isFALSE(filepat_arg)) {
        stop("file_pattern must set to the a string matching the name of the raw data file for child visit: e.g., 'Child_V3'")
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
        qv3_child_path <- list.files(path = paste0(data_path, '/Final_Covid/'), pattern = paste0(file_pattern, '_Lab'), full.names = TRUE)

        #if no files found, check direct data_path entered
        if (length(qv3_child_path) == 0) {
            qv3_child_path <- list.files(path = data_path, pattern = paste0(file_pattern, '_Lab'), full.names = TRUE)
        }
    } else {
        qv3_child_path <- paste0(pattern = paste0(file_pattern, '_Lab'), full.names = TRUE)
    }

    # check number of files found
    if (length(qv3_child_path) > 1) {
        stop("More than one file matched after adding '_Lab' to the file_pattern . Be sure thefile_pattern specifies both the respondent (Parent/Child) and visit number (V#). If have more than 1 file matching the pattern in the directory, may need to move to enter a more specific file_pattern than is standard.")
    } else if (length(qv3_child_path) == 0) {
        stop("No files found after adding '_Lab' to file_pattern. Be sure the data_path and file_pattern are correct and that the file exists.")
    }

    # check that file is of type '.sav'
    if (!grepl('.sav', qv3_child_path, fixed = TRUE)){
        stop("The file found is not an SPSS database (.sav)")
    }

    # check if file exists
    qv3_child_exists <- file.exists(qv3_child_path)

    # load data if it exists
    if (isTRUE(qv3_child_exists)) {
        qv3_child_dat <- as.data.frame(haven::read_spss(qv3_child_path))

    } else {
        if (isTRUE(datapath_arg)) {
            stop("File does not exist. Check date_str and data_path entered")
        } else {
            stop("File does not exist. Check date_str and that the data exists in current working directory")
        }
    }

    #### 3. Clean Data #####

    # 1) extract variable labels/descriptions ####
    qv3_child_labels <- lapply(qv3_child_dat, function(x) attributes(x)$label)

    # 2) selecting relevant data columns ####
    qv3_child_clean <- qv3_child_dat[c(1, 18, 22:30, 36:38, 41:69)]

    ## update labels
    qv3_child_clean_labels <- qv3_child_labels[c(1, 18, 22:30, 36:38, 41:69)]

    # 3) removing all practice events (e.g., 999) ####
    qv3_child_clean <- qv3_child_clean[!is.na(qv3_child_clean[["ID"]]) & qv3_child_clean[["ID"]] < 999, ]

    # 4) re-ordering and re-name data columns  ####

    #general order: 1) child information (ID, date), 2) freddies, 3) food, VAS 4) intakes (meal, meal duration) 5) notes

    qv3_child_clean <- qv3_child_clean[c(2, 1, 15:16, 3:14, 17:43)]

    qv3_child_clean_labels <- qv3_child_clean_labels[c(2, 1, 15:16, 3:14, 17:43)]

    ## re-name variables
    names(qv3_child_clean) <- c("id", "start_date", "freddy_pre_meal", "freddy_post_meal", "vas_mac_cheese", "vas_chkn_nug", "vas_broccoli", "vas_grape", "vas_water", "rank_mac_cheese", "rank_chkn_nug", "rank_broccoli", "rank_grape", "meal_start", "meal_end", "meal_dur", "noplate_chkn_nug_g", "plate_chkn_nug_g", "post_chkn_nug_g", "consumed_chkn_nug_g", "noplate_mac_cheese_g", "plate_mac_cheese_g", "post_mac_cheese_g", "consumed_mac_cheese_g", "noplate_grapes_g", "plate_grapes_g", "post_grapes_g", "consumed_grapes_g", "noplate_margerine_g", "noplate_broccoli_g", "plate_broccoli_g", "post_broccoli_g", "consumed_broccoli_g", "noplate_ketchup_g", "plate_ketchup_g", "post_ketchup_g", "consumed_ketchup_g", "noplate_water_g", "plate_water_g", "post_water_g", "consumed_water_g", "food_initials", "child_notes")

    ## update data labels
    names(qv3_child_clean_labels) <- names(qv3_child_clean)

    # 5) reformatting dates to be appropriate and computer readable YYYY-MM-DD ####

    qv3_child_clean[["start_date"]] <- lubridate::ymd(as.Date(qv3_child_clean[["start_date"]]))
    qv3_child_clean_labels[["start_date"]] <- "start_date from qualtrics survey meta-data converted to format yyyy-mm-dd in R"

    ## freddy fullness as numeric
    qv3_child_clean[c(3:4, 10:13, 16)] <- sapply(qv3_child_clean[c(3:4, 10:13, 16)], FUN = as.numeric)

    # 6) re-calculate manual variables ####

    ## re-calculate all intake values

    # get all intake variables
    intake_vars <- names(qv3_child_clean)[c(17:41)]

    # make all intake variables numeric
    for (var in 1:length(intake_vars)) {
        var_name <- intake_vars[[var]]

        qv3_child_clean[[var_name]] <- ifelse(qv3_child_clean[[var_name]] == "-", NA, qv3_child_clean[[var_name]])

        if (is.character(qv3_child_clean[[var_name]])) {
            qv3_child_clean[[var_name]] <- as.numeric(qv3_child_clean[[var_name]])
        }
    }

    # get all foods served - extract prefix and thne postfix in name
    food_strs_g <- unique(sapply(intake_vars, function(x) gsub(".*plate_|.*post_|.*consumed_", "\\1", x), USE.NAMES = FALSE))
    food_strs <- unique(sapply(food_strs_g, function(x) gsub("_g.*", "\\1", x), USE.NAMES = FALSE))

    # loop through foods
    for (f in 1:length(food_strs)) {

        # no post weights for margerine
        if (food_strs[f] != "margerine") {
            # get variable names for plate* and post* weights
            plate_var <- paste0("plate_", food_strs[f], "_g")
            post_var <- paste0("post_", food_strs[f], "_g")
            consumed_var <- paste0("consumed_", food_strs[f], "_g")

            # calculate amount consumed
            qv3_child_clean[[consumed_var]] <- qv3_child_clean[[plate_var]] - qv3_child_clean[[post_var]]
            qv3_child_clean[[consumed_var]] <- ifelse(qv3_child_clean[[consumed_var]] < 0, 0, qv3_child_clean[[consumed_var]])

            # update labels
            qv3_child_clean_labels[[consumed_var]] <- paste0(qv3_child_clean_labels[[consumed_var]], " - recalcuated difference in R with values < 0 set to 0")
        }
    }

    # 7) random fixes to factor level names and variable descriptions  ####
    qv3_child_clean_labels[["meal_start"]] <- "Meal start time"
    qv3_child_clean_labels[["meal_end"]] <- "Meal end time"

    for (var in 1:length(names(qv3_child_clean))) {
        var_name <- as.character(names(qv3_child_clean)[var])

        # remove v3 prefix from labels
        if (grepl("Visit 3", qv3_child_clean_labels[[var_name]], fixed = TRUE)) {
            qv3_child_clean_labels[[var_name]] <- gsub("Visit 3 ", "", qv3_child_clean_labels[[var_name]])
        }

        if (grepl("V3 -", qv3_child_clean_labels[[var_name]], fixed = TRUE)) {
            qv3_child_clean_labels[[var_name]] <- gsub("V3 - ", "", qv3_child_clean_labels[[var_name]])
        }

        if (grepl("V3", qv3_child_clean_labels[[var_name]], fixed = TRUE)) {
            qv3_child_clean_labels[[var_name]] <- gsub("V3 ", "", qv3_child_clean_labels[[var_name]])
        }

    }

    #### 8) Format for export ####
    #put data in order of participant ID for ease
    qv3_child_clean <- qv3_child_clean[order(qv3_child_clean[["id"]]), ]

    # make sure the variable labels match in the dataset
    qv3_child_clean = sjlabelled::set_label(qv3_child_clean, label = matrix(unlist(qv3_child_clean_labels, use.names = FALSE)))

    ## make list of data frame and associated labels
    qv3_child <- list(data = qv3_child_clean, dict = qv3_child_clean_labels)

    ## want an export options??

    return(qv3_child)
}
