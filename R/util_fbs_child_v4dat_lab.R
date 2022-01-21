#' util_fbs_child_v4dat_lab: Process raw qualtrics visit 4 data for the child
#'
#' This function loads the .sav raw data file for the child visit 4 data that was collected via Qualtrics in the lab when the procedure was split due to covid and cleans the data. Cleaning the data involves:
#' 1) extracting all variable descriptions,
#' 2) selecting relevant data columns,
#' 3) removing all practice events (e.g., 999)
#' 4) re-ordering and re-name data columns
#' 5) reformatting dates to be appropriate and computer readable: YYYY-MM-DD
#' 6) re-calculate manual variables
#' 7) re-ordering factor levels to start with value 0
#' 8) random fixes to factor level names and variable descriptions
#'
#' The databases MUST follow the naming convention: Child_V4_Lab_YYYY-MM-DD.sav
#'
#' @inheritParams util_fbs_parent_v1dat
#' @inheritParams util_fbs_parent_v1dat
#'
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from child visit 4 Qualtrics and 2) dict: all variable descriptions
#'
#' @examples
#' #if in same working directory as data. Note - there is no need to add the COVID protocol (i.e., 'Home' or 'Lab') to file_pattern:
#' child_v4_dat_lab <- util_fbs_child_v4dat_lab('Child_V4')
#'
#' \dontrun{
#' #file_pattern must be a string. The following will not run:
#' child_v4_dat_lab <- util_fbs_child_v4dat_lab(Child_V4)
#'
#' #file_pattern must have the respondent ('Child') and visit number ('V4'). If just enter 'Child', the script will not run because it will return multiple files for different parent visits. The following will not run:
#' child_v4_dat_lab <- util_fbs_child_v4dat_lab('Child')
#' }
#'
#'
#' @export
#'
util_fbs_child_v4dat_lab <- function(file_pattern, data_path) {

    #### 1. Set up/initial checks #####

    # check that file_pattern exist and is a string

    filepat_arg <- methods::hasArg(file_pattern)

    if (isTRUE(filepat_arg) & !is.character(file_pattern)) {
        stop("file_pattern must be entered as a string: e.g., 'Child_V4'")
    } else if (isFALSE(filepat_arg)) {
        stop("file_pattern must set to the a string matching the name of the raw data file for child visit: e.g., 'Child_V4'")
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
        qv4_child_path <- list.files(path = paste0(data_path, '/Final_Covid/'), pattern = paste0(file_pattern, '_Lab'), full.names = TRUE)

        #if no files found, check direct data_path entered
        if (length(qv4_child_path) == 0) {
            qv4_child_path <- list.files(path = data_path, pattern = paste0(file_pattern, '_Lab'), full.names = TRUE)
        }
    } else {
        qv4_child_path <- paste0(pattern = paste0(file_pattern, '_Lab'), full.names = TRUE)
    }

    # check number of files found
    if (length(qv4_child_path) > 1) {
        stop("More than one file matched after adding '_Lab' to the file_pattern . Be sure thefile_pattern specifies both the respondent (Parent/Child) and visit number (V#). If have more than 1 file matching the pattern in the directory, may need to move to enter a more specific file_pattern than is standard.")
    } else if (length(qv4_child_path) == 0) {
        stop("No files found after adding '_Lab' to file_pattern. Be sure the data_path and file_pattern are correct and that the file exists.")
    }

    # check that file is of type '.sav'
    if (!grepl('.sav', qv4_child_path, fixed = TRUE)){
        stop("The file found is not an SPSS database (.sav)")
    }

    # check if file exists
    qv4_child_exists <- file.exists(qv4_child_path)

    # load data if it exists
    if (isTRUE(qv4_child_exists)) {
        qv4_child_dat <- as.data.frame(haven::read_spss(qv4_child_path))

    } else {

        if (isTRUE(datapath_arg)) {
            stop("File does not exist. Check file_pattern and data_path entered")
        } else {
            stop("File does not exist. Check file_pattern and that the data exists in current working directory")
        }
    }

    #### 3. Clean Data #####

    # 1) extract variable labels/descriptions ####
    qv4_child_labels <- lapply(qv4_child_dat, function(x) attributes(x)$label)

    # 2) selecting relevant data columns ####
    qv4_child_clean <- qv4_child_dat[c(1, 18:20, 25:33, 39:41, 45, 48:75, 187)]

    ## update labels
    qv4_child_clean_labels <- qv4_child_labels[c(1, 18:20, 25:33, 39:41, 45, 48:75, 187)]

    # 3) removing all practice events (e.g., 999) ####
    qv4_child_clean <- qv4_child_clean[!is.na(qv4_child_clean[["ID"]]) & qv4_child_clean[["ID"]] < 999, ]

    # 4) re-ordering and re-name data columns ####

    #general order: 1) child information (ID, date), 2) freddies, 3) food VAS 4) intakes (meal, meal duration) 5) notes

    qv4_child_clean <- qv4_child_clean[c(2, 1, 3:4, 18:19, 5:16, 20:44, 17, 45:46)]

    qv4_child_clean_labels <- qv4_child_clean_labels[c(2, 1, 3:4, 18:19, 5:16, 20:44, 17, 45:46)]

    ## re-name variables
    names(qv4_child_clean) <- c("id", "start_date", "dob", "age", "freddy_pre_meal", "freddy_post_meal", "vas_mac_cheese", "vas_chkn_nug", "vas_broccoli", "vas_grape", "vas_water", "rank_mac_cheese", "rank_chkn_nug", "rank_broccoli", "rank_grape", "meal_start", "meal_end", "meal_dur", "noplate_chkn_nug_g", "plate_chkn_nug_g", "post_chkn_nug_g", "consumed_chkn_nug_g", "noplate_mac_cheese_g", "plate_mac_cheese_g", "post_mac_cheese_g", "consumed_mac_cheese_g", "noplate_grapes_g", "plate_grapes_g", "post_grapes_g", "consumed_grapes_g", "noplate_margerine_g", "noplate_broccoli_g", "plate_broccoli_g", "post_broccoli_g", "consumed_broccoli_g", "noplate_ketchup_g", "plate_ketchup_g", "post_ketchup_g", "consumed_ketchup_g", "noplate_water_g", "plate_water_g", "post_water_g", "consumed_water_g", "spacegame_reward", "food_initials", "child_notes")

    ## update data labels
    names(qv4_child_clean_labels) <- names(qv4_child_clean)

    # 5) reformatting dates to be appropriate and computer readableYYYY-MM-DD  ####
    #format start date
    qv4_child_clean[["start_date"]] <- lubridate::ymd(as.Date(qv4_child_clean[["start_date"]]))

    # dates are fomrated as dd-mstr-yy
    visit_dates[['RO1_V4_Date']] <- lubridate::ymd(as.Date(visit_dates[['RO1_V4_Date']], format = "%d-%b-%y"))

    # add validated dates
    names(visit_dates)[1] <- 'id'
    qv4_child_clean <- merge(qv4_child_clean, visit_dates[c('id', 'RO1_V4_Date')], by = 'id', all.x = TRUE, all.y = FALSE)

    #update start_date
    qv4_child_clean[["start_date"]] <- ifelse(!is.na(qv4_child_clean[['RO1_V4_Date']]), as.character(qv4_child_clean[['RO1_V4_Date']]), as.character(qv4_child_clean[["start_date"]]))

    #remove RO1_V date column
    qv4_child_clean <- qv4_child_clean[, names(qv4_child_clean) != "RO1_V4_Date"]

    # add label
    qv4_child_clean_labels[["start_date"]] <- "date from participant contacts databases ('verified_visit_dates*.csv) converted to format yyyy-mm-dd in R. If no date in database, uses start_date metadata from qualtrics"

    ## freddy fullness as numeric
    qv4_child_clean[c(5:6, 12:15, 18)] <- sapply(qv4_child_clean[c(5:6, 12:15, 18)], FUN = as.numeric)

    # 6) re-calculate manual variables ####

    # get all intake variables
    intake_vars <- names(qv4_child_clean)[c(19:43)]

    # make all intake variables numeric
    for (var in 1:length(intake_vars)) {
        var_name <- intake_vars[[var]]

        qv4_child_clean[[var_name]] <- ifelse(qv4_child_clean[[var_name]] == "-", NA, qv4_child_clean[[var_name]])

        if (is.character(qv4_child_clean[[var_name]])) {
            qv4_child_clean[[var_name]] <- as.numeric(qv4_child_clean[[var_name]])
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
            qv4_child_clean[[consumed_var]] <- qv4_child_clean[[plate_var]] - qv4_child_clean[[post_var]]
            qv4_child_clean[[consumed_var]] <- ifelse(qv4_child_clean[[consumed_var]] < 0, 0, qv4_child_clean[[consumed_var]])

            # update labels
            qv4_child_clean_labels[[consumed_var]] <- paste0(qv4_child_clean_labels[[consumed_var]], " - recalcuated difference in R with values < 0 set to 0")
        }
    }

    # 7) random fixes to factor level names and variable descriptions ####
    qv4_child_clean_labels[["meal_start"]] <- "Meal start time"
    qv4_child_clean_labels[["meal_end"]] <- "Meal end time"
    qv4_child_clean_labels[["spacegame_reward"]] <- "Type of candy selected for Space Game reward"

    for (var in 1:length(names(qv4_child_clean))) {
        var_name <- as.character(names(qv4_child_clean)[var])

        # remove v4 prefix from labels
        if (grepl("Visit 4", qv4_child_clean_labels[[var_name]], fixed = TRUE)) {
            qv4_child_clean_labels[[var_name]] <- gsub("Visit 4 ", "", qv4_child_clean_labels[[var_name]])
        }

        if (grepl("V4 -", qv4_child_clean_labels[[var_name]], fixed = TRUE)) {
            qv4_child_clean_labels[[var_name]] <- gsub("V4 - ", "", qv4_child_clean_labels[[var_name]])
        }

        if (grepl("V4", qv4_child_clean_labels[[var_name]], fixed = TRUE)) {
            qv4_child_clean_labels[[var_name]] <- gsub("V4 ", "", qv4_child_clean_labels[[var_name]])
        }

    }

    # 8) Format for export ####

    #put data in order of participant ID for ease
    qv4_child_clean <- qv4_child_clean[order(qv4_child_clean[["id"]]), ]

    # make sure the variable labels match in the dataset
    qv4_child_clean = sjlabelled::set_label(qv4_child_clean, label = matrix(unlist(qv4_child_clean_labels, use.names = FALSE)))

    ## make list of data frame and associated labels
    qv4_child <- list(data = qv4_child_clean, dict = qv4_child_clean_labels)

    ## want an export options??

    return(qv4_child)
}
