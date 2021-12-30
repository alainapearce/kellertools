#' util_fbs_child_v2dat: Process raw qualtrics visit 2 data for the child
#'
#' This function loads the .sav raw data file for the child visit 2 data that was collected via Qualtrics and cleans the data. Cleaning the data involves:
#' 1) extracting all variable descriptions,
#' 2) selecting relevant data columns,
#' 3) removing all practice events (e.g., 999)
#' 4) re-ordering and re-name data columns
#' 5) reformatting dates to be appropriate and computer readable: YYYY-MM-DD
#' 6) re-calculate manual variables
#' 7) re-ordering factor levels to start with value 0
#' 8) random fixes to factor level names and variable descriptions
#'
#' The databases MUST follow the naming convention: Child_V2_YYYY-MM-DD.sav
#'
#' @inheritParams util_fbs_parent_v1dat
#' @inheritParams util_fbs_parent_v1dat
#'
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from child visit 2 Qualtrics and 2) dict: all variable descriptions
#'
#' @examples
#' #if in same working directory as data:
#' child_v2_dat <- util_fbs_child_v2dat('Child_V2')
#'
#' \dontrun{
#' #file_pattern must be a string. The following will not run:
#' child_v2_dat <- util_fbs_child_v2dat(Child_V2)
#'
#' #file_pattern must have the respondent ('Child') and visit number ('V2'). If just enter 'Child', the script will not run because it will return multiple files for different parent visits. The following will not run:
#' child_v2_dat <- util_fbs_child_v2dat('Child')
#' }
#'
#'
#' @export
#'
util_fbs_child_v2dat <- function(file_pattern, data_path) {

    #### 1. Set up/initial checks #####

    # check that file_pattern exist and is a string

    filepat_arg <- methods::hasArg(file_pattern)

    if (isTRUE(filepat_arg) & !is.character(file_pattern)) {
        stop("file_pattern must be entered as a string: e.g., 'Child_V2'")
    } else if (isFALSE(filepat_arg)) {
        stop("file_pattern must set to the a string matching the name of the raw data file for child visit: e.g., 'Child_V2'")
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
        qv2_child_path <- list.files(path = data_path, pattern = file_pattern, full.names = TRUE)
    } else {
        qv2_child_path <- paste0(pattern = file_pattern, full.names = TRUE)
    }

    # check number of files found
    if (length(qv2_child_path) > 1) {
        stop("More than one file matched the file_pattern. Be sure thefile_pattern specifies both the respondent (Parent/Child) and visit number (V#). If have more than 1 file matching the pattern in the directory, may need to move to enter a more specific file_pattern than is standard.")
    } else if (length(qv2_child_path) == 0) {
        stop('No files found. Be sure the data_path and file_pattern are correct and that the file exists')
    }

    # check that file is of type '.sav'
    if (!grepl('.sav', qv2_child_path, fixed = TRUE)){
        stop("The file found is not an SPSS database (.sav)")
    }

    # check if file exists
    qv2_child_exists <- file.exists(qv2_child_path)

    # load data if it exists
    if (isTRUE(qv2_child_exists)) {
        qv2_child_dat <- as.data.frame(haven::read_spss(qv2_child_path))

    } else {
        if (isTRUE(datapath_arg)) {
            stop("File does not exist. Check date_str and data_path entered")
        } else {
            stop("File does not exist. Check date_str and that the data exists in current working directory")
        }
    }

    #### 3. Clean Data #####

    # 1) extract variable labels/descriptions ####
    qv2_child_labels <- lapply(qv2_child_dat, function(x) attributes(x)$label)

    # 2) selecting relevant data columns ####
    qv2_child_clean <- qv2_child_dat[c(1, 18, 23:31, 36:38, 40:146, 149:177)]

    ## update labels
    qv2_child_clean_labels <- qv2_child_labels[c(1, 18, 23:31, 36:38, 40:146, 149:177)]


    # 3) removing all practice events (e.g., 999) ####
    qv2_child_clean <- qv2_child_clean[!is.na(qv2_child_clean[["ID"]]) & qv2_child_clean[["ID"]] < 999, ]

    # 4) re-ordering and re-name data columns  ####
    # general order: 1) child information (ID), 2) freddies, 3) food VAS 4), intakes (meal, meal duration), 5) KFQ, TESQE, RCMAS 6) notes

    qv2_child_clean <- qv2_child_clean[c(2, 1, 122:123, 3:14, 124:148, 15:121, 149:150)]

    qv2_child_clean_labels <- qv2_child_clean_labels[c(2, 1, 122:123, 3:14, 124:148, 15:121, 149:150)]

    ## re-name variables -- make lowercase
    names(qv2_child_clean) <- tolower(names(qv2_child_clean))

    ## re-name variables -- remove v2 prefix
    names(qv2_child_clean)[names(qv2_child_clean) == "v2_food_initials"] <- "food_initials"

    for (var in 1:length(names(qv2_child_clean))) {
        var_name <- as.character(names(qv2_child_clean)[var])

        # remove v2 prefix from labels
        if (grepl("v2", var_name, fixed = TRUE)) {
            names(qv2_child_clean)[var] <- gsub("v2", "", names(qv2_child_clean)[var])
        }
    }

    ## manually update variables
    names(qv2_child_clean)[names(qv2_child_clean) == "childnotes"] <- "child_notes"

    names(qv2_child_clean)[2:41] <- c("start_date", "freddy_pre_meal", "freddy_post_meal", "vas_mac_cheese", "vas_chkn_nug", "vas_broccoli", "vas_grape", "vas_water", "rank_mac_cheese", "rank_chkn_nug", "rank_broccoli", "rank_grape", "meal_start", "meal_end", "meal_dur", "noplate_chkn_nug_g", "plate_chkn_nug_g", "post_chkn_nug_g", "consumed_chkn_nug_g", "noplate_mac_cheese_g", "plate_mac_cheese_g", "post_mac_cheese_g", "consumed_mac_cheese_g", "noplate_grapes_g", "plate_grapes_g", "post_grapes_g", "consumed_grapes_g", "noplate_margerine_g", "noplate_broccoli_g", "plate_broccoli_g", "post_broccoli_g", "consumed_broccoli_g", "noplate_ketchup_g", "plate_ketchup_g", "post_ketchup_g", "consumed_ketchup_g", "noplate_water_g", "plate_water_g", "post_water_g", "consumed_water_g")

    ## update data labels
    names(qv2_child_clean_labels) <- names(qv2_child_clean)

    # 5) reformatting dates to be appropriate and computer readable YYYY-MM-DD ####
    qv2_child_clean[["start_date"]] <- lubridate::ymd(as.Date(qv2_child_clean[["start_date"]]))
    qv2_child_clean_labels[["start_date"]] <- "start_date from qualtrics survey meta-data converted to format yyyy-mm-dd in R"

    ## freddy fullness as numeric
    qv2_child_clean[c(3:4, 10:13, 16)] <- sapply(qv2_child_clean[c(3:4, 10:13, 16)], FUN = as.numeric)


    # 6) re-calculate manual variables ####

    ## re-calculate all intake values

    # get all intake variables
    intake_vars <- names(qv2_child_clean)[c(17:41)]

    # make all intake variables numeric
    for (var in 1:length(intake_vars)) {
        var_name <- intake_vars[[var]]

        qv2_child_clean[[var_name]] <- ifelse(qv2_child_clean[[var_name]] == "-", NA, ifelse(qv2_child_clean[[var_name]] ==
            "513,63", "513.63", qv2_child_clean[[var_name]]))

        if (is.character(qv2_child_clean[[var_name]])) {
            qv2_child_clean[[var_name]] <- as.numeric(qv2_child_clean[[var_name]])
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
            qv2_child_clean[[consumed_var]] <- qv2_child_clean[[plate_var]] - qv2_child_clean[[post_var]]
            qv2_child_clean[[consumed_var]] <- ifelse(qv2_child_clean[[consumed_var]] < 0, 0, qv2_child_clean[[consumed_var]])

            # update labels
            qv2_child_clean_labels[[consumed_var]] <- paste0(qv2_child_clean_labels[[consumed_var]], " - recalcuated difference in R with values < 0 set to 0")
        }
    }

    # 7) re-ordering factor levels to start with value 0 ####

    ## fix kfq factor levels to start at 0
    kfq_names <- names(qv2_child_clean)[42:87]
    for (var in 1:length(kfq_names)) {
        var_name <- as.character(kfq_names[var])

        qv2_child_clean[[var_name]] <- sjlabelled::set_labels(qv2_child_clean[[var_name]], labels = c(`Never eat this` = 0, `Less than once in 7 days` = 1, `1-2 times in 7 days` = 2, `3-5 times in 7 days` = 3, `6-7 times in 7 days` = 4, `More than 7 times in 7 days` = 5))

        set_attr <- attributes(qv2_child_clean[[var_name]])

        qv2_child_clean[[var_name]] <- ifelse(is.na(qv2_child_clean[[var_name]]), NA, ifelse(qv2_child_clean[[var_name]] ==  1, 0, ifelse(qv2_child_clean[[var_name]] == 2, 1, ifelse(qv2_child_clean[[var_name]] == 3, 2, ifelse(qv2_child_clean[[var_name]] ==  4, 3, ifelse(qv2_child_clean[[var_name]] == 5, 4, 5))))))

        attributes(qv2_child_clean[[var_name]]) <- set_attr

        qv2_child_clean_labels[[var_name]] <- paste0(qv2_child_clean_labels[[var_name]], " - re-leveled in R to start with 0")
    }

    # 8) random fixes to factor level names and variable descriptions ####

    ## tesqe: Change value for 'Don't know' from 99 to -99
    tesqe_names <- names(qv2_child_clean)[88:111]

    for (var in 1:length(tesqe_names)) {
        var_name <- as.character(tesqe_names[var])
        qv2_child_clean[[var_name]] <- sjlabelled::set_labels(qv2_child_clean[[var_name]], labels = c(Never = 1, Rarely = 2, Sometimes = 3, Regularly = 4, Often = 5, `Dont know` = -99))

        set_attr <- attributes(qv2_child_clean[[var_name]])

        # convert 99s to -99 and make numeric variable labels only update
        qv2_child_clean[[var_name]] <- ifelse(qv2_child_clean[[var_name]] == 99, -99, qv2_child_clean[[var_name]])

        attributes(qv2_child_clean[[var_name]]) <- set_attr

        qv2_child_clean_labels[[var_name]] <- paste0(qv2_child_clean_labels[[var_name]], " - values modified in R so Dont know = -99")

    }

    for (var in 1:length(names(qv2_child_clean))) {
        var_name <- as.character(names(qv2_child_clean)[var])

        # remove v2 prefix from labels
        if (grepl("Visit 2", qv2_child_clean_labels[[var_name]], fixed = TRUE)) {
            qv2_child_clean_labels[[var_name]] <- gsub("Visit 2 ", "", qv2_child_clean_labels[[var_name]])
        }

        if (grepl("V2 -", qv2_child_clean_labels[[var_name]], fixed = TRUE)) {
            qv2_child_clean_labels[[var_name]] <- gsub("V2 - ", "", qv2_child_clean_labels[[var_name]])
        }

        if (grepl("V2", qv2_child_clean_labels[[var_name]], fixed = TRUE)) {
            qv2_child_clean_labels[[var_name]] <- gsub("V2 ", "", qv2_child_clean_labels[[var_name]])
        }

    }

    #### 9) Format for export ####

    #put data in order of participant ID for ease
    qv2_child_clean <- qv2_child_clean[order(qv2_child_clean[["id"]]), ]

    # make sure the variable labels match in the dataset
    qv2_child_clean = sjlabelled::set_label(qv2_child_clean, label = matrix(unlist(qv2_child_clean_labels, use.names = FALSE)))

    ## make list of data frame and associated labels
    qv2_child <- list(data = qv2_child_clean, dict = qv2_child_clean_labels)

    ## want an export options??

    return(qv2_child)
}
