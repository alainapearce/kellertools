#' qualtrics_child_v5dat: Process raw qualtrics visit 5 data for the child
#'
#' This function loads the .sav raw data file for the child visit 5 data that was
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
#' @param date_str the date used in the name of the .sav file (e.g., for file 'Child_V5_2021-10-11.sav',
#' the string '2021-10-11' would be entered)
#' @param data_path (optional) the full path to the V5 Child Qualtircs database EXCLUDING the .sav file name (e.g., '.../b-childfoodlab_Shared/Active_Studies/RO1_Brain_Mechanisms_IRB_5357/Participant_Data/untouchedRaw/Qualtrics_Raw/').
#'
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from child visit 5 Qualtrics
#' and 2) dict: all variable descriptions
#'
#' @examples
#' #if in same working directory as data:
#' ch_v5_dat <- qualtrics_child_v5dat('2021-10-11')
#'
#' \dontrun{
#' #date must be a string. The following will not run:
#' ch_v5_dat <- qualtrics_child_v5dat(2021-10-11)
#'
#' #date must match the file name - for file named 'Child_V5_2021_09_16', the
#' following will not run:
#' ch_v5_dat <- qualtrics_child_v5dat('2021_10_11')
#' }
#'
#'
#' @export
#'
qualtrics_child_v2dat_lab <- function(date_str, data_path) {

    #### 1. Set up/initial checks #####

    # check that date_str exist and is a string

    datastr_arg <- methods::hasArg(date_str)

    if (isTRUE(datastr_arg) & !is.character(date_str)) {
        stop("data_str must be enter as a string: e.g., '2021_10_11'")
    } else if (isFALSE(datastr_arg)) {
        stop("data_str must set to the data string from the child visit 5 file name: e.g., '2021_09_16'")
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
        qv5_child_path <- paste0(data_path, "/Final_CovidAtHome/Child_V5_Lab_",
            date_str, ".sav")
    } else {
        qv5_child_path <- paste0("Final_CovidAtHome/Child_V5_Lab", date_str, ".sav")
    }

    # check if file exists
    qv5_child_exists <- file.exists(qv5_child_path)

    # load data if it exists
    if (isTRUE(qv5_child_exists)) {
        qv5_child_dat <- as.data.frame(haven::read_spss(qv5_child_path))

    } else {
        if (isTRUE(datapath_arg)) {
            stop("File does not exist. Check date_str and data_path entered")
        } else {
            stop("File does not exist. Check date_str and that the data exists in current working directory")
        }
    }

    #### 3. Clean Data #####

    # 1) extract variable labels/descriptions #####
    qv5_child_labels <- lapply(qv5_child_dat, function(x) attributes(x)$label)

    # 2) selecting relevant data columns #####
    qv5_child_clean <- qv5_child_dat[c(1, 18, 22:30, 36:38, 40:62, 63:153, 154,
        155:184, 186)]

    ## update labels #####
    qv5_child_clean_labels <- qv5_child_labels[c(1, 18, 22:30, 36:38, 40:62, 63:153,
        154, 155:184, 186)]

    # 3) removing all practice events (e.g., 999) #####
    qv5_child_clean <- qv5_child_clean[!is.na(qv5_child_clean[["id"]]) & qv5_child_clean[["id"]] <
        999, ]

    # 4) re-ordering and re-name data columns  #####

    # general order: 1) child information (ID, date), 2) freddies, 3) food VAS 4) intakes (meal, meal duration) 5) LOC, interoception 6) notes

    qv5_child_clean <- qv5_child_clean[c(2, 1, 130:131, 3:14, 132:156, 15:37, 38:128, 129, 158:159, 157, 160)]

    qv5_child_clean_labels <- qv5_child_clean_labels[c(2, 1, 130:131, 3:14, 132:156, 15:37, 38:128, 129, 158:159, 157, 160)]

    ### UPDATE BELOW HERE ### re-name variables

    ## re-name variables -- make lowercase
    names(qv5_child_clean) <- tolower(names(qv5_child_clean))

    ## re-name variables -- remove v5 prefix
    names(qv5_child_clean)[names(qv5_child_clean) == "v5_food_initials"] <- "food_initials"
    for (var in 1:length(names(qv5_child_clean))) {
        var_name <- as.character(names(qv5_child_clean)[var])

        # remove v2 prefix from labels
        if (grepl("v5", var_name, fixed = TRUE)) {
            names(qv5_child_clean)[var] <- gsub("v5", "", names(qv5_child_clean)[var])
        }
    }

    ## Manually rename
    names(qv5_child_clean)[1:41] <- c("id", "start_date", "freddy_pre_meal", "freddy_post_meal",
        "vas_mac_cheese", "vas_chkn_nug", "vas_broccoli", "vas_grape", "vas_water",
        "mealrank_mac_cheese", "mealrank_chkn_nug", "mealrank_broccoli", "mealrank_grape",
        "meal_start", "meal_end", "meal_dur", "noplate_chkn_nug_g", "plate_chkn_nug_g",
        "post_chkn_nug_g", "consumed_chkn_nug_g", "noplate_mac_cheese_g", "plate_mac_cheese_g",
        "post_mac_cheese_g", "consumed_mac_cheese_g", "noplate_grapes_g", "plate_grapes_g",
        "post_grapes_g", "consumed_grapes_g", "noplate_margerine_g", "noplate_broccoli_g",
        "plate_broccoli_g", "post_broccoli_g", "consumed_broccoli_g", "noplate_ketchup_g",
        "plate_ketchup_g", "post_ketchup_g", "consumed_ketchup_g", "noplate_water_g",
        "plate_water_g", "post_water_g", "consumed_water_g")

    ## update data labels
    names(qv5_child_clean_labels) <- names(qv5_child_clean)

    # 5) reformatting dates to be appropriate and computer readable ####
    # YYYY-MM-DD
    qv5_child_clean[["start_date"]] <- lubridate::ymd(as.Date(qv5_child_clean[["start_date"]]))
    qv5_child_clean_labels[["start_date"]] <- "start_date from qualtrics survey meta-data converted to format yyyy-mm-dd in R"

    # 6) re-calculate manual variables ####

    ## re-calculate all intake values

    # get all intake variables
    intake_vars <- names(qv5_child_clean)[c(17:41)]

    # make all intake variables numeric
    for (var in 1:length(intake_vars)) {
        var_name <- intake_vars[[var]]

        qv5_child_clean[[var_name]] <- ifelse(qv5_child_clean[[var_name]] == "-" |
            qv5_child_clean[[var_name]] == "NA", NA, qv5_child_clean[[var_name]])

        if (is.character(qv5_child_clean[[var_name]])) {
            qv5_child_clean[[var_name]] <- as.numeric(qv5_child_clean[[var_name]])
        }
    }

    # get all foods served - extract prefix and thne postfix in name
    food_strs_g <- unique(sapply(intake_vars, function(x) gsub(".*plate_|.*post_|.*consumed_",
        "\\1", x), USE.NAMES = FALSE))
    food_strs <- unique(sapply(food_strs_g, function(x) gsub("_g.*", "\\1", x),
        USE.NAMES = FALSE))

    # loop through foods
    for (f in 1:length(food_strs)) {

        # no post weights for margerine
        if (food_strs[f] != "margerine") {
            # get variable names for plate* and post* weights
            plate_var <- paste0("plate_", food_strs[f], "_g")
            post_var <- paste0("post_", food_strs[f], "_g")
            consumed_var <- paste0("consumed_", food_strs[f], "_g")

            # calculate amount consumed
            qv5_child_clean[[consumed_var]] <- qv5_child_clean[[plate_var]] -
                qv5_child_clean[[post_var]]
            qv5_child_clean[[consumed_var]] <- ifelse(qv5_child_clean[[consumed_var]] <
                0, 0, qv5_child_clean[[consumed_var]])

            # update labels
            qv5_child_clean_labels[[consumed_var]] <- paste0(qv5_child_clean_labels[[consumed_var]],
                " - recalcuated difference in R with values < 0 set to 0")
        }
    }

    # 7) re-ordering factor levels to start with value 0 #### no levels to reorder

    # 8) random fixes to factor level names and variable descriptions
    qv5_child_clean_labels[["meal_start"]] <- "V3 meal start time"
    qv5_child_clean_labels[["meal_end"]] <- "V3 meal end time"

    #### 9) Format for export #### put data in order of participant ID for ease
    qv5_child_clean <- qv5_child_clean[order(qv5_child_clean[["id"]]), ]

    # make sure the variable labels match in the dataset
    qv5_child_clean = sjlabelled::set_label(qv5_child_clean, label = matrix(unlist(qv5_child_clean_labels,
        use.names = FALSE)))

    ## make list of data frame and associated labels
    qv5_child <- list(data = qv5_child_clean, dict = qv5_child_clean_labels)

    ## want an export options??

    return(qv5_child)
}
