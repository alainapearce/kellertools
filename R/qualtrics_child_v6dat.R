#' qualtrics_child_v6dat: Process raw qualtrics visit 6 data for the child
#'
#' This function loads the .sav raw data file for the child visit 4 data that was
#' collected via Qualtrics and cleans the data. Cleaning the data involves:
#' 1) extracting all variable descriptions,
#' 2) selecting relevant data columns,
#' 3) removing all practice events (e.g., 999)
#' 4) re-ordering and re-name data columns
#' 5) reformatting dates to be appropriate and computer readable: YYYY-MM-DD
#' 6) re-calculate manual variables
#' 7) fix 99's / create prefer not to answer database
#' 8) random fixes to factor level names and variable descriptions
#'
#' @param date_str the date used in the name of the .sav file (e.g., for file 'Child_V6_2021-10-11.sav',
#' the string '2021-10-11' would be entered)
#' @param data_path (optional) the full path to the V4 Child Qualtircs database EXCLUDING the .sav file name (e.g., '.../b-childfoodlab_Shared/Active_Studies/RO1_Brain_Mechanisms_IRB_5357/Participant_Data/untouchedRaw/Qualtrics_Raw/').
#'
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from child visit 6 Qualtrics
#' and 2) dict: all variable descriptions
#'
#' @examples
#' #if in same working directory as data:
#' ch_v6_dat <- qualtrics_child_v6dat('2021-10-11')
#'
#' \dontrun{
#' #date must be a string. The following will not run:
#' ch_v6_dat <- qualtrics_child_v6dat(2021-10-11)
#'
#' #date must match the file name - for file named 'Child_V4_2021_09_16', the
#' following will not run:
#' ch_v6_dat <- qualtrics_child_v6dat('2021_10_11')
#' }
#'
#'
#' @export
#'
qualtrics_child_v6dat <- function(date_str, data_path) {

    #### 1. Set up/initial checks #####

    # check that date_str exist and is a string

    datastr_arg <- methods::hasArg(date_str)

    if (isTRUE(datastr_arg) & !is.character(date_str)) {
        stop("data_str must be enter as a string: e.g., '2021_10_11'")
    } else if (isFALSE(datastr_arg)) {
        stop("data_str must set to the data string from the child visit 6 file name: e.g., '2021_09_16'")
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
        qv6_child_path <- paste0(data_path, "/", "Child_V6_", date_str, ".sav")
    } else {
        qv6_child_path <- paste0("Child_V6_", date_str, ".sav")
    }

    # check if file exists
    qv6_child_exists <- file.exists(qv6_child_path)

    # load data if it exists
    if (isTRUE(qv6_child_exists)) {
        qv6_child_dat <- as.data.frame(haven::read_spss(qv6_child_path))

    } else {
        if (isTRUE(datapath_arg)) {
            stop("File does not exist. Check date_str and data_path entered")
        } else {
            stop("File does not exist. Check date_str and that the data exists in current working directory")
        }
    }

    #### 3. Clean Data #####

    ##### NOTE: TROUBLESHOOT F2_FULL RATINGS. they are not currently making it into the clean database #####

    # 1) extract variable labels/descriptions
    qv6_child_labels <- lapply(qv6_child_dat, function(x) attributes(x)$label)

    # 2) removing all practice events (e.g., 999)
    qv6_child_clean <- qv6_child_dat[!is.na(qv6_child_dat$ID) & qv6_child_dat$ID <
        999, ]

    # 3) selecting relevant data columns

    ## Extract ID and Version A MRI VAS ratings
    MRIVAS_A <- qv6_child_clean[c(18, 58:207, 210:359)]

    ### fix Version A MRI VAS variable names
    names(MRIVAS_A)[names(MRIVAS_A) == "MRIVAS_C1_06_Like_A_1.0"] <- "MRIVAS_C1_06_Full_A_1"
    for (var in 1:length(names(MRIVAS_A))) {
        var_name <- as.character(names(MRIVAS_A)[var])

        # remove trailing '_A_1' from names
        if (grepl("_A_1", var_name, fixed = TRUE)) {
            names(MRIVAS_A)[var] <- gsub("_A_1", "", var_name)
        }
    }

    ### remove rows that have NAs for every question
    MRIVAS_A <- MRIVAS_A[rowSums(!is.na(MRIVAS_A[2:301])) == 300, ]

    ## Extract ID and Version A MRI VAS ratings
    MRIVAS_B <- qv6_child_clean[c(18, 360:659)]

    ### fix Version B MRI VAS variable names
    names(MRIVAS_B)[names(MRIVAS_B) == "MRIVAS_A1_04_Like_B_1.0"] <- "MRIVAS_A1_04_Full_B_1"
    for (var in 1:length(names(MRIVAS_B))) {
        var_name <- as.character(names(MRIVAS_B)[var])

        # remove trailing '_B_1' from names
        if (grepl("_B_1", var_name, fixed = TRUE)) {
            names(MRIVAS_B)[var] <- gsub("_B_1", "", var_name)
        }
    }
    ### remove rows that have NAs for every question
    MRIVAS_B <- MRIVAS_B[rowSums(!is.na(MRIVAS_B[2:301])) == 300, ]

    ## Combine MRI VAS ratings from version A and B Note: column order of
    ## MRIVAS_combined will be the same as MRIVAS_A (important for updating labels
    ## below)
    MRIVAS_combined <- rbind(MRIVAS_A, MRIVAS_B)

    ## Extract additional variables from qualtrics database
    qv6_child_clean <- qv6_child_clean[c(1, 18, 33, 37, 40:45, 50, 57, 661:667,
        668:686)]

    ## merge MRIVAS_combined and qv6_child_clean
    qv6_child_clean <- merge(qv6_child_clean, MRIVAS_combined, by = "ID")

    ## update labels and label names Note: MRIVAS labels (version A order) are
    ## added to the end
    qv6_child_clean_labels <- qv6_child_labels[c(1, 18, 33, 37, 40:45, 50, 57,
        661:667, 668:686, 58:207, 210:359)]
    names(qv6_child_clean_labels) <- names(qv6_child_clean)

    # 4) re-ordering and re-name data columns general order: 1) child information
    # (ID. date), 2) freddies, 3) intake (snack), 4) MRIVAS 5) notes

    qv6_child_clean <- qv6_child_clean[c(1:2, 13:30, 3:12, 39:338, 31:38)]

    qv6_child_clean_labels <- qv6_child_clean_labels[c(1:2, 13:30, 3:12, 39:338, 31:38)]

    ## re-name variables
    ### make lowercase
    names(qv6_child_clean) <- tolower(names(qv6_child_clean))

    names(qv6_child_clean)[1:30] <- c("id", "start_date", "freddy_pre_mrisnack", "freddy_post_mrisnack", "freddy_post_mrisnack2",
        "freddy_pre_mri", "freddy_pre_dg", "freddy_pre_mrivas", "freddy_pre_sst",
        "noplate_grapes_g", "plate_grapes_g", "post_grapes_g", "consumed_grapes_g",
        "noplate_ritz_g", "plate_ritz_g", "post_ritz_g", "consumed_ritz_g",
        "noplate_juice_g", "post_juice_g", "consumed_juice_g", "cams_pre_mri", "cams_post_mri",
        "dg_foodchoice", "dg_foodchoice2", "dg_foodchoice_amount", "dg_foodchoice_amount2",
        "dg_foodchoice2_amount", "dg_foodchoice2_amount2", "dg_wait", "mri_taskversion")

    names(qv6_child_clean)[331:338] <- c("notes_mri_mprage", "notes_mri_restingstate", "notes_mri_run1", "notes_mri_run2",
        "notes_mri_run3", "notes_mri_run4", "notes_mri_run5", "notes")

    ## update data labels
    names(qv6_child_clean_labels) <- names(qv6_child_clean)

    # 5) reformatting dates to be appropriate and computer readable ####
    # YYYY-MM-DD
    qv6_child_clean$start_date <- lubridate::ymd(as.Date(qv6_child_clean$start_date))
    qv6_child_clean_labels[["start_date"]] <- "start_date from qualtrics survey meta-data converted to format yyyy-mm-dd in R"

    # 6) re-calculate manual variables ####

    ## re-calculate all intake values

    # get all intake variables
    intake_vars <- names(qv6_child_clean)[c(10:20)]

    # make all intake variables numeric NOTE - there is a whole row I am not
    # manually fixing as every value has ',' instead of '.'
    for (var in 1:length(intake_vars)) {
        var_name <- intake_vars[[var]]

        qv6_child_clean[[var_name]] <- ifelse(qv6_child_clean[[var_name]] == "-" |
            qv6_child_clean[[var_name]] == "NA", NA, qv6_child_clean[[var_name]])

        if (is.character(qv6_child_clean[[var_name]])) {
            qv6_child_clean[[var_name]] <- as.numeric(qv6_child_clean[[var_name]])
        }
    }

    # get all foods served - extract prefix and thne postfix in name
    food_strs_g <- unique(sapply(intake_vars, function(x) gsub(".*plate_|.*post_|.*consumed_",
        "\\1", x), USE.NAMES = FALSE))
    food_strs <- unique(sapply(food_strs_g, function(x) gsub("_g.*", "\\1", x),
        USE.NAMES = FALSE))

    # loop through foods
    for (f in 1:length(food_strs)) {

        # post weights for juice are based on noplate_juice, as juice was weighed in the juice box without a plate
        if (food_strs[f] != "juice") {
            # get variable names for plate* and post* weights
            plate_var <- paste0("plate_", food_strs[f], "_g")
            post_var <- paste0("post_", food_strs[f], "_g")
            consumed_var <- paste0("consumed_", food_strs[f], "_g")

            # calculate amount consumed
            qv6_child_clean[[consumed_var]] <- qv6_child_clean[[plate_var]] -
                qv6_child_clean[[post_var]]
            qv6_child_clean[[consumed_var]] <- ifelse(qv6_child_clean[[consumed_var]] <
                0, 0, qv6_child_clean[[consumed_var]])

            # update labels
            qv6_child_clean_labels[[consumed_var]] <- paste0(qv6_child_clean_labels[[consumed_var]],
                " - recalcuated difference in R with values < 0 set to 0")

        } else {
            # get variable names for noplate* and post* weights
            noplate_var <- paste0("noplate_", food_strs[f], "_g")
            post_var <- paste0("post_", food_strs[f], "_g")
            consumed_var <- paste0("consumed_", food_strs[f], "_g")

            # calculate amount consumed
            qv6_child_clean[[consumed_var]] <- qv6_child_clean[[noplate_var]] -
                qv6_child_clean[[post_var]]
            qv6_child_clean[[consumed_var]] <- ifelse(qv6_child_clean[[consumed_var]] <
                                                          0, 0, qv6_child_clean[[consumed_var]])

            # update labels
            qv6_child_clean_labels[[consumed_var]] <- paste0(qv6_child_clean_labels[[consumed_var]],
                                                             " - recalcuated difference in R with values < 0 set to 0")
        }
    }

    # 7) fix 99's ####
        # No 99s to fix

    # 8) random fixes to factor level names and variable descriptions
    qv6_child_clean_labels[["id"]] <- "participant id"
    qv6_child_clean_labels[["cams_post_mri"]] <- "Post-scan CAMS"
    qv6_child_clean_labels[["notes_mri_mprage"]] <- "notes about MRI: mprage scan"
    qv6_child_clean_labels[["notes_mri_restingstate"]] <- "notes about MRI: resting state scan"
    qv6_child_clean_labels[["notes_mri_run1"]] <- "notes about MRI: food cue task - run1"
    qv6_child_clean_labels[["notes_mri_run2"]] <- "notes about MRI: food cue task - run2"
    qv6_child_clean_labels[["notes_mri_run3"]] <- "notes about MRI: food cue task - run3"
    qv6_child_clean_labels[["notes_mri_run4"]] <- "notes about MRI: food cue task - run4"
    qv6_child_clean_labels[["notes_mri_run5"]] <- "notes about MRI: food cue task - run5"
    qv6_child_clean_labels[["notes"]] <- "V6 notes"


    ## fix preMRI cams factor levels to start at 0
    var_name = "cams_pre_mri"
    qv6_child_clean[[var_name]] <- sjlabelled::set_labels(qv6_child_clean[[var_name]], labels = c(`0` = 0,
                                                                                                      `1` = 1, `2` = 2, `3` = 3, `4` = 4,`5` = 5,
                                                                                                      `6` = 6, `7` = 7,`8` = 8,`9` = 9, `10` = 10))
    set_attr <- attributes(qv6_child_clean$var_name)
    qv6_child_clean[[var_name]] <- ifelse(is.na(qv6_child_clean[[var_name]]), NA, ifelse(qv6_child_clean[[var_name]] ==
                                                                                                 1, 0, ifelse(qv6_child_clean[[var_name]] == 2, 1,
                                                                                                              ifelse(qv6_child_clean[[var_name]] == 3, 2,
                                                                                                                     ifelse(qv6_child_clean[[var_name]] == 4, 3,
                                                                                                                            ifelse(qv6_child_clean[[var_name]] == 5, 4,
                                                                                                                                   ifelse(qv6_child_clean[[var_name]] == 6, 5,
                                                                                                                                          ifelse(qv6_child_clean[[var_name]] == 7, 6,
                                                                                                                                                 ifelse(qv6_child_clean[[var_name]] == 8, 7,
                                                                                                                                                        ifelse(qv6_child_clean[[var_name]] == 9, 8,
                                                                                                                                                               ifelse(qv6_child_clean[[var_name]] == 10, 9, 10)))))))))))
    attributes(qv6_child_clean[[var_name]]) <- set_attr
    qv6_child_clean_labels[[var_name]] <- paste0(qv6_child_clean_labels[[var_name]], " - re-leveled in R to start with 0")


    # 9) Format for export #### put data in order of participant ID for ease
    qv6_child_clean <- qv6_child_clean[order(qv6_child_clean$id), ]

    # make sure the variable labels match in the dataset
    qv6_child_clean = sjlabelled::set_label(qv6_child_clean, label = matrix(unlist(qv6_child_clean_labels,
        use.names = FALSE)))

    ## make list of data frame and associated labels
    qv6_child <- list(data = qv6_child_clean, dict = qv6_child_clean_labels)

    ## want an export options??

    return(qv6_child)
}
