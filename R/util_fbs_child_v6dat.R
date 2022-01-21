#' util_fbs_child_v6dat: Process raw qualtrics visit 6 data for the child
#'
#' This function loads the .sav raw data file for the child visit 6 data that was collected via Qualtrics and cleans the data. Cleaning the data involves:
#' 1) extracting all variable descriptions,
#' 2) selecting relevant data columns,
#' 3) removing all practice events (e.g., 999)
#' 4) re-ordering and re-name data columns
#' 5) reformatting dates to be appropriate and computer readable: YYYY-MM-DD
#' 6) re-calculate manual variables
#' 7) fix 99's / create prefer not to answer database
#' 8) random fixes to factor level names and variable descriptions
#'
#' The databases MUST follow the naming convention: Child_V6_YYYY-MM-DD.sav
#'
#' @inheritParams util_fbs_parent_v1dat
#' @inheritParams util_fbs_parent_v1dat
#'
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from child visit 6 Qualtrics and 2) dict: all variable descriptions
#'
#' @examples
#' #if in same working directory as data:
#' child_v6_dat <- util_fbs_child_v6dat('Child_V6')
#'
#' \dontrun{
#' #file_pattern must be a string. The following will not run:
#' child_v6_dat <- util_fbs_child_v6dat(Child_V6)
#'
#' #file_pattern must have the respondent ('Child') and visit number ('V6'). If just enter 'Child', the script will not run because it will return multiple files for different parent visits. The following will not run:
#' child_v6_dat <- util_fbs_child_v6dat('Child')
#' }
#'
#'
#' @export
#'
util_fbs_child_v6dat <- function(file_pattern, data_path) {

    #### 1. Set up/initial checks #####

    # check that file_pattern exist and is a string

    filepat_arg <- methods::hasArg(file_pattern)

    if (isTRUE(filepat_arg) & !is.character(file_pattern)) {
        stop("file_pattern must be entered as a string: e.g., 'Child_V6'")
    } else if (isFALSE(filepat_arg)) {
        stop("file_pattern must set to the a string matching the name of the raw data file for child visit: e.g., 'Child_V6'")
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
        qv6_child_path <- list.files(path = data_path, pattern = file_pattern, full.names = TRUE)
    } else {
        qv6_child_path <- paste0(pattern = file_pattern, full.names = TRUE)
    }

    # check number of files found
    if (length(qv6_child_path) > 1) {
        stop("More than one file matched the file_pattern. Be sure thefile_pattern specifies both the respondent (Parent/Child) and visit number (V#). If have more than 1 file matching the pattern in the directory, may need to move to enter a more specific file_pattern than is standard.")
    } else if (length(qv6_child_path) == 0) {
        stop('No files found. Be sure the data_path and file_pattern are correct and that the file exists')
    }

    # check that file is of type '.sav'
    if (!grepl('.sav', qv6_child_path, fixed = TRUE)){
        stop("The file found is not an SPSS database (.sav)")
    }

    # check if file exists
    qv6_child_exists <- file.exists(qv6_child_path)

    # load data if it exists
    if (isTRUE(qv6_child_exists)) {
        qv6_child_dat <- as.data.frame(haven::read_spss(qv6_child_path))

    } else {
        if (isTRUE(datapath_arg)) {
            stop("File does not exist. Check file_pattern and data_path entered")
        } else {
            stop("File does not exist. Check file_pattern and that the data exists in current working directory")
        }
    }

    #### 3. Clean Data #####

    ##### NOTE: TROUBLESHOOT F2_FULL RATINGS. they are not currently making it into the clean database #####

    # 1) extract variable labels/descriptions ####
    qv6_child_labels <- lapply(qv6_child_dat, function(x) attributes(x)$label)

    # 2) selecting relevant data columns ####
    qv6_child_clean <- qv6_child_dat[c(1, 18, 29, 31:33, 35, 37, 39:45, 50, 56:207, 210:659, 661:687)]

    ## update labels
    qv6_child_clean_labels <- qv6_child_labels[c(1, 18, 29, 31:33, 35, 37, 39:45, 50, 56:207, 210:659, 661:687)]

    # 3) removing all practice events (e.g., 999) ####
    qv6_child_clean <- qv6_child_clean[!is.na(qv6_child_clean$ID) & qv6_child_clean$ID <
        999, ]

    # 4) re-ordering and re-name data columns ####
    qv6_child_clean <- qv6_child_clean[c(2, 1, 3:5, 619:621, 6:7, 622, 8:9, 623, 10:17, 624, 18:618, 625:645)]

    qv6_child_clean_labels <- qv6_child_clean_labels[c(2, 1, 3:5, 619:621, 6:7, 622, 8:9, 623, 10:17, 624, 18:618, 625:645)]

    # remove V6
    for (v in 1:length(names(qv6_child_clean))) {
        var_name <- names(qv6_child_clean)[v]

        # remove 'v6' from names
        if (grepl("V6", var_name, fixed = TRUE)) {
            names(qv6_child_clean)[v] <- gsub("V6", "", var_name)
        }
    }

    ## make lower case
    names(qv6_child_clean) <- tolower(names(qv6_child_clean))

    names(qv6_child_clean_labels) <- names(qv6_child_clean)

    ### remove rows that have NAs for every question
    mri_vas_a <- qv6_child_clean[c(1, 25:324)]
    mri_vas_b <- qv6_child_clean[c(1, 325:624)]

    #make names match for mri vas version
    for (v in 1:length(names(mri_vas_a))) {
        var_name <- names(mri_vas_a)[v]

        # remove trailing '_a_1.0' from names
        if (grepl("_a_1.0", var_name, fixed = TRUE)) {
            names(mri_vas_a)[v] <- gsub("like_a_1.0", "full", var_name, fixed = TRUE)
        } else if (grepl("_a_1", var_name, fixed = TRUE)) {
            names(mri_vas_a)[v] <- gsub("_a_1", "", var_name)
        }

    }

    for (v in 1:length(names(mri_vas_b))) {
        var_name <- names(mri_vas_b)[v]

        # remove trailing '_a_1.0' from names
        if (grepl("_b_1.0", var_name, fixed = TRUE)) {
            names(mri_vas_b)[v] <- gsub("like_b_1.0", "full", var_name, fixed = TRUE)
        } else if (grepl("_b_1", var_name, fixed = TRUE)) {
            names(mri_vas_b)[v] <- gsub("_b_1", "", var_name)
        }

    }

    #remove NAs
    mri_vas_a <- mri_vas_a[rowSums(!is.na(mri_vas_a[2:301])) == 300, ]
    mri_vas_b <- mri_vas_b[rowSums(!is.na(mri_vas_b[2:301])) == 300, ]

    ## Combine MRI VAS ratings from version A and B
    mri_vas <- rbind(mri_vas_a[ , order(names(mri_vas_a))], mri_vas_b[ , order(names(mri_vas_b))])

    # mri vas labels
    mri_vas_labels <- qv6_child_clean_labels[c(1, 25:324)]
    mri_vas_labels <- mri_vas_labels[order(names(mri_vas_a))]
    names(mri_vas_labels) <- names(mri_vas)

    ## merge
    qv6_child_clean <- merge(qv6_child_clean[c(1:24, 625:645)], mri_vas, by = 'id', all = TRUE)

    #organize
    qv6_child_clean <- qv6_child_clean[c(1:24, 46:345, 25:45)]

    qv6_child_clean_labels <- c(qv6_child_clean_labels[1:24], mri_vas_labels[2:301], qv6_child_clean_labels[625:645])

    names(qv6_child_clean_labels) <- names(qv6_child_clean)

    # 4) re-ordering and re-name data columns ####

    # general order: 1) child information (ID. date), 2) freddies, 3) intake (snack), 4) MRIVAS 5) notes

    qv6_child_clean <- qv6_child_clean[c(1:8, 10:11, 13:14, 22:23, 325:336, 9, 12, 24:324, 15:21, 337:344)]

    qv6_child_clean_labels <- qv6_child_clean_labels[c(1:8, 10:11, 13:14, 22:23, 325:336, 9, 12, 24:324, 15:21, 337:344)]

    ## re-name variables

    names(qv6_child_clean)[c(2:29)] <- c("start_date", "ff_premri_snacktime", "ff_postmri_snacktime", "ff_postmris_nacktime2", "ff_premri_snack", "ff_postmri_snack", "ff_postmri_snack2",  "ff_premri_time", "ff_premri", "ff_pre_dgtime", "ff_pre_dg", "ff_pre_mrivas_time", "ff_pre_mrivas", "ff_pre_sst", "noplate_grapes_g", "plate_grapes_g", "post_grapes_g", "consumed_grapes_g", "noplate_ritz_g", "plate_ritz_g", "post_ritz_g", "consumed_ritz_g", "noplate_juice_g", "post_juice_g", "consumed_juice_g", "cams_pre_mri", "cams_post_mri", "mri_version")

    names(qv6_child_clean)[c(330:336)] <- c( "dg_foodchoice", "dg_foodchoice2", "dg_foodchoice_amount", "dg_foodchoice_amount2", "dg_foodchoice2_amount", "dg_foodchoice2_amount2", "dg_wait")

    names(qv6_child_clean)[337:344] <- c("notes_mri_mprage", "notes_mri_restingstate", "notes_mri_run1", "notes_mri_run2", "notes_mri_run3", "notes_mri_run4", "notes_mri_run5", "childnotes")

    ## update data labels
    names(qv6_child_clean_labels) <- names(qv6_child_clean)

    # 5) reformatting dates to be appropriate and computer readable YYYY-MM-DD  ####
    #format start date
    qv6_child_clean[["start_date"]] <- lubridate::ymd(as.Date(qv6_child_clean[["start_date"]]))

    # dates are fomrated as dd-mstr-yy
    visit_dates[['RO1_V6_Date']] <- lubridate::ymd(as.Date(visit_dates[['RO1_V6_Date']], format = "%d-%b-%y"))

    # add validated dates
    names(visit_dates)[1] <- 'id'
    qv6_child_clean <- merge(qv6_child_clean, visit_dates[c('id', 'RO1_V6_Date')], by = 'id', all.x = TRUE, all.y = FALSE)

    #update start_date
    qv6_child_clean[["start_date"]] <- ifelse(!is.na(qv6_child_clean[['RO1_V6_Date']]), as.character(qv6_child_clean[['RO1_V6_Date']]), as.character(qv6_child_clean[["start_date"]]))

    #remove RO1_V date column
    qv6_child_clean <- qv6_child_clean[, names(qv6_child_clean) != "RO1_V6_Date"]

    # add label
    qv6_child_clean_labels[["start_date"]] <- "date from participant contacts databases ('verified_visit_dates*.csv) converted to format yyyy-mm-dd in R. If no date in database, uses start_date metadata from qualtrics"

    # 5a) re-label DG wait variable ####
    qv6_child_clean[["dg_wait"]] <- ifelse(is.na(qv6_child_clean[["dg_wait"]]) | qv6_child_clean[["dg_wait"]] == 99, NA, ifelse(qv6_child_clean[["dg_wait"]] == 1, 0, ifelse(qv6_child_clean[["dg_wait"]] == 2, 1, ifelse(qv6_child_clean[["dg_wait"]] == 3, 2, ifelse(qv6_child_clean[["dg_wait"]] == 88, 3, NA)))))

    qv6_child_clean[["dg_wait"]] <- sjlabelled::add_labels(qv6_child_clean[["dg_wait"]], labels = c(`Successfully Waited` = 0, `Rang Bell, Did NOT Eat` = 1, `Ate Without Ringing Bell` = 2, `Ate, did NOT understand task` = 3))
    class(qv6_child_clean[["dg_wait"]]) <- c("haven_labelled", "vctrs_vctr", "double")

    # 6) re-calculate manual variables ####

    ## re-calculate all intake values

    # get all intake variables
    intake_vars <- names(qv6_child_clean)[c(16:26)]

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
            qv6_child_clean[[consumed_var]] <- qv6_child_clean[[plate_var]] - qv6_child_clean[[post_var]]

            qv6_child_clean[[consumed_var]] <- ifelse(qv6_child_clean[[consumed_var]] < 0, 0, qv6_child_clean[[consumed_var]])

            # update labels
            qv6_child_clean_labels[[consumed_var]] <- paste0(qv6_child_clean_labels[[consumed_var]], " - recalcuated difference in R with values < 0 set to 0")

        } else {
            # get variable names for noplate* and post* weights
            noplate_var <- paste0("noplate_", food_strs[f], "_g")
            post_var <- paste0("post_", food_strs[f], "_g")
            consumed_var <- paste0("consumed_", food_strs[f], "_g")

            # calculate amount consumed
            qv6_child_clean[[consumed_var]] <- qv6_child_clean[[noplate_var]] - qv6_child_clean[[post_var]]

            qv6_child_clean[[consumed_var]] <- ifelse(qv6_child_clean[[consumed_var]] < 0, 0, qv6_child_clean[[consumed_var]])

            # update labels
            qv6_child_clean_labels[[consumed_var]] <- paste0(qv6_child_clean_labels[[consumed_var]], " - recalcuated difference in R with values < 0 set to 0")
        }
    }

    # 7) random fixes to factor level names and variable descriptions
    qv6_child_clean_labels[["id"]] <- "participant id"
    qv6_child_clean_labels[["cams_post_mri"]] <- "Post-scan CAMS"
    qv6_child_clean_labels[["notes_mri_mprage"]] <- "notes about MRI: mprage scan"
    qv6_child_clean_labels[["notes_mri_restingstate"]] <- "notes about MRI: resting state scan"
    qv6_child_clean_labels[["notes_mri_run1"]] <- "notes about MRI: food cue task - run1"
    qv6_child_clean_labels[["notes_mri_run2"]] <- "notes about MRI: food cue task - run2"
    qv6_child_clean_labels[["notes_mri_run3"]] <- "notes about MRI: food cue task - run3"
    qv6_child_clean_labels[["notes_mri_run4"]] <- "notes about MRI: food cue task - run4"
    qv6_child_clean_labels[["notes_mri_run5"]] <- "notes about MRI: food cue task - run5"


    ## fix preMRI cams to be continuous and re-set to start at 0 (subtract 1) to match post cams
    qv6_child_clean[['cams_pre_mri']] <- as.numeric(qv6_child_clean[['cams_pre_mri']]) - 1
    qv6_child_clean[['cams_post_mri']] <- as.numeric(qv6_child_clean[['cams_post_mri']])

    # make freddies continuous
    ff_vars <- names(qv6_child_clean)[c(6:8, 10, 12, 14:15)]

    for (v in 1:length(ff_vars)){
        var_name <- ff_vars[v]

        qv6_child_clean[[var_name]] <- as.numeric(qv6_child_clean[[var_name]])
    }

    # 8) Format for export ####

    #put data in order of participant ID for ease
    qv6_child_clean <- qv6_child_clean[order(qv6_child_clean[["id"]]), ]

    # make sure the variable labels match in the dataset
    qv6_child_clean = sjlabelled::set_label(qv6_child_clean, label = matrix(unlist(qv6_child_clean_labels, use.names = FALSE)))

    ## make list of data frame and associated labels
    qv6_child <- list(data = qv6_child_clean, dict = qv6_child_clean_labels)

    ## want an export options??

    return(qv6_child)
}
