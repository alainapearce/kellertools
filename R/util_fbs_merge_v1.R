#' util_fbs_merge_v1: Merge all Visit 1 raw Qualtrics databases and organize variable in database order
#'
#' This function merges the following visit 1 raw data into a single database and organizes variables in database order: child visit 1, child visit 1-home, child visit 1-lab, and parent visit 1
#'
#' The databases MUST follow the naming convention: Child_V1_YYYY-MM-DD.sav, Child_V1_Home_YYY-MM-DD.sav, Child_V1_Lab_YYY-MM-DD.sav, and Parent_V1_YYY-MM-DD.sav. The databases must all be in the SAME directory to be processed if the data_path is not entered and the directory organization does not follow the structure laid out in the DataManual.
#'
#' @param date_str If ALL databases have the same date used in the name of the .sav file, enter the date as a string (e.g., for file 'Parent_V1_2021-10-11.sav', the string '2021-10-11' would be entered)
#' @param child_date_str (optional) If the date string differs by file, enter the child full protocol date string
#' @param child_home_date_str (optional) If the date string differs by file, enter the child HOME protocol date string
#' @param child_lab_date_str (optional) If the date string differs by file, enter the child LAB protocol date string
#' @param parent_date_str (optional) If the date string differs by file, enter the parent date string
#' @inheritParams util_fbs_parent_v1dat
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from parent visit 1 Qualtrics; 2) dict: all variable descriptions; 3) pna_data: data.frame marking participants who 'preferred not to answer' (pna) specific questions; and 4) pna_dict: all variable descriptions for pna_data
#'
#' @examples
#' #if in same working directory as data with all data having the same date in filename:
#' v1dat_scored <- util_fbs_merge_v1(date_str = '2021-10-11')
#'
#' #if in same working directory as data and covid collecte data has different dates:
#' v1dat_scored <- util_fbs_merge_v1(child_date_str = '2021-10-11', child_home_date_str = '2021-9-15', child_lab_date_str = = '2021-9-15', parent_date_str = '2021-10-11')
#'
#' \dontrun{
#' #date must be a string. The following will not run:
#' v1dat_scored <- util_fbs_merge_v1(2021-10-11)
#'
#' #date must match the file name - for file named 'Child_V1_Home_2021_09_15', the
#' following will not run:
#' v1dat_scored <- util_fbs_merge_v1('2021_10_11')
#' }
#'
#' @seealso Raw data from Qualtrics is processed using the following scripts: \code{\link{util_fbs_child_v1dat}}, \code{\link{util_fbs_child_v1dat_home}}, \code{\link{util_fbs_child_v1dat_lab}}, \code{\link{util_fbs_parent_v1dat}}. Visit 1 data is scored using the following scripts: \code{\link{score_pds}}, \code{\link{score_paq}}, \code{\link{score_risk}}
#'
#'
#' @export

util_fbs_merge_v1 <- function(date_str, child_date_str, child_home_date_str, child_lab_date_str, parent_date_str, data_path) {

    #### 1. Set up/initial checks #####

    # check if date_str exists and is a string

    datestr_arg <- methods::hasArg(date_str)

    if (isTRUE(datestr_arg) & !is.character(date_str)) {
        stop("date_str must be entered as a string: e.g., '2021_10_11'")
    } else if (isFALSE(datestr_arg)) {

        # if no date_str, check all databases specific date strings
        child_datestr_arg <- methods::hasArg(child_date_str)
        child_home_datestr_arg <- methods::hasArg(child_home_date_str)
        child_lab_datestr_arg <- methods::hasArg(child_lab_date_str)
        parent_datestr_arg <- methods::hasArg(parent_date_str)

        if (sum(child_datestr_arg, child_home_datestr_arg, child_lab_datestr_arg, parent_datestr_arg) < 4){
            stop("if data_str is not set, then must enter each individual date string for the visit 1 databeses: child, child-home, child-lab, and parent")
        }

        if (!is.character(child_date_str) | !is.character(child_home_date_str) | !is.character(child_lab_date_str) | !is.character(parent_date_str)) {
            stop("all dates must be entered as a string: e.g., '2021_10_11'")
        }
    }

    # check datapath
    datapath_arg <- methods::hasArg(data_path)

    if (isTRUE(datapath_arg)) {
        if (!is.character(data_path)) {
            stop("data_path must be entered as a string: e.g., '.../Participant_Data/untouchedRaw/'")
        }
    }

    #### 2. Check Data Exists #####

    ## define function to test if file exists
    file_exist_fn <- function(data_path, date_str, respondant, covid_type = FALSE, loc){

        datapath_arg_fn <- methods::hasArg(data_path)

        #if not special case
        if (isFALSE(covid_type)) {
            if (isTRUE(datapath_arg_fn)) {
                qv1_path <- paste0(data_path, "/", respondant, "_V1_", date_str, ".sav")
            } else {
                qv1_path <- paste0("/", respondant, "_V1_", date_str, ".sav")
            }

            # check if file exists
            qv1_exists <- file.exists(qv1_path)

            #warning message if doesn't exist - stop is below outside of function
            if (isFALSE(qv1_exists)) {
                warning(paste0("The ", respondant, "_V1_", date_str, ".sav database does not exist at the specified path:", qv1_path))
            }

            #return check
            qv1_res <- list(data_path_full = qv1_path, data_exists = qv1_exists)

            return(qv1_res)
        }

        #if covid split protocol
        if (isTRUE(covid_type)) {

            if (isTRUE(datapath_arg_fn)) {
                qv1_path <- paste0(data_path, "/Final_CovidAtHome/", respondant, "_V1_", loc, "_", date_str, ".sav")
            } else {
                qv1_path <- paste0("/Final_CovidAtHome/", respondant, "_V1_", loc, "_", date_str, ".sav")
            }

            # check if file exists
            qv1_exists <- file.exists(qv1_path)

            if (isTRUE(qv1_exists)) {
                #return check
                qv1_res <- list(data_path_full = qv1_path, data_exists = qv1_exists)

                return(qv1_res)
            } else {

                #check if in the main database rather than 'Final_CovidAtHome' database
                if (isTRUE(datapath_arg_fn)) {
                    qv1_path2 <- paste0(data_path, "/", respondant, "_V1_", loc, "_", date_str, ".sav")
                } else {
                    qv1_path2 <- paste0("/", respondant, "_V1_", loc, "_", date_str, ".sav")
                }

                # check if file exists
                qv1_exists2 <- file.exists(qv1_path2)

                #warning message if doesn't exist - stop is below outside of function
                if (isFALSE(qv1_exists2)) {
                    warning(paste0("The ", respondant, "_V1_", loc, "_", date_str, ".sav database does not exist at either of the possible paths:", qv1_path, "or", qv1_path2))
                }

                #return check
                qv1_res <- list(data_path_full = qv1_path2, data_exists = qv1_exists2)

                return(qv1_res)
            }
        }
    }


    # check if files exist
    if (isTRUE(datestr_arg)){
        child_file <- file_exist_fn(data_path, date_str, respondant = 'Child')
        child_home_file <- file_exist_fn(data_path, date_str, respondant = 'Child', covid_type = TRUE, loc = 'Home')
        child_lab_file <- file_exist_fn(data_path, date_str, respondant = 'Child', covid_type = TRUE, loc = 'Lab')
        parent_file <- file_exist_fn(data_path, date_str, respondant = 'Parent')
    } else {
        child_file <- file_exist_fn(data_path, child_date_str, respondant = 'Child')
        child_home_file <- file_exist_fn(data_path, child_home_date_str, respondant = 'Child', covid_type = TRUE, loc = 'Home')
        child_lab_file <- file_exist_fn(data_path, child_lab_date_str, respondant = 'Child', covid_type = TRUE, loc = 'Lab')
        parent_file <- file_exist_fn(data_path, parent_date_str, respondant = 'Parent')
    }

    if (sum(child_file$data_exists, child_home_file$data_exists, child_lab_file$data_exists, parent_file$data_exists) < 4){
        stop('not all files exist - double check all files are in the correct directories and that the entered *date_str and data_path arguments are entered correctly')
    }

    #### 3. Process Raw Data #####

    if (isTRUE(datestr_arg)) {
        child_v1dat <- util_fbs_child_v1dat(date_str, data_path)
        child_home_v1dat <- util_fbs_child_v1dat_home(date_str, data_path)
        child_lab_v1dat <- util_fbs_child_v1dat_lab(date_str, data_path)
        parent_v1dat <- util_fbs_parent_v1dat(date_str, data_path)
    } else {
        child_v1dat <- util_fbs_child_v1dat(child_date_str, data_path)
        child_home_v1dat <- util_fbs_child_v1dat_home(child_home_date_str, data_path)
        child_lab_v1dat <- util_fbs_child_v1dat_lab(child_lab_date_str, data_path)
        parent_v1dat <- util_fbs_parent_v1dat(parent_date_str, data_path)
    }

    #### 4. Merge Child Raw Data #####

    # merge child home and lab into single database
    child_covidmerge_v1dat <- merge(child_lab_v1dat$data, child_home_v1dat$data[c(1, 7:95)], by = 'id', all = TRUE)

    # re-order so matches child_v1dat
    child_covidmerge_v1dat <- child_covidmerge_v1dat[c(1:160, 163:251, 161:162)]

    # add pss soup since it is missing from v1 home - need to have same columns to merge
    child_covidmerge_v1dat[['pss_soup_eat']] <- NA
    child_covidmerge_v1dat[['pss_soup_much']] <- NA
    child_covidmerge_v1dat[['pss_soup_like']] <- NA

    child_covidmerge_v1dat <- child_covidmerge_v1dat[c(1:243, 252:254, 244:251)]

    # merge all child into single database
    all_child_v1dat <- rbind.data.frame(child_v1dat$data, child_covidmerge_v1dat)


    #### 5. Merge Parent Raw Data #####

    # add 'p_' to pss data to mark parent
    parent_pss_vars <- names(parent_v1dat$data)[c(251:406)]

    for (v in 1:length(parent_pss_vars)) {
        var_name <- parent_pss_vars[v]

        parent_pss_vars[v] <- paste0('p_', var_name)
    }

    names(parent_v1dat$data)[c(251:406)] <- parent_pss_vars
    names(parent_v1dat$dict)[c(251:406)] <- parent_pss_vars

    # update labels with 'parent report'

    for (v in 1:ncol(parent_v1dat$data)) {
        var_name <- names(parent_v1dat$data)[v]

        # remove existing label
        if (grepl("parent-reported", parent_v1dat$dict[[var_name]], fixed = TRUE)) {
            parent_v1dat$dict[[var_name]] <- gsub("parent-reported", "", parent_v1dat$dict[[var_name]])
        }

        # add universal label
        parent_v1dat$dict[[var_name]] <- paste0('Parent Reported: ', parent_v1dat$dict[[var_name]])
    }

    v1dat <- merge(all_child_v1dat, parent_v1dat$data[c(1, 5:408)], by = 'id', all = TRUE)

    # merge labels/dictionary
    v1dat_labels <- c(child_v1dat$dict, parent_v1dat$dict[5:408])

    #### 6. Organize V1 data and score #####

    # order of vars: 1) Demographics, 2) Anthro (hw, DXA, sleep, PA), 3) Intake (FF, liking, intake, want), 4) feeding/food Q's, 5) cog/trait Q's, 6) Delay Discounting, 7) MRI related (CAMS, FF, snack info, image ratings), 8) Notes

    v1dat_org <- v1dat[c(1:2, 16, 360:361, 3:6, 258:279, 362:364, 386:420, 429:440, 7:15, 255:257, 365:385, 280:359, 17:160, 421:428, 441:500, 161:252, 501:656, 657:658, 253:254)]

    v1dat_labels <- v1dat_labels[c(1:2, 16, 360:361, 3:6, 258:279, 362:364, 386:420, 429:440, 7:15, 255:257, 365:385, 280:359, 17:160, 421:428, 441:500, 161:252, 501:656, 657:658, 253:254)]

    # ensure labels are up to date
    v1dat_org = sjlabelled::set_label(v1dat_org, label = matrix(unlist(v1dat_labels, use.names = FALSE)))

    ## score puberty data
    pds_scored <- score_pds(pds_data = v1dat_org[c(1, 6, 18:29)], respondent = 'parent', male = 0, female = 1, parID = 'id')

    # get labels from scored data and simplify
    pds_scored_labels <- sapply(pds_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(pds_scored_labels) <- names(pds_scored)

    # merge and organize
    v1dat_scored <- merge(v1dat_org, pds_scored[c(1, 3:4)], by = 'id', all = TRUE)
    v1dat_scored <- v1dat_scored[c(1:29, 659:660, 30:658)]

    v1dat_scored_labels <- c(v1dat_labels[1:29], pds_scored_labels[3:4], v1dat_labels[30:658])

    ## score PAQ data and parent-reported sleep per day of the week
    paq_scored <- score_paq(paq_data = v1dat_org[c(1, 115:194)], study = 'fbs', sleep = TRUE, parID = 'id')

    # get labels from scored data and simplify
    paq_scored_labels <- sapply(paq_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(paq_scored_labels) <- names(paq_scored)

    # merge and organize
    v1dat_scored <- merge(v1dat_scored, paq_scored[c(1, 32:54)], by = 'id', all = TRUE)
    v1dat_scored <- v1dat_scored[c(1:196, 661:683, 197:660)]

    v1dat_scored_labels <- c(v1dat_scored_labels[1:196], paq_scored_labels[32:54], v1dat_scored_labels[197:660])

    ## get risk status
    risk_scored <- score_risk(risk_data = v1dat_org[c(1, 4, 100, 113:114)], respondent = 'parent_respondent', parID = 'id')

    # set id = 7 to low risk - only has dad measured, no mom
    risk_scored[risk_scored[['id']] == 7, 'risk_cat'] <- 'Low Risk'

    # manually exclude for other reasons/based on older criteria
    risk_scored[risk_scored[['id']] == 12, 'risk_cat'] <- 'Neither'
    risk_scored[risk_scored[['id']] == 14, 'risk_cat'] <- 'Neither'
    risk_scored[risk_scored[['id']] == 44, 'risk_cat'] <- 'Neither'
    risk_scored[risk_scored[['id']] == 66, 'risk_cat'] <- 'Neither'
    risk_scored[risk_scored[['id']] == 88, 'risk_cat'] <- 'Neither'
    risk_scored[risk_scored[['id']] == 29, 'risk_cat'] <- 'Neither'

    # get labels from scored data and simplify
    risk_scored_labels <- sapply(risk_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(risk_scored_labels) <- names(risk_scored)

    # merge and organize
    v1dat_scored <- merge(v1dat_scored, risk_scored, by = 'id', all = TRUE)
    v1dat_scored <- v1dat_scored[c(1:5, 684:686, 6:683)]

    v1dat_scored_labels <- c(v1dat_scored_labels[1:5], risk_scored_labels[2:4], v1dat_scored_labels[6:683])

    #### 8. Add manual vars to scored data and updated labels ####
    # create manual re-code to fit lab exceptions
    v1dat_scored[['man_recode_risk']] <- v1dat_scored[['risk_cat']]

    v1dat_scored[v1dat_scored[['id']] == 54, 'man_recode_risk'] <- 'High Risk'
    v1dat_scored[v1dat_scored[['id']] == 112, 'man_recode_risk'] <- 'Low Risk'
    v1dat_scored[v1dat_scored[['id']] == 113, 'man_recode_risk'] <- 'Low Risk'

    v1dat_scored_labels[['man_recode_risk']] <- 'Manually re-coded child risk category to match lab assigment'

    ## weight status
    v1dat_scored[['weight_status']] <- ifelse(v1dat_scored[['bmi_percentile']] < 5, 'UW', ifelse(v1dat_scored[['bmi_percentile']] < 85, 'HW', ifelse(v1dat_scored[['bmi_percentile']] < 95, 'OW', 'OB')))
    v1dat_scored_labels[['weight_status']] <- 'Child weight status using CDC BMI percentile cutoffs'

    v1dat_scored[['dad_weight_status']] <- ifelse(is.na(v1dat_scored[['parent_respondent']]) | v1dat_scored[['parent_respondent']] == 2, NA, ifelse(v1dat_scored[['parent_respondent']] == 1, ifelse(v1dat_scored[['parent_bmi']] < 18.5, 'UW', ifelse(v1dat_scored[['parent_bmi']] < 25, 'HW', ifelse(v1dat_scored[['parent_bmi']] < 30, 'OW', ifelse(v1dat_scored[['parent_bmi']] < 35, 'C1-OB', ifelse(v1dat_scored[['parent_bmi']] < 40, 'C2-OB', 'C3-Severe OB'))))), ifelse(v1dat_scored[['sr_dad_bmi']] < 18.5, 'UW', ifelse(v1dat_scored[['sr_dad_bmi']] < 25, 'HW', ifelse(v1dat_scored[['sr_dad_bmi']] < 30, 'OW', ifelse(v1dat_scored[['sr_dad_bmi']] < 35, 'C1-OB', ifelse(v1dat_scored[['sr_dad_bmi']] < 40, 'C2-OB', 'C3-Severe OB')))))))
    v1dat_scored_labels[['dad_weight_status']] <- 'Dad weight status using CDC cutoffs'

    v1dat_scored[['mom_weight_status']] <- ifelse(is.na(v1dat_scored[['parent_respondent']]) | v1dat_scored[['parent_respondent']] == 2, NA, ifelse(v1dat_scored[['parent_respondent']] == 0, ifelse(v1dat_scored[['parent_bmi']] < 18.5, 'UW', ifelse(v1dat_scored[['parent_bmi']] < 25, 'HW', ifelse(v1dat_scored[['parent_bmi']] < 30, 'OW', ifelse(v1dat_scored[['parent_bmi']] < 35, 'C1-OB', ifelse(v1dat_scored[['parent_bmi']] < 40, 'C2-OB', 'C3-Severe OB'))))), ifelse(v1dat_scored[['sr_mom_bmi']] < 18.5, 'UW', ifelse(v1dat_scored[['sr_mom_bmi']] < 25, 'HW', ifelse(v1dat_scored[['sr_mom_bmi']] < 30, 'OW', ifelse(v1dat_scored[['sr_mom_bmi']] < 35, 'C1-OB', ifelse(v1dat_scored[['sr_mom_bmi']] < 40, 'C2-OB', 'C3-Severe OB')))))))
    v1dat_scored_labels[['mom_weight_status']] <- 'Mom weight status using CDC cutoffs'


    ## organize data
    v1dat_scored <- v1dat_scored[c(1:8, 687, 9:95, 688, 96:120, 689:690, 121:686)]
    v1dat_scored_labels <- v1dat_scored_labels[c(1:8, 687, 9:96, 688, 97:120, 689:690, 121:686)]

    #### 9. Food Intake ####

    ## 9a) EAH ####
    v1_eah_kcal <- fbs_kcal_intake(v1dat_scored[c(1, 303:342)], meal = 'EAH', parID = 'id')
    names(v1_eah_kcal)[11:12] <- c('eah_total_g', 'eah_total_kcal')

    # get labels from scored data and simplify
    v1_eah_labels <- sapply(v1_eah_kcal, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(v1_eah_labels) <- names(v1_eah_kcal)

    # merge and organize
    v1dat_scored <- merge(v1dat_scored, v1_eah_kcal, by = 'id', all = TRUE)
    v1dat_scored_labels <- c(v1dat_scored_labels, v1_eah_labels[2:12])

    # organize
    v1dat_scored <- v1dat_scored[c(1:306, 691, 307:310, 692, 311:314, 693, 315:318, 694, 319:322, 695, 323:326, 696, 327:330, 697, 331:334, 698, 335:338, 699, 339:342, 700:701, 343:690)]

    v1dat_scored_labels <- v1dat_scored_labels[c(1:306, 691, 307:310, 692, 311:314, 693, 315:318, 694, 319:322, 695, 323:326, 696, 327:330, 697, 331:334, 698, 335:338, 699, 339:342, 700:701, 343:690)]

    ## 9b) Standard Meal ####
    v1_meal_kcal <- fbs_kcal_intake(v1dat_scored[c(1, 255:302)], meal = 'std_meal', parID = 'id')
    names(v1_meal_kcal)[14:15] <- c('meal_total_g', 'meal_total_kcal')

    # get labels from scored data and simplify
    v1_meal_labels <- sapply(v1_meal_kcal, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(v1_meal_labels) <- names(v1_meal_kcal)

    # merge and organize
    v1dat_scored <- merge(v1dat_scored, v1_meal_kcal, by = 'id', all = TRUE)
    v1dat_scored_labels <- c(v1dat_scored_labels, v1_meal_labels[c(2:15)])

    # organize
    v1dat_scored <- v1dat_scored[c(1:258, 702, 259:262, 703, 263:266, 704, 267:270, 705, 271:274, 706, 275:278, 707, 279:282, 708, 283:286, 709, 287:290, 710, 291:294, 711, 295:298, 712, 299:302, 713:715, 303:701)]

    v1dat_scored_labels <- v1dat_scored_labels[c(1:258, 702, 259:262, 703, 263:266, 704, 267:270, 705, 271:274, 706, 275:278, 707, 279:282, 708, 283:286, 709, 287:290, 710, 291:294, 711, 295:298, 712, 299:302, 713:715, 303:701)]

    #### 10. PNA data #####

    # only parent has pna data to organize
    v1dat_pna <- parent_v1dat$pna_data

    #### 11. save to list #####

    # put data in order of participant ID for ease
    v1dat_scored <- v1dat_scored[order(v1dat_scored[["id"]]), ]
    v1dat_pna <- v1dat_pna[order(v1dat_pna[["id"]]), ]

    # set labels
    v1dat_scored = sjlabelled::set_label(v1dat_scored, label = matrix(unlist(v1dat_scored_labels, use.names = FALSE)))
    v1dat_pna = sjlabelled::set_label(v1dat_pna, label = matrix(unlist(parent_v1dat$pna_dict, use.names = FALSE)))

    v1data_list <- list(data = v1dat_scored, dict = v1dat_scored_labels, pna_dat = v1dat_pna, pna_dict = parent_v1dat$pna_dict)

    return(v1data_list)
}

