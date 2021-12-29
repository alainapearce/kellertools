#' util_fbs_merge_v7: Merge all Visit 7 raw Qualtrics databases and organize variable in database order
#'
#' This function merges the following visit 7 raw data into a single database and organizes variables in database order: child visit 7, child visit 7-home, child visit 7-lab, and parent visit 7
#'
#' The databases MUST follow the naming convention: Child_V7_YYYY-MM-DD.sav, Child_V7_Home_YYY-MM-DD.sav, Child_V7_Lab_YYY-MM-DD.sav, and Parent_V7_YYY-MM-DD.sav. The databases must all be in the SAME directory to be processed if the data_path is not entered and the directory organization does not follow the structure laid out in the DataManual.
#'
#' @inheritParams util_fbs_merge_v1
#' @inheritParams util_fbs_merge_v1
#' @inheritParams util_fbs_merge_v1
#' @inheritParams util_fbs_merge_v1
#' @inheritParams util_fbs_merge_v1
#' @param parent_home_date_str (optional) If the date string differs by file, enter the parent HOME protocol date string
#' @inheritParams util_fbs_parent_v1dat
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from parent visit 7 Qualtrics; 2) dict: all variable descriptions; 3) pna_data: data.frame marking participants who 'preferred not to answer' (pna) specific questions; and 4) pna_dict: all variable descriptions for pna_data
#'
#' @examples
#' #if in same working directory as data:
#' v7dat_scored <- util_fbs_merge_v7(date_str = '2021-10-11')
#'
#' #if in same working directory as data and covid collecte data has different dates:
#' v7dat_scored <- util_fbs_merge_v7(child_date_str = '2021-10-11', child_home_date_str = '2021-9-15', child_lab_date_str = = '2021-9-15', parent_date_str = '2021-10-11')
#'
#' \dontrun{
#' #date must be a string. The following will not run:
#' v7dat_scored <- util_fbs_merge_v7(2021-10-11)
#'
#' #date must match the file name - for file named 'Child_V7_Home_2021_09_15', the
#' following will not run:
#' v7dat_scored <- util_fbs_merge_v7('2021_10_11')
#' }
#'
#' @seealso Raw data from Qualtrics is processed using the following scripts: \code{\link{util_fbs_child_v7dat}}, \code{\link{util_fbs_child_v7dat_home}}, \code{\link{util_fbs_child_v7dat_lab}}, \code{\link{util_fbs_parent_v7dat}}. Visit 7 data is scored using the following scripts: \code{\link{score_pds}}, \code{\link{score_ctc}}, \code{\link{score_audit}}, \code{\link{score_cshqa}}, \code{\link{score_paq}}, \code{\link{score_cwc}}, \code{\link{score_cebq}}, \code{\link{score_cfq}}, \code{\link{score_brief2}}
#'
#'
#' @export

util_fbs_merge_v7 <- function(date_str, child_date_str, child_home_date_str, child_lab_date_str, parent_date_str, parent_home_date_str, data_path) {

    #### 1. Set up/initial checks #####

    # check if date_strs exist and is a string

    datestr_arg <- methods::hasArg(date_str)

    if (isTRUE(datestr_arg) & !is.character(date_str)) {
        stop("date_str must be entered as a string: e.g., '2021_10_11'")
    } else if (isFALSE(datestr_arg)) {

        # if no date_str, check all databases specific date strings
        child_datestr_arg <- methods::hasArg(child_date_str)
        child_home_datestr_arg <- methods::hasArg(child_home_date_str)
        child_lab_datestr_arg <- methods::hasArg(child_lab_date_str)
        parent_datestr_arg <- methods::hasArg(parent_date_str)
        parent_home_datestr_arg <- methods::hasArg(parent_home_date_str)

        if (sum(child_datestr_arg, child_home_datestr_arg, child_lab_datestr_arg, parent_datestr_arg, parent_home_datestr_arg) < 5){
            stop("if data_str is not set, then must enter each individual date string for the visit 7 databeses: child, child-home, child-lab, and parent")
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
                qv7_path <- paste0(data_path, "/", respondant, "_V7_", date_str, ".sav")
            } else {
                qv7_path <- paste0("/", respondant, "_V7_", date_str, ".sav")
            }

            # check if file exists
            qv7_exists <- file.exists(qv7_path)

            #warning message if doesn't exist - stop is below outside of function
            if (isFALSE(qv7_exists)) {
                warning(paste0("The ", respondant, "_V7_", date_str, ".sav database does not exist at the specified path:", qv7_path))
            }

            #return check
            qv7_res <- list(data_path_full = qv7_path, data_exists = qv7_exists)

            return(qv7_res)
        }

        #if covid split protocol
        if (isTRUE(covid_type)) {

            if (isTRUE(datapath_arg_fn)) {
                qv7_path <- paste0(data_path, "/Final_CovidAtHome/", respondant, "_V7_", loc, "_", date_str, ".sav")
            } else {
                qv7_path <- paste0("/Final_CovidAtHome/", respondant, "_V7_", loc, "_", date_str, ".sav")
            }

            # check if file exists
            qv7_exists <- file.exists(qv7_path)

            if (isTRUE(qv7_exists)) {
                #return check
                qv7_res <- list(data_path_full = qv7_path, data_exists = qv7_exists)

                return(qv7_res)
            } else {

                #check if in the main database rather than 'Final_CovidAtHome' database
                if (isTRUE(datapath_arg_fn)) {
                    qv7_path2 <- paste0(data_path, "/", respondant, "_V7_", loc, "_", date_str, ".sav")
                } else {
                    qv7_path2 <- paste0("/", respondant, "_V7_", loc, "_", date_str, ".sav")
                }

                # check if file exists
                qv7_exists2 <- file.exists(qv7_path2)

                #warning message if doesn't exist - stop is below outside of function
                if (isFALSE(qv7_exists2)) {
                    warning(paste0("The ", respondant, "_V7_", loc, "_", date_str, ".sav database does not exist at either of the possible paths:", qv7_path, "or", qv7_path2))
                }

                #return check
                qv7_res <- list(data_path_full = qv7_path2, data_exists = qv7_exists2)

                return(qv7_res)
            }
        }
    }

    # check if files exist
    if (isTRUE(datestr_arg)){
        child_file <- file_exist_fn(data_path, date_str, respondant = 'Child')
        child_home_file <- file_exist_fn(data_path, date_str, respondant = 'Child', covid_type = TRUE, loc = 'Home')
        child_lab_file <- file_exist_fn(data_path, date_str, respondant = 'Child', covid_type = TRUE, loc = 'Lab')
        parent_file <- file_exist_fn(data_path, date_str, respondant = 'Parent')
        parent_home_file <- file_exist_fn(data_path, date_str, respondant = 'Parent', covid_type = TRUE, loc = 'Home')
    } else {
        child_file <- file_exist_fn(data_path, child_date_str, respondant = 'Child')
        child_home_file <- file_exist_fn(data_path, child_home_date_str, respondant = 'Child', covid_type = TRUE, loc = 'Home')
        child_lab_file <- file_exist_fn(data_path, child_lab_date_str, respondant = 'Child', covid_type = TRUE, loc = 'Lab')
        parent_file <- file_exist_fn(data_path, parent_date_str, respondant = 'Parent')
        parent_home_file <- file_exist_fn(data_path, parent_home_date_str, respondant = 'Parent', covid_type = TRUE, loc = 'Home')
    }

    if (sum(child_file$data_exists, child_home_file$data_exists, child_lab_file$data_exists, parent_file$data_exists, parent_home_file$data_exists) < 5){
        stop('not all files exist - double check all files are in the correct directories and that the entered *date_str and data_path arguments are entered correctly')
    }


    #### 3. Process Raw Data #####

    if (isTRUE(datestr_arg)) {
        child_v7dat <- util_fbs_child_v7dat(date_str, data_path)
        child_home_v7dat <- util_fbs_child_v7dat_home(date_str, data_path)
        child_lab_v7dat <- util_fbs_child_v7dat_lab(date_str, data_path)
        parent_v7dat <- util_fbs_parent_v7dat(date_str, data_path)
        parent_home_v7dat <- util_fbs_parent_v7dat_home(date_str, data_path)
    } else {
        child_v7dat <- util_fbs_child_v7dat(child_date_str, data_path)
        child_home_v7dat <- util_fbs_child_v7dat_home(child_home_date_str, data_path)
        child_lab_v7dat <- util_fbs_child_v7dat_lab(child_lab_date_str, data_path)
        parent_v7dat <- util_fbs_parent_v7dat(parent_date_str, data_path)
        parent_home_v7dat <- util_fbs_parent_v7dat(parent_home_date_str, data_path)
    }

    #### 4. Merge Child Raw Data #####

    # merge child home and lab into single database
    child_covidmerge_v7dat <- merge(child_lab_v7dat$data, child_home_v7dat$data[c(1, 5:39)], by = 'id', all = TRUE)

    # re-order so matches child_v7dat
    child_covidmerge_v7dat <- child_covidmerge_v7dat[c(1:15, 190:203, 16:186, 204:224, 187:189)]

    # merge all child into single database

    #weird issue - couldn't rbind due to some `labels` to logical issue - brute force fix
    set_attr_loc4 <- attributes(child_covidmerge_v7dat[['loc4']])
    set_attr_loc14 <- attributes(child_covidmerge_v7dat[['loc14']])

    child_v7dat$data[['loc4']] <- as.numeric(child_v7dat$data[['loc4']])
    child_covidmerge_v7dat[['loc4']] <- as.numeric(child_covidmerge_v7dat[['loc4']])

    child_v7dat$data[['loc14']] <- as.numeric(child_v7dat$data[['loc14']])
    child_covidmerge_v7dat[['loc14']] <- as.numeric(child_covidmerge_v7dat[['loc14']])

    all_child_v7dat <- rbind.data.frame(child_v7dat$data, child_covidmerge_v7dat)

    attributes(all_child_v7dat[['loc4']]) <- set_attr_loc4
    attributes(all_child_v7dat[['loc14']]) <- set_attr_loc14

    #### 5. Merge Parent Raw Data #####

    # update labels with 'parent report'

    for (v in 1:ncol(parent_v7dat$data)) {
        var_name <- names(parent_v7dat$data)[v]

        # remove existing label
        if (grepl("parent-reported", parent_v7dat$dict[[var_name]], fixed = TRUE)) {
            parent_v7dat$dict[[var_name]] <- gsub("parent-reported", "", parent_v7dat$dict[[var_name]])
        }

        # add universal label
        parent_v7dat$dict[[var_name]] <- paste0('Parent Reported: ', parent_v7dat$dict[[var_name]])
    }

    for (v in 1:ncol(parent_home_v7dat$data)) {
        var_name <- names(parent_home_v7dat$data)[v]

        # remove existing label
        if (grepl("parent-reported", parent_home_v7dat$dict[[var_name]], fixed = TRUE)) {
            parent_home_v7dat$dict[[var_name]] <- gsub("parent-reported", "", parent_home_v7dat$dict[[var_name]])
        }

        # add universal label
        parent_home_v7dat$dict[[var_name]] <- paste0('Parent Reported: ', parent_home_v7dat$dict[[var_name]])
    }

    ##need to had measured parent h/w to home data to match standard columns
    parent_home_v7dat$data['parent_height1'] <- NA
    parent_home_v7dat$data['parent_height2'] <- NA
    parent_home_v7dat$data['parent_weight1'] <- NA
    parent_home_v7dat$data['parent_weight2'] <- NA
    parent_home_v7dat$data['parent_height_avg'] <- NA
    parent_home_v7dat$data['parent_weight_avg'] <- NA
    parent_home_v7dat$data['parent_bmi'] <- NA

    parent_home_v7dat$data <- parent_home_v7dat$data[c(1:119, 543:549, 120:542)]

    #merge parent data
    #brute force to get same data type for pss_milk_freq
    parent_home_v7dat$data[[356]] <- as.numeric(parent_home_v7dat$data[[356]])
    parent_v7dat$data[[356]] <- as.numeric(parent_v7dat$data[[356]])

    v7dat_parent <- rbind.data.frame(parent_v7dat$data, parent_home_v7dat$data)

    #re-name pds data for parents
    for (var in 6:19){
        p_var_name <- paste0('p_', names(v7dat_parent)[var])
        names(v7dat_parent)[var] <- p_var_name
    }

    #re-name pss data for parents
    for (var in 266:408){
        p_var_name <- paste0('p_', names(v7dat_parent)[var])
        names(v7dat_parent)[var] <- p_var_name
    }

    names(parent_v7dat$dict) <- names(v7dat_parent)

    #merge parent and child
    v7dat <- merge(all_child_v7dat, v7dat_parent[c(1, 5:549)], by = 'id', all = TRUE)

    # merge labels/dictionary
    v7dat_labels <- c(child_v7dat$dict, parent_v7dat$dict[5:549])

    #### 6. Organize V7 data ####

    # order of vars: 1) Demographics, 2) Anthro (hw, DXA, sleep, PA), 3) Intake (FF, liking, intake, want), 4) feeding/food Q's, 5) cog/trait Q's, 6) Delay Discounting, 7) MRI related (CAMS, FF, snack info, image ratings), 8) Notes

    v7dat_org <- v7dat[c(1:6, 225, 338:339, 16:29, 226:239, 361:395, 201:216, 396:405, 414:425, 7:15, 320:337, 240:319, 217:221, 340:360, 30:177, 406:413, 629:693, 426:628, 178:200, 694:756, 222:224, 757:769)]

    v7dat_labels <- v7dat_labels[c(1:6, 225, 338:339, 16:29, 226:239, 361:395, 201:216, 396:405, 414:425, 7:15, 320:337, 240:319, 217:221, 340:360, 30:177, 406:413, 629:693, 426:628, 178:200, 694:756, 222:224, 757:769)]

    # ensure labels are up to date
    v7dat_org = sjlabelled::set_label(v7dat_org, label = matrix(unlist(v7dat_labels, use.names = FALSE)))

    #### 7. Organize V7 data ####

    ## 7a) score the Self-Report Pubertal Development Scale ####
    ## need to make
    pds_scored <- score_pds(pds_data = v7dat_org[c(1, 3, 10:21)], respondent = 'child', parID = 'id')

    # get labels from scored data and simplify
    pds_scored_labels <- sapply(pds_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(pds_scored_labels) <- names(pds_scored)

    # merge and organize
    v7dat_scored <- merge(v7dat_org, pds_scored[c(1, 3:4)], by = 'id', all = TRUE)
    v7dat_scored <- v7dat_scored[c(1:21, 770:771, 22:769)]

    v7dat_scored_labels <- c(v7dat_labels[1:21], pds_scored_labels[3:4], v7dat_labels[22:769])

    ## 7b) score the Parent-Report Pubertal Development Scale ####
    ## need to make
    parent_pds_data <- v7dat_org[c(1, 3, 24:35)]
    names(parent_pds_data)[3:14] <- c("pds_1", "pds_2", "pds_3", "pds_4m", "pds_5m", "pds_6m", "pds_4f", "pds_5fa", "pds_5fb", "pds_5fc", "pds_5fd", "pds_6f")

    parent_pds_scored <- score_pds(pds_data = parent_pds_data, respondent = 'parent', parID = 'id')
    names(parent_pds_scored)[3:4] <- c('p_pds_score', 'p_pds_tanner_cat')

    # get labels from scored data and simplify
    parent_pds_scored_labels <- sapply(parent_pds_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(parent_pds_scored_labels) <- names(parent_pds_scored)

    # merge and organize
    v7dat_scored <- merge(v7dat_scored, parent_pds_scored[c(1, 3:4)], by = 'id', all = TRUE)
    v7dat_scored <- v7dat_scored[c(1:37, 772:773, 38:771)]

    v7dat_scored_labels <- c(v7dat_scored_labels[1:37], parent_pds_scored_labels[3:4], v7dat_scored_labels[38:771])

    ## 7c) score the Communities that Care survey ####
    ctc_scored <- score_ctc(ctc_data = v7dat_org[c(1, 7, 73:88)], study = 'fbs', parID = 'id')

    # get labels from scored data and simplify
    ctc_scored_labels <- sapply(ctc_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(ctc_scored_labels) <- names(ctc_scored)

    # merge and organize
    v7dat_scored <- merge(v7dat_scored, ctc_scored, by = 'id', all = TRUE)
    v7dat_scored <- v7dat_scored[c(1:92, 774:777, 93:773)]

    v7dat_scored_labels <- c(v7dat_scored_labels[1:92], ctc_scored_labels[2:5], v7dat_scored_labels[93:773])

    ## 7d) score the Alcohol Use Disorders Identification Test ####
    audit_scored <- score_audit(audit_data = v7dat_org[c(1, 89:98)], parID = 'id')

    # get labels from scored data and simplify
    audit_scored_labels <- sapply(audit_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(audit_scored_labels) <- names(audit_scored)

    # merge and organize
    v7dat_scored <- merge(v7dat_scored, audit_scored, by = 'id', all = TRUE)
    v7dat_scored <- v7dat_scored[c(1:106, 778:779, 107:777)]

    v7dat_scored_labels <- c(v7dat_scored_labels[1:106], audit_scored_labels[2:3], v7dat_scored_labels[107:777])

    ## 7e) score the Child Sleep Habits Questionnaire - Abbreviated ####
    cshqa_scored <- score_cshqa(cshqa_data = v7dat_org[c(1, 120:137)], study = 'fbs', parID = 'id')

    # get labels from scored data and simplify
    cshqa_scored_labels <- sapply(cshqa_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(cshqa_scored_labels) <- names(cshqa_scored)

    # merge and organize
    v7dat_scored <- merge(v7dat_scored, cshqa_scored, by = 'id', all = TRUE)
    v7dat_scored <- v7dat_scored[c(1:147, 780:787, 148:779)]

    v7dat_scored_labels <- c(v7dat_scored_labels[1:147], cshqa_scored_labels[2:9], v7dat_scored_labels[148:779])

    ## 7f) score the Physical Activity Questionnaire ####
    paq_scored <- score_paq(paq_data = v7dat_org[c(1, 138:217)], study = 'fbs', sleep = TRUE, parID = 'id')

    # get labels from scored data and simplify
    paq_scored_labels <- sapply(paq_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(paq_scored_labels) <- names(paq_scored)

    # merge and organize
    v7dat_scored <- merge(v7dat_scored, paq_scored[c(1, 32:54)], by = 'id', all = TRUE)
    v7dat_scored <- v7dat_scored[c(1:235, 788:810, 236:787)]

    v7dat_scored_labels <- c(v7dat_scored_labels[1:235], paq_scored_labels[32:54], v7dat_scored_labels[236:787])

    ## 7g) score the Child Weight Concerns ####
    cwc_scored <- score_cwc(cwc_data = v7dat_org[c(1, 218:222)], parID = 'id')

    # get labels from scored data and simplify
    cwc_scored_labels <- sapply(cwc_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(cwc_scored_labels) <- names(cwc_scored)

    # merge and organize
    v7dat_scored <- merge(v7dat_scored, cwc_scored, by = 'id', all = TRUE)
    v7dat_scored <- v7dat_scored[c(1:263, 811, 264:810)]

    v7dat_scored_labels <- c(v7dat_scored_labels[1:263], cwc_scored_labels[2], v7dat_scored_labels[264:810])

    ## 7h) score the Children's Eating Behavior Questionnaire ####
    cebq_scored <- score_cebq(cebq_data = v7dat_org[c(1, 400:434)], parID = 'id')

    # get labels from scored data and simplify
    cebq_scored_labels <- sapply(cebq_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(cebq_scored_labels) <- names(cebq_scored)

    # merge and organize
    v7dat_scored <- merge(v7dat_scored, cebq_scored, by = 'id', all = TRUE)
    v7dat_scored <- v7dat_scored[c(1:476, 812:821, 477:811)]

    v7dat_scored_labels <- c(v7dat_scored_labels[1:476], cebq_scored_labels[2:11], v7dat_scored_labels[477:811])

    ## 7i) score the Child Feeding Questionnaire ####
    cfq_scored <- score_cfq(cfq_data = v7dat_org[c(1, 435:464)], parID = 'id')

    # get labels from scored data and simplify
    cfq_scored_labels <- sapply(cfq_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(cfq_scored_labels) <- names(cfq_scored)

    # merge and organize
    v7dat_scored <- merge(v7dat_scored, cfq_scored, by = 'id', all = TRUE)
    v7dat_scored <- v7dat_scored[c(1:516, 822:828, 517:821)]

    v7dat_scored_labels <- c(v7dat_scored_labels[1:516], cfq_scored_labels[2:8], v7dat_scored_labels[517:821])

    ## 7j) score the Behavioral Rating Inventory of Executive Function-2 ####
    brief_scored <- score_brief2(brief_data = v7dat_org[c(1, 3, 5, 691:753)], age_var = 'age_yr', sex_var = 'sex', parID = 'id')

    # get labels from scored data and simplify
    brief_scored_labels <- sapply(brief_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(brief_scored_labels) <- names(brief_scored)

    # merge and organize
    v7dat_scored <- merge(v7dat_scored, brief_scored, by = 'id', all = TRUE)
    v7dat_scored <- v7dat_scored[c(1:812, 829:876, 813:828)]

    v7dat_scored_labels <- c(v7dat_scored_labels[1:812], brief_scored_labels[2:49], v7dat_scored_labels[813:828])

    #### 9. Food Intake ####

    ## 9a) EAH ####
    v7_eah_kcal <- fbs_kcal_intake(v7dat_scored[c(1, 362:401)], meal = 'EAH', parID = 'id')
    names(v7_eah_kcal)[11:12] <- c('eah_total_g', 'eah_total_kcal')

    # get labels from scored data and simplify
    v7_eah_labels <- sapply(v7_eah_kcal, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(v7_eah_labels) <- names(v7_eah_kcal)

    # merge and organize
    v7dat_scored <- merge(v7dat_scored, v7_eah_kcal, by = 'id', all = TRUE)
    v7dat_scored_labels <- c(v7dat_scored_labels, v7_eah_labels[2:12])

    # organize
    v7dat_scored <- v7dat_scored[c(1:365, 877, 366:369, 878, 370:373, 879, 374:377, 880, 378:381, 881, 382:385, 882, 386:389, 883, 390:393, 884, 394:397, 885, 398:401, 886:887, 402:876)]

    v7dat_scored_labels <- v7dat_scored_labels[c(1:365, 877, 366:369, 878, 370:373, 879, 374:377, 880, 378:381, 881, 382:385, 882, 386:389, 883, 390:393, 884, 394:397, 885, 398:401, 886:887, 402:876)]

    ## 9b) Standard Meal ####
    v7_meal_kcal <- fbs_kcal_intake(v7dat_scored[c(1, 314:361)], meal = 'std_meal', parID = 'id')
    names(v7_meal_kcal)[14:15] <- c('meal_total_g', 'meal_total_kcal')

    # get labels from scored data and simplify
    v7_meal_labels <- sapply(v7_meal_kcal, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(v7_meal_labels) <- names(v7_meal_kcal)

    # merge and organize
    v7dat_scored <- merge(v7dat_scored, v7_meal_kcal, by = 'id', all = TRUE)
    v7dat_scored_labels <- c(v7dat_scored_labels, v7_meal_labels[c(2:15)])

    # organize
    v7dat_scored <- v7dat_scored[c(1:317, 888, 318:321, 889, 322:325, 890, 326:329, 891, 330:333, 892, 334:337, 893, 338:341, 894, 342:345, 895, 346:349, 896, 350:353, 897, 354:357, 898, 358:361, 899:901, 362:887)]

    v7dat_scored_labels <- v7dat_scored_labels[c(1:317, 888, 318:321, 889, 322:325, 890, 326:329, 891, 330:333, 892, 334:337, 893, 338:341, 894, 342:345, 895, 346:349, 896, 350:353, 897, 354:357, 898, 358:361, 899:901, 362:887)]

    #### 9. PNA data #####

    # child pna data

    #merge lab and home
    child_v7dat_covidmerge_pna <- merge(child_lab_v7dat$pna_data, child_home_v7dat$pna_data, by = 'id', all = TRUE)

    #find names in common and unique
    common_names <- intersect(names(child_v7dat$pna_data), names(child_v7dat_covidmerge_pna))
    child_v7dat_uniq_names <- c('id', names(child_v7dat$pna_data)[!(names(child_v7dat$pna_data) %in% common_names)])
    child_covidmerge_v7dat_uniq_names <- c('id', names(child_v7dat_covidmerge_pna)[!(names(child_v7dat_covidmerge_pna) %in% common_names)])

    # initial merge with common names
    child_v7dat_pna_m1 <- rbind.data.frame(child_v7dat$pna_data[common_names], child_v7dat_covidmerge_pna[common_names])

    # full merge with unique names
    if (length(child_v7dat_uniq_names) > 1 & length(child_covidmerge_v7dat_uniq_names) > 1){
        child_v7dat_m2 <- merge(child_v7dat_pna_m1, child_v7dat$pna_data[child_v7dat_uniq_names], by = 'id', all = TRUE)
        child_v7dat_pna <- merge(child_v7dat_m2, child_v7dat_covidmerge_pna[child_covidmerge_v7dat_uniq_names], by = 'id', all = TRUE)
    } else if (length(child_v7dat_uniq_names) > 1) {
        child_v7dat_pna <- merge(child_v7dat_pna_m1, child_v7dat$pna_data[child_v7dat_uniq_names], by = 'id', all = TRUE)
    } else if (length(child_covidmerge_v7dat_uniq_names) > 1){
        child_v7dat_pna <- merge(child_v7dat_m1, child_v7dat_covidmerge_pna[child_covidmerge_v7dat_uniq_names], by = 'id', all = TRUE)
    } else {
        child_v7dat_pna <- child_v7dat_m1
    }

    #labels
    child_v7dat_pna_labels <- lapply(child_v7dat_pna, function(x) attributes(x)$label)

    #loop through to get all labels
    for (v in 1:length(names(child_v7dat_pna))){
        var_name = names(child_v7dat_pna)[v]

        if (var_name %in% names(child_v7dat$pna_data)){
            child_v7dat_pna_labels[[var_name]] <- child_v7dat$pna_dict[[var_name]]
        } else if (var_name %in% names(child_lab_v7dat$pna_data)){
            child_v7dat_pna_labels[[var_name]] <- child_lab_v7dat$pna_dict[[var_name]]
        } else if (var_name %in% names(child_home_v7dat$pna_data)){
            child_v7dat_pna_labels[[var_name]] <- child_home_v7dat$pna_dict[[var_name]]
        }

    }

    ## parent pna
    #find names in common and unique
    p_common_names <- intersect(names(parent_v7dat$pna_data), names(parent_home_v7dat$pna_data))
    parent_v7dat_uniq_names <- c('id', names(parent_v7dat$pna_data)[!(names(parent_v7dat$pna_data) %in% p_common_names)])
    parent_home_v7dat_uniq_names <- c('id', names(parent_home_v7dat$pna_data)[!(names(parent_home_v7dat$pna_data) %in% p_common_names)])

    # initial merge with common names
    parent_v7dat_pna_m1 <- rbind.data.frame(parent_v7dat$pna_data[p_common_names], parent_home_v7dat$pna_data[p_common_names])

    # full merge with unique names
    if (length(parent_v7dat_uniq_names) > 1 & length(parent_home_v7dat_uniq_names) > 1){
        parent_v7dat_m2 <- merge(parent_v7dat_pna_m1, parent_v7dat$pna_data[parent_v7dat_uniq_names], by = 'id', all = TRUE)
        parent_v7dat_pna <- merge(parent_v7dat_m2, parent_home_v7dat$pna_data[parent_home_v7dat_uniq_names], by = 'id', all = TRUE)
    } else if (length(parent_v7dat_uniq_names) > 1) {
        parent_v7dat_pna <- merge(parent_v7dat_pna_m1, parent_v7dat$pna_data[parent_v7dat_uniq_names], by = 'id', all = TRUE)
    } else if (length(parent_home_v7dat_uniq_names) > 1){
        parent_v7dat_pna <- merge(parent_v7dat_m1, parent_home_v7dat$pna_data[parent_home_v7dat_uniq_names], by = 'id', all = TRUE)
    } else {
        parent_v7dat_pna <- parent_v7dat_m1
    }

    #labels
    parent_v7dat_pna_labels <- lapply(parent_v7dat_pna, function(x) attributes(x)$label)

    #loop through to get all labels
    for (v in 1:length(names(parent_v7dat_pna))){
        var_name = names(parent_v7dat_pna)[v]

        if (var_name %in% names(parent_v7dat$pna_data)){
            parent_v7dat_pna_labels[[var_name]] <- parent_v7dat$pna_dict[[var_name]]
        } else if (var_name %in% names(parent_home_v7dat$pna_data)){
            parent_v7dat_pna_labels[[var_name]] <- parent_home_v7dat$pna_dict[[var_name]]
        }
    }

    # parent pna data
    for (v in 1:ncol(parent_v7dat$pna_data)) {
        var_name <- names(parent_v7dat$pna_data)[v]

        # remove existing label
        if (grepl("parent-reported", parent_v7dat$pna_dict[[var_name]], fixed = TRUE)) {
            parent_v7dat$pna_dict[[var_name]] <- gsub("parent-reported", "", parent_v7dat$pna_dict[[var_name]])
        }

        # add universal label
        parent_v7dat$pna_dict[[var_name]] <- paste0('Parent Reported: ', parent_v7dat$pna_dict[[var_name]])
    }

    v7dat_pna <- merge(child_v7dat_pna, parent_v7dat_pna, by = 'id', all = TRUE)

    v7dat_pna_labels <- c(child_v7dat_pna_labels, parent_v7dat_pna_labels[2:length(parent_v7dat_pna_labels)])

    #### 10. save to list #####

    # put data in order of participant ID for ease
    v7dat_scored <- v7dat_scored[order(v7dat_scored[["id"]]), ]
    v7dat_pna <- v7dat_pna[order(v7dat_pna[["id"]]), ]

    # set labels
    v7dat_scored = sjlabelled::set_label(v7dat_scored, label = matrix(unlist(v7dat_scored_labels, use.names = FALSE)))
    v7dat_pna = sjlabelled::set_label(v7dat_pna, label = matrix(unlist(v7dat_pna_labels, use.names = FALSE)))

    v7data_list <- list(data = v7dat_scored, dict = v7dat_scored_labels, pna_dat = v7dat_pna, pna_dict = v7dat_pna_labels)

    return(v7data_list)
}

