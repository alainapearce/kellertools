#' util_merge_v3: Merge all Visit 3 raw Qualtrics databases and organize variable in database order
#'
#' This function merges the following visit 3 raw data into a single database and organizes variables in database order: child visit 3, child visit 3-home, child visit 3-lab, and parent visit 3
#'
#' The databases MUST follow the naming convention: Child_V3_YYYY-MM-DD.sav, Child_V3_Home_YYY-MM-DD.sav, Child_V3_Lab_YYY-MM-DD.sav, and Parent_V3_YYY-MM-DD.sav. The databases must all be in the SAME directory to be processed if the data_path is not entered. If it is entered, it must follow
#'
#' @inheritParams util_merge_v1
#' @inheritParams util_merge_v1
#' @inheritParams util_merge_v1
#' @inheritParams util_merge_v1
#' @inheritParams util_merge_v1
#' @inheritParams util_parent_v1dat
#' @param model_DD Indicate if delay discounting data should be modeled. This will take an addition 3-5 minutes of processing time. Default = FALSE.
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from parent visit 3 Qualtrics; 2) dict: all variable descriptions; 3) pna_data: data.frame marking participants who 'preferred not to answer' (pna) specific questions; and 4) pna_dict: all variable descriptions for pna_data
#'
#' @examples
#' #if in same working directory as data:
#' v3dat_scored <- util_merge_v3(date_str = '2021-10-11')
#'
#' #if in same working directory as data and covid collecte data has different dates:
#' v3dat_scored <- util_merge_v3(child_date_str = '2021-10-11', child_home_date_str = '2021-9-15', child_lab_date_str = = '2021-9-15', parent_date_str = '2021-10-11')
#'
#' \dontrun{
#' #date must be a string. The following will not run:
#' v3dat_scored <- util_merge_v3(2021-10-11)
#'
#' #date must match the file name - for file named 'Child_V3_Home_2021_09_15', the
#' following will not run:
#' v3dat_scored <- util_merge_v3('2021_10_11')
#' }
#'
#' @seealso Raw data from Qualtrics is processed using the following scripts: \code{\link{util_child_v3dat}}, \code{\link{util_child_v3dat_home}}, \code{\link{util_child_v3dat_lab}}, \code{\link{util_parent_v3dat}}. Visit 3 data is scored using the following scripts: \code{\link{score_lbc}}, \code{\link{score_pwlb}}, \code{\link{score_tfeq}}, \code{\link{score_bisbas}}, \code{\link{score_spsrq}}, \code{\link{model_dd}}
#'
#'
#' @export

util_merge_v3 <- function(date_str, child_date_str, child_home_date_str, child_lab_date_str, parent_date_str, data_path, model_DD) {

    #### 1. Set up/initial checks #####

    # check if date_str exist and is a string

    datestr_arg <- methods::hasArg(date_str)

    if (isTRUE(datestr_arg) & !is.character(date_str)) {
        stop("date_str must be enter as a string: e.g., '2021_10_11'")
    } else if (isFALSE(datestr_arg)) {

        # if no date_str, check all databases specific date strings
        child_datestr_arg <- methods::hasArg(child_date_str)
        child_home_datestr_arg <- methods::hasArg(child_home_date_str)
        child_lab_datestr_arg <- methods::hasArg(child_lab_date_str)
        parent_datestr_arg <- methods::hasArg(parent_date_str)
        parentV4_datestr_arg <- methods::hasArg(parentV4_date_str)

        if (sum(child_datestr_arg, child_home_datestr_arg, child_lab_datestr_arg, parent_datestr_arg) < 4){
            stop("if data_str is not set, then must enter each individual date string for the visit 3 databeses: child, child-home, child-lab, and parent")
        }

        if (!is.character(child_date_str) | !is.character(child_home_date_str) | !is.character(child_lab_date_str) | !is.character(parent_date_str)) {
            stop("all dates must be enter as a string: e.g., '2021_10_11'")
        }
    }

    # check that file exists
    datapath_arg <- methods::hasArg(data_path)

    if (isTRUE(datapath_arg)) {
        if (!is.character(date_str)) {
            stop("data_path must be enter as a string: e.g., '.../Participant_Data/untouchedRaw/util_Raw/'")
        }
    }

    #### 2. Check Data Exists #####

    ## define function to test if file exists
    file_exist_fn <- function(data_path, date_str, respondant, covid_type = FALSE, loc){

        datapath_arg_fn <- methods::hasArg(data_path)

        #if not special case
        if (isFALSE(covid_type)) {
            if (isTRUE(datapath_arg_fn)) {
                qv3_path <- paste0(data_path, "/", respondant, "_V3_", date_str, ".sav")
            } else {
                qv3_path <- paste0("/", respondant, "_V3_", date_str, ".sav")
            }

            # check if file exists
            qv3_exists <- file.exists(qv3_path)

            #warning message if doesn't exist - stop is below outside of function
            if (isFALSE(qv3_exists)) {
                warning(paste0("The ", respondant, "_V3_", date_str, ".sav database does not exist at the specified path:", qv3_path))
            }

            #return check
            qv3_res <- list(data_path_full = qv3_path, data_exists = qv3_exists)

            return(qv3_res)
        }

        #if covid split protocol
        if (isTRUE(covid_type)) {

            if (isTRUE(datapath_arg_fn)) {
                qv3_path <- paste0(data_path, "/Final_CovidAtHome/", respondant, "_V3_", loc, "_", date_str, ".sav")
            } else {
                qv3_path <- paste0("/Final_CovidAtHome/", respondant, "_V3_", loc, "_", date_str, ".sav")
            }

            # check if file exists
            qv3_exists <- file.exists(qv3_path)

            if (isTRUE(qv3_exists)) {
                #return check
                qv3_res <- list(data_path_full = qv3_path, data_exists = qv3_exists)

                return(qv3_res)
            } else {

                #check if in the main database rather than 'Final_CovidAtHome' database
                if (isTRUE(datapath_arg_fn)) {
                    qv3_path2 <- paste0(data_path, "/", respondant, "_V3_", loc, "_", date_str, ".sav")
                } else {
                    qv3_path2 <- paste0("/", respondant, "_V3_", loc, "_", date_str, ".sav")
                }

                # check if file exists
                qv3_exists2 <- file.exists(qv3_path2)

                #warning message if doesn't exist - stop is below outside of function
                if (isFALSE(qv3_exists2)) {
                    warning(paste0("The ", respondant, "_V3_", loc, "_", date_str, ".sav database does not exist at either of the possible paths:", qv3_path, "or", qv3_path2))
                }

                #return check
                qv3_res <- list(data_path_full = qv3_path2, data_exists = qv3_exists2)

                return(qv3_res)
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
        child_v3dat <- util_child_v3dat(date_str, data_path)
        child_home_v3dat <- util_child_v3dat_home(date_str, data_path)
        child_lab_v3dat <- util_child_v3dat_lab(date_str, data_path)
        parent_v3dat <- util_parent_v3dat(date_str, data_path)
    } else {
        child_v3dat <- util_child_v3dat(child_date_str, data_path)
        child_home_v3dat <- util_child_v3dat_home(child_home_date_str, data_path)
        child_lab_v3dat <- util_child_v3dat_lab(child_lab_date_str, data_path)
        parent_v3dat <- util_parent_v3dat(parent_date_str, data_path)
    }

    #### 4. Merge Child Raw Data #####

    # merge child home and lab into single database
    child_covidmerge_v3dat <- merge(child_lab_v3dat$data, child_home_v3dat$data[c(1, 3:71)], by = 'id', all = TRUE)

    # re-order so matches child_v3dat
    child_covidmerge_v3dat <- child_covidmerge_v3dat[c(1:41, 44:112, 42:43)]

    # merge all child into single database
    all_child_v3dat <- rbind.data.frame(child_v3dat$data, child_covidmerge_v3dat)

    #### 5. Merge Parent Raw Data #####

    # update labels with 'parent report'

    for (v in 1:ncol(parent_v3dat$data)) {
        var_name <- names(parent_v3dat$data)[v]

        # remove existing label
        if (grepl("parent-reported", parent_v3dat$dict[[var_name]], fixed = TRUE)) {
            parent_v3dat$dict[[var_name]] <- gsub("parent-reported", "", parent_v3dat$dict[[var_name]])
        }

        # add universal label
        parent_v3dat$dict[[var_name]] <- paste0('Parent Reported: ', parent_v3dat$dict[[var_name]])
    }

    v3dat <- merge(all_child_v3dat, parent_v3dat$data[c(1, 3:188)], by = 'id', all = TRUE)

    # merge labels/dictionary
    v3dat_labels <- c(child_v3dat$dict, parent_v3dat$dict[3:188])

    #### 6. Organize V3 data ####

    # order of vars: 1) Demographics, 2) Anthro (hw, DXA, sleep, PA), 3) Intake (FF, liking, intake, want), 4) feeding/food Q's, 5) cog/trait Q's, 6) Delay Discounting, 7) MRI related (CAMS, FF, snack info, image ratings), 8) Notes

    v3dat_org <- v3dat[c(1:2, 3:41, 114:213, 262:285, 214:261, 42:110, 113, 111:112, 286:298)]

    v3dat_labels <- v3dat_labels[c(1:2, 3:41, 114:213, 262:285, 214:261, 42:110, 113, 111:112, 286:298)]

    # ensure labels are up to date
    v3dat_org = sjlabelled::set_label(v3dat_org, label = matrix(unlist(v3dat_labels, use.names = FALSE)))

    #### 7. Organize V3 data ####

    ## 7a) score the Lifestyle Behavior Checklist ####
    lbc_scored <- score_lbc(lbc_data = v3dat_org[c(1, 42:61)], study = 'fbs', parID = 'id')

    # get labels from scored data and simplify
    lbc_scored_labels <- sapply(lbc_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(lbc_scored_labels) <- names(lbc_scored)

    # merge and organize
    v3dat_scored <- merge(v3dat_org, lbc_scored, by = 'id', all = TRUE)
    v3dat_scored <- v3dat_scored[c(1:61, 299:303, 62:298)]

    v3dat_scored_labels <- c(v3dat_labels[1:61], lbc_scored_labels[2:6], v3dat_labels[62:298])

    ## 7b) score the Parent Weight-Loss Behavior Questionnaire ####
    pwlb_scored <- score_pwlb(pwlb_data = v3dat_org[c(1, 62:90)], parID = 'id')

    # get labels from scored data and simplify
    pwlb_scored_labels <- sapply(pwlb_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(pwlb_scored_labels) <- names(pwlb_scored)

    # merge and organize
    v3dat_scored <- merge(v3dat_scored, pwlb_scored, by = 'id', all = TRUE)
    v3dat_scored <- v3dat_scored[c(1:95, 304:306, 96:303)]

    v3dat_scored_labels <- c(v3dat_scored_labels[1:95], pwlb_scored_labels[2:4], v3dat_scored_labels[96:303])

    ## 7c) score the Three Factor Eating Questionnaire ####
    tfeq_scored <- score_tfeq(tfeq_data = v3dat_org[c(1, 91:141)], parID = 'id')

    # get labels from scored data and simplify
    tfeq_scored_labels <- sapply(tfeq_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(tfeq_scored_labels) <- names(tfeq_scored)

    # merge and organize
    v3dat_scored <- merge(v3dat_scored, tfeq_scored, by = 'id', all = TRUE)
    v3dat_scored <- v3dat_scored[c(1:149, 307:309, 150:306)]

    v3dat_scored_labels <- c(v3dat_scored_labels[1:149], tfeq_scored_labels[2:4], v3dat_scored_labels[150:306])

    ## 7d) score the Behavioral Inhibition Scale/Behavioral Activation Scale ####
    bisbas_scored <- score_bisbas(bisbas_data = v3dat_org[c(1, 142:165)], parID = 'id')

    # get labels from scored data and simplify
    bisbas_scored_labels <- sapply(bisbas_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(bisbas_scored_labels) <- names(bisbas_scored)

    # merge and organize
    v3dat_scored <- merge(v3dat_scored, bisbas_scored, by = 'id', all = TRUE)
    v3dat_scored <- v3dat_scored[c(1:176, 310:314, 177:309)]

    v3dat_scored_labels <- c(v3dat_scored_labels[1:176], bisbas_scored_labels[2:6], v3dat_scored_labels[177:309])

    ## 7e) score the Sensitivity to Punishment and Sensitivity to Reward Questionnaire ####
    spsrq_scored <- score_spsrq(spsrq_data = v3dat_org[c(1, 166:213)], parID = 'id')

    # get labels from scored data and simplify
    spsrq_scored_labels <- sapply(spsrq_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(spsrq_scored_labels) <- names(spsrq_scored)

    # merge and organize
    v3dat_scored <- merge(v3dat_scored, spsrq_scored, by = 'id', all = TRUE)
    v3dat_scored <- v3dat_scored[c(1:229, 315:325, 230:314)]

    v3dat_scored_labels <- c(v3dat_scored_labels[1:229], spsrq_scored_labels[2:12], v3dat_scored_labels[230:314])

    ## 7f) model Delay Discounting ####

    if (isTRUE(model_DD)){

        message('Modeling Dealy Discounting data ...')

        dd_scored <- model_dd(dd_data = v3dat_org[c(1, 214:282)], parID = 'id')

        message('Finished modeling Dealy Discounting')

        # get labels from scored data and simplify
        dd_scored_labels <- sapply(dd_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

        # make names match because simplify duplicates - not sure why get nested lists
        names(dd_scored_labels) <- names(dd_scored)

        # merge and organize
        v3dat_scored <- merge(v3dat_scored, dd_scored, by = 'id', all = TRUE)
        v3dat_scored <- v3dat_scored[c(1:309, 326:333, 310:325)]

        v3dat_scored_labels <- c(v3dat_scored_labels[1:309], dd_scored_labels[2:9], v3dat_scored_labels[310:325])
    }

    #### 8. PNA data #####

    # only parent has pna data to organize
    v3dat_pna <- parent_v3dat$pna_data

    #### 9. save to list #####

    # put data in order of participant ID for ease
    v3dat_scored <- v3dat_scored[order(v3dat_scored[["id"]]), ]
    v3dat_pna <- v3dat_pna[order(v3dat_pna[["id"]]), ]

    # set labels
    v3dat_scored = sjlabelled::set_label(v3dat_scored, label = matrix(unlist(v3dat_scored_labels, use.names = FALSE)))
    v3dat_pna = sjlabelled::set_label(v3dat_pna, label = matrix(unlist(parent_v3dat$pna_dict, use.names = FALSE)))

    v3data_list <- list(data = v3dat_scored, dict = v3dat_scored_labels, pna_dat = v3dat_pna, pna_dict = parent_v3dat$pna_dict)

    return(v3data_list)
}

