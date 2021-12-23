#' util_merge_v5: Merge all Visit 5 raw Qualtrics databases and organize variable in database order
#'
#' This function merges the following visit 5 raw data into a single database and organizes variables in database order: child visit 5, child visit 5-home, child visit 5-lab, and parent visit 5
#'
#' The databases MUST follow the naming convention: Child_V5_YYYY-MM-DD.sav, Child_V5_Home_YYY-MM-DD.sav, Child_V5_Lab_YYY-MM-DD.sav, and Parent_V5_YYY-MM-DD.sav. The databases must all be in the SAME directory to be processed if the data_path is not entered. If it is entered, it must follow
#'
#' @inheritParams util_merge_v1
#' @inheritParams util_merge_v1
#' @inheritParams util_merge_v1
#' @inheritParams util_merge_v1
#' @inheritParams util_merge_v1
#' @inheritParams util_parent_v1dat
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from parent visit 5 Qualtrics; 2) dict: all variable descriptions; 3) pna_data: data.frame marking participants who 'preferred not to answer' (pna) specific questions; and 4) pna_dict: all variable descriptions for pna_data
#'
#' @examples
#' #if in same working directory as data:
#' v5dat_scored <- util_merge_v5(date_str = '2021-10-11')
#'
#' #if in same working directory as data and covid collecte data has different dates:
#' v5dat_scored <- util_merge_v5(child_date_str = '2021-10-11', child_home_date_str = '2021-9-15', child_lab_date_str = = '2021-9-15', parent_date_str = '2021-10-11')
#'
#' \dontrun{
#' #date must be a string. The following will not run:
#' v5dat_scored <- util_merge_v5(2021-10-11)
#'
#' #date must match the file name - for file named 'Child_V5_Home_2021_09_15', the
#' following will not run:
#' v5dat_scored <- util_merge_v5('2021_10_11')
#' }
#'
#' @seealso Raw data from Qualtrics is processed using the following scripts: \code{\link{util_child_v5dat}}, \code{\link{util_child_v5dat_home}}, \code{\link{util_child_v5dat_lab}}, \code{\link{util_parent_v5dat}}. Visit 5 data is scored using the following scripts: \code{\link{score_*}}, \code{\link{score_*}}, \code{\link{score_*}}
#'
#'
#' @export

util_merge_v5 <- function(date_str, child_date_str, child_home_date_str, child_lab_date_str, parent_date_str, data_path) {

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
        parentV5_datestr_arg <- methods::hasArg(parentV5_date_str)

        if (sum(child_datestr_arg, child_home_datestr_arg, child_lab_datestr_arg, parent_datestr_arg) < 4){
            stop("if data_str is not set, then must enter each individual date string for the visit 5 databeses: child, child-home, child-lab, and parent")
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
                qv5_path <- paste0(data_path, "/", respondant, "_V5_", date_str, ".sav")
            } else {
                qv5_path <- paste0("/", respondant, "_V5_", date_str, ".sav")
            }

            # check if file exists
            qv5_exists <- file.exists(qv5_path)

            #warning message if doesn't exist - stop is below outside of function
            if (isFALSE(qv5_exists)) {
                warning(paste0("The ", respondant, "_V5_", date_str, ".sav database does not exist at the specified path:", qv5_path))
            }

            #return check
            qv5_res <- list(data_path_full = qv5_path, data_exists = qv5_exists)

            return(qv5_res)
        }

        #if covid split protocol
        if (isTRUE(covid_type)) {

            if (isTRUE(datapath_arg_fn)) {
                qv5_path <- paste0(data_path, "/Final_CovidAtHome/", respondant, "_V5_", loc, "_", date_str, ".sav")
            } else {
                qv5_path <- paste0("/Final_CovidAtHome/", respondant, "_V5_", loc, "_", date_str, ".sav")
            }

            # check if file exists
            qv5_exists <- file.exists(qv5_path)

            if (isTRUE(qv5_exists)) {
                #return check
                qv5_res <- list(data_path_full = qv5_path, data_exists = qv5_exists)

                return(qv5_res)
            } else {

                #check if in the main database rather than 'Final_CovidAtHome' database
                if (isTRUE(datapath_arg_fn)) {
                    qv5_path2 <- paste0(data_path, "/", respondant, "_V5_", loc, "_", date_str, ".sav")
                } else {
                    qv5_path2 <- paste0("/", respondant, "_V5_", loc, "_", date_str, ".sav")
                }

                # check if file exists
                qv5_exists2 <- file.exists(qv5_path2)

                #warning message if doesn't exist - stop is below outside of function
                if (isFALSE(qv5_exists2)) {
                    warning(paste0("The ", respondant, "_V5_", loc, "_", date_str, ".sav database does not exist at either of the possible paths:", qv5_path, "or", qv5_path2))
                }

                #return check
                qv5_res <- list(data_path_full = qv5_path2, data_exists = qv5_exists2)

                return(qv5_res)
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
        child_v5dat <- util_child_v5dat(date_str, data_path)
        child_home_v5dat <- util_child_v5dat_home(date_str, data_path)
        child_lab_v5dat <- util_child_v5dat_lab(date_str, data_path)
        parent_v5dat <- util_parent_v5dat(date_str, data_path)
    } else {
        child_v5dat <- util_child_v5dat(child_date_str, data_path)
        child_home_v5dat <- util_child_v5dat_home(child_home_date_str, data_path)
        child_lab_v5dat <- util_child_v5dat_lab(child_lab_date_str, data_path)
        parent_v5dat <- util_parent_v5dat(parent_date_str, data_path)
    }

    #### 4. Merge Child Raw Data #####

    # merge child home and lab into single database
    child_covidmerge_v5dat <- merge(child_lab_v5dat$data, child_home_v5dat$data[c(1, 3:18)], by = 'id', all = TRUE)

    # re-order so matches child_v5dat
    child_covidmerge_v5dat <- child_covidmerge_v5dat[c(1:64, 156:171, 65:146, 147:155)]

    # merge all child into single database
    all_child_v5dat <- rbind.data.frame(child_v5dat$data, child_covidmerge_v5dat)

    #### 5. Merge Parent Raw Data #####

    # update labels with 'parent report'

    for (v in 1:ncol(parent_v5dat$data)) {
        var_name <- names(parent_v5dat$data)[v]

        # remove existing label
        if (grepl("parent-reported", parent_v5dat$dict[[var_name]], fixed = TRUE)) {
            parent_v5dat$dict[[var_name]] <- gsub("parent-reported", "", parent_v5dat$dict[[var_name]])
        }

        # add universal label
        parent_v5dat$dict[[var_name]] <- paste0('Parent Reported: ', parent_v5dat$dict[[var_name]])
    }

    v5dat <- merge(all_child_v5dat, parent_v5dat$data[c(1, 3:26)], by = 'id', all = TRUE)

    # merge labels/dictionary
    v5dat_labels <- c(child_v5dat$dict, parent_v5dat$dict[3:26])

    #### 6. Organize V5 data ####

    # order of vars: 1) Demographics, 2) Anthro (hw, DXA, sleep, PA), 3) Intake (FF, liking, intake, want), 4) feeding/food Q's, 5) cog/trait Q's, 6) Delay Discounting, 7) MRI related (CAMS, FF, snack info, image ratings), 8) Notes

    v5dat_org <- v5dat[c(1:2, 65:80, 172:181, 3:41, 42:64, 81:157, 158:171, 182:195)]

    v5dat_labels <- v5dat_labels[c(1:2, 65:80, 172:181, 3:41, 42:64, 81:157, 158:171, 182:195)]

    # ensure labels are up to date
    v5dat_org = sjlabelled::set_label(v5dat_org, label = matrix(unlist(v5dat_labels, use.names = FALSE)))

    #### 7. Organize V5 data ####

    ## 7a) score the US Household Food Security Survey Module: Three Stage ####
    ## need to make
    ctc_scored <- score_ctc(ctc_data = v5dat_org[c(1, 3:18)], parID = 'id')

    # get labels from scored data and simplify
    ctc_scored_labels <- sapply(ctc_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(ctc_scored_labels) <- names(ctc_scored)

    # merge and organize
    v5dat_scored <- merge(v5dat_org, ctc_scored, by = 'id', all = TRUE)
    v5dat_scored <- v5dat_scored[c(1:25, 215:222, 26:214)]

    v5dat_scored_labels <- c(v5dat_labels[1:25], ctc_scored_labels[2:9], v5dat_labels[26:214])

    ## 7b) score the Household Food Insecurity Access Scale ####
    hfias_scored <- score_hfias(hfias_data = v5dat_org[c(1, 26:43)], parID = 'id')

    # get labels from scored data and simplify
    hfias_scored_labels <- sapply(hfias_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(hfias_scored_labels) <- names(hfias_scored)

    # merge and organize
    v5dat_scored <- merge(v5dat_scored, hfias_scored, by = 'id', all = TRUE)
    v5dat_scored <- v5dat_scored[c(1:51, 223:224, 52:222)]

    v5dat_scored_labels <- c(v5dat_scored_labels[1:51], hfias_scored_labels[2:3], v5dat_scored_labels[52:222])

    ## 7c) score the Community Childhood Hunger Identification Project ####
    cchip_scored <- score_cchip(cchip_data = v5dat_org[c(1, 44:75)], parID = 'id')

    # get labels from scored data and simplify
    cchip_scored_labels <- sapply(cchip_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(cchip_scored_labels) <- names(cchip_scored)

    # merge and organize
    v5dat_scored <- merge(v5dat_scored, cchip_scored, by = 'id', all = TRUE)
    v5dat_scored <- v5dat_scored[c(1:85, 225:226, 86:224)]

    v5dat_scored_labels <- c(v5dat_scored_labels[1:85], cchip_scored_labels[2:3], v5dat_scored_labels[86:224])

    ## 7d) score the Child Weight Concerns Scale ####
    cwc_scored <- score_cwc(cwc_data = v5dat_org[c(1, 76:80)], parID = 'id')

    # get labels from scored data and simplify
    cwc_scored_labels <- sapply(cwc_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(cwc_scored_labels) <- names(cwc_scored)

    # merge and organize
    v5dat_scored <- merge(v5dat_scored, cwc_scored, by = 'id', all = TRUE)
    v5dat_scored <- v5dat_scored[c(1:92, 227, 93:226)]

    v5dat_scored_labels <- c(v5dat_scored_labels[1:92], cwc_scored_labels[2], v5dat_scored_labels[93:226])

    ## 7e) score the Children's Body Image Scale Questionnaire ####
    cbis_scored <- score_cbis(cbis_data = v5dat_org[c(1, 81:84)], parID = 'id')

    # get labels from scored data and simplify
    cbis_scored_labels <- sapply(cbis_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(cbis_scored_labels) <- names(cbis_scored)

    # merge and organize
    v5dat_scored <- merge(v5dat_scored, cbis_scored, by = 'id', all = TRUE)
    v5dat_scored <- v5dat_scored[c(1:98, 228:229, 99:227)]

    v5dat_scored_labels <- c(v5dat_scored_labels[1:98], cbis_scored_labels[2:3], v5dat_scored_labels[99:227])

    ## 7f) score the Behavioral Rating Inventory of Executive Function-2 ####
    brief_scored <- score_brief2(brief_data = v5dat_org[c(1, 4:5, 125:187)], age_var = 'age', sex_var = 'sex', parID = 'id')

    # get labels from scored data and simplify
    brief_scored_labels <- sapply(brief_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(brief_scored_labels) <- names(brief_scored)

    # merge and organize
    v5dat_scored <- merge(v5dat_scored, brief_scored, by = 'id', all = TRUE)
    v5dat_scored <- v5dat_scored[c(1:202, 230:277, 203:229)]

    v5dat_scored_labels <- c(v5dat_scored_labels[1:202], brief_scored_labels[2:49], v5dat_scored_labels[203:229])

    ## 7f) score the Parenting Style Inventory-II ####
    psi_scored <- score_psi(psi_data = v5dat_org[c(1, 188:197)], study = 'fbs', parID = 'id')

    # get labels from scored data and simplify
    psi_scored_labels <- sapply(psi_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

    # make names match because simplify duplicates - not sure why get nested lists
    names(psi_scored_labels) <- names(psi_scored)

    # merge and organize
    v5dat_scored <- merge(v5dat_scored, psi_scored, by = 'id', all = TRUE)
    v5dat_scored <- v5dat_scored[c(1:260, 278:279, 261:277)]

    v5dat_scored_labels <- c(v5dat_scored_labels[1:260], psi_scored_labels[2:3], v5dat_scored_labels[261:277])


    #### 8. PNA data #####

    # child pna data

    #find names in common and unique
    common_names <- intersect(names(child_v5dat$pna_data), names(child_home_v5dat$pna_data))
    child_v5dat_uniq_names <- c('id', names(child_v5dat$pna_data)[!(names(child_v5dat$pna_data) %in% common_names)])
    child_home_v5dat_uniq_names <- c('id', names(child_home_v5dat$pna_data)[!(names(child_home_v5dat$pna_data) %in% common_names)])

    # initial merge with common names
    child_v5dat_pna_m1 <- rbind.data.frame(child_v5dat$pna_data[common_names], child_home_v5dat$pna_data[common_names])

    # full merge with unique names
    if (length(child_v5dat_uniq_names) > 1 & length(child_home_v5dat_uniq_names) > 1){
        child_v5dat_m2 <- merge(child_v5dat_pna_m1, child_v5dat$pna_data[child_v5dat_uniq_names], by = 'id', all = TRUE)
        child_v5dat_pna <- merge(child_v5dat_m2, child_home_v5dat$pna_data[child_home_v5dat_uniq_names], by = 'id', all = TRUE)
    } else if (length(child_v5dat_uniq_names) > 1) {
        child_v5dat_pna <- merge(child_v5dat_pna_m1, child_v5dat$pna_data[child_v5dat_uniq_names], by = 'id', all = TRUE)
    } else if (length(child_home_v5dat_uniq_names) > 1){
        child_v5dat_pna <- merge(child_v5dat_m1, child_home_v5dat$pna_data[child_home_v5dat_uniq_names], by = 'id', all = TRUE)
    } else {
        child_v5dat_pna <- child_v5dat_m1
    }

    #labels
    child_v5dat_pna_labels <- lapply(child_v5dat_pna, function(x) attributes(x)$label)

    #loop through to get all labels
    for (v in 1:length(names(child_v5dat_pna))){
        var_name = names(child_v5dat_pna)[v]

        if (var_name %in% names(child_v5dat$pna_data)){
            child_v5dat_pna_labels[[var_name]] <- child_v5dat$pna_dict[[var_name]]
        } else if (var_name %in% names(child_home_v5dat$pna_data)){
            child_v5dat_pna_labels[[var_name]] <- child_home_v5dat$pna_dict[[var_name]]
        }
    }

    # parent pna data
    for (v in 1:ncol(parent_v5dat$pna_data)) {
        var_name <- names(parent_v5dat$pna_data)[v]

        # remove existing label
        if (grepl("parent-reported", parent_v5dat$pna_dict[[var_name]], fixed = TRUE)) {
            parent_v5dat$pna_dict[[var_name]] <- gsub("parent-reported", "", parent_v5dat$pna_dict[[var_name]])
        }

        # add universal label
        parent_v5dat$pna_dict[[var_name]] <- paste0('Parent Reported: ', parent_v5dat$pna_dict[[var_name]])
    }

    v5dat_pna <- merge(child_v5dat_pna, parent_v5dat$pna_data, by = 'id', all = TRUE)

    v5dat_pna_labels <- c(child_v5dat_pna_labels, parent_v5dat$pna_dict[2:length(parent_v5dat$pna_dict)])

    #### 9. save to list #####

    # put data in order of participant ID for ease
    v5dat_scored <- v5dat_scored[order(v5dat_scored[["id"]]), ]
    v5dat_pna <- v5dat_pna[order(v5dat_pna[["id"]]), ]

    # set labels
    v5dat_scored = sjlabelled::set_label(v5dat_scored, label = matrix(unlist(v5dat_scored_labels, use.names = FALSE)))
    v5dat_pna = sjlabelled::set_label(v5dat_pna, label = matrix(unlist(v5dat_pna_labels, use.names = FALSE)))

    v5data_list <- list(data = v5dat_scored, dict = v5dat_scored_labels, pna_dat = v5dat_pna, pna_dict = v5dat_pna_labels)

    return(v5data_list)
}

