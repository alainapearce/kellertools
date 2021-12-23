#' util_merge_v2: Merge all Visit 2 raw Qualtrics databases and organize variable in database order
#'
#' This function merges the following visit 2 raw data into a single database and organizes variables in database order: child visit 2, child visit 2-home, child visit 2-lab, and parent visit 2
#'
#' The databases MUST follow the naming convention: Child_V2_YYYY-MM-DD.sav, Child_V2_Home_YYY-MM-DD.sav, Child_V2_Lab_YYY-MM-DD.sav, and Parent_V2_YYY-MM-DD.sav. The databases must all be in the SAME directory to be processed if the data_path is not entered. If it is entered, it must follow
#'
#' @inheritParams util_merge_v1
#' @inheritParams util_merge_v1
#' @inheritParams util_merge_v1
#' @inheritParams util_merge_v1
#' @inheritParams util_merge_v1
#' @param parentV4_date_str (optional) If the date string differs for parent V4, enter the date string. Need to load raw data from parent V4 to get child grade to score the Revised Children's Manifest Anxiety Scale
#' @inheritParams util_parent_v1dat
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from parent visit 2 Qualtrics; 2) dict: all variable descriptions; 3) pna_data: data.frame marking participants who 'preferred not to answer' (pna) specific questions; and 4) pna_dict: all variable descriptions for pna_data
#'
#' @examples
#' #if in same working directory as data with all data having the same date in filename:
#' v2dat_scored <- util_merge_v2(date_str = '2021-10-11')
#'
#' #if in same working directory as data and covid collecte data has different dates:
#' v2dat_scored <- util_merge_v2(child_date_str = '2021-10-11', child_home_date_str = '2021-9-15', child_lab_date_str = = '2021-9-15', parent_date_str = '2021-10-11', parentV4_date_str = '2021-10-11')
#'
#' \dontrun{
#' #date must be a string. The following will not run:
#' v2dat_scored <- util_merge_v2(2021-10-11)
#'
#' #date must match the file name - for file named 'Child_V2_Home_2021_09_15', the
#' following will not run:
#' v2dat_scored <- util_merge_v2('2021_10_11')
#' }
#'
#' @seealso Raw data from Qualtrics is processed using the following scripts: \code{\link{util_child_v2dat}}, \code{\link{util_child_v2dat_home}}, \code{\link{util_child_v2dat_lab}}, \code{\link{util_parent_v2dat}}. Visit 2 data is scored using the following scripts: \code{\link{score_cshqa}}, \code{\link{score_tesqe}}, \code{\link{score_kfq}}, \code{\link{score_cebq}}, \code{\link{score_bes}}, \code{\link{score_cf1}}, \code{\link{score_ffbs}}, \code{\link{score_rcmas}}, and \code{\link{score_cbq}}
#'
#'
#' @export

util_merge_v2 <- function(date_str, child_date_str, child_home_date_str, child_lab_date_str, parent_date_str, parentV4_date_str, data_path) {

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
            stop("if data_str is not set, then must enter each individual date string for the visit 2 databeses: child, child-home, child-lab, and parent")
        }


        if (!is.character(child_date_str) | !is.character(child_home_date_str) | !is.character(child_lab_date_str) | !is.character(parent_date_str)) {
            stop("all dates must be enter as a string: e.g., '2021_10_11'")
        }
    }

    #check parent V4
    if (isTRUE(parentV4_datestr_arg)){
        if (!is.character(parentV4_datestr_arg)) {
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
                qv2_path <- paste0(data_path, "/", respondant, "_V2_", date_str, ".sav")
            } else {
                qv2_path <- paste0("/", respondant, "_V2_", date_str, ".sav")
            }

            # check if file exists
            qv2_exists <- file.exists(qv2_path)

            #warning message if doesn't exist - stop is below outside of function
            if (isFALSE(qv2_exists)) {
                warning(paste0("The ", respondant, "_V2_", date_str, ".sav database does not exist at the specified path:", qv2_path))
            }

            #return check
            qv2_res <- list(data_path_full = qv2_path, data_exists = qv2_exists)

            return(qv2_res)
        }

        #if covid split protocol
        if (isTRUE(covid_type)) {

            if (isTRUE(datapath_arg_fn)) {
                qv2_path <- paste0(data_path, "/Final_CovidAtHome/", respondant, "_V2_", loc, "_", date_str, ".sav")
            } else {
                qv2_path <- paste0("/Final_CovidAtHome/", respondant, "_V2_", loc, "_", date_str, ".sav")
            }

            # check if file exists
            qv2_exists <- file.exists(qv2_path)

            if (isTRUE(qv2_exists)) {
                #return check
                qv2_res <- list(data_path_full = qv2_path, data_exists = qv2_exists)

                return(qv2_res)
            } else {

                #check if in the main database rather than 'Final_CovidAtHome' database
                if (isTRUE(datapath_arg_fn)) {
                    qv2_path2 <- paste0(data_path, "/", respondant, "_V2_", loc, "_", date_str, ".sav")
                } else {
                    qv2_path2 <- paste0("/", respondant, "_V2_", loc, "_", date_str, ".sav")
                }

                # check if file exists
                qv2_exists2 <- file.exists(qv2_path2)

                #warning message if doesn't exist - stop is below outside of function
                if (isFALSE(qv2_exists2)) {
                    warning(paste0("The ", respondant, "_V2_", loc, "_", date_str, ".sav database does not exist at either of the possible paths:", qv2_path, "or", qv2_path2))
                }

                #return check
                qv2_res <- list(data_path_full = qv2_path2, data_exists = qv2_exists2)

                return(qv2_res)
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

    ## check if parent V4 exists
    if (isFALSE(parentV4_datestr_arg)){
        if (isTRUE(datapath_arg)) {
            qv4_parent_path <- paste0(data_path, "/Parent_V4_", date_str, ".sav")
        } else {
            qv4_parent_path <- paste0("/Parent_V4_", date_str, ".sav")
        }
    } else {
        if (isTRUE(datapath_arg)) {
            qv4_parent_path <- paste0(data_path, "/Parent_V4_", parentV4_date_str, ".sav")
        } else {
            qv4_parent_path <- paste0("/Parent_V4_", parentV4_date_str, ".sav")
        }
    }

# check if file exists
qv4_parent_exists <- file.exists(qv4_parent_path)

#warning message if doesn't exist - stop is below outside of function
if (isFALSE(qv4_parent_exists)) {
    warning(paste0("The Parent_V4_", date_str, ".sav database does not exist at the specified path:", qv4_parent_path, ". The Revised Children's Manifest Anxiety Scale will not be able to be fully scored with out child grade from Parent_V4_", date_str, ".sav"))
}

#### 3. Process Raw Data #####

if (isTRUE(datestr_arg)) {
    child_v2dat <- util_child_v2dat(date_str, data_path)
    child_home_v2dat <- util_child_v2dat_home(date_str, data_path)
    child_lab_v2dat <- util_child_v2dat_lab(date_str, data_path)
    parent_v2dat <- util_parent_v2dat(date_str, data_path)
} else {
    child_v2dat <- util_child_v2dat(child_date_str, data_path)
    child_home_v2dat <- util_child_v2dat_home(child_home_date_str, data_path)
    child_lab_v2dat <- util_child_v2dat_lab(child_lab_date_str, data_path)
    parent_v2dat <- util_parent_v2dat(parent_date_str, data_path)
}

#### 4. Merge Child Raw Data #####

# merge child home and lab into single database
child_covidmerge_v2dat <- merge(child_lab_v2dat$data, child_home_v2dat$data[c(1, 3:109)], by = 'id', all = TRUE)

# re-order so matches child_v2dat
child_covidmerge_v2dat <- child_covidmerge_v2dat[c(1:41, 44:150, 42:43)]

# merge all child into single database
all_child_v2dat <- rbind.data.frame(child_v2dat$data, child_covidmerge_v2dat)

#### 5. Merge Parent Raw Data #####

# update labels with 'parent report'

for (v in 1:ncol(parent_v2dat$data)) {
    var_name <- names(parent_v2dat$data)[v]

    # remove existing label
    if (grepl("parent-reported", parent_v2dat$dict[[var_name]], fixed = TRUE)) {
        parent_v2dat$dict[[var_name]] <- gsub("parent-reported", "", parent_v2dat$dict[[var_name]])
    }

    # add universal label
    parent_v2dat$dict[[var_name]] <- paste0('Parent Reported: ', parent_v2dat$dict[[var_name]])
}

v2dat <- merge(all_child_v2dat, parent_v2dat$data[c(1, 3:229)], by = 'id', all = TRUE)

# merge labels/dictionary
v2dat_labels <- c(child_v2dat$dict, parent_v2dat$dict[3:229])

#### 6. Organize V2 data ####

# order of vars: 1) Demographics, 2) Anthro (hw, DXA, sleep, PA), 3) Intake (FF, liking, intake, want), 4) feeding/food Q's, 5) cog/trait Q's, 6) Delay Discounting, 7) MRI related (CAMS, FF, snack info, image ratings), 8) Notes

v2dat_org <- v2dat[c(1:2, 152:169, 3:41, 42:111, 170:204, 235:250, 205:234, 251:270, 112:148, 271:364, 151, 149:150, 365:377)]

v2dat_labels <- v2dat_labels[c(1:2, 152:169, 3:41, 42:111, 170:204, 235:250, 205:234, 251:270, 112:148, 271:364, 151, 149:150, 365:377)]

# ensure labels are up to date
v2dat_org = sjlabelled::set_label(v2dat_org, label = matrix(unlist(v2dat_labels, use.names = FALSE)))

#### 7. Organize V2 data ####

## 7a) score Children's Sleep Habbits Questionnaire-Abbreviated ####
cshqa_scored <- score_cshqa(cshqa_data = v2dat_org[c(1, 3:20)], study = 'fbs', parID = 'id')

# get labels from scored data and simplify
cshqa_scored_labels <- sapply(cshqa_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

# make names match because simplify duplicates - not sure why get nested lists
names(cshqa_scored_labels) <- names(cshqa_scored)

# merge and organize
v2dat_scored <- merge(v2dat_org, cshqa_scored, by = 'id', all = TRUE)
v2dat_scored <- v2dat_scored[c(1:20, 378:385, 21:377)]

v2dat_scored_labels <- c(v2dat_labels[1:20], cshqa_scored_labels[2:9], v2dat_labels[21:377])

## 7b) score Kids Food Questionnaire data ####
kfq_scored <- score_kfq(kfq_data = v2dat_org[c(1, 60:105)], parID = 'id')

# get labels from scored data and simplify
kfq_scored_labels <- sapply(kfq_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

# make names match because simplify duplicates - not sure why get nested lists
names(kfq_scored_labels) <- names(kfq_scored)

# merge and organize
v2dat_scored <- merge(v2dat_scored, kfq_scored, by = 'id', all = TRUE)
v2dat_scored <- v2dat_scored[c(1:113, 386:403, 114:385)]

v2dat_scored_labels <- c(v2dat_scored_labels[1:113], kfq_scored_labels[2:19], v2dat_scored_labels[114:385])

## 7c) score the Tempest Self-Regulation Questionnaire for Eating ####
tesqe_scored <- score_tesqe(tesqe_data = v2dat_org[c(1, 106:129)], parID = 'id')

# get labels from scored data and simplify
tesqe_scored_labels <- sapply(tesqe_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

# make names match because simplify duplicates - not sure why get nested lists
names(tesqe_scored_labels) <- names(tesqe_scored)

# merge and organize
v2dat_scored <- merge(v2dat_scored, tesqe_scored, by = 'id', all = TRUE)
v2dat_scored <- v2dat_scored[c(1:155, 404:415, 156:403)]

v2dat_scored_labels <- c(v2dat_scored_labels[1:155], tesqe_scored_labels[2:13], v2dat_scored_labels[156:403])

## 7d) score the Children's Eating Behavior Questionnaire ####
cebq_scored <- score_cebq(cebq_data = v2dat_org[c(1, 130:164)], parID = 'id')

# get labels from scored data and simplify
cebq_scored_labels <- sapply(cebq_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

# make names match because simplify duplicates - not sure why get nested lists
names(cebq_scored_labels) <- names(cebq_scored)

# merge and organize
v2dat_scored <- merge(v2dat_scored, cebq_scored, by = 'id', all = TRUE)
v2dat_scored <- v2dat_scored[c(1:202, 416:425, 203:415)]

v2dat_scored_labels <- c(v2dat_scored_labels[1:202], cebq_scored_labels[2:11], v2dat_scored_labels[203:415])

## 7e) score the Binge Eating Scale ####
bes_scored <- score_bes(bes_data = v2dat_org[c(1, 165:180)], parID = 'id')

# get labels from scored data and simplify
bes_scored_labels <- sapply(bes_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

# make names match because simplify duplicates - not sure why get nested lists
names(bes_scored_labels) <- names(bes_scored)

# merge and organize
v2dat_scored <- merge(v2dat_scored, bes_scored, by = 'id', all = TRUE)
v2dat_scored <- v2dat_scored[c(1:228, 426, 229:425)]

v2dat_scored_labels <- c(v2dat_scored_labels[1:228], bes_scored_labels[2], v2dat_scored_labels[229:425])

## 7f) score the Child Feeding Questionnaire ####
cfq_scored <- score_cfq(cfq_data = v2dat_org[c(1, 181:210)], parID = 'id')

# get labels from scored data and simplify
cfq_scored_labels <- sapply(cfq_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

# make names match because simplify duplicates - not sure why get nested lists
names(cfq_scored_labels) <- names(cfq_scored)

# merge and organize
v2dat_scored <- merge(v2dat_scored, cfq_scored, by = 'id', all = TRUE)
v2dat_scored <- v2dat_scored[c(1:259, 427:433, 260:426)]

v2dat_scored_labels <- c(v2dat_scored_labels[1:259], cfq_scored_labels[2:8], v2dat_scored_labels[260:426])

## 7g) score the Family Food Behavior Survey ####
ffbs_scored <- score_ffbs(ffbs_data = v2dat_org[c(1, 211:230)], parID = 'id')

# get labels from scored data and simplify
ffbs_scored_labels <- sapply(ffbs_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

# make names match because simplify duplicates - not sure why get nested lists
names(ffbs_scored_labels) <- names(ffbs_scored)

# merge and organize
v2dat_scored <- merge(v2dat_scored, ffbs_scored, by = 'id', all = TRUE)
v2dat_scored <- v2dat_scored[c(1:286, 434:437, 287:433)]

v2dat_scored_labels <- c(v2dat_scored_labels[1:286], ffbs_scored_labels[2:5], v2dat_scored_labels[287:433])

## 7h) score the Revised Children's Manifest Anxiety Scale ####

#need child grade from parent V4 - HERE

if (isTRUE(qv4_parent_exists)){
    if (isFALSE(parentV4_datestr_arg)) {
        parent_v4dat <- util_parent_v4dat(date_str, data_path)
    } else {
        parent_v4dat <- util_parent_v4dat(parentV4_date_str, data_path)
    }
}

if (isTRUE(qv4_parent_exists)){
    rcmas_data <- merge(v2dat_org[c(1, 231:267)], parent_v4dat$data[c(1, 5)], by = 'id', all.x = TRUE, all.y = FALSE)
    rcmas_data[1:38] <- sjlabelled::set_label(rcmas_data[1:38], label = matrix(unlist(v2dat_scored_labels[c(1, 291:327)], use.names = FALSE)))
    rcmas_data['grade'] <- sjlabelled::set_label(rcmas_data['grade'], label = matrix(unlist(parent_v4dat$dict['grade'], use.names = FALSE)))

    rcmas_scored <- score_rcmas(rcmas_data = rcmas_data, parID = 'id')
} else {
    rcmas_scored <- score_rcmas(rcmas_data = v2dat_org[c(1, 231:267)], parID = 'id')
}

# get labels from scored data and simplify
rcmas_scored_labels <- sapply(rcmas_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

# make names match because simplify duplicates - not sure why get nested lists
names(rcmas_scored_labels) <- names(rcmas_scored)

# merge and organize
v2dat_scored <- merge(v2dat_scored, rcmas_scored, by = 'id', all = TRUE)

if (isTRUE(qv4_parent_exists)){
    v2dat_scored <- merge(v2dat_scored, parent_v4dat$data[c(1, 5)], by = 'id', all.x = TRUE, all.y = FALSE)

    v2dat_scored <- v2dat_scored[c(1:2, 448, 3:327, 438:447, 328:437)]

    v2dat_scored_labels <- c(v2dat_scored_labels[1:2], parent_v4dat$dict['grade'], v2dat_scored_labels[3:327], rcmas_scored_labels[2:11], v2dat_scored_labels[328:437])
} else {
    v2dat_scored <- v2dat_scored[c(1:327, 438:447, 328:437)]

    v2dat_scored_labels <- c(v2dat_scored_labels[1:327], rcmas_scored_labels[2:11], v2dat_scored_labels[328:437])
}

## 7i) score the Child Behavior Questionnaire ####
cbq_scored <- score_cbq(cbq_data = v2dat_org[c(1, 268:361)], parID = 'id')

# get labels from scored data and simplify
cbq_scored_labels <- sapply(cbq_scored, function(x) attributes(x)$label, simplify = TRUE, USE.NAMES = FALSE)

# make names match because simplify duplicates - not sure why get nested lists
names(cbq_scored_labels) <- names(cbq_scored)

# merge and organize
v2dat_scored <- merge(v2dat_scored, cbq_scored, by = 'id', all = TRUE)

# indexing differs by 1 if grade is included
if (isTRUE(qv4_parent_exists)){
    v2dat_scored <- v2dat_scored[c(1:432, 449:466, 433:448)]
    v2dat_scored_labels <- c(v2dat_scored_labels[1:432], cbq_scored_labels[2:19], v2dat_scored_labels[433:448])
} else {
    v2dat_scored <- v2dat_scored[c(1:431, 448:465, 432:447)]
    v2dat_scored_labels <- c(v2dat_scored_labels[1:431], cbq_scored_labels[2:19], v2dat_scored_labels[432:447])
}

#### 8. PNA data #####

# only parent has pna data to organize
v2dat_pna <- parent_v2dat$pna_data

#### 9. save to list #####

# put data in order of participant ID for ease
v2dat_scored <- v2dat_scored[order(v2dat_scored[["id"]]), ]
v2dat_pna <- v2dat_pna[order(v2dat_pna[["id"]]), ]

# set labels
v2dat_scored = sjlabelled::set_label(v2dat_scored, label = matrix(unlist(v2dat_scored_labels, use.names = FALSE)))
v2dat_pna = sjlabelled::set_label(v2dat_pna, label = matrix(unlist(parent_v2dat$pna_dict, use.names = FALSE)))

v2data_list <- list(data = v2dat_scored, dict = v2dat_scored_labels, pna_dat = v2dat_pna, pna_dict = parent_v2dat$pna_dict)

return(v2data_list)
}

