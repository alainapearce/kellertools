#' util_merge_v6: Merge all Visit 6 raw Qualtrics databases and organize variable in database order
#'
#' This function merges the following visit 6 raw data into a single database and organizes variables in database order: child visit 6, child visit 6-home, child visit 6-lab, and parent visit 6
#'
#' The databases MUST follow the naming convention: Child_V6_YYYY-MM-DD.sav, Child_V6_Home_YYY-MM-DD.sav, Child_V6_Lab_YYY-MM-DD.sav, and Parent_V6_YYY-MM-DD.sav. The databases must all be in the SAME directory to be processed if the data_path is not entered. If it is entered, it must follow
#'
#' @inheritParams util_merge_v1
#' @inheritParams util_merge_v1
#' @inheritParams util_merge_v1
#' @inheritParams util_parent_v1dat
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from parent visit 6 Qualtrics and 2) dict: all variable descriptions
#'
#' @examples
#' #if in same working directory as data:
#' v6dat_scored <- util_merge_v6(date_str = '2021-10-11')
#'
#' #if in same working directory as data and covid collecte data has different dates:
#' v6dat_scored <- util_merge_v6(child_date_str = '2021-10-11', child_home_date_str = '2021-9-15', child_lab_date_str = = '2021-9-15', parent_date_str = '2021-10-11')
#'
#' \dontrun{
#' #date must be a string. The following will not run:
#' v6dat_scored <- util_merge_v6(2021-10-11)
#'
#' #date must match the file name - for file named 'Child_V6_Home_2021_09_15', the
#' following will not run:
#' v6dat_scored <- util_merge_v6('2021_10_11')
#' }
#'
#' @seealso Raw data from Qualtrics is processed using the following scripts: \code{\link{util_child_v6dat}}, \code{\link{util_parent_v6dat}}. Visit 6 data is scored using the following scripts: \code{\link{score_mrivas}}.
#'
#'
#' @export

util_merge_v6 <- function(date_str, child_date_str, parent_date_str, data_path) {

    #### 1. Set up/initial checks #####

    # check if date_str exist and is a string

    datestr_arg <- methods::hasArg(date_str)

    if (isTRUE(datestr_arg) & !is.character(date_str)) {
        stop("date_str must be enter as a string: e.g., '2021_10_11'")
    } else if (isFALSE(datestr_arg)) {

        # if no date_str, check all databases specific date strings
        child_datestr_arg <- methods::hasArg(child_date_str)
        parent_datestr_arg <- methods::hasArg(parent_date_str)

        if (sum(child_datestr_arg, child_home_datestr_arg, child_lab_datestr_arg, parent_datestr_arg) < 2){
            stop("if data_str is not set, then must enter each individual date string for the visit 6 databeses: child, child-home, child-lab, and parent")
        }

        if (!is.character(child_date_str) | !is.character(parent_date_str)) {
            stop("all dates must be enter as a string: e.g., '2021_10_11'")
        }
    }

    # check that file exists
    datapath_arg <- methods::hasArg(data_path)

    if (isTRUE(datapath_arg)) {
        if (!is.character(data_path)) {
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
                qv6_path <- paste0(data_path, "/", respondant, "_V6_", date_str, ".sav")
            } else {
                qv6_path <- paste0("/", respondant, "_V6_", date_str, ".sav")
            }

            # check if file exists
            qv6_exists <- file.exists(qv6_path)

            #warning message if doesn't exist - stop is below outside of function
            if (isFALSE(qv6_exists)) {
                warning(paste0("The ", respondant, "_V6_", date_str, ".sav database does not exist at the specified path:", qv6_path))
            }

            #return check
            qv6_res <- list(data_path_full = qv6_path, data_exists = qv6_exists)

            return(qv6_res)
        }

        #if covid split protocol
        if (isTRUE(covid_type)) {

            if (isTRUE(datapath_arg_fn)) {
                qv6_path <- paste0(data_path, "/Final_CovidAtHome/", respondant, "_V6_", loc, "_", date_str, ".sav")
            } else {
                qv6_path <- paste0("/Final_CovidAtHome/", respondant, "_V6_", loc, "_", date_str, ".sav")
            }

            # check if file exists
            qv6_exists <- file.exists(qv6_path)

            if (isTRUE(qv6_exists)) {
                #return check
                qv6_res <- list(data_path_full = qv6_path, data_exists = qv6_exists)

                return(qv6_res)
            } else {

                #check if in the main database rather than 'Final_CovidAtHome' database
                if (isTRUE(datapath_arg_fn)) {
                    qv6_path2 <- paste0(data_path, "/", respondant, "_V6_", loc, "_", date_str, ".sav")
                } else {
                    qv6_path2 <- paste0("/", respondant, "_V6_", loc, "_", date_str, ".sav")
                }

                # check if file exists
                qv6_exists2 <- file.exists(qv6_path2)

                #warning message if doesn't exist - stop is below outside of function
                if (isFALSE(qv6_exists2)) {
                    warning(paste0("The ", respondant, "_V6_", loc, "_", date_str, ".sav database does not exist at either of the possible paths:", qv6_path, "or", qv6_path2))
                }

                #return check
                qv6_res <- list(data_path_full = qv6_path2, data_exists = qv6_exists2)

                return(qv6_res)
            }
        }
    }

    # check if files exist
    if (isTRUE(datestr_arg)){
        child_file <- file_exist_fn(data_path, date_str, respondant = 'Child')
        parent_file <- file_exist_fn(data_path, date_str, respondant = 'Parent')
    } else {
        child_file <- file_exist_fn(data_path, child_date_str, respondant = 'Child')
        parent_file <- file_exist_fn(data_path, parent_date_str, respondant = 'Parent')
    }

    if (sum(child_file$data_exists, parent_file$data_exists) < 2){
        stop('not all files exist - double check all files are in the correct directories and that the entered *date_str and data_path arguments are entered correctly')
    }


    #### 3. Process Raw Data #####

    if (isTRUE(datestr_arg)) {
        child_v6dat <- util_child_v6dat(date_str, data_path)
        parent_v6dat <- util_parent_v6dat(date_str, data_path)
    } else {
        child_v6dat <- util_child_v6dat(child_date_str, data_path)
        parent_v6dat <- util_parent_v6dat(parent_date_str, data_path)
    }

    #### 4. Merge Child and Parent Raw Data #####

    # update labels with 'parent report'

    for (v in 1:ncol(parent_v6dat$data)) {
        var_name <- names(parent_v6dat$data)[v]

        # remove existing label
        if (grepl("parent-reported", parent_v6dat$dict[[var_name]], fixed = TRUE)) {
            parent_v6dat$dict[[var_name]] <- gsub("parent-reported", "", parent_v6dat$dict[[var_name]])
        }

        # add universal label
        parent_v6dat$dict[[var_name]] <- paste0('Parent Reported: ', parent_v6dat$dict[[var_name]])
    }

    v6dat <- merge(child_v6dat$data, parent_v6dat$data[c(1, 3:18)], by = 'id', all = TRUE)

    # merge labels/dictionary
    v6dat_labels <- c(child_v6dat$dict, parent_v6dat$dict[3:18])

    # ensure labels are up to date
    v6dat = sjlabelled::set_label(v6dat, label = matrix(unlist(v6dat_labels, use.names = FALSE)))

    #### 5. save to list #####

    # put data in order of participant ID for ease
    v6dat <- v6dat[order(v6dat[["id"]]), ]

    # set labels
    v6dat = sjlabelled::set_label(v6dat, label = matrix(unlist(v6dat_labels, use.names = FALSE)))

    v6data_list <- list(data = v6dat, dict = v6dat_labels)

    return(v6data_list)
}

