#' fbs_databases: Generate all desired databases for the Food and Brain Study
#'
#' This function will generate up-to-date databases for the Food and Brain Study. The following databases are will be created unless only specific databases are specified:
#' 1) Demographics database: includes child (e.g., age, sex, pubertal status), parent characteristics (e.g., age, sex, education, alcohol use/abuse), and household characteristics (e.g., food insecurity, income)
#' 2) Anthropometrics: includes child and parent height/weight and child adiposity (DXA), actigraphy, physical activity, and sleep
#' 3) Intake: includes child liking ratings, Freddy Fullness, and intake for all eating paradigms
#' 4) Food Related Behaviors and Traits: includes all questionnaires assessing child or parent food related behaviors and/or traits (e.g., CEBQ, CFQ, TFEQ, etc.)
#' 5) (Neuro)Psychological Assessments: includes the WASI and all questionnaires assessing the child's cognitive and/or psychosocial functioning (e.g., BRIEF, BIS/BAS, anxiety, etc.)
#' 6) Delay Discounting: includes the modeling results for delay discounting
#' 7) Interoception: includes all data related to the heart beat interoception task
#' 8) Notes: this is a reference databases that includes any research and/or parent notes or updates from each visit
#' 9) Prefer Not to Answer: this is a reference databases that includes any questions the 'prefer not to answer' option was marked. Only saved in case there is a desire to determine the number of responses on an item that were missing versus 'prefer not to answer'.
#'
#' To process the raw data, the raw databases from Qualtrics MUST follow the naming convention: Child_V1_YYYY-MM-DD.sav, Child_V1_Home_YYY-MM-DD.sav, Child_V1_Lab_YYY-MM-DD.sav, and Parent_V1_YYY-MM-DD.sav. The databases must all be in the SAME directory to be processed if the data_path is not entered and the directory organization does not follow the structure laid out in the DataManual.
#'
#' @inheritParams util_fbs_merge_v1
#' @param v1_date_str (optional) If the date string differs by file for visit 1, enter the following date strings in a list: list(child = child data string, child_home = child home data string, child_lab = child lab data string, parent = parent data string)
#' @param v2_date_str (optional) If the date string differs by file for visit 2, enter the following date strings in a list: list(child = child data string, child_home = child home data string, child_lab = child lab data string, parent = parent data string)
#' @param v3_date_str (optional) If the date string differs by file for visit 3, enter the following date strings in a list: list(child = child data string, child_home = child home data string, child_lab = child lab data string, parent = parent data string)
#' @param v4_date_str (optional) If the date string differs by file for visit 4, enter the following date strings in a list: list(child = child data string, child_home = child home data string, child_lab = child lab data string, parent = parent data string)
#' @param v5_date_str (optional) If the date string differs by file for visit 5, enter the following date strings in a list: list(child = child data string, child_home = child home data string, child_lab = child lab data string, parent = parent data string)
#' @param v6_date_str (optional) If the date string differs by file for visit 6, enter the following date strings in a list: list(child = child data string, parent = parent data string)
#' @param v7_date_str (optional) If the date string differs by file for visit 7, enter the following date strings in a list: list(child = child data string, child_home = child home data string, child_lab = child lab data string, parent = parent data string, parent_home = parent home data string)
#' @inheritParams util_fbs_parent_v1dat
#' @param model_DD Indicate if delay discounting data should be modeled. This will take an addition 3-5 minutes of processing time. Default = FALSE. The Delay Discounting database will only be generate if set to TRUE.
#' @param databases (optional) list of strings to indicate which databases to process. If not entered, all databases will be generated. Options include: 1) 'demo' for Demographic, 2) 'anthro' for Anthropometrics, 3) 'intake' for Intake, 4) 'food_qs' for food-related questionnaires, 5) 'psych_qs' for cognitive and psych related data, 6) 'dd' for Delay Discounting, 7) 'intero' for Interoception data, 8) 'notes' for Notes, and 9) 'pna' for Prefer Not to Answer
#' @param write_dat indicate whether to write databases. Default is TRUE.
#' @param write_path (optional) a string with the path indicating where to save the generated databases if write_dat is TRUE (default option). If no path is given, databases will be written to working directory.
#'
#' @return A list containing all databases that were generated
#'
#' @examples
#' #if in same working directory as data with all data having the same date in filename:
#' fbs_data_proc <- fbs_databases(date_str = '2021-10-11')
#'
#' #if only want the Demographics and Intake databases:
#' fbs_data_proc <- fbs_databases(date_str = '2021-10-11', databases = c('demo', 'intake'))
#'
#' #if in same working directory as data and covid collecte data has different dates for visit 1:
#' #make visit 1 list of date strings:
#' v1_datestr = list(child = '2021-9-15', child_home = '2021-10-11', child_lab = '2021-10-11', parent = '2021-9-15')
#'
#' #run - note: value for date_str will be use for visits 2-7 dates since only visit 1 has a list of date strings specified
#' fbs_data_proc <- fbs_databases(date_str = '2021-10-11', v1_date_str = v1_datestr)
#'
#' \dontrun{
#' #date must be a string. The following will not run:
#' fbs_data_proc <- fbs_databases(2021-10-11)
#'
#' #date must match the file name - for file named 'Child_V1_Home_2021_09_15', the
#' following will not run:
#' fbs_data_proc <- fbs_databases('2021_10_11')
#'
#' #visit specific dates must be saved as strings in a list. The following will NOT work in the function:
#' #make visit 1 list of date strings:
#' v1_datestr = c(child = '2021-9-15', child_home = '2021-10-11', child_lab = '2021-10-11', parent = '2021-9-15')
#' }
#'
#' @seealso Raw data from Qualtrics is processed using the following scripts: \code{\link{util_fbs_merge_v1dat}}, \code{\link{util_fbs_merge_v2dat}}, \code{\link{util_fbs_merge_v3dat}}, \code{\link{util_fbs_merge_v4dat}}, \code{\link{util_fbs_merge_v5dat}}, \code{\link{util_fbs_merge_v6dat}}, \code{\link{util_fbs_merge_v7dat}}
#'
#'
#' @export

fbs_databases <- function(date_str, v1_date_str, v2_date_str, v3_date_str, v4_date_str, v5_date_str, v6_date_str, v7_date_str, data_path, model_DD = FALSE, databases, write_dat = TRUE, write_path) {

    #### 1. Set up/initial checks #####

    # check if date_str exists and is a string

    datestr_arg <- methods::hasArg(date_str)

    if (isTRUE(datestr_arg) & !is.character(date_str)) {
        stop("date_str must be entered as a string: e.g., '2021_10_11'")
    }

    # check for visit specific dates - define universal function
    check_datestr_list <- function(visit_list, visit) {

        if (visit < 6){
            list_databases = c('child', 'child_home', 'child_lab', 'parent')

            if (!is.list(visit_list)) {
                stop(paste0("v", visit, "_date_str must be entered as a list of strings: date_list = list(child = 'date', child_home = 'date', child_lab = 'date', parent = 'date')"))
            } else if (sum(list_databases %in% names(visit_list)) < 4){
                stop(paste0("names of elements in v", visit, "_date_str did not match. The list must follow the format: date_list = list(child = 'date', child_home = 'date', child_lab = 'date', parent = 'date')"))
            } else {
                #check if entrees are strings
                visit_unlist <- unlist(visit_list)
                visit_check_strings <- sapply(visit_unlist, FUN = is.character)

                if (sum(visit_check_strings) < 4) {
                    stop(paste0("Not all entrees in the v", visit, "_date_str list are strings. Must enter all dates as strings (e.g., '2021-12-10')"))
                }
            }
        } else if (visit == 6){
            list_databases = c('child', 'parent')

            if (!is.list(visit_list)) {
                stop("v6_date_str must be entered as a list of strings: date_list = list(child = 'date', parent = 'date')")
            } else if (sum(list_databases %in% names(visit_list)) < 2){
                stop("names of elements in v6_date_str did not match. The list must follow the format: date_list = list(child = 'date', parent = 'date')")
            } else {
                #check if entrees are strings
                visit_unlist <- unlist(visit_list)
                visit_check_strings <- sapply(visit_unlist, FUN = is.character)

                if (sum(visit_check_strings) < 2) {
                    stop("Not all entrees in the v6_date_str list are strings. Must enter all dates as strings (e.g., '2021-12-10')")
                }
            }
        } else if (visit == 7){
            list_databases = c('child', 'child_home', 'child_lab', 'parent', 'parent_home')

            if (!is.list(visit_list)) {
                stop("v7_date_str must be entered as a list of strings: date_list = list(child = 'date', child_home = 'date', child_lab = 'date', parent = 'date', parent_home = 'date')")
            } else if (sum(list_databases %in% names(visit_list)) < 5){
                stop("names of elements in v7_date_str did not match. The list must follow the format: date_list = list(child = 'date', child_home = 'date', child_lab = 'date', parent = 'date', parent_home = 'date')")
            } else {
                #check if entrees are strings
                visit_unlist <- unlist(visit_list)
                visit_check_strings <- sapply(visit_unlist, FUN = is.character)

                if (sum(visit_check_strings) < 5) {
                    stop("Not all entrees in the v7_date_str list are strings. Must enter all dates as strings (e.g., '2021-12-10')")
                }
            }
        }
    }

    ## visit 1
    v1_datestr_arg <- methods::hasArg(v1_date_str)

    if (isTRUE(v1_datestr_arg)) {
        check_datestr_list(v1_date_str, 1)
    }

    ## visit 2
    v2_datestr_arg <- methods::hasArg(v2_date_str)

    if (isTRUE(v2_datestr_arg)) {
        check_datestr_list(v2_date_str, 2)
    }

    ## visit 3
    v3_datestr_arg <- methods::hasArg(v3_date_str)

    if (isTRUE(v3_datestr_arg)) {
        check_datestr_list(v3_date_str, 3)
    }

    ## visit 4
    v4_datestr_arg <- methods::hasArg(v4_date_str)

    if (isTRUE(v4_datestr_arg)) {
        check_datestr_list(v4_date_str, 4)
    }

    ## visit 5
    v5_datestr_arg <- methods::hasArg(v5_date_str)

    if (isTRUE(v5_datestr_arg)) {
        check_datestr_list(v5_date_str, 5)
    }

    ## visit 6
    v6_datestr_arg <- methods::hasArg(v6_date_str)

    if (isTRUE(v6_datestr_arg)) {
        check_datestr_list(v6_date_str, 6)
    }

    ## visit 7
    v7_datestr_arg <- methods::hasArg(v7_date_str)

    if (isTRUE(v7_datestr_arg)) {
        check_datestr_list(v7_date_str, 7)
    }

    # check datapath
    datapath_arg <- methods::hasArg(data_path)

    if (isTRUE(datapath_arg)) {
        if (!is.character(data_path)) {
            stop("data_path must be entered as a string: e.g., '.../Participant_Data/untouchedRaw/")
        }
    }

    # check databases argument
    databases_arg <- methods::hasArg(databases)

    if (isTRUE(model_DD)){
        database_list <- c('demo', 'anthro', 'intake', 'food_qs', 'psych_qs', 'dd', 'intero', 'notes', 'pna')
    } else {
        database_list <- c('demo', 'anthro', 'intake', 'food_qs', 'psych_qs', 'intero', 'notes', 'pna')
    }


    if (isTRUE(databases_arg)) {

        databases_string <- sapply(database_list, FUN = is.character)

        if (sum(databases_string) != ndatabases){
            stop("Not all items listed in databases are strings. All databases must be entered as strings and matach the following options: 'demo', 'anthro', 'intake', 'food_qs', 'psych_qs', 'dd', 'intero', 'notes', 'pna'.")
        }

        #check if entered databases match database_list
        if (sum(databases %in% database_list) != ndatabases){
            stop("Not all items listed in databases match available options. Options include: 'demo', 'anthro', 'intake', 'food_qs', 'psych_qs', 'dd', 'intero', 'notes', 'pna'.")
        }
    }

    # check write_path
    datapath_arg <- methods::hasArg(data_path)

    if (isTRUE(write_dat)) {
        if (!is.character(write_path)) {
            stop("write_path must be entered as a string: e.g., '.../Participant_Data/Databases/")
        }
    }

    #### 2. Get Visit Databases #####

    ## Visit 1 data - need for all databases
    if (isTRUE(v1_datestr_arg)){
        v1_data <- util_fbs_merge_v1(child_date_str = v1_date_str[['child']], child_home_date_str = v1_date_str[['child_home']], child_lab_date_str = v1_date_str[['child_lab']], parent_date_str = v1_date_str[['parent']], data_path = data_path)
    } else {
        v1_data <- util_fbs_merge_v1(date_str = date_str, data_path = data_path)
    }

    ## Visit 2 - need for Anthroprometrics, food/eating behavior, and cog/psych databases
    # requires the V4 parent database so check both v2_datestr_arg and v4_datestr_arg
    if (isFALSE(databases_arg) | 'anthro' %in% database_list | 'food_qs' %in% database_list | 'psych_qs' %in% database_list){

        if (isTRUE(v2_datestr_arg)){
            if (isTRUE(v4_datestr_arg)){
                v2_data <- util_fbs_merge_v2(child_date_str = v2_date_str[['child']], child_home_date_str = v2_date_str[['child_home']], child_lab_date_str = v2_date_str[['child_lab']], parent_date_str = v2_date_str[['parent']], parentV4_date_str = v4_date_str[['parent']], data_path = data_path)
            } else {
                v2_data <- util_fbs_merge_v2(child_date_str = v2_date_str[['child']], child_home_date_str = v2_date_str[['child_home']], child_lab_date_str = v2_date_str[['child_lab']], parent_date_str = v2_date_str[['parent']], parentV4_date_str = date_str, data_path = data_path)
            }

        } else {

            if (isTRUE(v4_datestr_arg)){
                v2_data <- util_fbs_merge_v2(date_str = date_str, parentV4_date_str = v4_date_str[['parent']], data_path = data_path)
            } else {
                v2_data <- util_fbs_merge_v2(date_str = date_str, data_path = data_path)
            }
        }
    }

    ## Visit 3 data - need for the food/eating behavior, cog/psych, and delay discounting databases
    if (isFALSE(databases_arg) | 'food_qs' %in% database_list | 'psych_qs' %in% database_list | 'dd' %in% database_list){
        if (isTRUE(v3_datestr_arg)){
            v3_data <- util_fbs_merge_v3(child_date_str = v3_date_str[['child']], child_home_date_str = v3_date_str[['child_home']], child_lab_date_str = v3_date_str[['child_lab']], parent_date_str = v3_date_str[['parent']], data_path = data_path, model_DD = model_DD)
        } else {
            v3_data <- util_fbs_merge_v3(date_str = date_str, data_path = data_path, model_DD = model_DD)
        }
    }

    ## Visit 4 data - need for the demographics, food/eating behavior, and cog/psych databases
    if (isFALSE(databases_arg) | 'demo' %in% database_list | 'food_qs' %in% database_list | 'psych_qs' %in% database_list){
        if (isTRUE(v4_datestr_arg)){
            v4_data <- util_fbs_merge_v4(child_date_str = v4_date_str[['child']], child_home_date_str = v4_date_str[['child_home']], child_lab_date_str = v4_date_str[['child_lab']], parent_date_str = v4_date_str[['parent']], data_path = data_path)
        } else {
            v4_data <- util_fbs_merge_v4(date_str = date_str, data_path = data_path)
        }
    }

    ## Visit 5 data - need for the demographics database
    if (isFALSE(databases_arg) | 'demo' %in% database_list){
        if (isTRUE(v5_datestr_arg)){
            v5_data <- util_fbs_merge_v5(child_date_str = v5_date_str[['child']], child_home_date_str = v5_date_str[['child_home']], child_lab_date_str = v5_date_str[['child_lab']], parent_date_str = v5_date_str[['parent']], data_path = data_path)
        } else {
            v5_data <- util_fbs_merge_v5(date_str = date_str, data_path = data_path)
        }
    }

    ## Visit 6 data - need for the fMRI database
    if (isTRUE(v6_datestr_arg)){
        v6_data <- util_fbs_merge_v6(child_date_str = v6_date_str[['child']], child_home_date_str = v6_date_str[['child_home']], child_lab_date_str = v6_date_str[['child_lab']], parent_date_str = v6_date_str[['parent']], data_path = data_path)
    } else {
        v6_data <- util_fbs_merge_v6(date_str = date_str, data_path = data_path)
    }

    ## Visit 7 data - need for the demographics, anthroprometrics, food/eating behavior, and cog/psych databases
    if (isFALSE(databases_arg) | 'demo' %in% database_list | 'anthro' %in% database_list | 'food_qs' %in% database_list | 'psych_qs' %in% database_list){
        if (isTRUE(v7_datestr_arg)){
            v7_data <- util_fbs_merge_v7(child_date_str = v7_date_str[['child']], child_home_date_str = v7_date_str[['child_home']], child_lab_date_str = v7_date_str[['child_lab']], parent_date_str = v7_date_str[['parent']], data_path = data_path)
        } else {
            v7_data <- util_fbs_merge_v7(date_str = date_str, data_path = data_path)
        }
    }


    #### 3. Make Databases #####

    #get cross-database variables
    common_demo_data <- v1_data[['data']][c(1:13, 20:21, 60:64, 94:97, 123:124)]
    common_demo_labels <- v1_data[['dict']][c(1:13, 20:21, 60:64, 94:97, 123:124)]

    names(common_demo_data)[2] <- 'v1_date'
    names(common_demo_labels)[2] <- 'v1_date'

    ## 3b) Demographics ####
    if (isFALSE(databases_arg) | 'demo' %in% database_list){

        #visit 1
        v1_demo_data <- v1_data[['data']][c(1:13, 20:21, 60:61, 63, 94:97, 123:124, 14:19, 22:59, 65:87)]
        v1_demo_labels <- v1_data[['dict']][c(1:13, 20:21, 60:61, 63, 94:97, 123:124, 14:19, 22:59, 65:87)]

        names(v1_demo_data)[2] <- 'v1_date'
        names(v1_demo_labels)[2] <- 'v1_date'

        #visit 4
        v4_demo_data <- v4_data[['data']][c(1:2, 6:99)]
        v4_demo_labels <- v4_data[['dict']][c(1:2, 6:99)]

        names(v4_demo_data)[2] <- 'v4_date'
        names(v4_demo_labels)[2] <- 'v4_date'

        #visit 5
        v5_demo_data <- v5_data[['data']][c(1:34)]
        v5_demo_labels <- v5_data[['dict']][c(1:34)]

        names(v5_demo_data)[2] <- 'v5_date'
        names(v5_demo_labels)[2] <- 'v5_date'

        #visit 7
        v7_demo_data <- v7_data[['data']][c(1:2, 5:120)]
        v7_demo_labels <- v7_data[['dict']][c(1:2, 5:120)]

        names(v7_demo_data)[2] <- 'v7_date'
        names(v7_demo_labels)[2] <- 'v7_date'

        #re-name variables with 'v7_' and add 'Visit 7 - ' to labels
        for (v in 3:118){
            #names
            var_name <- names(v7_demo_data)[v]
            v7_name <- paste0('v7_', var_name)
            names(v7_demo_data)[v] <- v7_name

            #labels
            v7_demo_labels[[var_name]] <- paste0('Visit 7 - ', v7_demo_labels[[var_name]])
        }

        #make names match
        names(v7_demo_labels) <- names(v7_demo_data)

        ## merge databases - set all.x = FALSE so only get the participants with data at later visits/have not been screened out
        demo_v1v4_data <- merge(v1_demo_data, v4_demo_data, by = 'id', all.x = FALSE, all.y = TRUE)
        ## other merges - set all = TRUE so get all participants in visits 2-7
        demo_v1v4v5_data <- merge(demo_v1v4_data, v5_demo_data, by = 'id', all = TRUE)
        demo_v1v4v5v7_data <- merge(demo_v1v4v5_data, v7_demo_data, by = 'id', all = TRUE)

        #get labels
        demographic_labels <- c(v1_demo_labels, v4_demo_labels[2:length(v4_demo_labels)], v5_demo_labels[2:length(v5_demo_labels)], v7_demo_labels[2:length(v7_demo_labels)])

        # ensure labels are up to date
        demographic_data = sjlabelled::set_label(demo_v1v4v5v7_data, label = matrix(unlist(demographic_labels, use.names = FALSE)))

    }

    ## 3b) Anthropometrics ####
    if (isFALSE(databases_arg) | 'anthro' %in% database_list){

        #visit 1
        v1_anthro_data <- v1_data[['data']][c(1, 88:226)]
        v1_anthro_labels <- v1_data[['dict']][c(1, 88:226)]

        #visit 2
        v2_anthro_data <- v2_data[['data']][c(1:2, 4:29)]
        v2_anthro_labels <- v2_data[['dict']][c(1:2, 4:29)]

        names(v2_anthro_data)[2] <- 'v2_date'
        names(v2_anthro_labels)[2] <- 'v2_date'

        #visit 7
        v7_anthro_data <- v7_data[['data']][c(1:2, 121:285)]
        v7_anthro_labels <- v7_data[['dict']][c(1:2, 121:285)]

        names(v7_anthro_data)[2] <- 'v7_date'
        names(v7_anthro_labels)[2] <- 'v7_date'

        #re-name variables with 'v7_' and add 'Visit 7 - ' to labels
        for (v in 3:140){
            #names
            var_name <- names(v7_anthro_data)[v]
            v7_name <- paste0('v7_', var_name)
            names(v7_anthro_data)[v] <- v7_name

            #labels
            v7_anthro_labels[[var_name]] <- paste0('Visit 7 - ', v7_anthro_labels[[var_name]])
        }

        #make names match
        names(v7_anthro_labels) <- names(v7_anthro_data)

        ## merge databases from v1
        anthro_demov1_data <- merge(common_demo_data[1:20], v1_anthro_data, by = 'id', all = TRUE)

        ## merge databases - set all.x = FALSE so only get the participants with data at later visits/have not been screened out
        anthro_demov1v2_data <- merge(anthro_demov1_data, v2_anthro_data, by = 'id', all.x = FALSE, all.y = TRUE)

        ## other merges - set all = TRUE so get all participants in visits 2-7
        anthro_demov1v2v7_data <- merge(anthro_demov1v2_data, v7_anthro_data, by = 'id', all = TRUE)

        #get labels
        anthrographic_labels <- c(common_demo_labels[1:20], v1_anthro_labels[2:length(v1_anthro_labels)], v2_anthro_labels[2:length(v2_anthro_labels)], v7_anthro_labels[2:length(v7_anthro_labels)])

        # ensure labels are up to date
        anthrographic_data = sjlabelled::set_label(anthro_demov1v2v7_data, label = matrix(unlist(anthrographic_labels, use.names = FALSE)))

    }

    ## 3b) Intake ####
    if (isFALSE(databases_arg) | 'intake' %in% database_list){

        #visit 1
        v1_intake_data <- v1_data[['data']][c(1, 227:395)]
        v1_intake_labels <- v1_data[['dict']][c(1, 227:395)]

        #re-name variables with 'v1_' and add 'Visit 1 - ' to labels
        for (v in 2:170){
            #names
            var_name <- names(v1_intake_data)[v]
            v1_name <- paste0('v1_', var_name)
            names(v1_intake_data)[v] <- v1_name

            #labels
            v1_intake_labels[[var_name]] <- paste0('Visit 1 - ', v1_intake_labels[[var_name]])
        }

        names(v1_intake_labels) <- names(v1_intake_data)

        #visit 2
        v2_intake_data <- v2_data[['data']][c(1:2, 30:76)]
        v2_intake_labels <- v2_data[['dict']][c(1:2, 30:76)]

        names(v2_intake_data)[2] <- 'v2_date'
        names(v2_intake_labels)[2] <- 'v2_date'

        #re-name variables with 'v2_' and add 'Visit 2 - ' to labels
        for (v in 3:49){
            #names
            var_name <- names(v2_intake_data)[v]
            v2_name <- paste0('v2_', var_name)
            names(v2_intake_data)[v] <- v2_name

            #labels
            v2_intake_labels[[var_name]] <- paste0('Visit 2 - ', v2_intake_labels[[var_name]])
        }

        names(v2_intake_labels) <- names(v2_intake_data)

        #visit 3
        v3_intake_data <- v3_data[['data']][1:49]
        v3_intake_labels <- v3_data[['dict']][1:49]

        names(v3_intake_data)[2] <- 'v3_date'
        names(v3_intake_labels)[2] <- 'v3_date'

        #re-name variables with 'v3_' and add 'Visit 3 - ' to labels
        for (v in 3:49){
            #names
            var_name <- names(v3_intake_data)[v]
            v3_name <- paste0('v3_', var_name)
            names(v3_intake_data)[v] <- v3_name

            #labels
            v3_intake_labels[[var_name]] <- paste0('Visit 3 - ', v3_intake_labels[[var_name]])
        }

        names(v3_intake_labels) <- names(v3_intake_data)

        #visit 4
        v4_intake_data <- v4_data[['data']][c(1:2, 100:146)]
        v4_intake_labels <- v4_data[['dict']][c(1:2, 100:146)]

        names(v4_intake_data)[2] <- 'v4_date'
        names(v4_intake_labels)[2] <- 'v4_date'

        #re-name variables with 'v4_' and add 'Visit 4 - ' to labels
        for (v in 3:49){
            #names
            var_name <- names(v4_intake_data)[v]
            v4_name <- paste0('v4_', var_name)
            names(v4_intake_data)[v] <- v4_name

            #labels
            v4_intake_labels[[var_name]] <- paste0('Visit 4 - ', v4_intake_labels[[var_name]])
        }

        names(v4_intake_labels) <- names(v4_intake_data)

        #visit 5
        v5_intake_data <- v5_data[['data']][c(1:2, 35:81)]
        v5_intake_labels <- v5_data[['dict']][c(1:2, 35:81)]

        names(v5_intake_data)[2] <- 'v5_date'
        names(v5_intake_labels)[2] <- 'v5_date'

        #re-name variables with 'v5_' and add 'Visit 5 - ' to labels
        for (v in 3:49){
            #names
            var_name <- names(v5_intake_data)[v]
            v5_name <- paste0('v5_', var_name)
            names(v5_intake_data)[v] <- v5_name

            #labels
            v5_intake_labels[[var_name]] <- paste0('Visit 5 - ', v5_intake_labels[[var_name]])
        }

        names(v5_intake_labels) <- names(v5_intake_data)

        #visit 7
        v7_intake_data <- v7_data[['data']][c(1:2, 286:458)]
        v7_intake_labels <- v7_data[['dict']][c(1:2, 286:458)]

        names(v7_intake_data)[2] <- 'v7_date'
        names(v7_intake_labels)[2] <- 'v7_date'

        #re-name variables with 'v7_' and add 'Visit 7 - ' to labels
        for (v in 3:175){
            #names
            var_name <- names(v7_intake_data)[v]
            v7_name <- paste0('v7_', var_name)
            names(v7_intake_data)[v] <- v7_name

            #labels
            v7_intake_labels[[var_name]] <- paste0('Visit 7 - ', v7_intake_labels[[var_name]])
        }

        #make names match
        names(v7_intake_labels) <- names(v7_intake_data)

        ## merge databases from v1
        intake_demov1_data <- merge(common_demo_data, v1_intake_data, by = 'id', all = TRUE)

        ## merge databases - set all.x = FALSE so only get the participants with data at later visits/have not been screened out
        intake_demov1v2_data <- merge(intake_demov1_data, v2_intake_data, by = 'id', all.x = FALSE, all.y = TRUE)

        ## other merges - set all = TRUE so get all participants in visits 2-7
        intake_demov1v2v3_data <- merge(intake_demov1v2_data, v3_intake_data, by = 'id', all.x = FALSE, all.y = TRUE)
        intake_demov1v2v3v4_data <- merge(intake_demov1v2v3_data, v4_intake_data, by = 'id', all.x = FALSE, all.y = TRUE)
        intake_demov1v2v3v4v5_data <- merge(intake_demov1v2v3v4_data, v5_intake_data, by = 'id', all.x = FALSE, all.y = TRUE)
        intake_demov1v23v4v5v7_data <- merge(intake_demov1v2v3v4v5_data, v7_intake_data, by = 'id', all.x = FALSE, all.y = TRUE)

        #get labels
        intake_labels <- c(common_demo_labels, v1_intake_labels[2:length(v1_intake_labels)], v2_intake_labels[2:length(v2_intake_labels)], v3_intake_labels[2:length(v3_intake_labels)], v4_intake_labels[2:length(v4_intake_labels)], v5_intake_labels[2:length(v5_intake_labels)], v7_intake_labels[2:length(v7_intake_labels)])

        # ensure labels are up to date
        intake_data = sjlabelled::set_label(intake_demov1v23v4v5v7_data, label = matrix(unlist(intake_labels, use.names = FALSE)))

    }

    #### 9. PNA data #####

    # only parent has pna data to organize
    v1dat_pna <- parent_v1dat$pna_data

    #### 10. save to list #####

    # put data in order of participant ID for ease
    v1dat_scored <- v1dat_scored[order(v1dat_scored[["id"]]), ]
    v1dat_pna <- v1dat_pna[order(v1dat_pna[["id"]]), ]

    # set labels
    v1dat_scored = sjlabelled::set_label(v1dat_scored, label = matrix(unlist(v1dat_scored_labels, use.names = FALSE)))
    v1dat_pna = sjlabelled::set_label(v1dat_pna, label = matrix(unlist(parent_v1dat$pna_dict, use.names = FALSE)))

    v1data_list <- list(data = v1dat_scored, dict = v1dat_scored_labels, pna_dat = v1dat_pna, pna_dict = parent_v1dat$pna_dict)

    return(v1data_list)
}

