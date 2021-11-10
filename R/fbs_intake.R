#' fbs_kcal_intake: Calculate the amount served and eaten in kcals for the Food and Brain Study
#'
#' This function caluclates the amount served and eaten in kcals for the Food and Brain Study using food energy density and measured gram weights. This is done for all meals included in the study: baseline meal + eating in the absence of hunger (EAH), the 4 portion size meals, and follow-up meal + EAH
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data individual food weights for the amount served and the amount remaining after the meal
#' 2) The columns/variables in data must match the following naming conventions for each meal:
#' 2a) baseline/follow-up meal: 'sex', 'pds_1', 'pds_2', 'pds_3', 'pds_4m', 'pds_5m', 'pds_4f', 'pds_5fa'.
#' 2b) baseline/follow-up EAH:
#' 2c) portion size meals
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#'
#' @param intake_data a data.frame including grams served for each food item following the naming conventions described above
#' @param meal string indicating which meals are included in intake_data: 'EAH' - eating in the absence of hunger paradigm, 'pre_meal' - baseline/follow-up before EAH; 'PS_meal' - portion size meal
#' @param visit Visit number the meal occured on
#' @param portion_size (optional) for meal 'PS_meal', enter the portion size
#' @param parID (optional) name of participant ID column in intake_data. If included the output dataset will be matched by parID, if not included the output dataset will be in the order of intake_data but will have no participant identifier.
#'
#' @return A dataset with a kcal served and consumed calculated.
#'
#' @examples
#' #default male/female
#' intake_data <- intake_data(intake_data, respondent = 'parent', parID = 'ID')
#'
#' #specify male/female
#' fbs_kcal_intake <- intake_data(intake_data, respondent = 'parent', male = 'male', female = 'female', parID = 'ID')
#'
#' \dontrun{
#' }
#'
#' @seealso For the Food and Brain Study, raw data from Qualtrics was processed using \code{\link{qualtrics_child_v1dat}}, \code{\link{qualtrics_parent_v2dat}}, \code{\link{qualtrics_parent_v3dat}}, \code{\link{qualtrics_parent_v4dat}}, \code{\link{qualtrics_parent_v5dat}}, \code{\link{qualtrics_parent_v7dat}}
#'
#' @export

fbs_kcal_intake <- function(intake_data, meal, visit, portion_size, parID) {

    #### 1. Set up/initial checks #####

    # check that pds_dat exist and is a data.frame
    data_arg <- methods::hasArg(intake_data)

    if (isTRUE(data_arg) & !is.data.frame(intake_data)) {
        stop("pds_dat must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("pds_dat must set to the data.frame with all responses to the Pubertal Development Scale")
    }

    #make a working dataset to preserve original data
    intake_data_edits <- intake_data

    # check that respondent exist and is a string
    resp_arg <- methods::hasArg(respondent)

    if (isTRUE(resp_arg)){
        if (!is.character(respondent)) {
        stop("respondent must be entered as a string and can either be 'parent' or 'child'")
        } else if (respondent != 'parent' | respondent != 'child') {
            stop("the optional values for respondent are: 'parent' or 'child'")
        }
    } else if (isFALSE(resp_arg)) {
        stop("respondent must be entered as a string and can either be 'parent' or 'child'")
    }

    # check varaible 'sex' exists and for male and female arguments
    male_arg <- methods::hasArg(male)
    female_arg <- methods::hasArg(female)

    #check number of unique values in dataset
    nsex_unique <- length(unique(intake_data_edits[['sex']]))

    if(!('sex' %in% names(intake_data_edits))){
        stop("There is no variable 'sex' in intake_data")
    } else {
        #entered arguments match number of different sexes in data
        if (isTRUE(male_arg) | isTRUE(female_arg)){
            if (sum(isTRUE(male_arg), isTRUE(female_arg)) == nsex_unique) {
                if (nsex_unique == 2) {
                    #change values of sex in intake_data to default
                    intake_data_edits[['sex']] <- ifelse(intake_data_edits[['sex']] == male, 0, 1)
                    intake_data_edits[['sex']] <- factor(intake_data_edits[['sex']], levels = c(0, 1))
                } else if (nsex_unique == 1 & isTRUE(male_arg)){
                    # if only 1 value for sex and male is specified, set default to 0
                    intake_data_edits[['sex']] <- 0
                } else if (nsex_unique == 1 & isTRUE(female_arg)){
                    # if only 1 value for sex and female is specified, set default to 1
                    intake_data_edits[['sex']] <- 1
                }
            } else {
                stop("The number of alternate values entered for sex do not match the number of differen sexes in data")
            }
        } else if (sum(isTRUE(male_arg), isTRUE(female_arg)) == 0) {
            intake_data_edits[['sex']] <- factor(intake_data_edits[['sex']])
        }
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (!(parID %in% names(intake_data_edits))) {
        stop("variable name entered as parID is not in intake_data")
    }

    # check variables in intake_data

    ##standard variable names
    pds_varnames <- c('pds_1', 'pds_2', 'pds_3', 'pds_4m', 'pds_5m', 'pds_4f', 'pds_5fa')

    if (sum(pds_varnames[1:3] %in% names(intake_data_edits)) < 3){
        stop("Not all required variable are in intake_data or variable names match: 'pds_1', 'pds_2', 'pds_3'")
    }

    ##determine single or mixed sex
    sex_levels <- levels(intake_data_edits[['sex_default']])

    #check male variable names
    if (length(sex_nlevels) == 2 | sex_levels == "0"){
        if (sum(pds_varnames[4:5] %in% names(intake_data_edits)) < 2){
            stop("The dataset contains data from males - Not all required male variable names are in intake_data or not all variable names match required namining: 'pds_4m', 'pds_5m'")
        }
    }

    #check female variable names
    if (length(sex_nlevels) == 2 | sex_levels == "1"){
        if (sum(pds_varnames[6:7] %in% names(intake_data_edits)) < 2){
            stop("The dataset contains data from female - Not all required female variable names are in intake_data or not all variable names match required namining: 'pds_4f', 'pds_5fa'")
        }
    }

    #### 2. Set Up Data #####

    #check if variables are coded in appropriate range and change 99/'I Don't Know' to NA
    for (var in 1:length(pds_varnames)){
        var_name <- as.character(pds_varnames[var])

        if (var_name %in% names(intake_data_edits)){
            #convert 'I Don't Know'/99
            intake_data_edits[[var_name]] <- ifelse(intake_data_edits[[var_name]] == 99, NA, intake_data_edits[[var_name]])

            #check range of values
            if (min(intake_data_edits[[var_name]], na.rm = TRUE) < 1 & max(intake_data_edits[[var_name]], na.rm = TRUE) > 4){
                stop("coded level values should fall between the values 1 and 4")
            }

            #recode menarche
            if (var_name == 'pds_5fa'){
                intake_data_edits[[var_name]] <- ifelse(is.na(intake_data_edits[[var_name]]), NA,
                                                     ifelse(intake_data_edits[[var_name]] == 1, 4, 0))
            }
        }
    }

    #### 3. Score Data #####
    #pds_score
    male_vars <- pds_varnames[c(1:5)]
    female_vars <- pds_varnames[c(1:3, 6:7)]

    ## number of NAs
    intake_data_edits[['pds_score_na']] <- ifelse(intake_data_edits[['sex']] == 0, rowSums(is.na(intake_data_edits[male_vars])), rowSums(is.na(intake_data_edits[female_vars])))

    intake_data_edits[['pds_score']] <- ifelse(intake_data_edits[['sex']] == 0,
                                            ifelse(intake_data_edits[['pds_score_na']] <= 1, rowMeans(intake_data_edits[male_vars], na.rm = TRUE), NA),
                                            ifelse(intake_data_edits[['pds_score_na']] <= 1 & !is.na( intake_data_edits[['pds_5fa']]), rowMeans(intake_data_edits[female_vars], na.rm = TRUE), NA))

    #tanner category
    male_tanner_vars <- pds_varnames[c(2, 4:5)]
    female_tanner_vars <- pds_varnames[c(2, 6)]

    intake_data_edits[['pds_tanner_sum']] <- ifelse(intake_data_edits[['sex']] == 0, rowSums(intake_data_edits[male_tanner_vars]), rowSums(intake_data_edits[female_tanner_vars]))

    intake_data_edits[['pds_tanner_cat']] <- ifelse(is.na(intake_data_edits[['pds_tanner_sum']]), NA,
        ifelse(intake_data_edits[['sex']] == 0,
               ifelse(intake_data_edits[['pds_tanner_sum']] == 12, 'Postpubertal',
                      ifelse(intake_data_edits[['pds_tanner_sum']] >=9 , 'Late Pubertal',
                             ifelse(intake_data_edits[['pds_tanner_sum']] >=6, ifelse(intake_data_edits['pds_2'] < 4 & intake_data_edits['pds_4m'] < 4 & intake_data_edits['pds_5m'] < 4, 'Mid-Pubertal', 'Late Pubertal'),
                                    ifelse(intake_data_edits[['pds_tanner_sum']] >=4, ifelse(intake_data_edits['pds_2'] < 3 & intake_data_edits['pds_4m'] < 3 & intake_data_edits['pds_5m'] < 3, 'Early Pubertal', 'Mid-Pubertal'), 'Prepubertal')))),
               ifelse(intake_data_edits[['pds_5fa']] == 4,
                      ifelse(intake_data_edits[['pds_tanner_sum']] == 8, 'Postpubertal',
                             ifelse(intake_data_edits[['pds_tanner_sum']] <= 7, 'Late Pubertal', NA)),
                      ifelse(intake_data_edits[['pds_tanner_sum']] > 3, 'Mid-Pubertal',
                             ifelse(intake_data_edits[['pds_tanner_sum']] == 3, 'Early Pubertal',
                                    ifelse(intake_data_edits[['pds_tanner_sum']] == 2, 'Prepubertal', NA))))))


    #### 3. Clean Export/Scored Data #####

    #make results dataset
    if (isTRUE(ID_arg)){
        pds_score_dat <- data.frame(id = intake_data_edits[[parID]],
                                    sex = intake_data_edits[['sex']],
                                    pds_score = intake_data_edits[['pds_score']],
                                    pds_tanner_cat = intake_data_edits[['pds_tanner_cat']])
    } else {
        pds_score_dat <- data.frame(sex = intake_data_edits[['sex']],
                                    pds_score = intake_data_edits[['pds_score']],
                                    pds_tanner_cat = intake_data_edits[['pds_tanner_cat']])
    }

    #set up labels for pds_score_dat
    pds_score_dat_labels <- lapply(pds_score_dat, function(x) attributes(x)$label)

    if (isTRUE(ID_arg)){
        pds_score_dat_labels[['id']] <- 'Participant ID'
    }

    pds_score_dat_labels[['sex']] <- 'Child sex coded as male = 0, female = 1'
    pds_score_dat_labels[['pds_score']] <- 'Pubertal Development Scale score: average of all responses for each sex with menarche yes = 4 points and menarche no = 1 point'
    pds_score_dat_labels[['pds_tanner']] <- 'Tanner equivaluent category'

    ## add attributes to for tanner category to data
    pds_score_dat[['pds_tanner_cat']] <- sjlabelled::add_labels(pds_score_dat[['pds_tanner_cat']], labels = c('Prepubertal' = 1, 'Early Puberty' = 2, 'Mid-Puberty' = 3, 'Late Puberty' = 4, 'Postpubertal' = 5))
    pds_score_dat[['pds_tanner_cat']] <- sjlabelled::as_numeric(pds_score_dat[['pds_tanner_cat']])

    ##set names based on respondent
    if (respondent == 'child'){
        child_names <-
        if (isTRUE(ID_arg)){
            names(pds_score_dat)[3:4] <- c('pds_score_self', 'pds_tanner_self')
            names(pds_score_dat_labels)[2:3] <- c('pds_score_self', 'pds_tanner_self')
        } else {
            names(pds_score_dat)[2:3] <- c('pds_score_self', 'pds_tanner_self')
            names(pds_score_dat_labels)[2:3] <- c('pds_score_self', 'pds_tanner_self')
        }
    }

    ## make sure the variable labels match in the dataset
    pds_score_dat = sjlabelled::set_label(pds_score_dat, label = matrix(unlist(pds_score_dat_labels,
        use.names = FALSE)))

    return(pds_score_dat)
}
