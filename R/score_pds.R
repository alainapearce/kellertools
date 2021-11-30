#' score_pds: Scored data from Parent- or Self-Report Pubertal Development Scale
#'
#' This function scores the the Child- and Parent-Pubertal Development Scale. The pubertal development score (pds_score) is an average of all questions, ignoring any 'I Don't Know' responses. Following the unpublished manuscript guidelines by Crockett, L. J. (1988), the Tanner Stage equivalent is computed by sex: Males - on the sum of the scores for body hair growth, voice change, and facial hair growth; Females - sum of scores for body hair growth and breast development along with the binary response to menarche (yes/no). For males, 1 of the 3 items is allowed to be 'I Don't Know' while for females, 1 of the 2 growth questions was allowed to be 'I Don't Know' but response to menarche was required.
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must included a 'sex' variable (defualt: male = 0, female = 1)
#' 2) The following questions are required from the Pubertal Development Scale: questions 1-3 and the sex-specific versions of questions 4-5. Additional variables are allowed in the provided data so long as the above naming conventions below are followed for the questions necessary to score the assessment.
#' 3) The data set columns/variables must match the following naming convention: 'sex', 'pds_1', 'pds_2', 'pds_3', 'pds_4m', 'pds_5m', 'pds_4f', 'pds_5fa'.
#' 4) All variable should be converted to numeric values such that 1 = not started yet, 2 = barely started, 3 = definitely started, 4 = seems complete, and 99 = I Don't Know. For Female ('pds_5fa') question on menarche, the response can be coded as Yes = 1, No = 0.
#'
#' Note, as long as variable names match those listed, pds_data can include only data from male and/or female version scales.
#'
#' Primary References for the Pubertal Development Scale and Scoring:
#' Carskadon, Mary A., and Christine Acebo. 'A self-administered rating scale for pubertal development.' Journal of Adolescent Health 14, no. 3 (1993): 190-195. https://doi.org/10.1016/1054-139X(93)90004-9
#'
#' Crockett, L. J. 'Pubertal development scale: Pubertal categories.' Unpublished manuscript (1988).
#'
#'
#' @param pds_data a data.frame with child sex and all questions from the Pubertal Development Scale. The required data to score the Pubertal Development Scale include sex, questions 1-3, and male/female specific questions 4-5. These questions must have the following names in the dataset: 'sex', 'pds_1', 'pds_2', 'pds_3', 'pds_4m', 'pds_5m', 'pds_4f', 'pds_5fa'. Note, as long as variable names match those listed, pds_data can include only data from male and/or female version scales.
#' @param respondent string indicate if parent or child completed the Pubertal Development Scale. Must enter either 'parent' or 'child'
#' @param male (optional) value for male. Default value is male = 0
#' @param female (optional) value for female. Default value is female = 1
#' @inheritParams fbs_intake
#'
#' @return A dataset with a Pubertal Development Score and the Tanner Stage equivalents.
#'
#' @examples
#' #default male/female
#' pds_tanner_data <- pds_data(pds_data, respondent = 'parent', parID = 'ID')
#'
#' #specify male/female
#' pds_tanner_data <- pds_data(pds_data, respondent = 'parent', male = 'male', female = 'female', parID = 'ID')
#'
#' \dontrun{
#' }
#'
#' @seealso For the Food and Brain Study, raw data from Qualtrics was processed using \code{\link{qualtrics_parent_v1dat}}
#'
#' @export

score_pds <- function(pds_data, respondent, male = 0, female = 1, parID) {

    #### 1. Set up/initial checks #####

    # check that pds_dat exist and is a data.frame
    data_arg <- methods::hasArg(pds_data)

    if (isTRUE(data_arg) & !is.data.frame(pds_data)) {
        stop("pds_dat must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("pds_dat must set to the data.frame with all responses to the Pubertal Development Scale")
    }

    # make a working dataset to preserve original data
    pds_data_edits <- pds_data

    # check that respondent exist and is a string
    resp_arg <- methods::hasArg(respondent)

    if (isTRUE(resp_arg)) {
        if (!is.character(respondent)) {
            stop("respondent must be entered as a string and can either be 'parent' or 'child'")
        } else if (respondent != "parent" | respondent != "child") {
            stop("the optional values for respondent are: 'parent' or 'child'")
        }
    } else if (isFALSE(resp_arg)) {
        stop("respondent must be entered as a string and can either be 'parent' or 'child'")
    }

    # check varaible 'sex' exists and for male and female arguments
    male_arg <- methods::hasArg(male)
    female_arg <- methods::hasArg(female)

    # check number of unique values in dataset
    nsex_unique <- length(unique(pds_data_edits[["sex"]]))

    if (!("sex" %in% names(pds_data_edits))) {
        stop("There is no variable 'sex' in pds_data")
    } else {
        # entered arguments match number of different sexes in data
        if (isTRUE(male_arg) | isTRUE(female_arg)) {
            if (sum(isTRUE(male_arg), isTRUE(female_arg)) == nsex_unique) {
                if (nsex_unique == 2) {
                  # change values of sex in pds_data to default
                  pds_data_edits[["sex"]] <- ifelse(pds_data_edits[["sex"]] ==
                    male, 0, 1)
                  pds_data_edits[["sex"]] <- factor(pds_data_edits[["sex"]], levels = c(0,
                    1))
                } else if (nsex_unique == 1 & isTRUE(male_arg)) {
                  # if only 1 value for sex and male is specified, set default to 0
                  pds_data_edits[["sex"]] <- 0
                } else if (nsex_unique == 1 & isTRUE(female_arg)) {
                  # if only 1 value for sex and female is specified, set default to 1
                  pds_data_edits[["sex"]] <- 1
                }
            } else {
                stop("The number of alternate values entered for sex do not match the number of differen sexes in data")
            }
        } else if (sum(isTRUE(male_arg), isTRUE(female_arg)) == 0) {
            pds_data_edits[["sex"]] <- factor(pds_data_edits[["sex"]])
        }
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (isTRUE(ID_arg)){
        if (!(parID %in% names(pwlb_data))) {
            stop("variable name entered as parID is not in pwlb_data")
        }
    }

    # check variables in pds_data

    ## standard variable names
    pds_varnames <- c("pds_1", "pds_2", "pds_3", "pds_4m", "pds_5m", "pds_4f",
        "pds_5fa")

    if (sum(pds_varnames[1:3] %in% names(pds_data_edits)) < 3) {
        stop("Not all required variable are in pds_data or variable names match: 'pds_1', 'pds_2', 'pds_3'")
    }

    ## determine single or mixed sex
    sex_levels <- levels(pds_data_edits[["sex_default"]])

    # check male variable names
    if (length(sex_nlevels) == 2 | sex_levels == "0") {
        if (sum(pds_varnames[4:5] %in% names(pds_data_edits)) < 2) {
            stop("The dataset contains data from males - Not all required male variable names are in pds_data or not all variable names match required namining: 'pds_4m', 'pds_5m'")
        }
    }

    # check female variable names
    if (length(sex_nlevels) == 2 | sex_levels == "1") {
        if (sum(pds_varnames[6:7] %in% names(pds_data_edits)) < 2) {
            stop("The dataset contains data from female - Not all required female variable names are in pds_data or not all variable names match required namining: 'pds_4f', 'pds_5fa'")
        }
    }

    #### 2. Set Up Data #####

    # check if variables are coded in appropriate range and change 99/'I Don't
    # Know' to NA
    for (var in 1:length(pds_varnames)) {
        var_name <- as.character(pds_varnames[var])

        if (var_name %in% names(pds_data_edits)) {
            # convert 'I Don't Know'/99
            pds_data_edits[[var_name]] <- ifelse(pds_data_edits[[var_name]] ==
                99, NA, pds_data_edits[[var_name]])

            # check range of values
            if (min(pds_data_edits[[var_name]], na.rm = TRUE) < 1 & max(pds_data_edits[[var_name]],
                na.rm = TRUE) > 4) {
                stop("coded level values should fall between the values 1 and 4")
            }

            # recode menarche
            if (var_name == "pds_5fa") {
                pds_data_edits[[var_name]] <- ifelse(is.na(pds_data_edits[[var_name]]),
                  NA, ifelse(pds_data_edits[[var_name]] == 1, 4, 0))
            }
        }
    }

    #### 3. Score Data ##### pds_score
    male_vars <- pds_varnames[c(1:5)]
    female_vars <- pds_varnames[c(1:3, 6:7)]

    ## number of NAs
    pds_data_edits[["pds_score_na"]] <- ifelse(pds_data_edits[["sex"]] == 0, rowSums(is.na(pds_data_edits[male_vars])),
        rowSums(is.na(pds_data_edits[female_vars])))

    pds_data_edits[["pds_score"]] <- ifelse(pds_data_edits[["sex"]] == 0, ifelse(pds_data_edits[["pds_score_na"]] <=
        1, rowMeans(pds_data_edits[male_vars], na.rm = TRUE), NA), ifelse(pds_data_edits[["pds_score_na"]] <=
        1 & !is.na(pds_data_edits[["pds_5fa"]]), rowMeans(pds_data_edits[female_vars],
        na.rm = TRUE), NA))

    # tanner category
    male_tanner_vars <- pds_varnames[c(2, 4:5)]
    female_tanner_vars <- pds_varnames[c(2, 6)]

    pds_data_edits[["pds_tanner_sum"]] <- ifelse(pds_data_edits[["sex"]] == 0,
        rowSums(pds_data_edits[male_tanner_vars]), rowSums(pds_data_edits[female_tanner_vars]))

    pds_data_edits[["pds_tanner_cat"]] <- ifelse(is.na(pds_data_edits[["pds_tanner_sum"]]),
        NA, ifelse(pds_data_edits[["sex"]] == 0, ifelse(pds_data_edits[["pds_tanner_sum"]] ==
            12, "Postpubertal", ifelse(pds_data_edits[["pds_tanner_sum"]] >= 9,
            "Late Pubertal", ifelse(pds_data_edits[["pds_tanner_sum"]] >= 6, ifelse(pds_data_edits["pds_2"] <
                4 & pds_data_edits["pds_4m"] < 4 & pds_data_edits["pds_5m"] <
                4, "Mid-Pubertal", "Late Pubertal"), ifelse(pds_data_edits[["pds_tanner_sum"]] >=
                4, ifelse(pds_data_edits["pds_2"] < 3 & pds_data_edits["pds_4m"] <
                3 & pds_data_edits["pds_5m"] < 3, "Early Pubertal", "Mid-Pubertal"),
                "Prepubertal")))), ifelse(pds_data_edits[["pds_5fa"]] == 4, ifelse(pds_data_edits[["pds_tanner_sum"]] ==
            8, "Postpubertal", ifelse(pds_data_edits[["pds_tanner_sum"]] <= 7,
            "Late Pubertal", NA)), ifelse(pds_data_edits[["pds_tanner_sum"]] >
            3, "Mid-Pubertal", ifelse(pds_data_edits[["pds_tanner_sum"]] == 3,
            "Early Pubertal", ifelse(pds_data_edits[["pds_tanner_sum"]] == 2,
                "Prepubertal", NA))))))


    #### 3. Clean Export/Scored Data #####

    # make results dataset
    if (isTRUE(ID_arg)) {
        pds_score_dat <- data.frame(id = pds_data_edits[[parID]], sex = pds_data_edits[["sex"]],
            pds_score = pds_data_edits[["pds_score"]], pds_tanner_cat = pds_data_edits[["pds_tanner_cat"]])
    } else {
        pds_score_dat <- data.frame(sex = pds_data_edits[["sex"]], pds_score = pds_data_edits[["pds_score"]],
            pds_tanner_cat = pds_data_edits[["pds_tanner_cat"]])
    }

    # set up labels for pds_score_dat
    pds_score_dat_labels <- lapply(pds_score_dat, function(x) attributes(x)$label)

    if (isTRUE(ID_arg)) {
        pds_score_dat_labels[["id"]] <- "Participant ID"
    }

    pds_score_dat_labels[["sex"]] <- "Child sex coded as male = 0, female = 1"
    pds_score_dat_labels[["pds_score"]] <- "Pubertal Development Scale score: average of all responses for each sex with menarche yes = 4 points and menarche no = 1 point"
    pds_score_dat_labels[["pds_tanner"]] <- "Tanner equivaluent category"

    ## add attributes to for tanner category to data
    pds_score_dat[["pds_tanner_cat"]] <- sjlabelled::add_labels(pds_score_dat[["pds_tanner_cat"]],
        labels = c(Prepubertal = 1, `Early Puberty` = 2, `Mid-Puberty` = 3, `Late Puberty` = 4,
            Postpubertal = 5))
    pds_score_dat[["pds_tanner_cat"]] <- sjlabelled::as_numeric(pds_score_dat[["pds_tanner_cat"]])

    ## set names based on respondent
    if (respondent == "child") {
        child_names <- if (isTRUE(ID_arg)) {
            names(pds_score_dat)[3:4] <- c("pds_score_self", "pds_tanner_self")
            names(pds_score_dat_labels)[2:3] <- c("pds_score_self", "pds_tanner_self")
        } else {
            names(pds_score_dat)[2:3] <- c("pds_score_self", "pds_tanner_self")
            names(pds_score_dat_labels)[2:3] <- c("pds_score_self", "pds_tanner_self")
        }
    }

    ## make sure the variable labels match in the dataset
    pds_score_dat = sjlabelled::set_label(pds_score_dat, label = matrix(unlist(pds_score_dat_labels,
        use.names = FALSE)))

    return(pds_score_dat)
}
