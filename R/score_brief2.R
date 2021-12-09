#' score_brief2: Scored data from the Behavioral Rating Inventory of Executive Function-2
#'
#' This function scores the Behavioral Rating Inventory of Executive Function-2 and provides subscale scores for the following behaviors: Inhibit, Self-Monitor, Shift, Emotional Control, Initiate, Working Memory, Plan/Organize, Task Monitoring, Organization of Materials. There are also 4 index scores: Behavioral Regulation Index, Emotion Regulation Index, Cognitive Regulation Index, and the General Executive Composite. Three scores help indicate if responses were valid: Negativity Score, Inconsistency Score, and Infrequency Score
#'
#' These data are scored using the brief2_scoretables data available in the kellertools package to look up age- and sex-nomed t-scores and percentiles.
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include sex, age in years, and all individual questionnaire items
#' 2) The questionnaire items must match the following naming convention: brief#' where # is the question number (1-63)
#' 3) Questions must have the numeric value for the choices: 1 - Never, 2 - Sometimes, 3 - Often
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Gioia GA, Isquith PK, Guy SC, Kenworthy L. BRIEF-2: Behavior Rating Inventory of Executive Function: Professional Manual. Psychological Assessment Resources; 2015.
#'
#' @param brief_data a data.frame all items for the Behavioral Rating Inventory of Executive Function-2 following the naming conventions described above
#' @param age_var a string with the name of the age variable in brief_data
#' @param sex_var a string with the name of the sex variable in brief_data
#' @inheritParams score_pds
#' @inheritParams score_pds
#' @inheritParams fbs_intake
#'
#' @return A dataset with subscale scores for the Behavioral Rating Inventory of Executive Function-2
#' @examples
#'
#' # scoring for the brief with IDs
#' brief_score_data <- score_brief2(brief_data, age_var = 'age', sex_var = 'sex', parID = 'ID')
#'
#' # scoring for the brief with specified levels for male and female
#' brief_score_data <- score_brief2(brief_data, age_var = 'age', sex_var = 'sex', male = 'male', female = 'female', parID = 'ID')
#'
#' \dontrun{
#'
#' # age and sex variable names must be strings - the following would not be correct
#' brief_score_data <- score_brief2(brief_data, age_var = age, sex_var = sex)
#'
#' # male and female specification must match the data in brief_data. Do not give the value label if brief_data has label attributes for sex.
#'
#' #check attributes for sex
#' attributes(brief_data$sex)
#'
#' #$labels
#' #Male Female
#' # 0      1
#'
#' #with the above attributes, the following will not run as the data.frame contains 0's and 1's, not the labels
#' brief_score_data <- score_brief2(brief_data, age_var = 'age', sex_var = 'sex', male = 'Male', female = 'Female')
#'
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{qualtrics_parent_v4dat}}. Normed values were be identified using \code{\link{ref_brief2_lookup}} but could also be done through \code{\link{ref_lookup_fm}}
#'
#'
#' @export

score_brief2 <- function(brief_data, age_var, sex_var, male = 0, female = 1, parID) {

    #### 1. Set up/initial checks #####

    # check that brief_data exist and is a data.frame
    data_arg <- methods::hasArg(brief_data)

    if (isTRUE(data_arg) & !is.data.frame(brief_data)) {
        stop("brief_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("brief_data must set to the data.frame with amount consumed for each food item")
    }

    # check that age_var exist and is a string
    age_arg <- methods::hasArg(age_var)

    if (isTRUE(age_arg)) {
        if (!is.character(age_var)) {
            stop("age_var must be entered as a string and match the age variable name in brief_data")
        } else if (!(age_var %in% names(brief_data))) {
            stop(paste0("the provided age_var: '", age_var, "' does not exist in breif_data. To check variable names use 'names(brief_data)'."))
        }
    } else if (isFALSE(age_arg)) {
        stop("age_var must be entered as a string and match the age variable name in brief_data")
    }

    # check that sex_var exist and is a string
    sex_arg <- methods::hasArg(sex_var)

    if (isTRUE(sex_arg)) {
        if (!is.character(sex_var)) {
            stop("sex_var must be entered as a string and match the sex variable name in brief_data")
        } else if (!(sex_var %in% names(brief_data))) {
            stop(paste0("the provided sex_var: '", sex_var, "' does not exist in breif_data. To check variable names use 'names(brief_data)'."))
        }
    } else if (isFALSE(sex_arg)) {
        stop("sex_var must be entered as a string and match the sex variable name in brief_data")
    }

    # check varaible 'sex' exists and for male and female arguments
    male_arg <- methods::hasArg(male)
    female_arg <- methods::hasArg(female)

    # check number of unique values in dataset
    nsex_unique <- length(unique(!is.na(brief_data[["sex"]])))

    # entered arguments match number of different sexes in data
    if (isTRUE(male_arg) | isTRUE(female_arg)) {

        if (sum(isTRUE(male_arg), isTRUE(female_arg)) == nsex_unique) {
            if (nsex_unique == 2) {
                # change values of sex in pds_data to default
                brief_data[["sex"]] <- ifelse(brief_data[["sex"]] ==
                                                  male, 0, 1)
                brief_data[["sex"]] <- factor(brief_data[["sex"]], levels = c(0,
                                                                              1))
            } else if (nsex_unique == 1 & isTRUE(male_arg)) {
                # if only 1 value for sex and male is specified, set default to 0
                brief_data[["sex"]] <- 0
            } else if (nsex_unique == 1 & isTRUE(female_arg)) {
                # if only 1 value for sex and female is specified, set default to 1
                brief_data[["sex"]] <- 1
            }
        } else {
            stop("The number of alternate values entered for sex do not match the number of different sexes in data. If specifying non-default values for male and/or female, must provide all values that exist in data (e.g., if have both males and females, need to provide values for both)")
        }
    } else if (sum(isTRUE(male_arg), isTRUE(female_arg)) == 0) {
        brief_data[["sex"]] <- factor(brief_data[["sex"]])
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (isTRUE(ID_arg)){
        if (!(parID %in% names(brief_data))) {
            stop("variable name entered as parID is not in brief_data")
        }
    }

    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    brief_score_dat <- data.frame(inhibit_raw = rep(NA, nrow(brief_data)), inhibit_t = rep(NA, nrow(brief_data)), inhibit_p = rep(NA, nrow(brief_data)), selfmon_raw = rep(NA, nrow(brief_data)), selfmon_t = rep(NA, nrow(brief_data)), selfmon_p = rep(NA, nrow(brief_data)), shift_raw = rep(NA, nrow(brief_data)), shift_t = rep(NA, nrow(brief_data)), shift_p = rep(NA, nrow(brief_data)), emcont_raw = rep(NA, nrow(brief_data)), emcont_t = rep(NA, nrow(brief_data)), emcont_p = rep(NA, nrow(brief_data)), initiate_raw = rep(NA, nrow(brief_data)), initiate_t = rep(NA, nrow(brief_data)), initiate_p = rep(NA, nrow(brief_data)), wm_raw = rep(NA, nrow(brief_data)), wm_t = rep(NA, nrow(brief_data)), wm_p = rep(NA, nrow(brief_data)), planorg_raw = rep(NA, nrow(brief_data)), planorg_t = rep(NA, nrow(brief_data)), planorg_p = rep(NA, nrow(brief_data)), taskmon_raw = rep(NA, nrow(brief_data)), taskmon_t = rep(NA, nrow(brief_data)), taskmon_p = rep(NA, nrow(brief_data)), orgmat_raw = rep(NA, nrow(brief_data)), orgmat_t = rep(NA, nrow(brief_data)), orgmat_p = rep(NA, nrow(brief_data)), bri_raw = rep(NA, nrow(brief_data)), bri_t = rep(NA, nrow(brief_data)), bri_p = rep(NA, nrow(brief_data)), eri_raw = rep(NA, nrow(brief_data)), eri_t = rep(NA, nrow(brief_data)), eri_p = rep(NA, nrow(brief_data)), cri_raw = rep(NA, nrow(brief_data)), cri_t = rep(NA, nrow(brief_data)), cri_p = rep(NA, nrow(brief_data)), gec_raw = rep(NA, nrow(brief_data)), gec_t = rep(NA, nrow(brief_data)), gec_p = rep(NA, nrow(brief_data)), negativity_raw = rep(NA, nrow(brief_data)), negativity_p = rep(NA, nrow(brief_data)), negativity_cat = rep(NA, nrow(brief_data)), inconsistency_raw = rep(NA, nrow(brief_data)), inconsistency_p = rep(NA, nrow(brief_data)), inconsistency_cat = rep(NA, nrow(brief_data)), infrequency_raw = rep(NA, nrow(brief_data)), infrequency_p = rep(NA, nrow(brief_data)), infrequency_cat = rep(NA, nrow(brief_data)))

    if (isTRUE(ID_arg)) {
        brief_score_dat <- data.frame(brief_data[[parID]], brief_score_dat)
        names(brief_score_dat)[1] <- parID
    }

    if(class(brief_data[["sex"]])[1] == 'haven_labelled'){
        haven::labelled(brief_data[["sex"]], labels = NULL)
    }

    # set up labels for brief_score_dat
    brief_score_dat_labels <- lapply(brief_score_dat, function(x) attributes(x)$label)

    ## Score Subscales

    # Inhibit
    inhib_vars <- c("brief1", "brief10", "brief16", "brief24", "brief30", "brief39", "brief48", "brief62")
    brief_score_dat[["inhibit_raw"]] <- rowSums(brief_data[inhib_vars])

    # look up T-score and percentile
    inhib_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[["inhibit_raw"]], sex = brief_data[['sex']], age = brief_data[['age']], MoreArgs = list(item = 'inhibit'))

    inhib_scores <- as.data.frame(matrix(unlist(inhib_scores_list), byrow = TRUE, ncol = 3))
    names(inhib_scores) <- c('item', 't', 'p')

    brief_score_dat[["inhibit_t"]] <- as.numeric(inhib_scores[['t']])
    brief_score_dat[["inhibit_p"]] <- inhib_scores[['p']]

    ## add labels to data
    brief_score_dat_labels[["inhibit_raw"]] <- "BRIEF2 Inhibit Raw Score"
    brief_score_dat_labels[["inhibit_t"]] <- "BRIEF2 Inhibit T-Score"
    brief_score_dat_labels[["inhibit_p"]] <- "BRIEF2 Inhibit Percentile"

    # Self-Monitoring
    selfmon_vars <- c("brief4", "brief13", "brief20", "brief26")
    brief_score_dat[["selfmon_raw"]] <- rowSums(brief_data[selfmon_vars])

    # look up T-score and percentile
    selfmon_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[["selfmon_raw"]], sex = brief_data[['sex']], age = brief_data[['age']], MoreArgs = list(item = 'selfmon'))

    selfmon_scores <- as.data.frame(matrix(unlist(selfmon_scores_list), byrow = TRUE, ncol = 3))
    names(selfmon_scores) <- c('item', 't', 'p')

    brief_score_dat[["selfmon_t"]] <- as.numeric(selfmon_scores[['t']])
    brief_score_dat[["selfmon_p"]] <- selfmon_scores[['p']]

    ## add labels to data
    brief_score_dat_labels[["selfmon_raw"]] <- "BRIEF2 Self-Monitoring Raw Score"
    brief_score_dat_labels[["selfmon_t"]] <- "BRIEF2 Self-Monitoring T-Score"
    brief_score_dat_labels[["selfmon_p"]] <- "BRIEF2 Self-Monitoring Percentile"

    # Shifting
    shift_vars <- c("brief2", "brief11", "brief17", "brief31", "brief40", "brief49", "brief58", "brief60")
    brief_score_dat[["shift_raw"]] <- rowSums(brief_data[shift_vars])

    # look up T-score and percentile
    shift_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[["shift_raw"]], sex = brief_data[['sex']], age = brief_data[['age']], MoreArgs = list(item = 'shift'))

    shift_scores <- as.data.frame(matrix(unlist(shift_scores_list), byrow = TRUE, ncol = 3))
    names(shift_scores) <- c('item', 't', 'p')

    brief_score_dat[["shift_t"]] <- as.numeric(shift_scores[['t']])
    brief_score_dat[["shift_p"]] <- shift_scores[['p']]

    ## add labels to data
    brief_score_dat_labels[["shift_raw"]] <- "BRIEF2 Shifting Raw Score"
    brief_score_dat_labels[["shift_t"]] <- "BRIEF2 Shifting T-Score"
    brief_score_dat_labels[["shift_p"]] <- "BRIEF2 Shifting Percentile"

    # Self-Monitoring
    selfmon_vars <- c("brief4", "brief13", "brief20", "brief26")
    brief_score_dat[["selfmon_raw"]] <- rowSums(brief_data[selfmon_vars])

    # look up T-score and percentile
    selfmon_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[["selfmon_raw"]], sex = brief_data[['sex']], age = brief_data[['age']], MoreArgs = list(item = 'selfmon'))

    selfmon_scores <- as.data.frame(matrix(unlist(selfmon_scores_list), byrow = TRUE, ncol = 3))
    names(selfmon_scores) <- c('item', 't', 'p')

    brief_score_dat[["selfmon_t"]] <- as.numeric(selfmon_scores[['t']])
    brief_score_dat[["selfmon_p"]] <- selfmon_scores[['p']]

    ## add labels to data
    brief_score_dat_labels[["selfmon_raw"]] <- "BRIEF2 Self-Monitoring Raw Score"
    brief_score_dat_labels[["selfmon_t"]] <- "BRIEF2 Self-Monitoring T-Score"
    brief_score_dat_labels[["selfmon_p"]] <- "BRIEF2 Self-Monitoring Percentile"

    # Emotional Control
    emcont_vars <- c("brief6", "brief14", "brief22", "brief27", "brief34", "brief43", "brief51", "brief56")
    brief_score_dat[["emcont_raw"]] <- rowSums(brief_data[emcont_vars])

    # look up T-score and percentile
    emcont_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[["emcont_raw"]], sex = brief_data[['sex']], age = brief_data[['age']], MoreArgs = list(item = 'emcont'))

    emcont_scores <- as.data.frame(matrix(unlist(emcont_scores_list), byrow = TRUE, ncol = 3))
    names(emcont_scores) <- c('item', 't', 'p')

    brief_score_dat[["emcont_t"]] <- as.numeric(emcont_scores[['t']])
    brief_score_dat[["emcont_p"]] <- emcont_scores[['p']]

    ## add labels to data
    brief_score_dat_labels[["emcont_raw"]] <- "BRIEF2 Emotional Control Raw Score"
    brief_score_dat_labels[["emcont_t"]] <- "BRIEF2 Emotional Control T-Score"
    brief_score_dat_labels[["emcont_p"]] <- "BRIEF2 Emotional Control Percentile"

    # Initiate
    initiate_vars <- c("brief9", "brief38", "brief50", "brief55", "brief61")
    brief_score_dat[["initiate_raw"]] <- rowSums(brief_data[initiate_vars])

    # look up T-score and percentile
    initiate_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[["initiate_raw"]], sex = brief_data[['sex']], age = brief_data[['age']], MoreArgs = list(item = 'initiate'))

    initiate_scores <- as.data.frame(matrix(unlist(initiate_scores_list), byrow = TRUE, ncol = 3))
    names(initiate_scores) <- c('item', 't', 'p')

    brief_score_dat[["initiate_t"]] <- as.numeric(initiate_scores[['t']])
    brief_score_dat[["initiate_p"]] <- initiate_scores[['p']]

    ## add labels to data
    brief_score_dat_labels[["initiate_raw"]] <- "BRIEF2 Initiate Raw Score"
    brief_score_dat_labels[["initiate_t"]] <- "BRIEF2 Initiate T-Score"
    brief_score_dat_labels[["initiate_p"]] <- "BRIEF2 Initiate Percentile"

    # Working Memory
    wm_vars <- c("brief3", "brief12", "brief19", "brief25", "brief28", "brief32", "brief41", "brief46")
    brief_score_dat[["wm_raw"]] <- rowSums(brief_data[wm_vars])

    # look up T-score and percentile
    wm_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[["wm_raw"]], sex = brief_data[['sex']], age = brief_data[['age']], MoreArgs = list(item = 'wm'))

    wm_scores <- as.data.frame(matrix(unlist(wm_scores_list), byrow = TRUE, ncol = 3))
    names(wm_scores) <- c('item', 't', 'p')

    brief_score_dat[["wm_t"]] <- as.numeric(wm_scores[['t']])
    brief_score_dat[["wm_p"]] <- wm_scores[['p']]

    ## add labels to data
    brief_score_dat_labels[["wm_raw"]] <- "BRIEF2 Working Memory Raw Score"
    brief_score_dat_labels[["wm_t"]] <- "BRIEF2 Working Memory T-Score"
    brief_score_dat_labels[["wm_p"]] <- "BRIEF2 Working Memory Percentile"

    # Planing and Organization
    planorg_vars <- c("brief7", "brief15", "brief23", "brief35", "brief44", "brief52", "brief57", "brief59")
    brief_score_dat[["planorg_raw"]] <- rowSums(brief_data[planorg_vars])

    # look up T-score and percentile
    planorg_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[["planorg_raw"]], sex = brief_data[['sex']], age = brief_data[['age']], MoreArgs = list(item = 'planorg'))

    planorg_scores <- as.data.frame(matrix(unlist(planorg_scores_list), byrow = TRUE, ncol = 3))
    names(planorg_scores) <- c('item', 't', 'p')

    brief_score_dat[["planorg_t"]] <- as.numeric(planorg_scores[['t']])
    brief_score_dat[["planorg_p"]] <- planorg_scores[['p']]

    ## add labels to data
    brief_score_dat_labels[["planorg_raw"]] <- "BRIEF2 Planing and Organization Raw Score"
    brief_score_dat_labels[["planorg_t"]] <- "BRIEF2 Planing and Organization T-Score"
    brief_score_dat_labels[["planorg_p"]] <- "BRIEF2 Planing and Organization Percentile"

    # Task Monitoring
    taskmon_vars <- c("brief5", "brief21", "brief29", "brief33", "brief42")
    brief_score_dat[["taskmon_raw"]] <- rowSums(brief_data[taskmon_vars])

    # look up T-score and percentile
    taskmon_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[["taskmon_raw"]], sex = brief_data[['sex']], age = brief_data[['age']], MoreArgs = list(item = 'taskmon'))

    taskmon_scores <- as.data.frame(matrix(unlist(taskmon_scores_list), byrow = TRUE, ncol = 3))
    names(taskmon_scores) <- c('item', 't', 'p')

    brief_score_dat[["taskmon_t"]] <- as.numeric(taskmon_scores[['t']])
    brief_score_dat[["taskmon_p"]] <- taskmon_scores[['p']]

    ## add labels to data
    brief_score_dat_labels[["taskmon_raw"]] <- "BRIEF2 Task Monitoring Raw Score"
    brief_score_dat_labels[["taskmon_t"]] <- "BRIEF2 Task Monitoring T-Score"
    brief_score_dat_labels[["taskmon_p"]] <- "BRIEF2 Task Monitoring Percentile"

    # Organization of Materials
    orgmat_vars <- c("brief8", "brief37", "brief45", "brief47", "brief53", "brief63")
    brief_score_dat[["orgmat_raw"]] <- rowSums(brief_data[orgmat_vars])

    # look up T-score and percentile
    orgmat_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[["orgmat_raw"]], sex = brief_data[['sex']], age = brief_data[['age']], MoreArgs = list(item = 'orgmat'))

    orgmat_scores <- as.data.frame(matrix(unlist(orgmat_scores_list), byrow = TRUE, ncol = 3))
    names(orgmat_scores) <- c('item', 't', 'p')

    brief_score_dat[["orgmat_t"]] <- as.numeric(orgmat_scores[['t']])
    brief_score_dat[["orgmat_p"]] <- orgmat_scores[['p']]

    ## add labels to data
    brief_score_dat_labels[["orgmat_raw"]] <- "BRIEF2 Organization of Materials Raw Score"
    brief_score_dat_labels[["orgmat_t"]] <- "BRIEF2 Organization of Materials T-Score"
    brief_score_dat_labels[["orgmat_p"]] <- "BRIEF2 Organization of Materials Percentile"

    # Behavioral Regulation Index
    brief_score_dat[["bri_raw"]] <- rowSums(brief_data[c(inhib_vars, selfmon_vars)])

    # look up T-score and percentile
    bri_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[["bri_raw"]], sex = brief_data[['sex']], age = brief_data[['age']], MoreArgs = list(item = 'bri'))

    bri_scores <- as.data.frame(matrix(unlist(bri_scores_list), byrow = TRUE, ncol = 3))
    names(bri_scores) <- c('item', 't', 'p')

    brief_score_dat[["bri_t"]] <- as.numeric(bri_scores[['t']])
    brief_score_dat[["bri_p"]] <- bri_scores[['p']]

    ## add labels to data
    brief_score_dat_labels[["bri_raw"]] <- "BRIEF2 Behavioral Regulation Index Raw Score"
    brief_score_dat_labels[["bri_t"]] <- "BRIEF2 Behavioral Regulation Index T-Score"
    brief_score_dat_labels[["bri_p"]] <- "BRIEF2 Behavioral Regulation Index Percentile"

    # Emotional Regulation Index
    brief_score_dat[["eri_raw"]] <- rowSums(brief_data[c(shift_vars, emcont_vars)])

    # look up T-score and percentile
    eri_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[["eri_raw"]], sex = brief_data[['sex']], age = brief_data[['age']], MoreArgs = list(item = 'eri'))

    eri_scores <- as.data.frame(matrix(unlist(eri_scores_list), byrow = TRUE, ncol = 3))
    names(eri_scores) <- c('item', 't', 'p')

    brief_score_dat[["eri_t"]] <- as.numeric(eri_scores[['t']])
    brief_score_dat[["eri_p"]] <- eri_scores[['p']]

    ## add labels to data
    brief_score_dat_labels[["eri_raw"]] <- "BRIEF2 Emotional Regulation Index Raw Score"
    brief_score_dat_labels[["eri_t"]] <- "BRIEF2 Emotional Regulation Index T-Score"
    brief_score_dat_labels[["eri_p"]] <- "BRIEF2 Emotional Regulation Index Percentile"

    # General Executive Composite
    brief_score_dat[["cri_raw"]] <- rowSums(brief_data[c(initiate_vars, wm_vars, planorg_vars, taskmon_vars, orgmat_vars)])

    # look up T-score and percentile
    cri_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[["cri_raw"]], sex = brief_data[['sex']], age = brief_data[['age']], MoreArgs = list(item = 'cri'))

    cri_scores <- as.data.frame(matrix(unlist(cri_scores_list), byrow = TRUE, ncol = 3))
    names(cri_scores) <- c('item', 't', 'p')

    brief_score_dat[["cri_t"]] <- as.numeric(cri_scores[['t']])
    brief_score_dat[["cri_p"]] <- cri_scores[['p']]

    ## add labels to data
    brief_score_dat_labels[["cri_raw"]] <- "BRIEF2 General Executive Composite Raw Score"
    brief_score_dat_labels[["cri_t"]] <- "BRIEF2 General Executive Composite T-Score"
    brief_score_dat_labels[["cri_p"]] <- "BRIEF2 General Executive Composite Percentile"

    # Cognitive Regulation Index
    brief_score_dat[["gec_raw"]] <- rowSums(brief_data[c(inhib_vars, selfmon_vars, shift_vars, emcont_vars, initiate_vars, wm_vars, planorg_vars, taskmon_vars, orgmat_vars)])

    # look up T-score and percentile
    gec_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[["gec_raw"]], sex = brief_data[['sex']], age = brief_data[['age']], MoreArgs = list(item = 'gec'))

    gec_scores <- as.data.frame(matrix(unlist(gec_scores_list), byrow = TRUE, ncol = 3))
    names(gec_scores) <- c('item', 't', 'p')

    brief_score_dat[["gec_t"]] <- as.numeric(gec_scores[['t']])
    brief_score_dat[["gec_p"]] <- gec_scores[['p']]

    ## add labels to data
    brief_score_dat_labels[["gec_raw"]] <- "BRIEF2 Cognitive Regulation Index Raw Score"
    brief_score_dat_labels[["gec_t"]] <- "BRIEF2 Cognitive Regulation Index T-Score"
    brief_score_dat_labels[["gec_p"]] <- "BRIEF2 Cognitive Regulation Index Percentile"


    ## Scale Checks

    # Negativity
    neg_vars <- c("brief14", "brief28", "brief30", "brief34", "brief39", "brief41", "brief58", "brief60")
    brief_score_dat[["negativity_raw"]] <- rowSums(brief_data[neg_vars] == 3)

    # look up T-score and percentile
    neg_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[["negativity_raw"]], MoreArgs = list(item = 'negativity'))

    neg_scores <- as.data.frame(matrix(unlist(neg_scores_list), byrow = TRUE, ncol = 3))
    names(neg_scores) <- c('item', 'p', 'cat')

    brief_score_dat[["negativity_p"]] <- neg_scores[['p']]
    brief_score_dat[["negativity_cat"]] <- as.numeric(neg_scores[['cat']])

    ## add labels to data
    brief_score_dat_labels[["negativity_raw"]] <- "BRIEF2 Negativity Check Raw Score"
    brief_score_dat_labels[["negativity_p"]] <- "BRIEF2 Negativity Check Percentile"
    brief_score_dat_labels[["negativity_cat"]] <- "BRIEF2 Negativity Check Category"

    # Inconsistency
    incon_vars1 <- c("brief5", "brief9", "brief10", "brief17", "brief20", "brief22", "brief25", "brief37")
    incon_vars2 <- c("brief21", "brief55", "brief48", "brief40", "brief26",  "brief56", "brief50",  "brief63")

    brief_score_dat[["inconsistency_raw"]] <- rowSums(abs(brief_data[incon_vars1] - brief_data[incon_vars2]))

    # look up T-score and percentile
    incon_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[["inconsistency_raw"]], MoreArgs = list(item = 'inconsistency'))

    incon_scores <- as.data.frame(matrix(unlist(incon_scores_list), byrow = TRUE, ncol = 3))
    names(incon_scores) <- c('item', 'p', 'cat')

    brief_score_dat[["inconsistency_p"]] <- incon_scores[['p']]
    brief_score_dat[["inconsistency_cat"]] <- as.numeric(incon_scores[['cat']])

    ## add labels to data
    brief_score_dat_labels[["inconsistency_raw"]] <- "BRIEF2 Inconsistency Check Raw Score"
    brief_score_dat_labels[["inconsistency_p"]] <- "BRIEF2 Inconsistency Check Percentile"
    brief_score_dat_labels[["inconsistency_cat"]] <- "BRIEF2 Inconsistency Check Category"

    # Infrequency
    infreq_vars <- c("brief18", "brief36", "brief54")
    brief_score_dat[["infrequency_raw"]] <- rowSums(brief_data[infreq_vars] > 1)

    # look up T-score and percentile
    infreq_scores_list <- mapply(ref_brief2_lookup, value = brief_score_dat[["infrequency_raw"]], MoreArgs = list(item = 'infrequency'))

    infreq_scores <- as.data.frame(matrix(unlist(infreq_scores_list), byrow = TRUE, ncol = 3))
    names(infreq_scores) <- c('item', 'p', 'cat')

    brief_score_dat[["infrequency_p"]] <- incon_scores[['p']]
    brief_score_dat[["infrequency_cat"]] <- as.numeric(incon_scores[['cat']])

    ## add labels to data
    brief_score_dat_labels[["infrequency_raw"]] <- "BRIEF2 Infrequency Check Raw Score"
    brief_score_dat_labels[["infrequency_p"]] <- "BRIEF2 Infrequency Check Percentile"
    brief_score_dat_labels[["infrequency_cat"]] <- "BRIEF2 Infrequency Check Category"



    #### 3. Clean Export/Scored Data #####

    ## make sure the variable labels match in the dataset
    brief_score_dat = sjlabelled::set_label(brief_score_dat, label = matrix(unlist(brief_score_dat_labels,
                                                                                   use.names = FALSE)))

    return(brief_score_dat)
}

