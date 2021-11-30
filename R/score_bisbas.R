#' score_bisbas: Scored data from the Behavioral Inhibition Scale/Behavioral Activation Scale
#'
#' This function scores the Behavioral Inhibition Scale/Behavioral Activation Scale and provides subscale scores for the following behaviors (2011; 48 item subscales): Fear/Shyness, Anxiety, Conflict Avoidance, Sensory Reward, Drive, Responsiveness to Social Approval, Impulsivity/Fun Seeking. The original 4 subcales (2004; 34 item subscales): Sensitivity to Punishment, Impulsivity/Fun Seeking, Drive, and Reward Responsiveness.
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'bisbas#' where # is the question number (1-48)
#' 3) Questions 1-36 must have the numeric value for the choices: 1 - Strongly Disagree, 2 - Disagree, 3 - Neither Agree nor Disagree, 4 - Agree, 5 - Strongly Agree.
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' Primary References for the Behavioral Inhibition Scale/Behavioral Activation Scale and Scoring:
#' Updated/Revised Caregiver (2011 scoring; 48 items) - Colder CR, Trucco EM, Lopez HI, et al. Revised reinforcement sensitivity theory and laboratory assessment of BIS and BAS in children. Journal of Research in Personality. 2011;45(2):198-207. doi:10.1016/j.jrp.2011.01.005
#' Original Caregiver (2004 scoring; 34 items) - Colder CR, O’Connor RM. Gray’s Reinforcement Sensitivity Model and Child Psychopathology: Laboratory and Questionnaire Assessment of the BAS and BIS. J Abnorm Child Psychol. 2004;32(4):435-451. doi:10.1023/B:JACP.0000030296.54122.b6
#'
#' Original/adult citation:
#' Torrubia R, Ávila C, Moltó J, Caseras X. The Behavioral Inhibition Scale/Behavioral Activation Scale (BIS/BAS) as a measure of Gray’s anxiety and impulsivity dimensions. Personality and Individual Differences. 2001;31(6):837-862. doi:10.1016/S0191-8869(00)00183-5
#'
#' @param bisbas_data a data.frame all items for the Behavioral Inhibition Scale/Behavioral Activation Scale following the naming conventions described above
#' @inheritParams fbs_intake
#'
#' @return A dataset with subscale scores for the Behavioral Inhibition Scale/Behavioral Activation Scale
#' @examples
#'
#' # scoring for the bisbas with IDs
#' bisbas_score_data <- score_bisbas(bisbas_data, parID = 'ID')
#'
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{qualtrics_parent_v3dat}}
#'
#'
#' @export

score_bisbas <- function(bisbas_data, parID) {

    #### 1. Set up/initial checks #####

    # check that bisbas_data exist and is a data.frame
    data_arg <- methods::hasArg(bisbas_data)

    if (isTRUE(data_arg) & !is.data.frame(bisbas_data)) {
        stop("bisbas_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("bisbas_data must set to the data.frame with amount consumed for each food item")
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (isTRUE(ID_arg)){
        if (!(parID %in% names(bisbas_data))) {
            stop("variable name entered as parID is not in bisbas_data")
        }
    }

    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    bisbas_score_dat <- data.frame(bisbas34_punishment = rep(NA, nrow(bisbas_data)), bisbas34_impfun = rep(NA, nrow(bisbas_data)), bisbas34_drive = rep(NA, nrow(bisbas_data)), bisbas34_rewardresp = rep(NA, nrow(bisbas_data)), bisbas48_fearshy = rep(NA, nrow(bisbas_data)), bisbas48_anxiety = rep(NA, nrow(bisbas_data)), bisbas48_conflictavoid = rep(NA, nrow(bisbas_data)), bisbas48_impfun = rep(NA, nrow(bisbas_data)), bisbas48_drive = rep(NA, nrow(bisbas_data)), bisbas48_socialapproval = rep(NA, nrow(bisbas_data)), bisbas48_sensoryreward = rep(NA, nrow(bisbas_data)))


    if (isTRUE(ID_arg)) {
        bisbas_score_dat <- data.frame(bisbas_data[[parID]], bisbas_score_dat)
        names(bisbas_score_dat)[1] <- parID
    }

    # set up labels for bisbas_score_dat
    bisbas_score_dat_labels <- lapply(bisbas_score_dat, function(x) attributes(x)$label)

    ## Score Subscales

    # Connor 2004 - 4 factor
    # Sensitivity to Punishment
    sp34_vars <- c("bisbas3", "bisbas5", "bisbas14", "bisbas16", "bisbas18", "bisbas26", "bisbas28", "bisbas30", "bisbas32", "bisbas34", "bisbas36", "bisbas40", "bisbas44", "bisbas46", "bisbas48")
    bisbas_score_dat[["bisbas34_punishment"]] <- rowSums(bisbas_data[sp34_vars])

    ## add labels to data
    bisbas_score_dat_labels[["bisbas34_punishment"]] <- paste0("BIS/BAS-34 Sensitivity to Punishment Score")

    # Impulsivity/Fun Seeking
    impfun34_vars <- c("bisbas21", "bisbas25", "bisbas33", "bisbas35", "bisbas37", "bisbas39", "bisbas47")
    bisbas_score_dat[["bisbas34_impfun"]] <- rowSums(bisbas_data[impfun34_vars])

    ## add labels to data
    bisbas_score_dat_labels[["bisbas34_impfun"]] <- paste0("BIS/BAS-34 Impulsivity/Fun Seeking Score")

    # Drive
    drive34_vars <- c("bisbas27", "bisbas41", "bisbas43", "bisbas45")
    bisbas_score_dat[["bisbas34_drive"]] <- rowSums(bisbas_data[drive34_vars])

    ## add labels to data
    bisbas_score_dat_labels[["bisbas34_drive"]] <- paste0("BIS/BAS-34 Drive Score")

    # Reward Responsiveness
    reward34_vars <- c("bisbas2", "bisbas7", "bisbas9", "bisbas15", "bisbas17", "bisbas19", "bisbas23")
    bisbas_score_dat[["bisbas34_rewardresp"]] <- rowSums(bisbas_data[reward34_vars])

    ## add labels to data
    bisbas_score_dat_labels[["bisbas34_rewardresp"]] <- paste0("BIS/BAS-34 Reward Responsiveness Score")

    # Fear/Shyness
    fearshy48_vars <- c("bisbas14", "bisbas16", "bisbas18", "bisbas24", "bisbas26", "bisbas30", "bisbas32", "bisbas34", "bisbas40")
    bisbas_score_dat[["bisbas48_fearshy"]] <- rowSums(bisbas_data[fearshy48_vars])

    ## add labels to data
    bisbas_score_dat_labels[["bisbas48_fearshy"]] <- paste0("BIS/BAS-48 Fear/Shyness Score")

    # Anxiety
    anxiety48_vars <- c("bisbas8", "bisbas10", "bisbas20", "bisbas46", "bisbas48")
    bisbas_score_dat[["bisbas48_anxiety"]] <- rowSums(bisbas_data[anxiety48_vars])

    ## add labels to data
    bisbas_score_dat_labels[["bisbas48_anxiety"]] <- paste0("BIS/BAS-48 Anxiety Score")

    # Conflict Avoidance
    conflict48_vars <- c("bisbas6", "bisbas22")
    bisbas_score_dat[["bisbas48_conflictavoid"]] <- rowSums(bisbas_data[conflict48_vars])

    ## add labels to data
    bisbas_score_dat_labels[["bisbas48_conflictavoid"]] <- paste0("BIS/BAS-48 Conflict Avoidence Score")

    # Impulsivity/Fun Seeking
    impfun48_vars <- c("bisbas21", "bisbas23", "bisbas25", "bisbas35", "bisbas37", "bisbas39")
    bisbas_score_dat[["bisbas48_impfun"]] <- rowSums(bisbas_data[impfun48_vars])

    ## add labels to data
    bisbas_score_dat_labels[["bisbas48_impfun"]] <- paste0("BIS/BAS-48 Impulsivity/Fun Seeking Score")

    # Drive
    drive48_vars <- c("bisbas27", "bisbas41", "bisbas43", "bisbas45", "bisbas47")
    bisbas_score_dat[["bisbas48_drive"]] <- rowSums(bisbas_data[drive48_vars])

    ## add labels to data
    bisbas_score_dat_labels[["bisbas48_drive"]] <- paste0("BIS/BAS-48 Drive Score")

    # Responsiveness to Social Approval
    social48_vars <- c("bisbas7", "bisbas11", "bisbas13", "bisbas19")
    bisbas_score_dat[["bisbas48_socialapproval"]] <- rowSums(bisbas_data[social48_vars])

    ## add labels to data
    bisbas_score_dat_labels[["bisbas48_socialapproval"]] <- paste0("BIS/BAS-48 Responsiveness to Social Approval Score")

    # Sensory Reward
    reward48_vars <- c("bisbas29", "bisbas31")
    bisbas_score_dat[["bisbas48_sensoryreward"]] <- rowSums(bisbas_data[reward48_vars])

    ## add labels to data
    bisbas_score_dat_labels[["bisbas48_sensoryreward"]] <- paste0("BIS/BAS-48 Sensory Reward Score")

    #### 3. Clean Export/Scored Data #####

    ## make sure the variable labels match in the dataset
    bisbas_score_dat = sjlabelled::set_label(bisbas_score_dat, label = matrix(unlist(bisbas_score_dat_labels,
        use.names = FALSE)))

    return(bisbas_score_dat)
}

