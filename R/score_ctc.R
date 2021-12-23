#' score_ctc: Scored data from the Communities that Care
#'
#' This function scores the Child Behavior Questionnaire and provides subscale scores for the following behaviors: Activity Level, Anger/Frustration, Approach/Positive Anticipation, Attentional Focusing, Discomfort, Falling Reactivity/Soothability, Fear, High Intesity Pleasure, Impulsivity, Inhibitory Control, Low Intensity Pleasure, Perceptual Sensitivity, Sadness, Shyness, Smiling and Laughter. We can also get the Big 3 subcales: Surgency, Negative Affect, and Effortful Control.
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'ctc#' where # is the question number (1-16 for the Food and Brain Study)
#' 3) All questions must have the numeric value for the choice: 1 - Not at all, 2 - A little, 3 - Not sure/in the middle, 4 - Somewhat, 5 - A lot, 99 - Skip
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' NEED
#'
#'
#' @param ctc_data a data.frame all items for the Child Behavior Questionnaire following the naming conventions described above
#' @param study a string indicating which study collected the data. Currently, only option and default is 'fbs'. This parameter is included because the Food and Brain study used a highly adapted verison of the Communities that Care Questionnaire with only 16 questions
#' @inheritParams fbs_intake
#'
#' @return A dataset with subscale scores for the Child Behavior Questionnaire
#' @examples
#'
#' # scoring for the ctc with IDs
#' ctc_score_data <- score_ctc(ctc_data, parID = 'ID')
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{qualtrics_child_v5dat}}
#'
#'
#' @export

score_ctc <- function(ctc_data, parID) {

    #### 1. Set up/initial checks #####

    # check that ctc_data exist and is a data.frame
    data_arg <- methods::hasArg(ctc_data)

    if (isTRUE(data_arg) & !is.data.frame(ctc_data)) {
        stop("ctc_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("ctc_data must set to the data.frame with amount consumed for each food item")
    }

    # check that study exists and is a string
    study_arg <- methods::hasArg(study)

    if (isTRUE(study_arg)) {
        if (study != "fbs" & study != "FBS") {
            stop("only option currently available for study is 'fbs'. If have an alternative study you wish to use this script for please contact package maintainers to get your study added")
        }
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (isTRUE(ID_arg)){
        if (!(parID %in% names(ctc_data))) {
            stop("variable name entered as parID is not in ctc_data")
        }
    }

    #### 2. Set Up Data #####

    # set up database for results

    ## create empty matrix
    ctc_score_dat <- data.frame(ctc_activity = rep(NA, nrow(ctc_data)), ctc_anger = rep(NA,
        nrow(ctc_data)), ctc_approach = rep(NA, nrow(ctc_data)), ctc_attention = rep(NA,
        nrow(ctc_data)), ctc_discomfort = rep(NA, nrow(ctc_data)), ctc_soothability = rep(NA,
        nrow(ctc_data)), ctc_fear = rep(NA, nrow(ctc_data)), ctc_highintensity_pleasure = rep(NA,
        nrow(ctc_data)), ctc_impulsivity = rep(NA, nrow(ctc_data)), ctc_inhibitory_cont = rep(NA,
        nrow(ctc_data)), ctc_lowintensity_pleasure = rep(NA, nrow(ctc_data)),
        ctc_perceptual_sensitivity = rep(NA, nrow(ctc_data)), ctc_sadness = rep(NA,
            nrow(ctc_data)), ctc_shyness = rep(NA, nrow(ctc_data)), ctc_smile_laughter = rep(NA,
            nrow(ctc_data)), ctc_surgency = rep(NA, nrow(ctc_data)), ctc_neg_affect = rep(NA,
            nrow(ctc_data)), ctc_effortful_cont = rep(NA, nrow(ctc_data)))

    if (isTRUE(ID_arg)) {
        ctc_score_dat <- data.frame(ctc_data[[parID]], ctc_score_dat)
        names(ctc_score_dat)[1] <- parID
    }

    # set up labels for ctc_score_dat
    ctc_score_dat_labels <- lapply(ctc_score_dat, function(x) attributes(x)$label)

    # calculate reversed scores

    reverse_qs <- c("ctc3", "ctc11", "ctc16", "ctc18", "ctc19", "ctc21", "ctc25",
        "ctc34", "ctc35", "ctc36", "ctc43", "ctc48", "ctc49", "ctc50", "ctc53",
        "ctc54", "ctc56", "ctc60", "ctc61", "ctc68", "ctc74", "ctc75", "ctc78",
        "ctc80", "ctc82", "ctc83", "ctc84", "ctc90", "ctc91", "ctc92", "ctc93")

    for (var in 1:length(reverse_qs)) {
        var_name <- reverse_qs[var]
        reverse_name <- paste0(var_name, "_rev")

        ctc_data[[reverse_name]] <- ifelse(is.na(ctc_data[[var_name]]), NA, ifelse(ctc_data[[var_name]] ==
            1, 7, ifelse(ctc_data[[var_name]] == 2, 6, ifelse(ctc_data[[var_name]] ==
            3, 5, ifelse(ctc_data[[var_name]] == 5, 3, ifelse(ctc_data[[var_name]] ==
            6, 2, ifelse(ctc_data[[var_name]] == 7, 1, 4)))))))
    }

    ## covert -99 to NA; considered NA in scoring which is sum/#answered
    for (var in 1:ncol(ctc_data)) {
        var_name <- names(ctc_data)[var]

        if (grepl("ctc", var_name, fixed = TRUE)) {
            ctc_data[[var_name]] <- ifelse(is.na(ctc_data[[var_name]]), NA, ifelse(ctc_data[[var_name]] ==
                -99, NA, ctc_data[[var_name]]))
        }

    }
    ## Score Subscales

    # Activity Level
    activity_vars <- c("ctc1", "ctc12", "ctc18_rev", "ctc22", "ctc50_rev", "ctc85",
        "ctc93_rev")
    ctc_score_dat[["ctc_activity"]] <- rowSums(ctc_data[activity_vars], na.rm = TRUE)/rowSums(!is.na(ctc_data[activity_vars]))

    ## add labels to data
    ctc_score_dat_labels[["ctc_activity"]] <- "CBQ Activity Level Total Score"

    # Anger/Frustration
    anger_vars <- c("ctc2", "ctc14", "ctc30", "ctc40", "ctc61_rev", "ctc87")
    ctc_score_dat[["ctc_anger"]] <- rowSums(ctc_data[anger_vars], na.rm = TRUE)/rowSums(!is.na(ctc_data[anger_vars]))

    ## add labels to data
    ctc_score_dat_labels[["ctc_anger"]] <- "CBQ Anger/Frustration Total Score"

    # Approach/Positive Anticipation
    approach_vars <- c("ctc6", "ctc15", "ctc46", "ctc58", "ctc90_rev", "ctc92_rev")
    ctc_score_dat[["ctc_approach"]] <- rowSums(ctc_data[approach_vars], na.rm = TRUE)/rowSums(!is.na(ctc_data[approach_vars]))

    ## add labels to data
    ctc_score_dat_labels[["ctc_approach"]] <- "CBQ Approach/Positive Anticipation Total Score"

    # Attentional Focusing
    attention_vars <- c("ctc16_rev", "ctc21_rev", "ctc62", "ctc71", "ctc84_rev",
        "ctc89")
    ctc_score_dat[["ctc_attention"]] <- rowSums(ctc_data[attention_vars], na.rm = TRUE)/rowSums(!is.na(ctc_data[approach_vars]))

    ## add labels to data
    ctc_score_dat_labels[["ctc_attention"]] <- "CBQ Attentional Focusing Total Score"

    # Discomfort
    discomfort_vars <- c("ctc3_rev", "ctc9", "ctc29", "ctc49_rev", "ctc64", "ctc91_rev")
    ctc_score_dat[["ctc_discomfort"]] <- rowSums(ctc_data[discomfort_vars], na.rm = TRUE)/rowSums(!is.na(ctc_data[discomfort_vars]))

    ## add labels to data
    ctc_score_dat_labels[["ctc_discomfort"]] <- "CBQ Discomfort Total Score"

    # Falling Reactivity/Soothability
    sooth_vars <- c("ctc25_rev", "ctc34_rev", "ctc44", "ctc59", "ctc66", "ctc75_rev")
    ctc_score_dat[["ctc_soothability"]] <- rowSums(ctc_data[sooth_vars], na.rm = TRUE)/rowSums(!is.na(ctc_data[sooth_vars]))

    ## add labels to data
    ctc_score_dat_labels[["ctc_soothability"]] <- "CBQ Falling Reactivity/Soothability Total Score"

    # Fear
    fear_vars <- c("ctc17", "ctc23", "ctc35_rev", "ctc41", "ctc63", "ctc68_rev")
    ctc_score_dat[["ctc_fear"]] <- rowSums(ctc_data[fear_vars], na.rm = TRUE)/rowSums(!is.na(ctc_data[fear_vars]))

    ## add labels to data
    ctc_score_dat_labels[["ctc_fear"]] <- "CBQ Fear Total Score"

    # High Intensity Pleasure
    hi_pleasure_vars <- c("ctc4", "ctc10", "ctc33", "ctc69", "ctc78_rev", "ctc88")
    ctc_score_dat[["ctc_highintensity_pleasure"]] <- rowSums(ctc_data[hi_pleasure_vars],
        na.rm = TRUE)/rowSums(!is.na(ctc_data[hi_pleasure_vars]))

    ## add labels to data
    ctc_score_dat_labels[["ctc_highintensity_pleasure"]] <- "CBQ High Intensity Pleasure Total Score"

    # Impulsivity
    impulsivity_vars <- c("ctc7", "ctc28", "ctc36_rev", "ctc43_rev", "ctc51",
        "ctc82_rev")
    ctc_score_dat[["ctc_impulsivity"]] <- rowSums(ctc_data[impulsivity_vars],
        na.rm = TRUE)/rowSums(!is.na(ctc_data[impulsivity_vars]))

    ## add labels to data
    ctc_score_dat_labels[["ctc_impulsivity"]] <- "CBQ Impulsivity Total Score"

    # Inhibitory Control
    inhib_vars <- c("ctc38", "ctc45", "ctc53_rev", "ctc67", "ctc73", "ctc81")
    ctc_score_dat[["ctc_inhibitory_cont"]] <- rowSums(ctc_data[inhib_vars], na.rm = TRUE)/rowSums(!is.na(ctc_data[inhib_vars]))

    ## add labels to data
    ctc_score_dat_labels[["ctc_inhibitory_cont"]] <- "CBQ Inhibitory Control Total Score"

    # Low Intensity Pleasure
    li_pleasure_vars <- c("ctc26", "ctc39", "ctc57", "ctc65", "ctc72", "ctc76",
        "ctc86", "ctc94")
    ctc_score_dat[["ctc_lowintensity_pleasure"]] <- rowSums(ctc_data[li_pleasure_vars],
        na.rm = TRUE)/rowSums(!is.na(ctc_data[li_pleasure_vars]))

    ## add labels to data
    ctc_score_dat_labels[["ctc_lowintensity_pleasure"]] <- "CBQ Low Intensity Pleasure Total Score"

    # Perceptual Sensitivity
    percept_vars <- c("ctc5", "ctc13", "ctc24", "ctc32", "ctc47", "ctc83_rev")
    ctc_score_dat[["ctc_perceptual_sensitivity"]] <- rowSums(ctc_data[percept_vars],
        na.rm = TRUE)/rowSums(!is.na(ctc_data[percept_vars]))

    ## add labels to data
    ctc_score_dat_labels[["ctc_perceptual_sensitivity"]] <- "CBQ Perceptual Sensitivity Total Score"

    # Sadness
    sad_vars <- c("ctc8", "ctc20", "ctc27", "ctc31", "ctc54_rev", "ctc56_rev",
        "ctc74_rev")
    ctc_score_dat[["ctc_sadness"]] <- rowSums(ctc_data[sad_vars], na.rm = TRUE)/rowSums(!is.na(ctc_data[sad_vars]))

    ## add labels to data
    ctc_score_dat_labels[["ctc_sadness"]] <- "CBQ Sadness Total Score"

    # Shyness
    shy_vars <- c("ctc11_rev", "ctc37", "ctc42", "ctc52", "ctc60_rev", "ctc70")
    ctc_score_dat[["ctc_shyness"]] <- rowSums(ctc_data[shy_vars], na.rm = TRUE)/rowSums(!is.na(ctc_data[shy_vars]))

    ## add labels to data
    ctc_score_dat_labels[["ctc_shyness"]] <- "CBQ Shyness Total Score"

    # Smiling and Laughter
    smile_vars <- c("ctc19_rev", "ctc48_rev", "ctc55", "ctc77", "ctc79", "ctc80_rev")
    ctc_score_dat[["ctc_smile_laughter"]] <- rowSums(ctc_data[smile_vars], na.rm = TRUE)/rowSums(!is.na(ctc_data[smile_vars]))

    ## add labels to data
    ctc_score_dat_labels[["ctc_smile_laughter"]] <- "CBQ Smiling and Laughter Total Score"

    # Big 3 - Surgency reverse the shyness scale
    ctc_score_dat[["ctc_shyness_rev"]] <- 8 - ctc_score_dat["ctc_shyness"]

    ctc_score_dat[["ctc_surgency"]] <- rowMeans(ctc_score_dat[c("ctc_activity",
        "ctc_highintensity_pleasure", "ctc_impulsivity", "ctc_shyness_rev")])

    # remove shyness-reveresed scale
    ctc_score_dat <- ctc_score_dat[, !names(ctc_score_dat) == "ctc_shyness_rev"]

    ## add labels to data
    ctc_score_dat_labels[["ctc_surgency"]] <- "CBQ Big 3 - Surgency Total Score"

    # Big 3 - Negative Affect remove soothability scale
    ctc_score_dat[["ctc_soothability_rev"]] <- 8 - ctc_score_dat["ctc_soothability"]

    ctc_score_dat[["ctc_neg_affect"]] <- rowMeans(ctc_score_dat[c("ctc_anger",
        "ctc_discomfort", "ctc_fear", "ctc_sadness", "ctc_soothability_rev")])

    # remove soothability-reversed scale
    ctc_score_dat <- ctc_score_dat[, !names(ctc_score_dat) == "ctc_soothability_rev"]

    ## add labels to data
    ctc_score_dat_labels[["ctc_neg_affect"]] <- "CBQ Big 3 - Negative Affect Total Score"

    # Big 3 - Effortful Control remove soothability scale
    ctc_score_dat[["ctc_soothability_rev"]] <- 8 - ctc_score_dat["ctc_soothability"]

    ctc_score_dat[["ctc_effortful_cont"]] <- rowMeans(ctc_score_dat[c("ctc_attention",
        "ctc_inhibitory_cont", "ctc_lowintensity_pleasure", "ctc_perceptual_sensitivity")])

    # remove soothability-reversed scale
    ctc_score_dat <- ctc_score_dat[, !names(ctc_score_dat) == "ctc_soothability_rev"]

    ## add labels to data
    ctc_score_dat_labels[["ctc_effortful_cont"]] <- "CBQ Big 3 - Effortful Control Total Score"

    #### 3. Clean Export/Scored Data #####
    ## round data
    if (isTRUE(ID_arg)){
        ctc_score_dat[2:ncol(ctc_score_dat)] <- round(ctc_score_dat[2:ncol(ctc_score_dat)], digits = 3)
    } else {
        ctc_score_dat <- round(ctc_score_dat, digits = 3)
    }

    ## make sure the variable labels match in the dataset
    ctc_score_dat = sjlabelled::set_label(ctc_score_dat, label = matrix(unlist(ctc_score_dat_labels,
        use.names = FALSE)))

    return(ctc_score_dat)
}

