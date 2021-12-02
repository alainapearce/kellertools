#' score_brief2: Scored data from the Behavioral Rating Inventory of Executive Function-2
#'
#' This function scores the Behavioral Rating Inventory of Executive Function-2 and provides subscale scores for the following behaviors: Inhibit, Self-Monitor, Shift, Emotional Control, Initiate, Working Memory, Plan/Organize, Task Monitoring, Organization of Materials. There are also 4 index scores: Behavioral Regulation Index, Emotion Regulation Index, Cognitive Regulation Index, and the General Executive Composite. Three scores help indicate if responses were valid: Negativity Score, Inconsistency Score, and Infrequency Score
#'
#' Note - the factor structure of this questionnaire has been repeatedly questioned so may want to look further to decide on best approach for each study. The provided subscales are based on the canonical approach in the literature and should not be taken as a recommendation or 'best' factoring approach.
#'
#' These data are scored using the brief2_scoretables data available in the kellertools package to look up age- and sex-nomed t-scores and percentiles.
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'brief#' where # is the question number (1-63)
#' 3) Questions must have the numeric value for the choices: 1 - Never, 2 - Sometimes, 3 - Often
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Gioia GA, Isquith PK, Guy SC, Kenworthy L. BRIEF-2: Behavior Rating Inventory of Executive Function: Professional Manual. Psychological Assessment Resources; 2015.
#'
#' @param brief_data a data.frame all items for the Behavioral Rating Inventory of Executive Function-2 following the naming conventions described above
#' @inheritParams fbs_intake
#'
#' @return A dataset with subscale scores for the Behavioral Rating Inventory of Executive Function-2
#' @examples
#'
#' # scoring for the brief with IDs
#' brief_score_data <- score_brief2(brief_data, parID = 'ID')
#'
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{qualtrics_parent_v4dat}}
#'
#'
#' @export

score_brief2 <- function(brief_data, parID) {

    #### 1. Set up/initial checks #####

    # check that brief_data exist and is a data.frame
    data_arg <- methods::hasArg(brief_data)

    if (isTRUE(data_arg) & !is.data.frame(brief_data)) {
        stop("brief_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("brief_data must set to the data.frame with amount consumed for each food item")
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
    brief_score_dat <- data.frame(inhibit_raw = rep(NA, nrow(brief_data)), inhibit_t = rep(NA, nrow(brief_data)), inhibit_p = rep(NA, nrow(brief_data)), selfmon_raw = rep(NA, nrow(brief_data)), selfmon_t = rep(NA, nrow(brief_data)), selfmon_p = rep(NA, nrow(brief_data)), shift_raw = rep(NA, nrow(brief_data)), shift_t = rep(NA, nrow(brief_data)), shift_p = rep(NA, nrow(brief_data)), emcont_raw = rep(NA, nrow(brief_data)), emcont_t = rep(NA, nrow(brief_data)), emcont_p = rep(NA, nrow(brief_data)), initiate_raw = rep(NA, nrow(brief_data)), initiate_t = rep(NA, nrow(brief_data)), initiate_p = rep(NA, nrow(brief_data)), wm_raw = rep(NA, nrow(brief_data)), wm_t = rep(NA, nrow(brief_data)), wm_p = rep(NA, nrow(brief_data)), planorg_raw = rep(NA, nrow(brief_data)), planorg_t = rep(NA, nrow(brief_data)), planorg_p = rep(NA, nrow(brief_data)), taskmon_raw = rep(NA, nrow(brief_data)), taskmon_t = rep(NA, nrow(brief_data)), taskmon_p = rep(NA, nrow(brief_data)), orgmat_raw = rep(NA, nrow(brief_data)), orgmat_t = rep(NA, nrow(brief_data)), orgmat_p = rep(NA, nrow(brief_data)), bri_raw = rep(NA, nrow(brief_data)), bri_t = rep(NA, nrow(brief_data)), bri_p = rep(NA, nrow(brief_data)), eri_raw = rep(NA, nrow(brief_data)), eri_t = rep(NA, nrow(brief_data)), eri_p = rep(NA, nrow(brief_data)), cri_raw = rep(NA, nrow(brief_data)), cri_t = rep(NA, nrow(brief_data)), cri_p = rep(NA, nrow(brief_data)), gec_raw = rep(NA, nrow(brief_data)), gec_t = rep(NA, nrow(brief_data)), gec_p = rep(NA, nrow(brief_data)), negativity_p = rep(NA, nrow(brief_data)), negativity_cat = rep(NA, nrow(brief_data)), inconsistancy = rep(NA, nrow(brief_data)), inconsistancy_p = rep(NA, nrow(brief_data)), inconsistancy_cat = rep(NA, nrow(brief_data)), infrequency = rep(NA, nrow(brief_data)), infrequency_p = rep(NA, nrow(brief_data)), infrequency_cat = rep(NA, nrow(brief_data)))

    if (isTRUE(ID_arg)) {
        brief_score_dat <- data.frame(brief_data[[parID]], brief_score_dat)
        names(brief_score_dat)[1] <- parID
    }

    # set up labels for brief_score_dat
    brief_score_dat_labels <- lapply(brief_score_dat, function(x) attributes(x)$label)

    ## Score Subscales

    # Inhibit
    inhib_vars <- c("brief1", "brief10", "brief16", "brief24", "brief30", "brief39", "brief48", "brief62")
    brief_score_dat[["inhibit_raw"]] <- rowSums(brief_data[inhib_vars])

    # look up T-score and percentile

    ## add labels to data
    brief_score_dat_labels[["inhibit_raw"]] <- "BRIEF2 Inhibit Raw Score"
    brief_score_dat_labels[["inhibit_t"]] <- "BRIEF2 Inhibit T-Score"
    brief_score_dat_labels[["inhibit_p"]] <- "BRIEF2 Inhibit Percentile"

    #### 3. Clean Export/Scored Data #####
    ## round data
    if (isTRUE(ID_arg)){
        brief_score_dat[2:ncol(brief_score_dat)] <- round(brief_score_dat[2:ncol(brief_score_dat)], digits = 3)
    } else {
        brief_score_dat <- round(brief_score_dat, digits = 3)
    }

    ## make sure the variable labels match in the dataset
    brief_score_dat = sjlabelled::set_label(brief_score_dat, label = matrix(unlist(brief_score_dat_labels,
        use.names = FALSE)))

    return(brief_score_dat)
}

