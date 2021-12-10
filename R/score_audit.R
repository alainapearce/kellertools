#' score_audit: Score data from the Alcohol Use Disorders Identification Test
#'
#' This function scores the Alcohol Use Disorders Identification Test and provides subscale scores for the following behaviors: Food Responsiveness, Emotional Overeating, Enjoyment of Food, Desire to Drink, Satiety Responsiveness, Slowness in Eating, Emotional Undereating, and Food Fussiness
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'audit#' where # is the question number (1-35)
#' 3) All questions must have the numeric value for the choice: 1 - Never, 2 - Rarely, 3 - Sometimes, 4 - Often, 5 - Always
#' 4) This script will apply reverse scoring so all levels must be true to the scale described above
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Primary References for the Children's Eating Behavior Questionniare and Scoring:
#' Wardle, J., Guthrie, C. A., Sanderson, S., & Rapoport, L. (2001). Development of the children’s eating behaviour questionnaire. Journal of Child Psychology and Psychiatry, 42, 963–970. https://doi.org/10.1017/S0021963001007727 (\href{https://pubmed.ncbi.nlm.nih.gov/11693591/}{PubMed})
#'
#' @param audit_data a data.frame all items for the Alcohol Use Disorders Identification Test following the naming conventions described above
#' @inheritParams fbs_intake
#'
#' @return A dataset with subscale scores for the Alcohol Use Disorders Identification Test
#' @examples
#'
#' # scoring for the audit with IDs
#' audit_score_data <- score_audit(audit_data, parID = 'ID')
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{qualtrics_parent_v2dat}}
#'
#'
#' @export

score_audit <- function(audit_data, parID) {

    #### 1. Set up/initial checks #####

    # check that audit_data exist and is a data.frame
    data_arg <- methods::hasArg(audit_data)

    if (isTRUE(data_arg) & !is.data.frame(audit_data)) {
        stop("audit_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("audit_data must set to the data.frame with amount consumed for each food item")
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (isTRUE(ID_arg)){
        if (!(parID %in% names(pwlb_data))) {
            stop("variable name entered as parID is not in pwlb_data")
        }
    }

    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    audit_score_dat <- data.frame(audit_fr = rep(NA, nrow(audit_data)), audit_eoe = rep(NA,
        nrow(audit_data)), audit_ef = rep(NA, nrow(audit_data)), audit_dd = rep(NA,
        nrow(audit_data)), audit_sr = rep(NA, nrow(audit_data)), audit_se = rep(NA,
        nrow(audit_data)), audit_eue = rep(NA, nrow(audit_data)), audit_ff = rep(NA,
        nrow(audit_data)), audit_approach = rep(NA, nrow(audit_data)), audit_avoid = rep(NA,
        nrow(audit_data)))

    if (isTRUE(ID_arg)) {
        audit_score_dat <- data.frame(audit_data[[parID]], audit_score_dat)
        names(audit_score_dat)[1] <- parID
    }

    # set up labels for audit_score_dat
    audit_score_dat_labels <- lapply(audit_score_dat, function(x) attributes(x)$label)

    # calculate reversed scores

    reverse_qs <- c("audit3", "audit4", "audit10", "audit16", "audit32")

    for (var in 1:length(reverse_qs)) {
        var_name <- reverse_qs[var]
        reverse_name <- paste0(var_name, "_rev")

        audit_data[[reverse_name]] <- ifelse(is.na(audit_data[[var_name]]), NA,
            ifelse(audit_data[[var_name]] == 1, 5, ifelse(audit_data[[var_name]] ==
                2, 4, ifelse(audit_data[[var_name]] == 4, 2, ifelse(audit_data[[var_name]] ==
                5, 1, 3)))))
    }

    ## Score Subscales

    # Food Responsiveness
    FR_vars <- c("audit12", "audit14", "audit19", "audit28", "audit34")
    audit_score_dat[["audit_fr"]] <- rowMeans(audit_data[FR_vars])

    ## add labels to data
    audit_score_dat_labels[["audit_fr"]] <- "audit Food Responsiveness Total Score"

    # Emotional Overeating
    EOE_vars <- c("audit2", "audit13", "audit15", "audit27")
    audit_score_dat[["audit_eoe"]] <- rowMeans(audit_data[EOE_vars])

    ## add labels to data
    audit_score_dat_labels[["audit_eoe"]] <- "audit Emotional Overeating Total Score"

    # Enjoyment of Food
    EF_vars <- c("audit1", "audit5", "audit20", "audit22")
    audit_score_dat[["audit_ef"]] <- rowMeans(audit_data[EF_vars])

    ## add labels to data
    audit_score_dat_labels[["audit_ef"]] <- "audit Enjoyment of Food Total Score"

    # Desire to Drink
    DD_vars <- c("audit6", "audit29", "audit31")
    audit_score_dat[["audit_dd"]] <- rowMeans(audit_data[DD_vars])

    ## add labels to data
    audit_score_dat_labels[["audit_dd"]] <- "audit Desire to Drink Total Score"

    # Satiety Responsiveness
    SR_vars <- c("audit3_rev", "audit17", "audit21", "audit26", "audit30")
    audit_score_dat[["audit_sr"]] <- rowMeans(audit_data[SR_vars])

    ## add labels to data
    audit_score_dat_labels[["audit_sr"]] <- "audit Satiety Responsiveness Total Score"

    # Slowness in Eating
    SE_vars <- c("audit4_rev", "audit8", "audit18", "audit35")
    audit_score_dat[["audit_se"]] <- rowMeans(audit_data[SE_vars])

    ## add labels to data
    audit_score_dat_labels[["audit_se"]] <- "audit Slowness in Eating Total Score"

    # Emotional Under Eating
    EUE_vars <- c("audit9", "audit11", "audit23", "audit35")
    audit_score_dat[["audit_eue"]] <- rowMeans(audit_data[EUE_vars])

    ## add labels to data
    audit_score_dat_labels[["audit_eue"]] <- "audit Emotional Under Eating Total Score"

    # Food Fussiness
    FF_vars <- c("audit7", "audit10_rev", "audit16_rev", "audit24", "audit32_rev",
        "audit33")
    audit_score_dat[["audit_ff"]] <- rowMeans(audit_data[FF_vars])

    ## add labels to data
    audit_score_dat_labels[["audit_ff"]] <- "audit Food Fussiness Total Score"


    # Total Approach Score
    audit_score_dat[["audit_approach"]] <- rowMeans(audit_data[c(FR_vars, EOE_vars,
        EF_vars, DD_vars)])

    ## add labels to data
    audit_score_dat_labels[["audit_approach"]] <- "audit Approach Total Score"

    # Total Avoid Score
    audit_score_dat[["audit_avoid"]] <- rowMeans(audit_data[c(SR_vars, SE_vars, EUE_vars,
        FF_vars)])

    ## add labels to data
    audit_score_dat_labels[["audit_avoid"]] <- "audit Avoid Total Score"

    #### 3. Clean Export/Scored Data #####
    ## round data
    if (isTRUE(ID_arg)){
        audit_score_dat[2:ncol(audit_score_dat)] <- round(audit_score_dat[2:ncol(audit_score_dat)], digits = 3)
    } else {
        audit_score_dat <- round(audit_score_dat, digits = 3)
    }

    ## make sure the variable labels match in the dataset
    audit_score_dat = sjlabelled::set_label(audit_score_dat, label = matrix(unlist(audit_score_dat_labels,
        use.names = FALSE)))

    return(audit_score_dat)
}

