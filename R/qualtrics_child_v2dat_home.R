#' qualtrics_child_v2dat_home: Process raw qualtrics visit 2 home data for the child
#'
#' This function loads the .sav raw data file for the child visit 2 data that was
#' collected via Qualtrics at the child's home when the procedure was split due to covid
#' and cleans the data. Cleaning the data involves:
#' 1) extracting all variable descriptions,
#' 2) selecting relevant data columns,
#' 3) removing all practice events (e.g., 999)
#' 4) re-ordering and re-name data columns
#' 5) reformatting dates to be appropriate and computer readable: YYYY-MM-DD
#' 6) re-calculate manual variables
#' 7) re-ordering factor levels to start with value 0
#' 8) random fixes to factor level names and variable descriptions
#'
#' @param date_str the date used in the name of the .sav file (e.g., for file 'Child_V2_Home_2021-10-11.sav',
#' the string '2021-10-11' would be entered)
#' @param data_path (optional) the full path to the V2 Child Qualtircs database EXCLUDING the .sav file name (e.g., '.../b-childfoodlab_Shared/Active_Studies/RO1_Brain_Mechanisms_IRB_5357/Participant_Data/untouchedRaw/Qualtrics_Raw/').
#'
#'
#' @return A list containing: 1) data: data.frame with raw, cleaned data from child visit 2 Qualtrics
#' and 2) dict: all variable descriptions
#'
#' @examples
#' #if in same working directory as data:
#' ch_v2_dat <- qualtrics_child_v2dat('2021-10-11')
#'
#' \dontrun{
#' #date must be a string. The following will not run:
#' ch_v2_dat <- qualtrics_child_v2dat_home(2021-10-11)
#'
#' #date must match the file name - for file named 'Child_V2_Home_2021_09_16', the
#' following will not run:
#' ch_v2_dat <- qualtrics_child_v2dat('2021_10_11')
#' }
#'
#'
#' @export
#'
qualtrics_child_v2dat_home <- function(date_str, data_path) {

    #### 1. Set up/initial checks #####

    # check that date_str exist and is a string

    datastr_arg <- methods::hasArg(date_str)

    if (isTRUE(datastr_arg) & !is.character(date_str)) {
        stop("data_str must be enter as a string: e.g., '2021_10_11'")
    } else if (isFALSE(datastr_arg)) {
        stop("data_str must set to the data string from the child visit 1 file name: e.g., '2021_09_16'")
    }

    # check that file exists
    datapath_arg <- methods::hasArg(data_path)

    if (isTRUE(datapath_arg)) {
        if (!is.character(date_str)) {
            stop("data_path must be enter as a string: e.g., '.../Participant_Data/untouchedRaw/Qualtrics_Raw/'")
        }
    }


    #### 2. Load Data #####

    if (isTRUE(datapath_arg)) {
        qv2_child_path <- paste0(data_path, "/Final_CovidAtHome/Child_V2_Home_", date_str, ".sav")
    } else {
        qv2_child_path <- paste0("Final_CovidAtHome/Child_V2_Home", date_str, ".sav")
    }

    # check if file exists
    qv2_child_exists <- file.exists(qv2_child_path)

    # load data if it exists
    if (isTRUE(qv2_child_exists)) {
        qv2_child_dat <- as.data.frame(haven::read_spss(qv2_child_path))

    } else {
        if (isTRUE(datapath_arg)) {
            stop("File does not exist. Check date_str and data_path entered")
        } else {
            stop("File does not exist. Check date_str and that the data exists in current working directory")
        }
    }

    #### 3. Clean Data #####

    # 1) extract variable labels/descriptions ####
    qv2_child_labels <- lapply(qv2_child_dat, function(x) attributes(x)$label)

    # 2) selecting relevant data columns ####
    qv2_child_clean <- qv2_child_dat[c(1, 18:125)]

    ## update labels
    qv2_child_clean_labels <- qv2_child_labels[c(1, 18:125)]


    # 3) removing all practice events (e.g., 999)  ####

    #Note, ID variable is Q1
    qv2_child_clean <- qv2_child_clean[!is.na(qv2_child_clean[["Q1"]]) & qv2_child_clean[["Q1"]] < 999, ]

    # 4) re-ordering and re-name data columns  ####

    #general order: 1) child information (ID), 2) freddies, 3) food VAS 4), intakes (meal, meal duration), 5) KFQ, TESQE, RCMAS 6) notes

    qv2_child_clean <- qv2_child_clean[c(2, 1, 3:109)]

    qv2_child_clean_labels <- qv2_child_clean_labels[c(2, 1, 3:109)]

    ## re-name variables -- make lowercase
    names(qv2_child_clean) <- tolower(names(qv2_child_clean))

    ## manually update variables
    names(qv2_child_clean)[names(qv2_child_clean) == "childnotes"] <- "child_notes"
    names(qv2_child_clean)[names(qv2_child_clean) == "startdate"] <- "start_date"
    names(qv2_child_clean)[names(qv2_child_clean) == "q1"] <- "id"

    ## update data labels
    names(qv2_child_clean_labels) <- names(qv2_child_clean)

    # 5) reformatting dates to be appropriate and computer readable YYYY-MM-DD  ####
    qv2_child_clean[["start_date"]] <- lubridate::ymd(as.Date(qv2_child_clean[["start_date"]]))
    qv2_child_clean_labels[["start_date"]] <- "start_date from qualtrics survey meta-data converted to format yyyy-mm-dd in R"


    # 6) re-ordering factor levels to start with value 0 ####

    ## fix kfq factor levels to start at 0
    kfq_names <- names(qv2_child_clean)[3:48]
    for (var in 1:length(kfq_names)) {
        var_name <- as.character(kfq_names[var])

        qv2_child_clean[[var_name]] <- sjlabelled::set_labels(qv2_child_clean[[var_name]], labels = c(`Never eat this` = 0,
            `Less than once in 7 days` = 1, `1-2 times in 7 days` = 2, `3-5 times in 7 days` = 3, `6-7 times in 7 days` = 4,
            `More than 7 times in 7 days` = 5))

        set_attr <- attributes(qv2_child_clean[[var_name]])

        qv2_child_clean[[var_name]] <- ifelse(is.na(qv2_child_clean[[var_name]]), NA, ifelse(qv2_child_clean[[var_name]] ==
            1, 0, ifelse(qv2_child_clean[[var_name]] == 2, 1, ifelse(qv2_child_clean[[var_name]] == 3, 2, ifelse(qv2_child_clean[[var_name]] ==
            4, 3, ifelse(qv2_child_clean[[var_name]] == 5, 4, 5))))))

        attributes(qv2_child_clean[[var_name]]) <- set_attr

        qv2_child_clean_labels[[var_name]] <- paste0(qv2_child_clean_labels[[var_name]], " - re-leveled in R to start with 0")
    }

    # 7) random fixes to factor level names and variable descriptions ####

    ## tesqe: Change value for 'Don't know' from 99 to -99
    tesqe_names <- names(qv2_child_clean)[49:72]

    for (var in 1:length(tesqe_names)) {
        var_name <- as.character(tesqe_names[var])
        qv2_child_clean[[var_name]] <- sjlabelled::set_labels(qv2_child_clean[[var_name]], labels = c(Never = 1, Rarely = 2,
            Sometimes = 3, Regularly = 4, Often = 5, `Dont know` = -99))

        set_attr <- attributes(qv2_child_clean[[var_name]])

        # convert 99s to -99 and make numeric variable labels only update
        qv2_child_clean[[var_name]] <- ifelse(qv2_child_clean[[var_name]] == 99, -99, qv2_child_clean[[var_name]])

        attributes(qv2_child_clean[[var_name]]) <- set_attr

        qv2_child_clean_labels[[var_name]] <- paste0(qv2_child_clean_labels[[var_name]], " - values modified in R so Dont know = -99")

    }

    # id label
    qv2_child_clean_labels[["id"]] <- "participant ID"

    #### 8) Format for export ####

    # put data in order of participant ID for ease

    qv2_child_clean <- qv2_child_clean[order(qv2_child_clean[["id"]]), ]

    # make sure the variable labels match in the dataset
    qv2_child_clean = sjlabelled::set_label(qv2_child_clean, label = matrix(unlist(qv2_child_clean_labels, use.names = FALSE)))

    ## make list of data frame and associated labels
    qv2_child <- list(data = qv2_child_clean, dict = qv2_child_clean_labels)

    ## want an export options??

    return(qv2_child)
}
