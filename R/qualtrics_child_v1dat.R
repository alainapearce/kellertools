#' qualtrics_child_v1dat: Process raw qualtrics visit 1 data for the child
#'
#' This function ...
#'
#' @param date_str
#' @param data_path (optional)
#'
#'
#' @return A raw database containing child visit 1 data from qualtrics
#'
#' @examples
#'
#' \dontrun{
#' }
#'
#' @seealso To get best fit parameters use \code{\link{LODE_Fit}}.
#' To get fit your intake data using the  the Quadratic model
#' (Kissileff, 1982; Kissileff & Guss, 2001), see \code{\link{Quad_Fit}}
#' and \code{\link{Quad_n2ll}}.
#'
#' @export
qualtrics_child_v1dat <- function(date_str, data_path) {


  # while teseting only
  data_path <- '/Users/azp271/OneDrive - The Pennsylvania State University/b-childfoodlab_Shared/Active_Studies/RO1_Brain_Mechanisms_IRB_5357/Participant_Data/untouchedRaw/Qualtrics_Raw/'
  date_str <- '2021_09_16'

  ####             1. Set up/initial checks             #####
  # check that date_str exist and is a string
  datastr_arg <- methods::hasArg(date_str)

  if (isTRUE(datastr_arg) & !is.character(date_str)) {
    stop("data_str must be enter as a string: e.g., '2021_09_16'")
  } else if (isFALSE(datastr_arg)) {
    stop("data_str must set to the data string from the child visit 1 file name: e.g., '2021_09_16'")
  }

  # check that file exists
  datapath_arg <- methods::hasArg(data_path)

  if (isTRUE(datapath_arg)){
    if (!is.character(date_str)){
      stop("data_path must be enter as a string: e.g., '.../Participant_Data/untouchedRaw/Qualtrics_Raw/'")
    } else {
      qv1_child_path <- base::paste0(data_path, '/', 'Child_V1_', date_str, '.sav')
      qv1_child_exists <- base::file.exists(qv1_child_path)
    }

  } else {
    qv1_child_exists <- base::file.exists(qv1_child_path)
  }

  #load data if it exists
  if (isTRUE(qv1_child_exists)){
    qv1_child_dat <- as.data.frame(haven::read_spss(qv1_child_path))

  } else {
    if (isTRUE(datapath_arg)){
      stop('File does not exist. Check date_str and data_path entered')
    } else {
      stop('File does not exist. Check date_str and that the data exists in current working directory')
    }
  }


  ####             2. Clean Data             #####

  # extract variable labels
  qv1_child_labels <- lapply(qv1_child_dat, function(x) attributes(x)$label)

  # clean the data
  qv1_child_clean <- qv1_child_dat[c(1, 11:13, 20:39, 44:46, 55:81, 92:103, 112, 121, 130,
                                   139, 148, 157:335)]


  ## remove all 999 participants

  # re-label and organize
  ## order ideas: 1) child information (sex, dob, h/w, bmi, screen out), freddies, food VAS, intakes (preMeal, EAH, meal duration), wanting, PSD, PSS, notes/etc

  ## get start date in correct format

  ## re-name variables

  ## check labels to be sure all good (most shouldn't need to change)

  ## id all factors and change label values to start at 0 (e.g. change sex from 1-?, 2-? to 0-?, 1-?)
  ## probs can do with lapply statement
}
