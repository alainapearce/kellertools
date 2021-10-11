#' cebq_score: Calculates the Children's Eating Behavior Questionnaire sub-scales for PROPERLY formatted data
#'
#' This function calculates the the eight Children's Eating Behavior Questionnaire subscales:
#'
#' @param cebq_dat A dataset with the 35 Children's Eating Behavior Questionnaire questions and raw responses with each row representing a participant
#' @param parID_var variable name for column containg participant IDs, if cebq_dat contains IDS
#' @param reverse_scored (logic) TRUE/FLASE - raw responses have already been reversed scored (e.g., due to how the levels were coded in RedCap)
#'
#' @return A numeric value for each of the eight subscales for each row in the dataset
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
cebq_score <- function(cebq_dat, parID_var, reverse_scored) {

  ####             1. Set up/initial checks             #####
  # make sure parameters are numeric
  if (is.character(par[[1]])) {
    par <- as.numeric(par)
  } else if (is.data.frame(par)) {
    par <- data.matrix(par)
  }


  # get entered of default function names as characters
  if (model_str == 'LODE' | model_str == 'lode'){
    #set to standard str
    model_str <- 'LODE'

    #get functions
    time_fn <- substitute(LODE_Time)
    fit_fn <- substitute(LODE_Fit)
    intake_fn <- substitute(LODE_Intake)
  } else if (model_str == 'Quad' | model_str == 'quad'){
    #set to standard str
    model_str <- 'Quad'

    #get functions
    time_fn <- substitute(Quad_Time)
    fit_fn <- substitute(Quad_Fit)
    intake_fn <- substitute(Quad_Intake)
  } else {
    stop("model_str does not match available models. Options are 'LODE' or 'Quad'")
  }

  #check error input args
  error_arg <- methods::hasArg(error_method)
  error_measure_arg <- methods::hasArg(error_measure)
  if (isTRUE(error_arg)) {
    if (error_method == 'rmse' | error_method == 'RMSE'){
      #set standard str
      error_method <- 'rmse'
    } else if (error_method == 'R2' | error_method == 'r2'){
      #set standard str
      error_method <- 'R2'
    } else if (error_method == 'both' | error_method == 'Both'){
      #set standard str
      error_method <- 'both'
    } else {
      stop("Error was not calculated: Unrecognized value for error_method. Options include: 'rmse', 'R2', or 'both'.")
    }

    #add measure names
    if (isTRUE(error_measure_arg)) {
      if (error_measure == "timing" | error_measure == "Timing") {
        #set standard str
        error_measure <- 'timing'
      } else if (error_measure == "intake" | error_measure == "Intake") {
        #set standard str
        error_measure <- 'intake'
      } else if (error_measure == 'both' | error_measure == 'Both'){
        #set standard str
        error_measure <- 'both'
      } else {
        stop("Error was not calculated: Unrecognized value for error_measure. Options include: 'timing', 'intake', or 'both'.")
      }
    } else {
      #default is timing
      error_measure <- 'timing'
    }
  }

  #check data input
  data_arg <- methods::hasArg(data)

  # get estimated intake
  data$Estimated_intake <- sapply(data[, timeVar], LODE_Intake, parameters = c(par[1], par[2]), Emax = Emax)

  # re-name estimated intake variable
  estimated_name <- paste0("Estimated_", intakeVar)
  names(data)[length(names(data))] <- estimated_name

  # calculate the error/residual between predicted and actual intake
  data$resid <- data[, intakeVar] - data[, estimated_name]

  # get sigma
  sigma <- sum(data$resid^2) / length(data$resid)

  # ll equation
  ll <- (-length(data$resid) / 2) * (log(2 * pi * sigma^2)) + ((-1 / (2 * sigma^2)) * (sum(data$resid^2)))

  # rerun -2ll
  return(-2 * ll)
}
