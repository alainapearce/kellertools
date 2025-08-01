#' util_actigraph_ggir: Set up and run GGIR() from the GGIR package
#'
#' This function fills out necessary settings to set up and run GGIR actigraph processing
#'
#' Note: Uses activity thresholds set according to:
#'  \itemize{
#'    \item{Hildebrand M, VAN Hees VT, Hansen BH, Ekelund U. Age group comparability of raw accelerometer output from wrist- and hip-worn monitors. Med Sci Sports Exerc. 2014 Sep;46(9):1816-24. doi: 10.1249/MSS.0000000000000289. (\href{https://pubmed.ncbi.nlm.nih.gov/24887173/}{PubMed})}
#'    \item{Hildebrand M, Hansen BH, van Hees VT, Ekelund U. Evaluation of raw acceleration sedentary thresholds in children and adults. Scand J Med Sci Sports. 2017 Dec;27(12):1814-1823. doi: 10.1111/sms.12795. Epub 2016 Nov 22. PMID: 27878845.(\href{https://pubmed.ncbi.nlm.nih.gov/27878845/}{PubMed})}
#'  }
#'
#'  References:
#'  \itemize{
#'  \item{van Hees V, Migueles J, Sabia S, Patterson M, Fang Z, Heywood J, Capdevila Pujol J, Kushleyeva L, Chen M, Yerramalla M, Bos P, Sanders T, Zhao C, Meneghel Danilevicz I, Barreto Mesquita V, Segantin G, Medical Research Council UK, Accelting, French National Research Agency (2025). GGIR: Raw Accelerometer Data Analysis. doi:10.5281/zenodo.1051064, R package version 3.2-9, https://zenodo.org/records/1051064.}
#'  }
#'
#'
#' @param data_list list of file paths to raw data
#' @param deriv_dir full path to the derivative directory for output to be stored
#' @param study_name character string with study name
#' @inheritParams util_copy_to_source
#' @param sleepwindowType how to treat sleep window. Options include 'SPT' and 'TimeInBed'. Default is 'STP' (only use 'TimeInBed' if specifying sleep log location in loglocation)
#' @param loglocation Path to csv file with sleep log information. Default is c().
#' @param colid column with participant ID information. Defualt = 1.
#' @param part5_agg2_60seconds From GGIR: "Whether to use aggregate epochs of 60 sec as part of the GGIR g.part5 analysis. Aggregations is done by averaging. Note that when workign with count metrics such as Neishabouri counts, this means that the threshold can stay the same as in part 2, because the threshold is expressed relative to the original epoch size." Default = TRUE
#' @param part6CR From GGIR: "indicate whether circadian rhythm analysis should be run by part 6, this includes: cosinor analysis, extended cosinor analysis, IS, IV, and phi." Default = TRUE
#'
#' @examples
#'
#' # setup post-processing for actigraph data
#' util_actigraph_ggir(deriv_dir, sub_str, study_name, actigraph_path, ggir_path)
#'
#' \dontrun{
#' }
#'
#'
#' @export
util_actigraph_ggir <- function(data_list, deriv_dir, study_name, overwrite, sleepwindowType = 'SPT', loglocation = c(), colid = 1, part5_agg2_60seconds= TRUE, part6CR = TRUE){

  #make directory if needed
  if (!dir.exists(deriv_dir)) {
    dir.create(deriv_dir, recursive = TRUE)
  }

  # set up call
  ggir_data <- GGIR::GGIR(datadir = data_list,
                          outputdir = deriv_dir,
                          configfile = 'config_files/util_ggir_config.csv',
                          studyname = study_name,
                          mode = 1:6,
                          overwrite = overwrite,
                          loglocation = loglocation,
                          colid = colid,
                          coln = 1,
                          part5_agg2_60seconds = TRUE,
                          part6CR = TRUE)


  # rename ouput file
  if (!dir.exists(file.path(deriv_dir, 'ggir_output'))) {
    file.rename(file.path(deriv_dir, paste0('output_', study_name)), file.path(deriv_dir, 'ggir_output'))
  } else {
    print(paste0(deriv_dir, 'ggir_output already exisits. Could not rename output_actigraphy to ggir_output. Do manually or remove old ggir_output file and re-process data.'))
  }

  return(ggir_data)
}
