#' util_copy_to_source: A function to copy a file into sourcedata
#'
#' @param base_wd (string) full path to directory that contains both the untouchedRaw and bids directories
#' @param task_dir (string) path to directory with all task files to be copied into sourcedata
#' @param task_str (string) task string used in filenaming (e.g., 'actigraph')
#' @param sub_str (string) bids-formatted session string. e.g., 'sub-001'
#' @param sub_id (numeric) participant number
#' @param ses_str (string) bids-formatted session string. e.g., 'ses-1'
#' @param sourcefile_prefix (string) string to prefix filename with in sourcedata (optional)
#' @param file_pattern (string) sting to identify files (optional)
#' @param overwrite (logical) logical indicating whether file should be overwritten
#'
#' @examples
#' util_copy_to_source(base_wd, task_dir = foodview_dir, task_str = 'foodview', sub_str = 'sub-001', ses_str = 'ses-1', overwrite = TRUE)
#'
#' @export
#'


util_copy_to_source <- function(base_wd, task_dir, task_str, sub_str, ses_str, sub_id, file_pattern, overwrite) {


  # Actigraph ####
  if (task_str == 'actigraph'){

    # set sourcedata directory for task files
    sub_task_source_dir <- file.path(base_wd, 'BIDSdat', 'sourcedata', sub_str, ses_str, 'motion')

    raw_files <- basename(path = task_dir)

    if(grepl('.agd', raw_files)){
      rename_file <- paste0(sub_str, '_motion-10sec.agd')
    } else {
      rename_file <- paste0(sub_str, '_motion.gt3x')
    }
  }


  # Save ####
  # set sourcedata file
  source_paths <- file.path(sub_task_source_dir, rename_file)

  if (sum(file.exists(source_paths[1])) != length(source_paths) | isTRUE(overwrite)) {

    # create sub_task_source_dir if it doesnt exist
    if (!dir.exists(sub_task_source_dir)) {
      dir.create(sub_task_source_dir, recursive = TRUE)
    }

    # copy file into sub_task_source_dir
    file.copy(from = task_dir, to = source_paths, overwrite = overwrite)

    if (isTRUE(overwrite)){
      return('overwrote with new version')
    } else {
      return('complete')
    }

  } else {
    return('exists')
  }
}
