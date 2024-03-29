% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util_fbs_merge_v2.R
\name{util_fbs_merge_v2}
\alias{util_fbs_merge_v2}
\title{util_fbs_merge_v2: Merge all Visit 2 raw Qualtrics databases and organize variable in database order}
\usage{
util_fbs_merge_v2(
  child_file_pattern,
  parent_file_pattern,
  parentV4_file_pattern,
  data_path
)
}
\arguments{
\item{child_file_pattern}{string with the pattern to search for to find the raw child data files. The pattern must contain respondent and visit number (e.g., for files from child visit 1, would enter 'Child_V1').}

\item{parent_file_pattern}{string with the pattern to search for to find the raw parent data files. The pattern must contain respondent and visit number (e.g., for files from parent visit 1, would enter 'Parent_V1').}

\item{parentV4_file_pattern}{string with the pattern to search for to find the raw parent data file for visit 4. The pattern must contain respondent and visit number (e.g., 'Parent_V4').}

\item{data_path}{(optional) the full path to the directory where the raw Qualtrics database are saved, EXCLUDING the .sav file name (e.g., '.../b-childfoodlab_Shared/Active_Studies/RO1_Brain_Mechanisms_IRB_5357/Participant_Data/untouchedRaw/Qualtrics_Raw/'). If NOT entered, will assume the database is saved in the working directory.}
}
\value{
A list containing: 1) data: data.frame with raw, cleaned data from parent visit 2 Qualtrics; 2) dict: all variable descriptions; 3) pna_data: data.frame marking participants who 'preferred not to answer' (pna) specific questions; and 4) pna_dict: all variable descriptions for pna_data
}
\description{
This function merges the following visit 2 raw data into a single database and organizes variables in database order: child visit 2, child visit 2-home, child visit 2-lab, and parent visit 2
}
\details{
The databases MUST follow the naming convention: Child_V2_YYYY-MM-DD.sav, Child_V2_Home_YYY-MM-DD.sav, Child_V2_Lab_YYY-MM-DD.sav, and Parent_V2_YYY-MM-DD.sav. The databases must all be in the SAME directory to be processed if the data_path is not entered and the directory organization does not follow the structure laid out in the DataManual.
}
\examples{
#if in same working directory as data with all data. Note - there is no need to add the COVID protocol (i.e., 'Home' or 'Lab') to file_pattern - these files will be search for automatically:
v2dat_scored <- util_fbs_merge_v2(child_file_pattern = 'Child_V2', parent_file_pattern = 'Parent_V2')

\dontrun{
#child_file_pattern and parent_file_pattern must be a strings. The following will not run:
v2dat_scored <- util_fbs_merge_v2(child_file_pattern = Child_V2, parent_file_pattern = Parent_V2)

# *_file_pattern must have the respondent ('Child') and visit number ('V2'). If just enter 'Child' or 'Parent', the script will not run because it will return multiple files for different child visits. The following will not run:
v2dat_scored <- util_fbs_merge_v2(child_file_pattern = 'Child', parent_file_pattern = 'Parent')
}

}
\seealso{
Raw data from Qualtrics is processed using the following scripts: \code{\link{util_fbs_child_v2dat}}, \code{\link{util_fbs_child_v2dat_home}}, \code{\link{util_fbs_child_v2dat_lab}}, \code{\link{util_fbs_parent_v2dat}}. Visit 2 data is scored using the following scripts: \code{\link{score_cshqa}}, \code{\link{score_tesqe}}, \code{\link{score_kfq}}, \code{\link{score_cebq}}, \code{\link{score_bes}}, \code{\link{score_cfq}}, \code{\link{score_ffbs}}, \code{\link{score_rcmas}}, and \code{\link{score_cbq}}
}
