% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fbs_databases.R
\name{fbs_databases}
\alias{fbs_databases}
\title{fbs_databases: Generate all desired databases for the Food and Brain Study}
\usage{
fbs_databases(
  databases,
  model_DD = FALSE,
  write_dat = TRUE,
  write_path,
  data_path,
  return_data = FALSE,
  child_file_pattern,
  parent_file_pattern,
  visit_file_pattern
)
}
\arguments{
\item{databases}{(optional) list of strings to indicate which databases to process. If not entered, all databases will be generated. Options include: 1) 'demo' for Demographic, 2) 'anthro' for Anthropometrics, 3) 'intake' for Intake, 4) 'food_qs' for food-related questionnaires, 5) 'psych_qs' for cognitive and psych related data, 6) 'dd' for Delay Discounting, 7) 'intero' for Interoception data, 8) 'notes' for Notes, 9) 'pna' for Prefer Not to Answer, and 10) 'micro' for microstructure data (note - microstructure will always be in separate databases)}

\item{model_DD}{Indicate if delay discounting data should be modeled. This will take an addition 3-5 minutes of processing time. Default = FALSE. The Delay Discounting database will only be generate if set to TRUE.}

\item{write_dat}{indicate whether to write databases. Default is TRUE.}

\item{write_path}{(optional) a string with the path indicating where to save the generated databases if write_dat is TRUE (default option). If no path is given, databases will be written to working directory.}

\item{data_path}{(optional) the full path to the directory where the raw Qualtrics database are saved, EXCLUDING the .sav file name (e.g., '.../b-childfoodlab_Shared/Active_Studies/RO1_Brain_Mechanisms_IRB_5357/Participant_Data/untouchedRaw/Qualtrics_Raw/'). If NOT entered, will assume the database is saved in the working directory.}

\item{return_data}{indicate whether to return databases to the environment. Default is FALSE.}

\item{child_file_pattern}{(optional) This is only needed if file naming deviates from the standard naming: e.g., Child_V#_*sav'. Only enter the string indicating a respondent (e.g., 'Child').}

\item{parent_file_pattern}{(optional) This is only needed if file naming deviates from the standard naming: e.g., Parent_V#_*sav'. Only enter the string indicating a respondent (e.g., 'Child').}

\item{visit_file_pattern}{(optional) This is only needed if file naming deviates from the standard naming for visits: e.g., V# . Only enter the string indicating a visit (e.g., 'V' for 'V1' or 'Visit_' for 'Visit_1').}
}
\value{
A list containing all databases that were generated
}
\description{
This function will generate up-to-date databases for the Food and Brain Study. The following databases are will be created unless only specific databases are specified:
1) Demographics database: includes child (e.g., age, sex, pubertal status), parent characteristics (e.g., age, sex, education, alcohol use/abuse), and household characteristics (e.g., food insecurity, income)
2) Anthropometrics: includes child and parent height/weight and child adiposity (DXA), actigraphy, physical activity, and sleep
3) Intake: includes child liking ratings, Freddy Fullness, and intake for all eating paradigms
4) Food Related Behaviors and Traits: includes all questionnaires assessing child or parent food related behaviors and/or traits (e.g., CEBQ, CFQ, TFEQ, etc.) and body image
5) (Neuro)Psychological Assessments: includes the WASI and all questionnaires assessing the child's cognitive and/or psychosocial functioning (e.g., BRIEF, BIS/BAS, anxiety, etc.)
6) Delay Discounting: includes the modeling results for delay discounting
7) Interoception: includes all data related to the heart beat interoception task
8) Notes: this is a reference databases that includes any research and/or parent notes or updates from each visit
9) Prefer Not to Answer: this is a reference databases that includes any questions the 'prefer not to answer' option was marked. Only saved in case there is a desire to determine the number of responses on an item that were missing versus 'prefer not to answer'.
10) Microstrucure: includes databases related to microstructure coding of meals
}
\details{
To process the raw data, the raw databases from Qualtrics MUST follow the naming convention: Child_V1_YYYY-MM-DD.sav, Child_V1_Home_YYY-MM-DD.sav, Child_V1_Lab_YYY-MM-DD.sav, and Parent_V1_YYY-MM-DD.sav. The databases must all be in the SAME directory to be processed if the data_path is not entered and the directory organization does not follow the structure laid out in the DataManual.
}
\examples{
#if in same working directory as data, want all databases, and the databases to be written to working directory:
fbs_data_proc <- fbs_databases(model_DD = TRUE)

#if only want the Demographics and Intake databases written to working directory:
fbs_data_proc <- fbs_databases(databases = c('demo', 'intake'))

#if want to work with the Demographics database but not write it to a file:
fbs_data_proc <- fbs_databases(databases = 'demo', write_dat = FALSE)

\dontrun{
#databases must be strings. The following will not run:
fbs_data_proc <- fbs_databases(databases = c(demo, intake))

}

}
\seealso{
Raw data from Qualtrics is processed using the following scripts: \code{\link{util_fbs_merge_v1dat}}, \code{\link{util_fbs_merge_v2dat}}, \code{\link{util_fbs_merge_v3dat}}, \code{\link{util_fbs_merge_v4dat}}, \code{\link{util_fbs_merge_v5dat}}, \code{\link{util_fbs_merge_v6dat}}, \code{\link{util_fbs_merge_v7dat}}
}
