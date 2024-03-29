% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score_cshqa.R
\name{score_cshqa}
\alias{score_cshqa}
\title{score_cshqa: Scored data from the Child Sleep Habits Questionnaire - Abbreviated}
\usage{
score_cshqa(cshqa_data, study = "fbs", parID)
}
\arguments{
\item{cshqa_data}{a data.frame all items for the Child Sleep Habits Questionnaire - Abbreviated following the naming conventions described above}

\item{study}{a string indicating which study collected the data. Currently, only option and default is 'fbs'. This parameter is included so this script can be adapted for future studies that collect all subscales.}

\item{parID}{(optional) name of participant ID column in intake_data. If included the output dataset will be matched by parID, if not included the output dataset will be in the order of intake_data but will have no participant identifier.}
}
\value{
A dataset with total and subscale scores for the Child Sleep Habits Questionnaire - Abbreviated
}
\description{
This function scores the Child Sleep Habits Questionnaire - Abbreviated and provides both total scores and subscale scores for the following behaviors: Bedtime Resistance, Sleep Onset Delay, Sleep Duration, Sleep Anxiety, Night Wakings, Parasomnias, Sleep Disordered Breathing, and Daytime Sleepiness. For the Food and Brain Study, the Morning Wake Up subscale was left off the questionnaire
}
\details{
To use this function, the data must be prepared according to the following criteria:
1) The data must include all individual questionnaire items
2) The  columns/variables must match the following naming convention: 'cshq_a#' where # is the question number
3) All questions must have the numeric value for the choice: 1 - Never, 2 - Rarely, 3 - Sometimes, 4 - Usually, 5 - Always
4) This script will apply reverse scoring so all levels must be true to the scale described above

Note, as long as variable names match those listed, the dataset can include other variables
}
\examples{

# scoring for FBS study
cshqa_score_date <- score_cshqa(cshqa_data, study = 'fbs', parID = 'ID')

\dontrun{
}

}
\references{
Full Measure:
Owens, J. A., Spirito, A., & McGuinn, M. (2000). The Children’s Sleep Habits Questionnaire (CSHQ): Psychometric Properties of A Survey Instrument for School-Aged Children. SLEEP, 23(8), 1043–1052. (\href{https://pubmed.ncbi.nlm.nih.gov/11145319/}{PubMed})

Abreviated Scoring: C
hawla, J. K., Howard, A., Burgess, S., & Heussler, H. (2021). Sleep problems in Australian children with Down syndrome: The need for greater awareness. Sleep Medicine, 78, 81–87. https://doi.org/10.1016/j.sleep.2020.12.022 (\href{https://pubmed.ncbi.nlm.nih.gov/33412456/}{PubMed})
}
\seealso{
Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_parent_v2dat}}
}
