% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score_cebq.R
\name{score_cebq}
\alias{score_cebq}
\title{score_cebq: Scored data from the Children's Eating Behavior Questionnaire}
\usage{
score_cebq(cebq_data, parID)
}
\arguments{
\item{cebq_data}{a data.frame all items for the Children's Eating Behavior Questionnaire following the naming conventions described above}

\item{parID}{(optional) name of participant ID column in intake_data. If included the output dataset will be matched by parID, if not included the output dataset will be in the order of intake_data but will have no participant identifier.}
}
\value{
A dataset with subscale scores for the Children's Eating Behavior Questionnaire
}
\description{
This function scores the Children's Eating Behavior Questionnaire and provides subscale scores for the following behaviors: Food Responsiveness, Emotional Overeating, Enjoyment of Food, Desire to Drink, Satiety Responsiveness, Slowness in Eating, Emotional Undereating, and Food Fussiness
}
\details{
To use this function, the data must be prepared according to the following criteria:
1) The data must include all individual questionnaire items
2) The  columns/variables must match the following naming convention: 'cebq#' where # is the question number (1-35)
3) All questions must have the numeric value for the choice: 1 - Never, 2 - Rarely, 3 - Sometimes, 4 - Often, 5 - Always
4) This script will apply reverse scoring so all levels must be true to the scale described above

Note, as long as variable names match those listed, the dataset can include other variables
}
\examples{

# scoring for the CEBQ with IDs
cebq_score_data <- score_cebq(cebq_data, parID = 'ID')

\dontrun{
}

}
\references{
Primary References for the Children's Eating Behavior Questionniare and Scoring:
Wardle, J., Guthrie, C. A., Sanderson, S., & Rapoport, L. (2001). Development of the children’s eating behaviour questionnaire. Journal of Child Psychology and Psychiatry, 42, 963–970. https://doi.org/10.1017/S0021963001007727 (\href{https://pubmed.ncbi.nlm.nih.gov/11693591/}{PubMed})
}
\seealso{
Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_parent_v2dat}}
}
