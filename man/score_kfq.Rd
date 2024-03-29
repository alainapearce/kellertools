% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score_kfq.R
\name{score_kfq}
\alias{score_kfq}
\title{score_kfq: Score data from the Kid's Food Questionnaire}
\usage{
score_kfq(kfq_data, parID)
}
\arguments{
\item{kfq_data}{a data.frame all items for the Kid's Food Questionnaire following the naming conventions described above}

\item{parID}{(optional) name of participant ID column in intake_data. If included the output dataset will be matched by parID, if not included the output dataset will be in the order of intake_data but will have no participant identifier.}
}
\value{
A dataset with a score for the Kid's Food Questionnaire
}
\description{
This function scores the Kid's Food Questionnaire, which is a lab generated measure of child reported food frequency. The modal frequency and score is calculated for the following subscales: Breads and Grains, Vegetables, Fruits, Dairy, Protein, Beverages, Snacks, Deserts, Condiments.
}
\details{
Note: number of items in each category differs so the total score (i.e., sum) cannot be compared across categories.

To use this function, the data must be prepared according to the following criteria:
1) The data must include all individual questionnaire items
2) The  columns/variables must match the following naming convention: 'kfq#' where # is the question number (1-46)
3) All questions must have the numeric value for the choice:
0 - Never Eat This, 1 - Less than once in 7 days, 2 - 1-2 times in 7 days, 3 - 3-5 times in 7 days, 4 - 6-7 times in 7 days, 5 - More than 7 times in 7 days

Note, as long as variable names match those listed, the dataset can include other variables
}
\examples{

# scoring for the kfq with IDs
kfq_score_data <- score_kfq(kfq_data, parID = 'ID')

\dontrun{
}

}
\seealso{
Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_child_v2dat}} and \code{\link{util_fbs_child_v2dat_home}}
}
