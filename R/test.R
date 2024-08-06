#' A list that contains data.frames, used in some code examples of mysml.
#'
#' This test list object has been produced with the predProbsLs output, which has been produced with the simulated dummyData.
#' 
#' Each element of the list is again a list:
#' 
#' @format orderedObsLs contains a list of data.frames for each of both prediction models, with 734 rows (20% of full sample size, which is 3670) and 4 columns:
#' \itemize{
#'   \item observed (binary outcome; 0 = no event observed, 1 = event observed).
#'   \item predicted (predicted probabilities, i.e., values between > 0 and < 1).
#'   \item ids (dataset row number or some other identifier).
#'   \item id (sequence of 1 to 734).
#' }
#' 
#' @format dcaLs contains two lists (names: tableDCA and plotDCA). tableDCA contains data.frames, each containing 16 columns:
#' \itemize{
#'   \item TN (true negatives).
#'   \item FP (false positives).
#'   \item FN (false negatives).
#'   \item TP (true positives).
#'   \item sens (sensitivity, recall, or true positive rate).
#'   \item spec (specificity or true negative rate).
#'   \item ppv (positive predictive value or precision).
#'   \item npv (negative predictive value).
#'   \item thrsh (threshold probability).
#'   \item FPwt (the value used to weigh FP).
#'   \item nbTreatAll (net benefit of the treat all curve).
#'   \item nbTreatNone (net benefit of treat none; always 0).
#'   \item nbModel (net benefit of the prediction model).
#'   \item deltaNB (difference between nbModel and TreatAll or between nbModel and TreatNone).
#'   \item model (name of the prediction model).
#'   \item rep (repetition of cross-validation).
#' }
#' 
#' @format dcaLs contains two lists (names: tableDCA and plotDCA). plotDCA contains data.frames, each containing 3 columns:
#' \itemize{
#'   \item label (names of decision strategies, e.g., Treat all).
#'   \item threshold (selected reasonable range of theshold probabilities).
#'   \item net_benefit (net benefit).
#' }
#' 
#' The last list element of test ('calibLs') is very similar to the first list element ('orderedObsLs').
#'
#' @docType data
#' @keywords observed outcome, predicted probabilities, net benefit
#' @name test
#'
#' @usage data(test)
#' @examples
#' # Display the names of the list
#' names(test) # "orderedObsLs" "dcaLs" "calibLs"
"test"
