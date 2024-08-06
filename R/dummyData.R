#' A data set, used in some code examples of mysml.
#'
#' This dummyDataset is simulated.
#'
#' @format A data.frame with 3670 rows and 9 columns:
#' \itemize{
#'   \item y (binary outcome; 0 = no event observed, 1 = event observed).
#'   \item x1 (binary predictor; 0, 1).
#'   \item x2 (binary predictor; 0, 1).
#'   \item x3 (binary predictor; 0, 1).
#'   \item x4 (binary predictor; 0, 1).
#'   \item x5 (binary predictor; 0, 1).
#'   \item x6 (continuous predictor; minimum 1, mean 11.42, maximum 22).
#'   \item x7 (binary predictor; 0, 1).
#'   \item x8 (continuous predictor; minimum 18.4, mean 45.07, maximum 70.9)
#' }
#'
#' @docType data
#' @keywords simulated dataset
#' @name dummyData
#'
#' @usage data(dummyData)
#' @examples
#' # Display the structure of the data set in the console
#' str(dummyData)
"dummyData"
