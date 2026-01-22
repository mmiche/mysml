#' A simulated data set, used in published analysis code.
#'
#' This dummy dataset's purpose is to enable running published code (the original study data were prohibited from being published).
#'
#' @format A data.frame with 3654 rows and 9 columns:
#' \itemize{
#'   \item firstAud (binary outcome; 0 = no event observed, 1 = event observed).
#'   \item ADAPU2 (binary predictor; 0, 1).
#'   \item inactivity (binary predictor; 0, 1).
#'   \item MDDPD2 (binary predictor; 0, 1).
#'   \item Sex (binary predictor; 0, 1).
#'   \item MARIE (binary predictor; 0, 1).
#'   \item iSES15 (categorical predictor; minimum 1, maximum 5).
#'   \item smokingstatus (categorical predictor; minimum 0, maximum 2).
#'   \item Week_ALC_type6 (categorical predictor; minimum 1, maximum 6).
#' }
#'
#' @docType data
#' @keywords simulated dataset
#' @name audDummyData
#'
#' @usage data(audDummyData)
#' @examples
#' # Display the structure of the data set in the console
#' str(audDummyData)
"audDummyData"
