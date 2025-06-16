#' Apply random forest.
#
#' @description Apply the default random forest model to the data.
#
#' @param outcome Character that represents the column name in the data.frame which contains the observed outcome (default: 'observed').
#
#' @param dataTest Test subset, e.g., 20 percent, of the full sample.
#
#' @param dataTrain Training subset, e.g., 80 percent, of the full sample.
#
#' @param frmla.f An object of the class \code{formula}, used for the random forest model (f = outcome must be of class \code{factor}).
#
#' @return a list with two data.frames as elements (names: ApparentCV, TestCV), each data.frame having three columns:
#' \enumerate{
#' \item observed Observed outcome (0 = absent, 1 = present).
#' \item predicted Model-based probability estimation of the outcome being present.
#' \item ids Row numbers of the total sample (before data has been split into training and test subsets).
#' }
#
#' @author Marcel Miché
#
#' @importFrom ranger ranger
#' @importFrom stats predict
#
#' @examples
#' # 2 seeds and 5-fold cross-validation would lead to 10 performance results.
#' set.seed(1)
#' seeds <- sample(1:10e6, size=2)
#' cvLs <- myRepeatedkFoldcv(data=dummyData, outcome="y", folds = 5, seeds=seeds)
#' # Extract training and test subsets of the full dataset:
#' TrainLs <- cvLs$TrainLs
#' TestLs <- cvLs$TestLs
#' fmla.f <- formula(factor(y) ~ .)
#' applyRandomForest(dataTrain = TrainLs[[1]], dataTest = TestLs[[1]], frmla=fmla.f, outcome="y")
#
#' @references
#'
#' \insertRef{ranger2017}{mysml}
#
#' @export
#
applyRandomForest <- function(dataTrain=NULL, dataTest=NULL, frmla.f=NULL, outcome="y") {
    
    if(is.null(frmla.f)) {
        rfPred <- ranger::ranger(factor(y) ~ ., data = dataTrain, probability = TRUE)
    } else {
        rfPred <- ranger::ranger(frmla.f, data = dataTrain, probability = TRUE)
    }
    apparentCV <- predict(object=rfPred, data=dataTrain, type = "response")$predictions[,"1"]
    rfCV <- predict(object=rfPred, data=dataTest, type = "response")$predictions[,"1"]

    randomForestOut <- list()
    randomForestOut[["ApparentCV"]] <- data.frame(observed=dataTrain[[outcome]],
                                            predicted=apparentCV,
                                            ids=rownames(dataTrain))
    randomForestOut[["TestCV"]] <- data.frame(observed=dataTest[[outcome]],
                                        predicted=rfCV,
                                        ids=rownames(dataTest))
    return(randomForestOut)
}
