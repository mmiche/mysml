#' Apply logistic regression.
#
#' @description Apply the logistic regression model to the data.
#
#' @param outcome Character that represents the column name in the data.frame which contains the observed outcome (default: 'observed').
#
#' @param dataTest Test subset, e.g., 20 percent, of the full sample.
#
#' @param dataTrain Training subset, e.g., 80 percent, of the full sample.
#
#' @param frmla An object of the class \code{formula}, used for the logistic regression model.
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
#' @importFrom stats binomial family glm predict
#
#' @examples
#' # 2 seeds and 5-fold cross-validation would lead to 10 performance results.
#' set.seed(1)
#' seeds <- sample(1:10e6, size=2)
#' cvLs <- myRepeatedkFoldcv(data=dummyData, outcome="y", folds = 5, seeds=seeds)
#' # Extract training and test subsets of the full dataset:
#' TrainLs <- cvLs$TrainLs
#' TestLs <- cvLs$TestLs
#' fmla <- formula(y ~ .)
#' applyLogreg(dataTrain = TrainLs[[1]], dataTest = TestLs[[1]], frmla=fmla, outcome="y")
#
#' @export
#
applyLogreg <- function(dataTrain=NULL, dataTest=NULL, frmla=NULL, outcome="y") {
    
    if(is.null(frmla)) {
        glmPred <- glm(y ~ ., family = binomial(link="logit"), data = dataTrain)
    } else {
        glmPred <- glm(frmla, family = binomial(link="logit"), data = dataTrain)
    }
    apparentCV <- predict(object=glmPred, newdata=dataTrain, type = "response")
    glmCV <- predict(object=glmPred, newdata=dataTest, type = "response")
    
    logregOut <- list()
    logregOut[["ApparentCV"]] <- data.frame(observed=dataTrain[[outcome]],
                                            predicted=apparentCV,
                                            ids=rownames(dataTrain))
    logregOut[["TestCV"]] <- data.frame(observed=dataTest[[outcome]],
                                        predicted=glmCV,
                                        ids=rownames(dataTest))
    return(logregOut)
}
