#' Repeated k-fold cross-validation.
#
#' @description Setup a repeated k-fold cross-validation procedure.
#
#' @param outcome Character that represents the column name in the data.frame which contains the observed outcome (default: 'observed').
#
#' @param data The study data that is used for the prediction modeling.
#
#' @param folds Numeric, the number of folds in the repeated k-fold cross-validation.
#
#' @param seeds Numeric vector with a least one seed value (see \strong{Details}).
#
#' @param stratify Boolean value to determine whether the outcome rate in the cross-validation subsamples shall be (roughly) the same as in the total sample (TRUE) or whether the outcome rate may vary (FALSE).
#
#' @details A seed is a numeric value with which the user can guarantee that a sampling function yields reproducible output, independent of who runs the sampling function or when it is used.
#
#' @return a list with two lists as elements:
#' \enumerate{
#' \item TrainLs A list with all training subsets of the data.
#' \item TestLs A list with all test subsets of the data.
#' }
#
#' @author Marcel Miché
#
#' @examples
#' # 5 seeds and 5-fold cross-validation leads to 25 performance results.
#' set.seed(1)
#' seeds <- sample(1:10e6, size=5)
#' cvLs <- myRepeatedkFoldcv(data=dummyData, outcome="y", folds = 5, seeds=seeds)
#' # Extract training and test subsets of the full dataset:
#' TrainLs <- cvLs$TrainLs
#' TestLs <- cvLs$TestLs
#
#' @export
#
myRepeatedkFoldcv <- function(data=NULL, outcome="y", folds=NULL, stratify=TRUE, seeds=NULL) {
    
    # y = outcome (0 = no; 1 = yes)
    idNoCase <- which(data[,outcome]==0)
    idCase <- which(data[,outcome]==1)
    
    if(stratify) {
        data <- data[c(idCase, idNoCase),]
    }
    
    casesLs <- chunk(idCase, n = folds)
    for (i in 1:length(casesLs)) {
        casesLs[[i]] <- rep(i, times = length(casesLs[[i]]))
    }
    casesVec <- as.numeric(unlist(casesLs))
    
    nonCasesLs <- chunk(idNoCase, n = folds)
    for (i in 1:length(nonCasesLs)) {
        nonCasesLs[[i]] <- rep(i, times = length(nonCasesLs[[i]]))
    }
    nonCasesVec <- as.numeric(unlist(nonCasesLs))
    
    TrainLs <- TestLs <- list()
    count <- 1
    for (s in 1:length(seeds)) {
        set.seed(seeds[s])
        
        noCaseIds <- sample(nonCasesVec)
        caseIds <- sample(casesVec)
        allIds <- c(caseIds, noCaseIds)
        
        for (f in 1:folds) {
            TrainLs[[count]] <- data[allIds != f,]
            TestLs[[count]] <- data[allIds == f,]
            count <- count + 1
        }
    }
    return(list(TrainLs = TrainLs, TestLs = TestLs))
}
