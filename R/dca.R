# dca
#
#' @importFrom dplyr if_else bind_rows
#' @importFrom tibble tibble
#
#
dca <- function(inputDataset=NULL, truth=NULL, prob=NULL, selectedThresholds=NULL, plotStartAtZero=FALSE) {
    
    if(is.factor(inputDataset[,truth])) {
        inputDataset[,truth] <- as.numeric(as.character(inputDataset[,truth]))
    }
    
    trueClass <- factor(inputDataset[,truth], levels = c("0", "1"))
    # Empty list, used to collect the confusion matrices in the for-loop.
    confMatLs_i <- list()
    # Inner for-loop which generates the confusion matrix for the current iteration AND for each of the seven thresholds.
    for(j in 1:length(selectedThresholds)) {
        predClass <-
            factor(dplyr::if_else(inputDataset[,prob]<selectedThresholds[j],0,1),
                   levels = c("0", "1"))
        tbl_j <- table(predClass, trueClass)
        
        sens <- tbl_j[2,2]/sum(tbl_j[,2]) # sensitivity
        spec <- tbl_j[1,1]/sum(tbl_j[,1]) # specificity
        ppv <- tbl_j[2,2]/sum(tbl_j[2,]) # positive predictive value
        npv <- tbl_j[1,1]/sum(tbl_j[1,]) # negative predictive value
        
        # Transform matrix to vector.
        tblVec_j <- as.vector(tbl_j)
        # Collect values in a tibble (= a data.frame)
        confMatLs_i[[j]] <- tibble::tibble(
            # TN True Negative, FP False Positive
            TN=tblVec_j[1], FP=tblVec_j[2],
            # FN False Negative, TP True Positive
            FN=tblVec_j[3], TP=tblVec_j[4],
            sens=sens, spec=spec, ppv=ppv, npv=npv
        )
    }
    # Bind the tibbles together
    confMatDf_i <- dplyr::bind_rows(confMatLs_i)
    # Add column thrsh (= threshold) in percent.
    confMatDf_i$thrsh <- selectedThresholds
    
    # FPwt = false positive weight; 1/199 = 0.005025126, etc.
    FPwt <- probToOdds(selectedThresholds)
    # Outcome incidence in each of the 700 test subsets
    inc <- sum(inputDataset[,truth])/nrow(inputDataset)
    # Compute net benefit for the treat all scenario
    # Formula, see Van Calster et al. (2018; DOI: 10.1016/j.eururo.2018.08.038), Supplementary document, page 1.
    nbTreatAll <- inc - FPwt*(1-inc)
    # # Formula, see Van Calster et al. (2015; DOI: 10.1177/0272989X12470757), page 494, formula (8).
    # nbTreatAll <- (inc - selectedThresholds)/(1 - selectedThresholds)
    # # NOTE: The upper formula from Van Calster et al. (2018) is also the first of the two formulas in formula (8) in Van Calster et al. (2015).
    
    
    dcaDfSingleCV <- dcaSingleThresh(confMatDf = confMatDf_i, FPwt = FPwt, nbTreatAll = nbTreatAll)
    
    if(plotStartAtZero) {
        dcaDfPlot <- tibble(
            label = factor(
                rep(c("Treat all", "Treat none", "Prediction model"), each=length(selectedThresholds)+1),
                levels = c("Prediction model", "Treat all", "Treat none")),
            threshold = rep(c(0, selectedThresholds), times=3),
            # Net benefit of:
            # Treat all, treat none, prediction model.
            # inc = outcome incidence (10/810)
            net_benefit = c(inc, nbTreatAll,
                            rep(0, times=length(selectedThresholds)+1),
                            inc, dcaDfSingleCV$nbModel)
        )
    } else {
        dcaDfPlot <- tibble(
            label = factor(
                rep(c("Treat all", "Treat none", "Prediction model"), each=length(selectedThresholds)),
                levels = c("Prediction model", "Treat all", "Treat none")),
            threshold = rep(selectedThresholds, times=3),
            # Net benefit of:
            # Treat all, treat none, prediction model.
            # inc = outcome incidence (10/810)
            net_benefit = c(nbTreatAll,
                            rep(0, times=length(selectedThresholds)),
                            dcaDfSingleCV$nbModel)
        )
    }
    out <- list(tbl=dcaDfSingleCV, plotTbl=dcaDfPlot)
    return(out)
}
