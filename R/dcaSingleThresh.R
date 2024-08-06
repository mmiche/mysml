# dcaSingleThresh
#
#' @importFrom dplyr if_else
#
#
dcaSingleThresh <- function(confMatDf=NULL, FPwt=NULL, nbTreatAll=NULL) {
    # # Append a distinct column that shows the seven unique levels of threshold probabilities (logreg) or harm-to-benefit ratios (CART).
    # confMatDf$level <- 1:nrow(confMatDf)
    # Append the FPwt to confMatDf.
    confMatDf$FPwt <- FPwt
    # Append the treat all net benefit to confMatDf.
    confMatDf$nbTreatAll <- nbTreatAll
    # Append the treat none net benefit to confMatDf.
    confMatDf$nbTreatNone <- rep(0, times=nrow(confMatDf))
    # Compute and append the prediction model net benefit to confMat. Use custom function nb for this.
    confMatDf$nbModel <- nb(tp=confMatDf$TP, fp=confMatDf$FP, FPwt=confMatDf$FPwt, sampleSize=sum(confMatDf[1,1:4]))
    # deltaNb: Difference between net benefit of the prediction model and net benefit of the next best contender (e.g., treat all or treat none).
    confMatDf$deltaNb <-
        # Column 'level' in confMatDf represents the thresholds (for logreg) or FPwt (for CART).
        dplyr::if_else(confMatDf$nbTreatAll >= 0,
                       confMatDf$nbModel - confMatDf$nbTreatAll,
                       confMatDf$nbModel)
    return(confMatDf)
}
