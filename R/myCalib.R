#' Calibration related summary results.
#
#' @description Compute some calibration related summary results.
#
#' @param calibDf Object (a data.frame) that is the second out of three list elements (output) from function 'computeRelevantResults'.
#
#' @param outcome Character that represents the column name in the data.frame which contains the observed outcome (default: 'observed').
#
#' @details The returned calibration related results are: 
#' \enumerate{
#' \item Brier score: Best possible result (Bpr) = 0.
#' \item Calibration Intercept: Bpr = 0.
#' \item Calibration Slope: Bpr = 1.
#' \item Emax: Bpr = 0.
#' \item E90: Bpr = 0.
#' \item ECE: Bpr = 0.
#' \item MCE: Bpr = 0.
#' \item ICI: Bpr = 0.
#' }
#
#' @return a list with three data.frames (df) as elements:
#' \enumerate{
#' \item calibDf Return the same data that was received by the function.
#' \item Return calibration related results (see \strong{Details}) in long format.
#' \item Return calibration related results (see \strong{Details}) in wide format.
#' }
#
#' @author Marcel Miché
#
#' @importFrom rms val.prob
#' @importFrom CalibratR getECE
#' @importFrom tidyr pivot_longer
#' @importFrom forcats as_factor
#' @importFrom magrittr %>%
#
#' @examples
#' # Use part of the saved output from the function 'computeRelevantResults'.
#' calibResultsLs <- lapply(test$calibLs, FUN=function(x) {
#'     myCalib(calibDf = x, outcome="observed")
#' })
#' # Peek at part of output:
#' calibResultsLs[[1]]$calibPerfW[,c("Intercept", "Slope", "ici", "names")]
#' #              Intercept     Slope         ici        names
#' # logreg        0.142350 0.9843143 0.005361289       logreg
#' # randomForest  1.107592 1.3395060 0.006857659 randomForest
#
#' @export
#
myCalib <- function(calibDf=NULL, outcome="observed") {

    calibNames <- colnames(calibDf)

    calibRes <- list()
    # i <- 1
    for(i in 1:(ncol(calibDf)-1)) {
        # [1:18], because rms::val.prob can output an element no.19 with header <NA>
        calibRes[[names(calibDf)[i]]] <- rms::val.prob(p=calibDf[,i], y=calibDf[,outcome], pl=FALSE)[1:18]
        # ece: expected calibration error (using default number of bins = 10)
        eceRes <- CalibratR::getECE(actual = calibDf[,outcome], predicted = calibDf[,i])
        # mce: maximum calibration error (using default number of bins = 10)
        # Modified the mce function, because mce is not weighted, as opposed to ece.
        mceRes <- mce(actual = calibDf[,outcome], predicted = calibDf[,i], n_bins = 10)
        #
        iciRes <- ici(y=calibDf[,outcome], p=calibDf[,i])

        calibRes[[names(calibDf)[i]]] <- c(calibRes[[names(calibDf)[i]]], ece=eceRes, mce=mceRes, ici=iciRes)
    }
    calibPerf <- as.data.frame(calibRes)
    calibPerfNames <- rownames(calibPerf)

    calibPerfT <- data.frame(t(calibPerf[c(11:15,19:21),]))
    colnames(calibPerfT) <- calibPerfNames[c(11:15,19:21)]
    calibPerfT$names <- rownames(calibPerfT)

    # https://cran.r-project.org/web/packages/tidyr/vignettes/pivot.html
    calibPerfL <- calibPerfT %>%
        tidyr::pivot_longer(
            cols = !names,
            names_to = "perf",
            values_to = "vals"
        )
    calibPerfL$names <- forcats::as_factor(calibPerfL$names)

    calibDf$observed <- calibDf[,outcome]
    calibDf$id <- 1:nrow(calibDf)

    return(list(calibDf=calibDf, calibPerfW=calibPerfT, calibPerfL=calibPerfL))
}
