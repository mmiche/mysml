# myCalibPlot
#
#' @importFrom dplyr ntile
#
#
myCalibPlot <- function(calibDf=NULL, n=10, model="noNameModel") {
    idx <- dplyr::ntile(x=calibDf$predicted, n=10)
    vals <- c()
    for(i in 1:10) {
        vals <- c(vals, mean(calibDf$predicted[idx==i]),
                  mnci(x=as.numeric(as.character(calibDf$observed[idx==i]))))
    }
    dfTmp <- data.frame(matrix(data=vals, ncol=4, byrow = TRUE))
    colnames(dfTmp) <- c("mn_x", "mn_y", "lci_y", "uci_y")
    dfTmp$model <- model
    return(dfTmp)
}
