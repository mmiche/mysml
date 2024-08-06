#' Make an averaged calibration plot.
#
#' @description Average results across several calibration plots.
#
#' @param calibPlotLs Object that is the output from the function 'makeCalibPlotLs'.
#
#' @param model The name of the prediction model, as shown in the output of the function 'makeCalibPlotLs' or 'avrgCalibPlotLs'.
#
#' @return a list with three data.frames as elements (names: mn, lci, uci), each data.frame having six columns:
#' \enumerate{
#' \item mn_x Mean x-axis value (x-axis = predicted probabilities).
#' \item mn_y  Mean y-axis value (y-axis = observed outcome rate).
#' \item lci_y Lower limit of the 95 percent confidence interval.
#' \item uci_y Upper limit of the 95 percent confidence interval.
#' \item model Name of the prediction model.
#' \item tile Enumeration of selected number of approximately equal sized subsamples.
#' }
#
#' @author Marcel Miché
#
#' @importFrom dplyr bind_rows
#
#' @examples
#' # Use the saved output from the function 'computeRelevantResults'.
#' # Before 'avrgCalibPlotLs' can be used, use 'makeCalibPlotLs':
#' calibLRLs <- makeCalibPlotLs(calibLs=test$orderedObsLs, model="logreg")
#' # Use this output to continue with 'avrgCalibPlotLs':
#' testcalibLR <- avrgCalibPlotLs(calibPlotLs = calibLRLs, model="logreg")
#' # Peek at first 2 lines of output:
#' lapply(testcalibLR, function(x) head(x, n=2))
#' # $mn
#' #           mn_x mn_y lci_y uci_y  model tile
#' # 1 0.0005528451    0     0     0 logreg    1
#' # 2 0.0015136628    0     0     0 logreg    2
#' # 
#' # $lci
#' #           mn_x mn_y lci_y uci_y  model tile
#' # 1 0.0004820103    0     0     0 logreg    1
#' # 2 0.0013561840    0     0     0 logreg    2
#' # 
#' # $uci
#' #          mn_x mn_y lci_y uci_y  model tile
#' # 1 0.000623680    0     0     0 logreg    1
#' # 2 0.001671142    0     0     0 logreg    2
#
#' @export
#
avrgCalibPlotLs <- function(calibPlotLs=NULL, model="noModel") {
    if(model=="noModel") {
        stop("Select the model.")
    }
    
    calibPlotDf <- dplyr::bind_rows(calibPlotLs)
    
    if(!all(calibPlotDf[,"model"]==model)) {
        stop("Only one select model at a time.")
    }
    
    mnVals <- lciVals <- uciVals <- c()
    nTiles <- max(calibPlotDf[,"tile"])
    for(colmn in c("mn_x", "mn_y", "lci_y", "uci_y")) {
        for(t in 1:nTiles) {
            resultTmp <- mnci(x=calibPlotDf[calibPlotDf[,"tile"]==t,colmn])
            mnVals <- c(mnVals, resultTmp["mn"])
            lciVals <- c(lciVals, resultTmp["lci"])
            uciVals <- c(uciVals, resultTmp["uci"])
        }
    }
    # Average of all values
    mnDf0 <- data.frame(matrix(mnVals, nrow=nTiles))
    colnames(mnDf0) <- colnames(calibPlotDf)[1:4]
    mnDf <- cbind(mnDf0, calibPlotDf[1:nTiles,c("model", "tile")])
    # Lower 95% confidence interval of all values
    lciDf0 <- data.frame(matrix(lciVals, nrow=nTiles))
    colnames(lciDf0) <- colnames(calibPlotDf)[1:4]
    lciDf <- cbind(lciDf0, calibPlotDf[1:nTiles,c("model", "tile")])
    # Upper 95% confidence interval of all values
    uciDf0 <- data.frame(matrix(uciVals, nrow=nTiles))
    colnames(uciDf0) <- colnames(calibPlotDf)[1:4]
    uciDf <- cbind(uciDf0, calibPlotDf[1:nTiles,c("model", "tile")])
    # Put all in list
    out <- list(mn=mnDf, lci=lciDf, uci=uciDf)
    return(out)
}
