#' Results to produce a calibration plot.
#
#' @description Compute results that are necessary for a single calibration plot.
#
#' @param model The name of the prediction model, as shown in the output of the function 'makeCalibPlotLs' or 'avrgCalibPlotLs'.
#
#' @param calibLs Object that is the first out of three list elements (output) from function 'computeRelevantResults'.
#
#' @return a data.frame with seven columns:
#' \enumerate{
#' \item mn_x Mean x-axis value (x-axis = predicted probabilities).
#' \item mn_y  Mean y-axis value (y-axis = observed outcome rate).
#' \item lci_y Lower limit of the 95 percent confidence interval.
#' \item uci_y Upper limit of the 95 percent confidence interval.
#' \item model Name of the prediction model.
#' \item run The run among all of the cross-validations of the prediction model.
#' \item tile Enumeration of selected number of approximately equal sized subsamples.
#' }
#
#' @author Marcel Miché
#
#' @examples
#' # Use part of the saved output from the function 'computeRelevantResults'.
#' calibLRLs <- makeCalibPlotLs(calibLs=test$orderedObsLs, model="logreg")
#' # Peek at first 2 lines of output:
#' head(calibLRLs[[1]], n = 2)
#' #          mn_x mn_y lci_y uci_y  model run tile
#' # 1 0.000508724    0     0     0 logreg   1    1
#' # 2 0.001480403    0     0     0 logreg   1    2
#
#' @references
#' 
#' \insertRef{van2016calibration}{mysml}
#
#' @export
#
makeCalibPlotLs <- function(calibLs=NULL, model="noModel") {
    # 
    if(model=="noModel") {
        stop("Select the model.")
    }
    calibLs_model <- calibLs[[model]]
    calibPlotLs <- list()
    for(i in 1:length(calibLs_model)) {
        calibPlot_i <- myCalibPlot(calibDf = calibLs_model[[i]], model=model)
        calibPlot_i$run <- i
        calibPlot_i$tile <- 1:nrow(calibPlot_i)
        calibPlotLs[[i]] <- calibPlot_i
    }
    return(calibPlotLs)
}
