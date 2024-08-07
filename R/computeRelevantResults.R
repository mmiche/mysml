#' Relevant results for a clinical prediction model.
#
#' @description Compute results that are relevant for a clinical prediction model.
#
#' @param predictionOutputLs A list whose elements are data.frames, each data.frame containing a column with observed events (0 = no, 1 = yes) and a column with the predicted probabilities of observing the event.
#
#' @param dcaStartAtZero Boolean value. TRUE (default) if the x-axis of the decision curve plot shall include zero, else FALSE.
#
#' @param dcaReasonableThresholds Numeric vector with the selected reasonable range of threshold probabilities.
#
#' @return a list with three lists as elements:
#' \enumerate{
#' \item orderedObsLs A list that contains data.frames (df), each df contains the column observed (= observed outcome, 0 = no, 1 = yes) and the column predicted (= predicted probabilities).
#' \item dcaLs Two lists, named tableDCA and plotDCA, respectively (see \strong{Examples}).
#' \item calibLs A list that contains data.frames (df), each df contains the predicted probabilities of all prediction models (column name is the prediction model, e.g., logreg) and the observed outcome (column name 'observed').
#' }
#
#' @author Marcel Miché
#
#' @examples
#' # Use the predicted probabilities (output for several cross-validations
#' # of two prediction models, logistic regression and random forest), which
#' # are computed from using a simulated dummy dataset.
#' test <- computeRelevantResults(predictionOutputLs = predProbsLs,
#'                                dcaReasonableThresholds = c(.02, .03, .04, .05))
#
#' @references
#'
#' \insertRef{vickers2006decision}{mysml}
#'
#' \insertRef{vickers2019simple}{mysml}
#'
#' \insertRef{van2018reporting}{mysml}
#'
#' \insertRef{van2016calibration}{mysml}
#
#' @export
#
# Use output from prediction models, i.e., compute all relevant results:
# 'Relevant', as determined by the researchers who are involved in this research project.

# predictionOutputLs <- predProbsLs
# predictionOutputLs <- fullModelLs
# dcaReasonableThresholds <- c(.01, .02, .03, .04, .05)
# dcaStartAtZero <- TRUE

computeRelevantResults <- function(predictionOutputLs=NULL, dcaStartAtZero=TRUE, dcaReasonableThresholds=c(.01, .02, .03, .04, .05)) {
    
    # Extract names from the object (of class 'list') predictionOutputLs
    listNames <- names(predictionOutputLs)
    # Produce empty list with same list names (task: collect the relevant results).
    relevantResultsLs <- sapply(listNames, function(x) NULL)
    
    reps <- length(predictionOutputLs[[listNames[1]]])
    if(all(is.null(names(predictionOutputLs[[listNames[1]]])))) {
        singleDataframe <- FALSE
    } else {
        singleDataframe <- TRUE
        reps <- 1
    }
    
    # Ordered observations, relevant for visualization.
    orderedObsLs <- list()
    # reps is equal or less than 3, if the list contains only one data.frame for each model.
    if(singleDataframe) {
        # i <- 1
        for(i in listNames) {
            output_i <- predictionOutputLs[[i]]
            output_i <- output_i[order(output_i$observed),]
            output_i$id <- 1:nrow(output_i)
            output_i$observed <- factor(output_i$observed, levels=c("0", "1"))
            orderedObsLs[[i]] <- output_i
        }
    } else {
        # i <- j <- 1
        for(i in listNames) {
            for(j in 1:reps) {
                output_ij <- predictionOutputLs[[i]][[j]]
                # head(output_ij)
                output_ij <- output_ij[order(output_ij$observed),]
                output_ij$id <- 1:nrow(output_ij)
                output_ij$observed <- factor(output_ij$observed, levels=c("0", "1"))
                orderedObsLs[[i]][[j]] <- output_ij
            }
        }
    }
    
    # ---------------------------------------------------------
    # 1.
    # Most important for clinical practice: Net benefit (= clinical utility, regarding the only question of interest: Does the algorithm or does it not potentially improve decision making in the clinical real world?)
    # ---------------------------------------------------------
    
    # dcaLs <- list()
    dcaLs <- sapply(c("tableDCA", "plotDCA"), function(x) NULL)
    # i <- 1
    # j <- "logreg"
    # j <- "rf"
    for(i in 1:reps) {
        dcaLs_i <- list()
        for(j in listNames) {
            if(singleDataframe) {
                output_j <- predictionOutputLs[[j]]
                # DCA results for logistic regression or random forest
                dca_j <- dca(inputDataset = output_j, truth="observed", prob="predicted", selectedThresholds = dcaReasonableThresholds, plotStartAtZero=dcaStartAtZero)
                dcaLs_i[[j]] <- dca_j
            } else {
                output_ji <- predictionOutputLs[[j]][[i]]
                # DCA results for logistic regression or random forest
                dca_j <- dca(inputDataset = output_ji, truth="observed", prob="predicted", selectedThresholds = dcaReasonableThresholds, plotStartAtZero=dcaStartAtZero)
                dcaLs_i[[j]] <- dca_j
            }
            
        }
        # names(dcaLs_i)
        
        # Extract and extend the wide format of the net benefit results of logreg.
        dcaTblLogreg <- dcaLs_i[["logreg"]]$tbl
        dcaTblLogreg$model <- "logreg"
        dcaTblLogreg$rep <- i
        
        # Extract and extend the wide format of the net benefit results of random forest.
        dcaTblrndFrst <- dcaLs_i[["rf"]]$tbl
        dcaTblrndFrst$model <- "randomForest"
        dcaTblrndFrst$rep <- i
        # Combine wide format DCA tables of logreg and random forest.
        dcaTbl <- dplyr::bind_rows(dcaTblLogreg, dcaTblrndFrst)
        # Append combined results to the list object 'dcaLs'.
        dcaLs[["tableDCA"]][[i]] <- dcaTbl
        
        # Combine dca results of single prediction modesl.
        plotDCA <- combineDCA(plotTblLs = list("Logistic regression"=dcaLs_i[["logreg"]]$plotTbl, "Random forest"=dcaLs_i[["rf"]]$plotTbl))
        # Extract all factor levels from combined dca results
        lvls <- levels(plotDCA$label)
        # Prepare for putting the labels in a nice order
        idxRelevel <- levels(plotDCA$label) %in% c("Treat all", "Treat none")
        # Execute the preparation = bring the labels in a nice order
        plotDCA$label <- factor(plotDCA$label, levels=c(lvls[!idxRelevel], lvls[idxRelevel]))
        # Append combined results to the list object 'dcaLs'.
        dcaLs[["plotDCA"]][[i]] <- plotDCA
    }
    
    # ---------------------------------------------------------
    # 2.
    # Calibration (more important for clinical practice than discrimination).
    # ---------------------------------------------------------
    
    # calib: calibration
    calibLs <- list()
    # i <- 1
    # j <- "logreg"
    # j <- "rf"
    for(i in 1:reps) {
        calibLs_i <- list()
        for(j in listNames) {
            if(singleDataframe) {
                output_j <- predictionOutputLs[[j]]
                # 
                calibLs_i[[j]] <- output_j
            } else {
                output_ji <- predictionOutputLs[[j]][[i]]
                # 
                calibLs_i[[j]] <- output_ji
            }
            
        }
        calibLs[[i]] <- data.frame(logreg=calibLs_i[["logreg"]]$predicted,
                                   randomForest=calibLs_i[["rf"]]$predicted,
                                   observed=calibLs_i[["logreg"]]$observed)
    }
    # head(calibLs[[1]])
    
    return(list(orderedObsLs=orderedObsLs, dcaLs=dcaLs, calibLs=calibLs))
}

