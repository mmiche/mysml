#' Get relevant output from the bootstrap cross-validation procedure.
#
#' @description Extract data for performance visualization.
#
#' @param x Output of function \code{validate} from the \code{pminternal} package.
#
#' @param listName Character string. Name of the model (default: "modelName").
#
#' @param withApparent Boolean value (default: FALSE). If apparent predicted probabilities shall be included, change argument to TRUE.
#
#' @return a list with b + 1 or b + 2 elements (b = number of bootstrap repetitions). Each element is a dataframe with as many rows as in the original sample size and two columns:
#' \enumerate{
#' \item observed Binary numeric values 0 (no event observed) and 1 (event observed).
#' \item predicted Numeric value between 0 and 1 (estimated probability that event was observed).
#' }
#' The apparent predicted probabilities (from the original sample) can either be included or (this is the default) excluded.
#
#' @author Stephen Rhodes (see \strong{Details})
#
#' @importFrom pminternal get_stability
#
#' @examples
#' # Assuming that the output of pminternal::boot_optimism has
#' # been assigned to the variable 'val'. Then continue with:
#' # getRelevantBootstrapOutputLs(x=val, listName = "LR", withApparent=FALSE)
#
#' @references
#'
#' \insertRef{pminternal2025}{mysml}
#
#' @export
#
getRelevantBootstrapOutputLs <- function(x=NULL, listName="modelName", withApparent=FALSE) {
    stabil <- pminternal::get_stability(x)
    y <- stabil$y
    stabil <- stabil$stability
    sDf <- as.data.frame(stabil)
    if(colnames(sDf)[1] != "p_app") {
        stop("The function argument 'x' must be the result from the function 'validate' or 'boot_optimism' from the R package 'pminternal'.")
    }
    if(!withApparent) {
        sDf <- sDf[,-1]
    }
    sLs <- list()
    sLs <- sapply(listName, function(x) NULL)
    for(i in 1:ncol(sDf)) {
        sLs[[listName]][[i]] <- data.frame(observed=y, predicted=sDf[,i])
    }
    return(sLs)
}
