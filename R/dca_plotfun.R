#' Decision curve analysis functionality.
#
#' @description Compute decision curve analysis output.
#
#' @param dcaReasonableThresholds Numeric vector with the selected reasonable range of threshold probabilities.
#
#' @param y Vector containing integer values 0 and 1 (0 = event absent, 1 = event present).
#
#' @param p Vector containing probabilities, i.e., continuous values between 0 and 1.
#
#' @return a data.frame with three columns:
#' \enumerate{
#' \item label Prediction model.
#' \item threshold The selected reasonable threshold probabilities.
#' \item net_benefit The net benefit for the respective threshold.
#' }
#
#' @author Marcel Miché
#
#' @examples
#' dca_plotfun(y=rep(c(0,1), times=c(10, 10)),
#' p=c(.03, .06, .09, .11, .35, .05, .3, .02, .15, .11,
#'     .2, .39, .13, .4, .44, .07, .22, .31, .32, .5),
#' dcaReasonableThresholds=c(.15, .2, .25))
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
dca_plotfun <- function(y=NULL, p=NULL, dcaReasonableThresholds=NULL) {
    dcaTbl <- dca(inputDataset = data.frame(y=y, p=p),
                          truth = "y", prob = "p",
                          selectedThresholds = dcaReasonableThresholds)$plotTbl
    dcaTbl$label <- as.character(dcaTbl$label)
    dcaTbl <- dcaTbl[dcaTbl$label=="Prediction model",]
    return(dcaTbl)
}
