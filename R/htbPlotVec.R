#' Harm component of harm-to-benefit.
#
#' @description Compute the 'harm' component of the 'harm-to-benefit' ratio.
#
#' @param x Numeric values, representing probabilities, here excluding the values 0 and 1.
#
#' @return A numeric value that represents the harm component of the harm-to-benefit ratio.
#
#' @author Marcel Miché
#
#' @examples
#' # Harm component of harm-to-benefit ratio for a probability of .25 is
#' # 3: .25/(1-.25) = .25:.75 = 1:3 (harm-to-benefit ratio = 1:3).
#' htbPlotVec(x=c(.05, .2, .25)) # Output: 19, 4, 3
#
#' @export
#
htbPlotVec <- function(x) {
    # paste0("1:", round((1-x)/x, digits=2))
    
    if(any(x == 0 | x == 1 | x < 0 | x > 1)) {
        stop("Function argument x must be between > 0 and < 1.")
    }
    
    return((1-x)/x)
}
