#' Mean and percantage confidence interval.
#
#' @description Compute mean with some percentage confidence interval.
#
#' @param x Numeric values.
#
#' @param alpha Numeric value that represents the selected alpha statistical significance level, default is 0.05.
#
#' @return a named vector with three elements:
#' \enumerate{
#' \item mn Mean.
#' \item lci lower limit of the 95 percent confidence interval.
#' \item uci upper limit of the 95 percent confidence interval.
#' }
#
#' @author Marcel Miché
#
#' @importFrom stats var sd qnorm
#
#' @examples
#' mnci(x=50:60)
#
#' @references
#'
#' Recommended link \href{https://matheusfacure.github.io/python-causality-handbook/03-Stats-Review-The-Most-Dangerous-Equation.html}{The most dangerous equation}.
#
#' @export
#
mnci <- function(x, alpha=.05) {
    # If x has variance 0, set results to 0.
    if(var(x)==0) {
        out <- c(mn=0, lci=0, uci=0)
    } else {
        mean_x <- mean(x)
        # The 'most dangerous equation' (standard error: se)
        # https://matheusfacure.github.io/python-causality-handbook/03-Stats-Review-The-Most-Dangerous-Equation.html
        se_x <- sd(x)/sqrt(length(x))
        alphaTwoSided <- alpha/2
        # Standardized normal distribution 95% CI
        ci_x <- mean_x + c(-1, 1) * qnorm(1-alphaTwoSided) * se_x
        out <- c(mn=mean_x, lci=ci_x[1], uci=ci_x[2])
    }
    return(out)
}
