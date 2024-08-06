#' Mean, 25th percentile, and 75th percentile.
#
#' @description Compute mean with a 25th percentile to 75th percentile interval.
#
#' @param x Numeric values.
#
#' @return a named vector with three elements:
#' \enumerate{
#' \item mn Mean.
#' \item q1 First quartile (= 25th percentile).
#' \item q3 Third quartile (= 75th percentile).
#' }
#
#' @author Marcel Miché
#
#' @importFrom stats var
#
#' @examples
#' mnq1q3(x=50:60)
#
#' @export
#
mnq1q3 <- function(x) {
    # If x has variance 0, set results to 0.
    if(var(x)==0) {
        out <- c(mn=0, q1=0, q3=0)
    } else {
        out <- c(mn=mean(x), q1=quant(x), q3=quant(x, p="75%"))
    }
    return(out)
}
