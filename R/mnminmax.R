#' Mean, minimum, and maximum.
#
#' @description Compute mean with a minimum to maximum interval.
#
#' @param x Numeric values.
#
#' @return a named vector with three elements:
#' \enumerate{
#' \item mn Mean.
#' \item min Minimum.
#' \item max Maximum.
#' }
#
#' @author Marcel Miché
#
#' @importFrom stats var
#
#' @examples
#' mnminmax(x=50:60)
#
#' @export
#
mnminmax <- function(x) {
    # If x has variance 0, set results to 0.
    if(var(x)==0) {
        out <- c(mn=0, min=0, max=0)
    } else {
        out <- c(mn=mean(x), min=min(x), max=max(x))
    }
    return(out)
}
