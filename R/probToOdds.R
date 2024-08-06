#' Transform probabilities to odds.
#
#' @description Compute odds from probabilities.
#
#' @param x Numeric value that represents a probability, i.e., a number between 0 and 1.
#
#' @return The odds that is computed from the given probability.
#
#' @author Marcel Miché
#
#' @examples
#' probToOdds(x=c(.1, .2)) # Output: 0.1111111 0.2500000
#
#' @references
#' \insertRef{greenland1998probability}{mysml}
#
#' @export
#
probToOdds <- function(x) {x/(1-x)}
# See Greenland 1998 (Greenland_1998b.pdf), page 324, bottom left text column.
