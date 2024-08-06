#' Transform odds to probabilities.
#
#' @description Compute probabilities from odds.
#
#' @param x Numeric value that represents odds, e.g., 0.25 (resuling from 0.2/(1-0.2)).
#
#' @return The probability that is computed from the given odds.
#
#' @author Marcel Miché
#
#' @examples
#' oddsToProb(x=c(.1, .25)) # Output: 0.09090909 0.20000000
#
#' @references
#' \insertRef{greenland1998probability}{mysml}
#
#' @export
#
oddsToProb <- function(x) {x/(1+x)}
