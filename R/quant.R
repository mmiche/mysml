# quant
#
#' @importFrom stats quantile
#
#
quant <- function(x, p="25%") {as.numeric(quantile(x)[p])}
