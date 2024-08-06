# ici
#
#' @importFrom stats loess predict
#
#
ici <- function(y=NULL, p=NULL) {
    # Source: Austin PC, Steyerberg EW. The Integrated Calibration Index (ICI) and related metrics for quantifying the calibration of logistic regression models. Statistics in Medicine. 2019;38:4051–4065. https://doi.org/10.1002/sim.8281
    loess.calibrate <- loess(y ~ p)
    p.calibrate <- predict(loess.calibrate, newdata=p)
    return(mean(abs(p.calibrate - p)))
}
