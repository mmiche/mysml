#' Probabilities from given harm-to-benefit ratios.
#
#' @description Compute the probability for a given harm-to-benefit ratio.
#
#' @param x Character that represents a harm-to-benefit ratio, such as (default) '1:9'.
#
#' @return A numeric value that represents the probability that corresponds to the given harm-to-benefit ratio.
#
#' @author Marcel Miché
#
#' @examples
#' # Reverse a harm-to-benefit ratio to the corresponding probability:
#' htbToProb(x=c("1:3", "1:9")) # Output: 0.25, 0.10
#
#' @export
#
htbToProb <- function(x="1:9") {
    
    if(!all(is.character(x)) || any(x == "")) {
        stop("The function argument x must be of class character, e.g., '1:9'. Empty character strings are not permitted.")
    }
    
    if(!all(grepl(pattern=":", x))) {
        stop("The function argument x must be of class character, e.g., '1:9'. Empty character strings are not permitted.")
    }
    i <- 3
    for(i in 1:length(x)) {
        all_ok <- strsplit(x=x[i], split=":")[[1]]
        if(all_ok[1] != 1) {
            stop("The function argument x must be of class character, e.g., '1:9'. First element of the expression must be the value 1.")
        }
    }
    
    prob <- c()
    # i <- 1
    for(i in 1:length(x)) {
        tpfp <- as.numeric(strsplit(x[i], ":")[[1]])
        odds <- tpfp[1]/tpfp[2]
        prob <- c(prob, oddsToProb(odds))
    }
    return(prob)
}
