# mce
#
#
mce <- function(actual=NULL, predicted=NULL, n_bins=NULL) {
    
    # Source is from CalibratR:::getMCE. Modified (debugged) by myself: Remove the weight W.
    # Noticed the bug after comparing results with results from code provided by Huang et al. (2020; DOI: https://doi.org/10.1093/jamia/ocz228), in combination with the paper by Naeini et al. (2015; DOI: https://doi.org/10.1609/aaai.v29i1.9602)
    predicted <- predicted
    labels <- actual
    idx <- order(predicted)
    pred_actual <- (cbind(predicted[idx], actual[idx]))
    N <- nrow(pred_actual)
    rest <- N%%n_bins
    B <- min(N, n_bins)
    S <- 0
    # W <- c()
    for (i in 1:B) {
        if (i <= rest) {
            group_pred <- (pred_actual[(((i - 1) * ceiling(N/n_bins) + 
                                             1):(i * ceiling(N/n_bins))), 1])
            group_actual <- (pred_actual[(((i - 1) * ceiling(N/n_bins) + 
                                               1):(i * ceiling(N/n_bins))), 2])
        }
        else {
            group_pred <- (pred_actual[((rest + (i - 1) * floor(N/n_bins) + 
                                             1):(rest + i * floor(N/n_bins))), 1])
            group_actual <- (pred_actual[((rest + (i - 1) * floor(N/n_bins) + 
                                               1):(rest + i * floor(N/n_bins))), 2])
        }
        n <- length(group_pred)
        expected <- mean(group_pred)
        observed <- mean(group_actual)
        S[i] <- abs(observed - expected)
        # W[i] <- n/N
    }
    # res <- max(S * W)
    res <- max(S)
    return(res)
}
