#' Translate net benefit.
#
#' @description Translate a net benefit result.
#
#' @param nb Numeric values that represent a positive net benefit result, i.e., values between > 0 and < 1 (see \strong{Details}).
#
#' @details The theoretical maximum net benefit is 1, which is irrelevant for this translation function.
#
#' @return A list with a named vector for each given net benefit (nb), i.e., its translation. For example, nb = 0.1 returns a count of 1 (true positive) per 10 (predicted probabilities). In words: A net benefit of 0.1 means that if the prediction model is used as the clinical decision strategy, then one true positive (TP) individual is among every 10 individuals whose outcome risk has been predicted with the clinical prediction model. This one TP individual is obtained without any further increase of false positive (FP) individuals (which is the reason why it is called 'net' benefit).
#
#' @author Marcel Miché
#
#' @examples
#' translatenb(nb=c(.003, .15))
#' # Output:
#' # [[1]]
#' #      nb count   per
#' # 1 0.003     3 1'000
#' # 
#' # [[2]]
#' #     nb count per
#' # 1 0.15    15 100
#
#' @export
#
translatenb <- function(nb=NULL) {
    
    # Translate any NB result to a full count number, e.g., NB = .1 = 10 more TP among 100 risk predictions, without FP increase.
    
    # Set up net benefit values to be compared to.
    nbCompare <- c(.0000001, .000001, .00001, .0001, .001, .01, .1, 1)
    # The reference number that corresponds to the NB comparison categories (nbCompare).
    nbRef <- c("10'000'000", "1'000'000", "100'000", "10'000", "1'000", "100", "10", "1")
    
    nbLs <- list()
    # i <- 1
    for(i in 1:length(nb)) {
        if(all(nb[i] < nbCompare)) {
            # message(paste(nb[i], "too small"))
            nbLs[[i]] <- data.frame(nb=nb[i], count=as.numeric(nb[i]), per="Too small.")
            next
        }
        # Compare the NB to the comparison categories, take first true, then subtract 1.
        idx <- which(nb[i] < nbCompare)[1]-1
        # Turn NB into an integer number, e.g., 13, instead of 1.3 or 0.13.
        nbNum <- as.character(format(nb[i], scientific = FALSE))
        # Remove decimal point.
        nbNum <- gsub("\\.", "", nbNum)
        # Remove zeros.
        nbNum <- gsub("0", "", nbNum)
        # nbNum has 2 or more digits, adjust idx accordingly.
        if(nchar(nbNum)>=2) {
            if(nchar(nbNum)==2) {
                idx <- idx - 1
            } else if(nchar(nbNum)==3) {
                idx <- idx - 2
            } else if(nchar(nbNum)==4) {
                idx <- idx - 3
            } else {
                stop("Too many digits in NB.")
            }
            
            if(idx <= 0) {
                stop("Something went wrong, idx cannot be zero or negative.")
            }
            
        }
        nbLs[[i]] <- data.frame(nb=nb[i], count=as.numeric(nbNum), per=nbRef[idx])
    }
    return(nbLs)
}
