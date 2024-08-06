# nb
#
#
nb <- function(tp=NULL, fp=NULL, FPwt=NULL, sampleSize=NULL) {
    return((tp - FPwt*fp)/sampleSize)
}
