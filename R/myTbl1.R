#' Counts and percentages.
#
#' @description Compute values that usually are presented in a table 1 of an empirical research paper.
#
#' @param data The study data that is used for the prediction modeling.
#
#' @param nlvl Numeric value which represents the maximum number of categories that a variable may have in the data.
#
#' @return a data.frame with four columns:
#' \enumerate{
#' \item name Column name in the dataset.
#' \item categ Category of the identified categorical column in the dataset.
#' \item absNum Absolute number (count) of this category.
#' \item percent Percentage of this category.
#' }
#
#' @author Marcel Miché
#
#' @examples
#' myTable1(data=dummyData)
#
#' @export
#
myTable1 <- function(data=NULL, nlvl=10) {
    # Index vector with which to collect the variables that are likely to be categorical variables.
    idxCat <- c()
    for(i in 1:ncol(data)) {
        # If the attempt to convert to numeric values leads to R converting the values to NA, this indicates that this variable contains text, e.g., answers by the participants which are idiosyncratic. That means: It does not make sense to convert them into anything.
        if(all(is.na(as.numeric(data[[i]])))) {
            next
            # Else: If the attempt to convert to numeric values is successful, try the next step.
        } else {
            tmp <- as.numeric(data[[i]])
            # This next step is: Compute the modulo of division by 1. If all modulo results are equal to 0, then the variable consists of only integer values, which makes it possible that the variable is a categorical variable.
            if(all(tmp[!is.na(tmp)] %% 1 == 0L)) {
                # Collect this index variable as a possible categorical variable.
                idxCat <- c(idxCat, i)
                # Convert this column to the class factor.
                data[,i] <- as.factor(as.numeric(data[[i]]))
            }
        }
    }
    
    NLEVELS <- as.numeric(sapply(data[,idxCat], nlevels))
    IDXCAT <- idxCat[NLEVELS <= nlvl]
    # End of preparation phase.
    
    # Start making the overview:
    name <- categ <- absNum <- c()
    smry <- summary(data[,IDXCAT])
    names <- attributes(smry)$dimnames[[2]]
    
    for(j in names) {
        tmp <- smry[,j]
        tmp1 <- tmp[!is.na(tmp)]
        name <- c(name, rep(j, times=length(tmp1)))
        
        for(k in 1:length(tmp1)) {
            categk <- strsplit(tmp1[k], split=":")[[1]][1]
            categ <- c(categ, gsub(" ", "", categk))
            absNum <- c(absNum,
                        as.numeric(strsplit(tmp1[k], split=":")[[1]][2]))
        }
    }
    # Return the overview as a data.frame to the user.
    return(data.frame(name, categ, absNum, percent=absNum/nrow(data)*100))
}
