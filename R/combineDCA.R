# combineDCA
#
#' @importFrom dplyr bind_rows
#
#
combineDCA <- function(plotTblLs=NULL) {
    plotTbl1 <- plotTblLs[[1]]
    # 
    levels(plotTbl1$label) <- c(names(plotTblLs)[1], "Treat all", "Treat none")
    # 
    if(length(plotTblLs)>1) {
        for(d in 2:length(plotTblLs)) {
            levels(plotTblLs[[d]]$label) <- c(names(plotTblLs)[d], "Treat all", "Treat none")
            idxAdd <- plotTblLs[[d]]$label == names(plotTblLs)[d]
            plotTbl1 <- dplyr::bind_rows(plotTbl1, plotTblLs[[d]][idxAdd,])
        }
    }
    return(plotTbl1)
}
