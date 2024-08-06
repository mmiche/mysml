# combineDCA
#
#' @importFrom dplyr bind_rows
#
#
combineDCA <- function(plotTblLs=NULL) {
    plotTbl1 <- plotTblLs[[1]]
    # levels(plotTbl1$label)
    # plotTbl1$label <- forcats::fct_recode(plotTbl1$label, names(plotTblLs)[1] = "Prediction model", "Treat all" = "Treat all", "Treat none" = "Treat none")
    levels(plotTbl1$label) <- c(names(plotTblLs)[1], "Treat all", "Treat none")
    # print(plotTbl1, n=nrow(plotTbl1))
    # d <- 4
    for(d in 2:length(plotTblLs)) {
        levels(plotTblLs[[d]]$label) <- c(names(plotTblLs)[d], "Treat all", "Treat none")
        idxAdd <- plotTblLs[[d]]$label == names(plotTblLs)[d]
        plotTbl1 <- dplyr::bind_rows(plotTbl1, plotTblLs[[d]][idxAdd,])
        # print(plotTbl1, n=nrow(plotTbl1))
    }
    return(plotTbl1)
}
