% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compressCalibPlot.R
\name{compressCalibPlot}
\alias{compressCalibPlot}
\title{Select compression to produce an averaged calibration plot.}
\usage{
compressCalibPlot(mnSpanLs = NULL, model = "noNameModel")
}
\arguments{
\item{mnSpanLs}{The list that is returned by the function 'avrgCalibPlotLs'.}

\item{model}{The name of the prediction model, as shown in the output of the function 'makeCalibPlotLs' or 'avrgCalibPlotLs'.}
}
\value{
a data.frame with five columns:
\enumerate{
\item mn_x Mean x-axis value (x-axis = predicted probabilities).
\item mn_y  Mean y-axis value (y-axis = observed outcome rate).
\item lci_y Lower limit of the 95 percent confidence interval.
\item uci_y Upper limit of the 95 percent confidence interval.
\item model Name of the prediction model.
}
}
\description{
Compress output of function 'avrgCalibPlotLs' in order to produce an average plot across several calibration plots.
}
\examples{
# Preparations:
calibLRLs <- makeCalibPlotLs(calibLs=test$orderedObsLs, model="logreg")
testcalibLR <- avrgCalibPlotLs(calibPlotLs = calibLRLs, model="logreg")
# With the output of function 'avrgCalibPlotLs', use 'compressCalibPlot':
testcalibLRPlot <- compressCalibPlot(mnSpanLs = testcalibLR, model = "logreg")
# Peek at first 2 lines of output:
head(testcalibLRPlot, n=2)
#           mn_x mn_y lci_y uci_y  model
# 1 0.0005528451    0     0     0 logreg
# 2 0.0015136628    0     0     0 logreg
}
\author{
Marcel Miché
}
