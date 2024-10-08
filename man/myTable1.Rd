% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myTbl1.R
\name{myTable1}
\alias{myTable1}
\title{Counts and percentages.}
\usage{
myTable1(data = NULL, nlvl = 10)
}
\arguments{
\item{data}{The study data that is used for the prediction modeling.}

\item{nlvl}{Numeric value which represents the maximum number of categories that a variable may have in the data.}
}
\value{
a data.frame with four columns:
\enumerate{
\item name Column name in the dataset.
\item categ Category of the identified categorical column in the dataset.
\item absNum Absolute number (count) of this category.
\item percent Percentage of this category.
}
}
\description{
Compute values that usually are presented in a table 1 of an empirical research paper.
}
\examples{
myTable1(data=dummyData)
}
\author{
Marcel Miché
}
