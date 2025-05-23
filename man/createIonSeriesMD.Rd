% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MS_annotation.R
\name{createIonSeriesMD}
\alias{createIonSeriesMD}
\title{createIonSeriesMD}
\usage{
createIonSeriesMD(
  whichSeries = "y",
  theSeries = NA,
  whichCharge = 1,
  positive = TRUE
)
}
\arguments{
\item{whichSeries}{letter or name assigned to the series, eg y for y-ions,
b for b-ions, etc}

\item{theSeries}{integer vector, eg 1:10 to specify the numbers of the ion series}

\item{whichCharge}{integer vector, gives the charge state (1,2,3,...)}

\item{positive}{logical vector. If TRUE than positive ions series is generated,
else negative ion series}
}
\value{
a character vector or NA
}
\description{
function to generate a series of markdown/html-style labels for
ions
}
\note{
essentially a wrapper for \link[MS.Analysis]{createIonSeries}
}
\examples{
createIonSeriesMD(theSeries = 1:5)
createIonSeriesMD(whichSeries = "b", theSeries = 1:5, whichCharge = 2)

}
