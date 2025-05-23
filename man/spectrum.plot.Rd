% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/msData.R
\name{spectrum.plot}
\alias{spectrum.plot}
\title{spectrum.plot}
\usage{
spectrum.plot(data, index = 1, id = NA, ...)
}
\arguments{
\item{data}{msInfo object containing the spectrum with index ...  or id ...}

\item{index}{index of the spectrum to be plotted}

\item{id}{id of the spectrum to be plotted, overrides index}

\item{...}{to pass additional arguments on to \link[MS.Analysis]{plotSpectrum} which
does the actual plotting}
}
\value{
a ggplot object or NA
}
\description{
plots a spectrum present in an msInfo object. Can be used stand-alone
or as part of a \link[dataInfo]{dataFunctions} object.
}
\examples{
msR <- msInfo$new()
demoFile <- fs::path_package("extdata", "spec2.csv", package = "MS.Analysis")
result <- read.table(demoFile, sep = ",", header = TRUE)
msR$spectra$add(
  dataElements = dataInfo::readDataFrame(dataframes = list(result))
)
spectrum.plot(data = msR$spectra)

}
