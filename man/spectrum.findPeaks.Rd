% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/msData.R
\name{spectrum.findPeaks}
\alias{spectrum.findPeaks}
\title{spectrum.findPeaks}
\usage{
spectrum.findPeaks(
  data,
  index = 1,
  id = NA,
  spectrumDetectPeaksMethod = NA,
  ...
)
}
\arguments{
\item{data}{msInfo object containing the spectrum with index ...  or id ...}

\item{index}{index of the spectrum on which to do peak detection}

\item{id}{id of the spectrum to perform peak detection on, overrides index}

\item{spectrumDetectPeaksMethod}{function to use for peak detection. If NA (default)
then the function will determine which function to use, if possible based on
whether the spectrum is centroided or not}

\item{...}{to pass additional arguments on to \link[MS.Analysis]{spectrumDetectPeaks}
(or spectrumDetectPeaks.Centroid or spectrumDetectPeaks.Profile) which does the actual
work}
}
\value{
a data.frame or NA
}
\description{
performs peak detection on spectrum present in an msInfo object.
Can be used stand-alone or as part of a \link[dataInfo]{dataFunctions} object.
}
\examples{
msR <- msInfo$new()
demoFile <- fs::path_package("extdata", "spec2.csv", package = "MS.Analysis")
result <- read.table(demoFile, sep = ",", header = TRUE)
msR$spectra$add(
  dataElements = dataInfo::readDataFrame(dataframes = list(result))
)
spectrum.findPeaks(data = msR$spectra) |> head(10)

}
