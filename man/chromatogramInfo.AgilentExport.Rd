% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readFunctionsAgilent.R
\name{chromatogramInfo.AgilentExport}
\alias{chromatogramInfo.AgilentExport}
\title{chromatogramInfo.AgilentExport}
\usage{
chromatogramInfo.AgilentExport(commentString, defaultCollapse = ";")
}
\arguments{
\item{commentString}{The description character vector to be converted. Note
that the characters '\' and '#', which are present after the initial file
read (readLines), need to be removed before using this function}

\item{defaultCollapse}{only used in case of unknown traces. For these, the
resulting elements are joined together into a single character vector which
can be split by this character}
}
\value{
a data.frame
}
\description{
translates the chromatogram description line for Agilent
Masshunter chromatograms. Supported chromatogram descriptions are mass
spectrometry descriptions (TIC, BPC & EIC), UV (DAD) & traces for the pump
(Pressure, Flow, etc)
}
\examples{
chromatogramInfo.AgilentExport("+ESI TIC Scan Frag=125.0V Data0001.d ")
chromatogramInfo.AgilentExport("+ESI BPC Scan Frag=125.0V Data0001.d ")
chromatogramInfo.AgilentExport("+ESI EIC(372.8974) Scan Frag=125.0V Data0001.d ")
chromatogramInfo.AgilentExport("DAD1 - A:Sig=280.0,4.0  Ref=550.0,100.0 Data0001.d")
chromatogramInfo.AgilentExport("BinPump1 - A: Pressure Data0001.d")

}
