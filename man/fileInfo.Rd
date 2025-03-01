% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readFunctions.R
\name{fileInfo}
\alias{fileInfo}
\title{fileInfo}
\usage{
fileInfo(filename)
}
\arguments{
\item{filename}{name of the file from which the info is to be read}
}
\value{
a function that returns list of two objects: info (contains only
filename) and data (empty)
}
\description{
Internal function factory that generates a function for a simple
info element for the fileInfo.CSV function. Note that it does not read data,
it only generates an object with the filename in the info object
}
\note{
currently only for internal use, may be removed
}
\examples{
filename <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".txt")
writeLines(c("test", "file"), filename)
result <- fileInfo(filename)()
result[[1]]$info
result[[1]]$data
unlink(filename)

}
