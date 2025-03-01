% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readFunctions.R
\name{readExcel}
\alias{readExcel}
\title{readExcel}
\usage{
readExcel(
  filename,
  sheet = 1,
  columns = NA,
  columnNames = NA,
  rowNames = NULL,
  additionalInfo = NA
)
}
\arguments{
\item{filename}{name of the excel file from which the data to read}

\item{sheet}{name or number of the sheet in the excel file to read data from}

\item{columns}{columns from the excel file to read}

\item{columnNames}{new names for the columns of the data read from the excel
file (length column names should be the same as the length of columns)}

\item{rowNames}{specifies which row names to give to the data.frame. Should
be same length as the number of rows of the data being read. Default is NULL}

\item{additionalInfo}{additional info to be added to the result of the read
data function. Should be named list format, default is NA}
}
\value{
a function that reads the data from the specified excel file and
returns a list of two objects: info and data
}
\description{
function factory to read excel data
}
\note{
Uses the 'XLConnect' package to read the data from the file
}
\examples{
# mtcars xlsx file from demoFiles subfolder of package XLConnect
demoExcelFile <- system.file("demoFiles/mtcars.xlsx", package = "XLConnect")
result <- readExcel(demoExcelFile)()
result[["info"]]
result[["data"]] |> head()
result <- readExcel(demoExcelFile, rowNames = rownames(datasets::mtcars))()
result[["data"]] |> head()
result <- readExcel(demoExcelFile, columns = 1:2, columnNames = c("x","y"))()
result[["data"]] |> head()

}
