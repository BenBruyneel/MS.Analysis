# ---- Basic data load functions ----

#' @title readData
#'
#' @description basic function factory which generates a function that returns
#'  a function that itself returns the data in a data.frame plus (if added)
#'  some info about the data.frame
#'
#' @note the reason these readData functions are function factories is that (depending on
#'  the data source) the actual reading in of data may take quite some time. readData
#'  provides a base function that reads the data in and returns a list of an info data.frame
#'  object with info on the data and a data data.frame object that contains the actual data.
#'  The result can be used as a data source for info objects (and their descendants)
#'
#' @param dataFrame data.frame containing the data
#' @param columns specifies which columns to take from the dataFrame. Can be a character or
#'  an integer vector
#' @param columnNames specifies which column names to give to the data.frame. Should be
#'  same length as the columns argument.
#' @param rowNames specifies which row names to give to the data.frame. Should be same
#'  length as the number of rows in the dataFrame argument. If rowNames = NA (default) then
#'  rownames of the dataFrame are used. If set to NULL, then existing row names will be
#'  removed
#' @param info named list object that contains info to be put into the info data.frame of
#'  the info objects. The intention is to put some relevant information there, like source
#'  of the data, file names and other information which makes it possible to recreate the
#'  data
#'
#' @return a function that returns a list of two objects: info and data
#'
#' @examples
#' result <- readData(dataFrame = datasets::mtcars)()
#' result$data
#' result$info
#' result <- readData(dataFrame = datasets::mtcars, info = list(source = "datasets",
#'                                                              name = "mtcars"))()
#'                                                              result$info
#' result <- readData(dataFrame = datasets::mtcars, columns = 1:2)()
#' result$data
#' result <- readData(dataFrame = datasets::mtcars, columns = 1:2,
#'                    columnNames = c("x","y"), rowNames = NULL)()
#' result[["data"]]
#'
#' @export
readData <- function(dataFrame = NA,
                     columns = ifelseProper(identical(dataFrame, NA), NA,
                                            1:ncol(dataFrame)),
                     columnNames = NA,
                     rowNames = NA,
                     info = list(source = "data")){
  force(dataFrame)
  force(columns)
  force(columnNames)
  force(rowNames)
  force(info)
  function(...){
    if (!identical(dataFrame, NA)){
      if (!identical(columns, NA)){
        dataFrame <- dataFrame[, columns]
      }
      if (!identical(columnNames, NA)){
        colnames(dataFrame) <- columnNames
      }
      if (!identical(rowNames, NA)){
        rownames(dataFrame) = rowNames
      }
    }
    return(
      list(
        info = info,
        data = dataFrame)
    )
  }
}


#' @title readDataFrame
#'
#' @description basic function factory which generates a function that returns
#'  a organized list of readData functions from a list of data.frames's.
#'  Essentially a list version of \link{readData}
#'
#' @param dataFrame a -list- of data.frame's (or NA)
#' @param columns specifies which columns to take from the dataFrame. Can be a character or
#'  an integer vector. Please note that the specified columns HAVE to exist in all of the
#'  data.frame's in the list (dataFrame argument)
#' @param columnNames specifies which column names to give to the data.frame. Should be
#'  same length as the columns argument. Applied to ALL data.frame's in the list
#' @param rowNames specifies which row names to give to the data.frame. Should be same
#'  length as the number of rows in the dataFrame argument. If rowNames = NA (default) then
#'  rownames of the dataFrame are used. If set to NULL, then existing row names will be
#'  removed. Except for the values NA and NULL, this argument can only be used when the
#'  number of rows in the different data.frame's in the list is exactly the same,
#'  otherwise there may be errors or unexpected effects
#' @param emptyData defines what needs to be used for data when an element of the data.frame
#'  list is NA. Default is data.frame(data = "No Data" ). This argument is used when
#'  data.frame's are expected for the result.
#' @param info named list object that contains info to be put into the info data.frame of
#'  the info objects. The intention is to put some relevant information there, like source
#'  of the data, file names and other information which makes it possible to recreate the
#'  data. Again, this argument is applied to all elements in the data.frame list
#'
#' @returns a function that will generate a list of elements which each two objects:
#'  info and data
#'
#' @examples
#' result <- readDataFrame(dataFrame = list(datasets::mtcars, datasets::iris))
#' result
#' result()
#' result <- readDataFrame(dataFrame = list(datasets::mtcars, datasets::iris))()
#' result[[1]]
#' result[[1]]$data |> head()
#' result[[2]]$data |> head()
#' result <- readDataFrame(dataFrame = list(datasets::mtcars, datasets::iris),
#'                         columns = 1:2, columnNames = c("x","y"),
#'                         rowNames = NULL)()
#' result[[1]]$data |> head()
#' result[[2]]$data |> head()
#'
#' @export
readDataFrame <- function(dataFrame = NA,
                          columns = NA,
                          columnNames = NA,
                          rowNames = NA,
                          emptyData = data.frame(data = "No Data"),
                          info = list(source = "data")){
  force(dataFrame)
  force(columns)
  force(columnNames)
  force(rowNames)
  force(info)
  function(...){
    result <- list()
    for (counter in 1:length(dataFrame)){
      if (!identical(dataFrame[[counter]], NA)){
        if (!identical(columns, NA)){
          dataFrame[[counter]] <- dataFrame[[counter]][, columns]
        }
        if (!identical(columnNames, NA)){
          colnames(dataFrame[[counter]]) <- columnNames
        }
        if (!identical(rowNames, NA)){
          rownames(dataFrame[[counter]]) = rowNames
        }
      } else {
        dataFrame[[counter]] = emptyData
      }
      result[[counter]] <- list(
        info = info,
        data = dataFrame[[counter]]
      )
    }
    return(result)
  }
}

#' @title readCSV
#'
#' @description function factory which generates a function that reads a .csv
#'  file and returns an single element list of data/info object (also a list).
#'  See also \link{readDataFrame}
#'
#' @param filename the name of the file which the data are to be read from.
#' @param columns specifies which columns to take from the dataFrame. Can be a character or
#'  an integer vector. Please note that the specified columns HAVE to exist in all of the
#'  data.frame's in the list (dataFrame argument)
#' @param columnNames specifies which column names to give to the data.frame. Should be
#'  same length as the columns argument. Applied to ALL data.frame's in the list
#' @param sep the field separator character.
#' @param skip integer: the number of lines of the data file to skip before
#'  beginning to read data.
#' @param header a logical value indicating whether the file contains the names
#'  of the variables as its first line. See also \link[utils]{read.csv}
#' @param additionalInfo named list object that contains info to be put into the
#'  info data.frame of the info objects. The intention is to put some relevant
#'  information there, like source of the data, file names and other information
#'  which makes it possible to recreate the data. Again, this argument is applied
#'  to all elements in the data.frame list. If not specified (default is NA), then
#'  only a basic info element is generated with source = "csv" and filename =
#'  filename.
#'
#' @returns a function that will generate a list of elements (length is 1 in this case!)
#'  which each two objects: info and data
#'
#' @examples
#' filename <- tempfile()
#' utils::write.csv(datasets::mtcars, file = filename, row.names = FALSE)
#' result <- readCSV(filename = filename)()
#' result[[1]]$info
#' result[[1]]$data |> head(10)
#' unlink(filename)
#' @export
readCSV <- function(filename, columns = 1:2,
                    columnNames = c("x","y"),
                    sep = ",", skip = 0, header = TRUE,
                    additionalInfo = NA){
  force(filename)
  force(columns)
  force(columnNames)
  force(sep)
  force(skip)
  force(header)
  force(additionalInfo)
  function(...){
    tempdf <- utils::read.csv(filename,
                              sep = sep, skip = skip,
                              header = header)
    readDataFrame(dataFrame = list(tempdf),
                  columns = columns,
                  columnNames = columnNames,
                  rowNames = NULL,
                  info = ifelseProper(identical(additionalInfo, NA),
                                      list(source = "csv",
                                           filename = filename),
                                      append(list(source = "csv",
                                                  filename = filename),
                                             additionalInfo)))()
  }
}

#' @title readExcel
#'
#' @description function factory to read excel data
#'
#' @note Uses the 'XLConnect' package to read the data from the file
#'
#' @param filename name of the excel file from which the data to read
#' @param sheet name or number of the sheet in the excel file to read data from
#' @param columns columns from the excel file to read
#' @param columnNames new names for the columns of the data read from the excel
#'  file (length column names should be the same as the length of columns)
#' @param rowNames specifies which row names to give to the data.frame. Should
#'  be same length as the number of rows of the data being read. Default is NULL
#' @param additionalInfo additional info to be added to the result of the read
#'  data function. Should be named list format, default is NA
#'
#' @return a function that reads the data from the specified excel file and
#'  returns a list of two objects: info and data
#'
#' @examples
#' # mtcars xlsx file from demoFiles subfolder of package XLConnect
#' demoExcelFile <- system.file("demoFiles/mtcars.xlsx", package = "XLConnect")
#' result <- readExcel(demoExcelFile)()
#' result[["info"]]
#' result[["data"]] |> head()
#' result <- readExcel(demoExcelFile, rowNames = rownames(datasets::mtcars))()
#' result[["data"]] |> head()
#' result <- readExcel(demoExcelFile, columns = 1:2, columnNames = c("x","y"))()
#' result[["data"]] |> head()
#'
#' @export
readExcel <- function(filename, sheet = 1,
                      columns = NA, columnNames = NA,
                      rowNames = NULL, additionalInfo = NA){
  force(filename)
  force(sheet)
  force(columns)
  force(columnNames)
  force(rowNames)
  force(additionalInfo)
  function(...){
    if (!file.exists(filename)){
      stop(paste(c("File ", filename," does not exist"), collapse = ""))
    }
    workbook <- XLConnect::loadWorkbook(filename)
    if (is.Class(sheet, 'character')){
      stopit <- !(sheet %in% XLConnect::getSheets(workbook))
    } else {
      stopit <- ((sheet < 1) | (sheet > length(XLConnect::getSheets(workbook))))
    }
    if (stopit) {
      stop(paste(c("Sheet ",sheet," does not exist"), collapse = ""))
    }
    tempdf <- XLConnect::readWorksheet(workbook, sheet = sheet)
    readData(dataFrame = tempdf,
             columns = columns,
             columnNames = columnNames,
             rowNames = rowNames,
             info = ifelseProper(identical(additionalInfo, NA),
                                 list(source = "xlsx",
                                      filename = filename),
                                 append(list(source = "xlsx",
                                             filename = filename),
                                        additionalInfo)))()
  }
}

# ---- File Info ----

#' @title fileInfo
#'
#' @description Internal function factory that generates a function for a simple
#'  info element for the fileInfo.CSV function. Note that it does not read data,
#'  it only generates an object with the filename in the info object
#'
#' @param filename name of the file from which the info is to be read
#'
#' @return a function that returns list of two objects: info (contains only
#'  filename) and data (empty)
#'
#' @note currently only for internal use, may be removed
#'
#' @examples
#' filename <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".txt")
#' writeLines(c("test", "file"), filename)
#' result <- fileInfo(filename)()
#' result[[1]]$info
#' result[[1]]$data
#' unlink(filename)
#'
#' @export
fileInfo <- function(filename){
  force(filename)
  function(...){
    if (!file.exists(filename)){
      stop("File does not exist")
    }
    return(
      readDataFrame(dataFrame = list(data.frame(data = NA)),
                    info = list(filename = filename))()
    )
  }
}

#' @title fileInfo.CSV
#'
#' @description generates a function for a simple info element for a CSV function.
#'  Note that it does not read data, it only generates an object with the filename
#'  and the names of the columns in the info object
#'
#' @param filename name of the file from which the info is to be read. This must
#'  be a csv-style file that has headers and can be read using
#'  \link[utils]{read.csv}
#'
#' @returns a function that returns list of two objects: info (contains filename
#'  and description) and data (empty)
#'
#' @note the returned function has two (functional) arguments: sep (default = ",")
#'  and ... which can be used to pass additional arguments onto
#'  \link[utils]{read.csv}
#'
#' @examples
#' filename <- tempfile()
#' utils::write.csv(datasets::mtcars, file = filename, row.names = FALSE)
#' result <- fileInfo.CSV(filename = filename)()
#' result[[1]]$info
#' result[[1]]$data
#'
#' @export
fileInfo.CSV <- function(filename){
  force(filename)
  function(sep = ",", ...){
    result <- fileInfo(filename)()
    tempResult <- utils::read.csv(file = filename, header = TRUE, sep = sep, nrows = 1, ...)
    result[[1]]$info[["description"]] <- colnames(tempResult)
    return(result)
  }
}

