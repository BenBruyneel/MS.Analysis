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
#' @param dataFrame a -list- of data.frame's
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

#' @title readChromatogram.Thermo
#'
#' @description function factory that generates a function that reads data from
#'  a thermo MS chromatogram file.
#'
#' @note the file for this function needs to be '.raw' format. Internally the
#'  'rawrr' package is used to do the actual data extraction
#'
#' @param filename name of the .raw file from which the data is to be read
#' @param mz specifies the m/z('s) to make an extracted ion chormatogram of.
#'  Ignored unless the 'type' argument is "xic"
#' @param tolerance spcifies the tolerance to use when extracting an ion
#'  chromatogram. Ignored unless the 'type' argument is "xic". Please note that
#'  this value is in 'ppm' and that a value of 10 is the same as a mass
#'  tolerance of 5 ppm in eg the Thermo freestyle software (the 5 specified
#'  there is a range m/z-5 till m/z+5). Here 10 means m/z-5 ppm till m/z+5 ppm.
#' @param filter specifies the scan filter to be used, default = "ms". Valid is
#'  also eg "ms2" for ms2 data (if present in the file).
#' @param type specifies the data type to read, possible is: "tic"  (total ion
#'  current), "bpc" (base peak chromatogram) or "xic" (extracted ion
#'  chromatogram)
#' @param additionalInfo additional info to be added to the result of the read
#'  data function. Should be named list format or a data.frame, default is NA
#'
#' @return a function that reads the data from the specified .raw file and returns a
#'  list of two objects: info and data
#'
#' @examples
#' demoRaw <- fs::path_package("extdata", "drugx_15.raw", package = "MS.Analysis")
#' result <- readChromatogram.Thermo(filename = demoRaw, type = "tic", filter = "ms2")()
#' result[[1]]$info
#' result[[1]]$data |> head(10)
#' with(result[[1]]$data, plot(rt, intensity, type = "l"))
#'
#' @export
readChromatogram.Thermo <- function(filename,
                                    mz = NA,
                                    tolerance = 10,
                                    filter = "ms",
                                    type = "xic",
                                    additionalInfo = NA){    # additionalInfo: either a list, data.frame or NA
  force(filename)
  force(mz)
  force(tolerance)
  force(filter)
  force(type)
  force(additionalInfo)
  function(...){
    tempData <- rawrr::readChromatogram(rawfile = filename,
                                        mass = mz,
                                        tol = tolerance,
                                        filter = filter,
                                        type = type)
    result <- list()
    #    if (type == "xic"){
    if (!identical(additionalInfo, NA )){
      if (is.Class(additionalInfo, "data.frame")){
        if (length(mz) != nrow(additionalInfo)){
          additionalInfo <- purrr::map_df(1:length(mz), ~additionalInfo[1,])
        }
      } else {
        if (length(additionalInfo) != length(mz)){
          additionalInfo <- rep(additionalInfo[1], length(mz))
        }

      }
    }
    if (type == "xic"){
      for (counter in 1:length(tempData)){
        result[[counter]] <- readData(
          dataFrame = data.frame(rt = as.numeric(tempData[[counter]]$times),
                                 intensity = as.numeric(tempData[[counter]]$intensities)),
          columnNames = c("rt", "intensity"),
          info = list(source = "thermo",
                      filename = filename,
                      mz = mz[counter],
                      tolerance = tolerance,
                      filter = filter,
                      type = "xic")
          )()
        if (!identical(additionalInfo, NA)){
          if (is.Class(additionalInfo, "data.frame")){
            toAdd <- as.list(additionalInfo[counter,])
            names(toAdd) <- colnames(additionalInfo)
          } else {
            if (!identical(additionalInfo[[counter]], NA)){
              toAdd <- additionalInfo[counter]
            } else {
              toAdd <- NA
            }
          }
          if (!identical(toAdd, NA)){
            result[[counter]]$info <- append(result[[counter]]$info, toAdd)
          }
        }
      }
    } else {
      result[[1]] <- readData(
        dataFrame = data.frame(rt = as.numeric(tempData$times),
                               intensity = as.numeric(tempData$intensities)),
        columnNames = c("rt", "intensity"),
        info = list(source = "thermo",
        filename = filename,
        mz = ifelse(is.null(mz),
                    NA,
                    mz),
        tolerance = tolerance,
        filter = filter,
        type = type)
        )()
      if (!identical(additionalInfo[[1]], NA)){
        if (is.Class(additionalInfo[[1]], "data.frame")){
          cnames <- colnames(additionalInfo[[1]])
          additionalInfo[[1]] <- as.list(additionalInfo[[1]])
          names(additionalInfo[[1]]) < cnames
        }
        result[[1]]$info <- append(result[[1]]$info, additionalInfo)
      }
    }
    return(result)
  }
}


