# ---- basic data load functions ----

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
                     columnNames = NA, # c("x","y"),
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
readDataFrame <- function(dataFrame = NA,  # LIST of dataframes
                          columns = NA,
                          columnNames = NA, # c("x","y"),
                          rowNames = NA,
                          emptyData = data.frame(data = "No Data"),  # to replace NA with
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
