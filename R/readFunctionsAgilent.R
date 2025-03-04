# ---- Agilent data export load functions ----

#' @title chromatogramInfo.AgilentExport
#'
#' @description translates the chromatogram description line for Agilent
#'  Masshunter chromatograms. Supported chromatogram descriptions are mass
#'  spectrometry descriptions (TIC, BPC & EIC), UV (DAD) & traces for the pump
#'  (Pressure, Flow, etc)
#'
#' @param commentString The description character vector to be converted. Note
#'  that the characters '\' and '#', which are present after the initial file
#'  read (readLines), need to be removed before using this function
#' @param defaultCollapse only used in case of unknown traces. For these, the
#'  resulting elements are joined together into a single character vector which
#'  can be split by this character
#'
#' @returns a data.frame
#'
#' @examples
#' chromatogramInfo.AgilentExport("+ESI TIC Scan Frag=125.0V Data0001.d ")
#' chromatogramInfo.AgilentExport("+ESI BPC Scan Frag=125.0V Data0001.d ")
#' chromatogramInfo.AgilentExport("+ESI EIC(372.8974) Scan Frag=125.0V Data0001.d ")
#' chromatogramInfo.AgilentExport("DAD1 - A:Sig=280.0,4.0  Ref=550.0,100.0 Data0001.d")
#' chromatogramInfo.AgilentExport("BinPump1 - A: Pressure Data0001.d")
#'
#' @export
chromatogramInfo.AgilentExport <- function(commentString, defaultCollapse = ";"){
  tempResult <- unlist(stringr::str_split(commentString, pattern = "\\s"))
  tempResult <- tempResult[nchar(tempResult) !=0 & tempResult != "-"]
  if (grepl(tempResult[1], pattern = "ESI") | grepl(tempResult[1], pattern ="APCI")){
    result <- data.frame(
      polarity = substr(tempResult[1],1,1),
      signal = substr(tempResult[1],2,nchar(tempResult[1])),
      scan = substr(tempResult[2],1,3),
      mz = as.numeric(stringr::str_extract(tempResult[2], pattern = "[:digit:]*\\.[:digit:]*")),
      MS = tempResult[3],
      fragmentor = as.numeric(stringr::str_extract(tempResult[4], pattern = "[:digit:]*\\.[:digit:]*")),
      filename = tempResult[length(tempResult)]
    )
  } else {
    if (grepl(tempResult[1], pattern = "DAD")){
      result <- data.frame(
        signal = tempResult[1],
        scan = tempResult[2],
        wavelength = as.numeric(stringr::str_extract(tempResult[2], pattern = "[:digit:]*\\.[:digit:]")),
        filename = tempResult[length(tempResult)]
      )
    } else {
      if (grepl(tempResult[1], pattern = "BinPump")){
        result <- data.frame(
          signal = tempResult[1],
          scan = paste(c(tempResult[3:(length(tempResult)-1)]), collapse = " "),
          filename = tempResult[length(tempResult)]
        )
      } else {
        result <- data.frame(
          unknown = paste(tempResult, collapse = defaultCollapse)
        )
      }
    }
  }
  return(result)
}

#' @title readChromatogram.AgilentExport.memory
#'
#' @description function factory that returns a function that takes the data
#'  from a character vector which is in the format of an Agilent chromatogram
#'  export (single chromatogram, not multiple) and returns an object (list) with
#'  two elements: data (data.frame) and info (list)
#'
#' @note this function gets called by \code{link[MS.Analysis]{readChromatogram.AgilentExport}}
#'
#' @param textLines character vector of the data in Agilent chromatogram export
#'  format: first line = description, second line = Agilent names for x & y (not
#'  used) and the rest of the lines are c(rownumbers, x, y). The rownumbers are
#'  ignored.
#' @param sep defines the separator for the rownumber-x-y data. Default is ','
#' @param translateComment defines the function to be used to translate the
#'  description line. Default is NA. \code{link[MS.Analysis]{readChromatogram.AgilentExport}}
#'  is this package's function that can be used.
#' @param removePatterns defines which characters to remove from the description
#'  line (defore translation)
#' @param dataColumns defines which columns to extract from the data. Default is
#'  c(2,3). The rownumbers are ignored
#' @param columnNames defines which names to give to the extracted columns.
#'  Default is 'rt' (retention time) for x and 'intensity' for y
#'
#' @returns a list with two elements: data (data.frame) and info (list)
#'
#' @examples
#' demoFile <- fs::path_package("extdata", "Data0001.CSV", package = "MS.Analysis")
#' result <- readLines(demoFile, n = 10597)
#' result |> head()
#' result <- readChromatogram.AgilentExport.memory(result,
#'  translateComment = chromatogramInfo.AgilentExport)()
#' result$data |> head()
#' result$info
#'
#' @export
readChromatogram.AgilentExport.memory <- function(textLines,
                                                  sep = ",",
                                                  translateComment = NA,
                                                  removePatterns = c("\\\"", "#"),
                                                  dataColumns = c(2,3),
                                                  columnNames = c("rt", "intensity")){
  force(textLines)
  force(sep)
  force(translateComment)
  force(removePatterns)
  force(dataColumns)
  force(columnNames)
  function(...){
    tempComment <- strReplaceAll(textLines[1],
                                 pattern = removePatterns,
                                 replacement = "")
    tempdf <-purrr:: map_df(textLines[3:length(textLines)],
                            ~as.data.frame(t(stringr::str_split(.x,
                                                                pattern = sep)[[1]]))) %>%
      dplyr::select(dataColumns)
    colnames(tempdf) <- columnNames
    tempdf$rt <- as.numeric(tempdf$rt)
    tempdf$intensity <- as.numeric(tempdf$intensity)
    readData(dataFrame = tempdf,
             info = append(list(source = "Agilent",
                                comment = tempComment),
                           ifelseProper(
                             !identical(translateComment, NA),
                             as.list(translateComment(tempComment)),
                             NULL)
             )
    )()
  }
}
