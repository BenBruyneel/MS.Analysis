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
