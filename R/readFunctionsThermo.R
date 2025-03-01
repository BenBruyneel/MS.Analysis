# ---- Thermo data load functions ----

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
#'  list (of lists with two objects: info and data)
#'
#' @examples
#' demoRaw <- fs::path_package("extdata", "reserpine07.RAW", package = "MS.Analysis")
#' result <- readChromatogram.Thermo(filename = demoRaw, type = "tic", filter = "ms2")()
#' result[[1]]$info
#' result[[1]]$data |> head(10)
#' with(result[[1]]$data, plot(rt, intensity, type = "l"))
#' demoRaw <- fs::path_package("extdata", "reserpine07.RAW", package = "MS.Analysis")
#' result <- readChromatogram.Thermo(filename = demoRaw, type = "xic", filter = "ms2",
#'  mz = c(397.16, 448.13), tolerance = 500)()
#' result[[1]]$info
#' with(result[[1]]$data, plot(rt, intensity, type = "l"))
#' result[[2]]$info
#' with(result[[2]]$data, plot(rt, intensity, type = "l"))
#'
#' @export
readChromatogram.Thermo <- function(filename,
                                    mz = NA,
                                    tolerance = 10,
                                    filter = "ms",
                                    type = "xic",
                                    additionalInfo = NA){
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

#' @title readSpectrum.Thermo
#'
#' @description function factory that generates a function that reads spectral
#'  data from a thermo MS chromatogram file
#'
#' @note the file(s) for this function needs to be '.raw' format. Internally the 'rawrr'
#'  package is used to do the actual data extraction
#'
#' @param filename name of the .raw file from which the data is to be read
#' @param scan one or more integer vectors: the scan numbers to be extracted
#' @param centroided whether to retrieve the centroided spectrum (TRUE) or not (FALSE,
#'  default). If set to TRUE, please note that this only works properly when there
#'  is centroided data included in the spectrum. If there is no centroided data,
#'  then the 'regular' spectral data 'stream' will be used - which can be centroid
#'  (in nature), but doesn't have to be. In such cases it is up to the user to
#'  know (via the acquisition method) what type of data it is. Please also note
#'  that the info section a returned spectrum will contain this parameter. If it
#'  is not correct, then it should be changed manually to the correct value.
#'
#' @return a function that reads data from the specified .raw file and returns a
#'  list (of lists with two objects: info and data). The function generated takes
#'  a single argument called parameters, which defines what ends up in the info
#'  object. Options are "basic" (default), "extended" and "full". "full" attempts
#'  to get all meta data from the spectrum into the info object and is somewhat
#'  experimental.
#'
#' @examples
#' demoRaw <- fs::path_package("extdata", "reserpine07.RAW", package = "MS.Analysis")
#' result <- readSpectrum.Thermo(filename = demoRaw, scan = 245, centroided = TRUE)()
#' result[[1]]$info
#' with(result[[1]]$data, plot(mz, intensity, type = "h"))
#' result <- readSpectrum.Thermo(filename = demoRaw, scan = c(245, 246),
#'  centroided = TRUE)()
#' result[[1]]$info
#' with(result[[1]]$data, plot(mz, intensity, type = "h"))
#' result[[2]]$info
#' with(result[[2]]$data, plot(mz, intensity, type = "h"))
#'
#' @export
readSpectrum.Thermo <- function(filename,
                                scan = NULL,
                                centroided = FALSE){
  force(filename)
  force(scan)
  force(centroided)
  function(parameters = c("basic", "extended", "full")[1]){
    tempData <- rawrr::readSpectrum(rawfile = filename,
                                    scan = scan)
    result <- list()
    for (counter in 1:length(scan)){
      data <- NA
      if (centroided){
        if (("centroid.mZ" %in% names(tempData)) &
            ("centroid.intensity" %in% names(tempData))){
          data <- data.frame(mz = tempData[[counter]]$centroid.mZ,
                             intensity = tempData[[counter]]$centroid.intensity)
        }
      }
      if (identical(data, NA)) {
        data <- data.frame(mz = tempData[[counter]]$mZ,
                           intensity = tempData[[counter]]$intensity)
      }
      info <- list(source = "thermo",
                   filename = filename,
                   rt = tempData[[counter]]$rtinseconds/60,
                   scan = scan[counter],
                   scanType = tempData[[counter]]$scanType,
                   centroided = centroided)
      if (parameters == "extended"){
        info <- append(info,
                       list(
                         microScanCount = as.integer(tempData[[counter]][["Micro Scan Count:"]]),
                         ionInjectionTime = as.numeric(tempData[[counter]]["Ion Injection Time (ms):"]),
                         elapsedTime = as.numeric(tempData[[counter]]["Elapsed Scan Time (sec):"]),
                         resolutionMS = as.integer(tempData[[counter]][["Orbitrap Resolution:"]]),
                         masterScan = as.integer(tempData[[counter]][["Master Scan Number:"]])))

      } else {
        if (parameters == "full"){
          whichOnes <- which(unname(purrr::map_int(tempData[[counter]], ~length(.x))) == 1)
          tempData[[counter]] <- tempData[[counter]][whichOnes]
          removeOdd <- which(names(tempData[[counter]]) == "\001")
          if (length(removeOdd)>0){
            tempData[[counter]] <- tempData[[counter]][-c(removeOdd)]
          }
          names(tempData[[counter]]) <- strReplaceAll(names(tempData[[counter]]),
                                                pattern = c(" ", ":", "/", "\\(", "\\)"), replacement = "")
          findDoubles <- which(table(names(tempData[[counter]])) > 1)
          for (counter2 in 1:length(findDoubles)){
            whichOnes <- which(names(tempData[[counter]]) == names(findDoubles[counter2]))
            for (counter3 in 1:length(whichOnes)){
              names(tempData[[counter]])[whichOnes[counter3]] <- paste(c(names(tempData[[counter]])[whichOnes[counter3]], "_", counter2), collapse = "")
            }
          }
          info <- append(list(source = "thermo",
                              filename = filename,
                              rt = tempData[[counter]]$rtinseconds/60,
                              centroided = centroided),
                         tempData[[counter]])
        }
      }
      result[[counter]] <- readData(dataFrame = data,
                                    columnNames = c("mz","intensity"),
                                    info = info)()
    }
    return(result)
  }
}

# ---- Thermo file info function ----

#' @title fileInfo.Thermo
#'
#' @description function factory that generates a function for an info & data
#' element with info on a Thermo .raw mass spectrometry file
#'
#' @param filename name of the file from which the data is to be read. Must be a
#'  Thermo Scientific mass spectrometry .raw file
#' @param readIndex logical vector. If TRUE the data element will contain info
#'  on all scans in the file. If FALSE then data element will be empty
#' @param collapseCharacter some of the file info data consists of more than one
#'  element. These will be pasted together where collapseCharacter defines the
#'  separator
#'
#' @returns a function that returns list of two objects: info (contains only
#'  file info) and data (empty or index of scans)
#'
#' @examples
#' demoRaw <- fs::path_package("extdata", "reserpine07.RAW", package = "MS.Analysis")
#' result <- fileInfo.Thermo(filename = demoRaw)()
#' result[[1]]$info |> as.data.frame()
#' result[[1]]$data |> head(10)
#' result <- fileInfo.Thermo(filename = demoRaw, readIndex = FALSE)()
#' result[[1]]$info |> as.data.frame()
#' result[[1]]$data
#' @export
fileInfo.Thermo <- function(filename, readIndex = TRUE, collapseCharacter = "-"){
  force(filename)
  force(readIndex)
  force(collapseCharacter)
  function(...){
    if (!file.exists(filename)){
      stop("File does not exist")
    }
    tempList <- list(filename = filename)
    tempList <- append(tempList , rawrr::readFileHeader(filename))
    tLengths <- unlist(lapply(tempList, length))
    for (counter in which(tLengths > 1)){
      # paste together multiple element vectors in the list
      tempList[[counter]] <- paste(tempList[[counter]],
                                   collapse = collapseCharacter)
    }
    if (readIndex){
      tempIndex <- rawrr::readIndex(filename)
    } else {
      tempIndex <- NA
    }
    names(tempList) <- gsub(names(tempList),
                            pattern = " ", replacement = "")
    names(tempList) <- gsub(names(tempList),
                            pattern = "\\(", replacement = "")
    names(tempList) <- gsub(names(tempList),
                            pattern = "\\)", replacement = "")
    return(
      readDataFrame(dataFrame = ifelseProper(identical(tempIndex, NA),
                                             NA,
                                             list(tempIndex)),
                    info = tempList)()
    )
  }
}

# ---- Thermo processing output ----
# methods for reading XLS files output by Thermo Scientific QuanBrowser (XCalibur)
# may be converted to S3 OOP...

#' @title readProcessing.Thermo
#'
#' @description reads in the excel files generated by the processing methods in
#'  the Thermo Scientific XCalibur software. The export file is an excel file
#'  in which the integration data is stored per sheet
#'
#' @param filename name of the file from which the data is to be read. Must be
#'  an export file in excel format (.XLS), see Xcalibur software manual.
#' @param leaveIn character vector (or NA). Defines which sheets (components) to
#'  extract. Default is NA, which causes all sheets to be read (except those
#'  defined im the 'leaveout' argument).
#' @param leaveOut character vector which defines which sheets to leave out of
#'  the result. The internal functions (packages readxl & XLConnect) see two
#'  extra sheets which do not contain info on the components defined in the
#'  processing method. They have to do with formatting & macros. This package
#'  doesn't do anything with that, so this argument can be used to remove them
#'  from the result. If set to NA, then every sheet in the file is read.
#'
#' @returns NA (in case it fails reading the sheets from the file) or a list of
#'  data.frame's containing the integration data (etc) in the .XLS file exported
#'  from Xcalibur
#'
#' @examples
#' demoXLS <- fs::path_package("extdata", "ExcelExp_Long.XLS", package = "MS.Analysis")
#' result <- readProcessing.Thermo(filename = demoXLS)
#' result[[1]]
#'
#' @export
readProcessing.Thermo <- function(filename,
                                  leaveIn = NA,
                                  leaveOut = c("Component", "mdlCalcs")){
  if (file.exists(filename)){
    shtnames <- readxl::excel_sheets(filename)
    if (!identical(leaveIn, NA)){
      shtnames <- shtnames[(shtnames %in% leaveIn)]
    }
    if (!identical(leaveOut, NA)){
      shtnames <- shtnames[!(shtnames %in% leaveOut)]
    }
    if (length(shtnames) > 0){
      shts <- purrr::map(shtnames, ~XLConnect::readWorksheetFromFile(file = filename, sheet = .x))
      return(shts)
    }
  }
  warning(paste0("File '", filename, "' does not exist or has an error"))
  return(NA)
}

#' @title processingThermo.componentName
#'
#' @description extracts the component names from the data read by
#'  read.ThermoProcessing
#'
#' @param sheet either a list or an element from a data.frame coming from
#'  read.ThermoProcessing
#'
#' @returns a character vector
#'
#' @examples
#' demoXLS <- fs::path_package("extdata", "ExcelExp_Short.XLS", package = "MS.Analysis")
#' result <- readProcessing.Thermo(filename = demoXLS)
#' processingThermo.componentName(result)
#' processingThermo.componentName(result[[1]])
#'
#' @export
processingThermo.componentName <- function(sheet){
  if (is.Class(sheet, "list")){
    return(purrr::map_chr(sheet, ~processingThermo.componentName(.x)))
  } else {
    whereIs <- which(grepl(as.data.frame(sheet)[,1], pattern = "Component Name"))
    return(as.data.frame(sheet)[whereIs+1,1])
  }
}

#' @title processingThermo.creatorInfo
#'
#' @description extracts the creator (user) info from the data read by
#'  read.ThermoProcessing
#'
#' @param sheet either a list or an element from a data.frame coming from
#'  read.ThermoProcessing
#'
#' @returns data.frame
#'
#' @examples
#' demoXLS <- fs::path_package("extdata", "ExcelExp_Short.XLS", package = "MS.Analysis")
#' result <- readProcessing.Thermo(filename = demoXLS)
#' processingThermo.creatorInfo(result)
#' processingThermo.creatorInfo(result[[1]])
#'
#' @export
processingThermo.creatorInfo <- function(sheet){
  if (is.Class(sheet, "list")){
    tempdf2 <- purrr::map(sheet, ~processingThermo.creatorInfo(.x))
    resultdf <- tempdf2[[1]]
    for (counter in 2:length(tempdf2)){
      resultdf <- rbind(resultdf, tempdf2[[counter]])
    }
    return(resultdf)
  } else {
    whereIs <- which(grepl(as.data.frame(sheet)[,1], pattern = "Created By:"))
    tempdf <- as.data.frame(sheet)[whereIs+2,c(1,3,5)]
    colnames(tempdf) <- sheet[whereIs+1,c(1,3,5)]
    rownames(tempdf) <- NULL
    return(tempdf)
  }
}

#' @title processingThermo.componentData
#'
#' @description extracts the component data from the data read by
#'  read.ThermoProcessing
#'
#' @param sheet either a list or an element from a data.frame coming from
#'  read.ThermoProcessing
#'
#' @returns data.frame
#'
#' @examples
#' demoXLS <- fs::path_package("extdata", "ExcelExp_Short.XLS", package = "MS.Analysis")
#' result <- readProcessing.Thermo(filename = demoXLS)
#' processingThermo.componentData(result)
#' processingThermo.componentData(result[[1]])
#'
#' @export
processingThermo.componentData <- function(sheet){
  if (is.Class(sheet, "list")){
    tempdf2 <- purrr::map(sheet, ~processingThermo.componentData(.x))
    resultdf <- tempdf2[[1]]
    for (counter in 2:length(tempdf2)){
      resultdf <- rbind(resultdf, tempdf2[[counter]])
    }
    return(resultdf)
  } else {
    thesheet <- as.data.frame(sheet)
    whereIs <- which(grepl(thesheet[,1], pattern = "Filename"))
    whereIsEnd <- which(grepl(thesheet[,1], pattern = "Created By:"))
    tempdf <- thesheet[(whereIs+1):(whereIsEnd-1),]
    names(tempdf) <- thesheet[whereIs,]
    rownames(tempdf) <- NULL
    # remove empty rows
    toKeep <- purrr::map_lgl(1:nrow(tempdf), ~sum(unname(apply(tempdf[.x,], MARGIN = 2, is.na))) != ncol(tempdf))
    tempdf <- tempdf[toKeep,]
    tempdf$Component <- processingThermo.componentName(sheet)
    colnames(tempdf) <- strReplaceAll(colnames(tempdf), pattern = c(" ","\\."), replacement = "")
    return(tempdf)
  }
}
