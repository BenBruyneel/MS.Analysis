
# ---- Spectrum specific functions ----

#' @title spectrum.plot
#'
#' @description plots a spectrum present in an msInfo object. Can be used stand-alone
#'  or as part of a \link[dataInfo]{dataFunctions} object.
#'
#' @param data msInfo object containing the spectrum with index ...  or id ...
#' @param index index of the spectrum to be plotted
#' @param id id of the spectrum to be plotted, overrides index
#' @param ... to pass additional arguments on to \link[MS.Analysis]{plotSpectrum} which
#'  does the actual plotting
#'
#' @returns a ggplot object or NA
#'
#' @examples
#' msR <- msInfo$new()
#' demoFile <- fs::path_package("extdata", "spec2.csv", package = "MS.Analysis")
#' result <- read.table(demoFile, sep = ",", header = TRUE)
#' msR$spectra$add(
#'   dataElements = dataInfo::readDataFrame(dataframes = list(result))
#' )
#' spectrum.plot(data = msR$spectra)
#'
#' @export
spectrum.plot <- function(data , index = 1, id = NA, ...){
  index <- data$getIndex(index = index, id = id)
  if (!identical(index, NA)){
      if (!("centroidPlot" %in% names(list(...))) & ("centroided" %in% colnames(data$info))){
        return(plotSpectrum(spectrum = data$data[[index[1]]],
                            centroidPlot = data$info$centroided[index[1]],
                            ...))
      } else {
        return(plotSpectrum(spectrum = data$data[[index[1]]],
                            ...))
      }
    } else {
  }
  return(NA)
}

#' @title spectrum.plot.overlay
#'
#' @description plots an overlay of spectra present in an msInfo object. Can be used
#'  stand-alone or as part of a \link[dataInfo]{dataFunctions} object.
#'
#' @param data msInfo object containing the spectrum with index ...  or id ...
#' @param index index of the spectrum to be plotted
#' @param id id of the spectrum to be plotted, overrides index
#' @param ... to pass additional arguments on to \link[MS.Analysis]{plotSpectrumOverlay} which
#'  does the actual plotting
#'
#' @returns a ggplot object or NA
#'
#' @export
spectrum.plot.overlay <- function(data, index = 1:data$length, id = NA, ...){
  index <- data$getIndex(index = index, id = id)
  if (!identical(index, NA)){
    if (!("centroidPlot" %in% names(list(...))) & ("centroided" %in% colnames(data$info))){
      plotSpectrumOverlay(spectrumList = data$data[index],
                          centroidPlot = data$info$centroided[index], ...)
    } else {
      plotSpectrumOverlay(spectrumList = data$data[index], ...)
    }
  } else {
    return(NA)
  }
}

#' @title spectrum.plot.mirror
#'
#' @description creates a mirror plot of spectra present in an msInfo object. Can be used
#'  stand-alone or as part of a \link[dataInfo]{dataFunctions}
#'
#' @param data msInfo object containing the spectrum with index ...  or id ...
#' @param index index of the spectrum to be plotted
#' @param id id of the spectrum to be plotted, overrides index
#' @param ... to pass additional arguments on to \link[MS.Analysis]{plotSpectrumMirror} which
#'  does the actual plotting
#'
#' @returns a ggplot object or NA
#'
#' @export
spectrum.plot.mirror <- function(data, index = 1:2, id = NA, ...){
  index <- data$getIndex(index = index, id = id)
  if (!identical(index, NA)){
    if (!("centroidPlot" %in% names(list(...))) & ("centroided" %in% colnames(data$info))){
      plotSpectrumMirror(spectrumList = data$data[index[1:2]],
                         centroidPlot = data$info$centroided[index[1:2]], ...)
    } else {
      plotSpectrumMirror(spectrumList = data$data[index[1:2]], ...)
    }
  } else {
    return(NA)
  }
}

#' @title spectrum.findPeaks
#'
#' @description performs peak detection on spectrum present in an msInfo object.
#'  Can be used stand-alone or as part of a \link[dataInfo]{dataFunctions} object.
#'
#' @param data msInfo object containing the spectrum with index ...  or id ...
#' @param index index of the spectrum on which to do peak detection
#' @param id id of the spectrum to perform peak detection on, overrides index
#' @param spectrumDetectPeaksMethod function to use for peak detection. If NA (default)
#'  then the function will determine which function to use, if possible based on
#'  whether the spectrum is centroided or not
#' @param ... to pass additional arguments on to \link[MS.Analysis]{spectrumDetectPeaks}
#'  (or spectrumDetectPeaks.Centroid or spectrumDetectPeaks.Profile) which does the actual
#'  work
#'
#' @returns a data.frame or NA
#'
#' @examples
#' msR <- msInfo$new()
#' demoFile <- fs::path_package("extdata", "spec2.csv", package = "MS.Analysis")
#' result <- read.table(demoFile, sep = ",", header = TRUE)
#' msR$spectra$add(
#'   dataElements = dataInfo::readDataFrame(dataframes = list(result))
#' )
#' spectrum.findPeaks(data = msR$spectra) |> head(10)
#'
#' @export
spectrum.findPeaks <-function(data, index = 1, id = NA,
                              spectrumDetectPeaksMethod = NA, ...){
  index <- data$getIndex(index = index, id = id)
  if (!identical(index, NA)){
    if (identical(spectrumDetectPeaksMethod, NA)){
      if ("centroided" %in% colnames(data$info)){
        if (data$info$centroided[index]){
          return(spectrumDetectPeaks.Centroid(...)(spectrum = data$data[[index]]))
        } else {
          return(spectrumDetectPeaks.Profile(...)(spectrum = data$data[[index]]))
        }
      } else {
        return(spectrumDetectPeaks(spectrum = data$data[[index]], ...))
      }
    } else {
      return(spectrumDetectPeaksMethod(spectrum = data$data[[index]], ...))
    }
  } else {
    return(NA)
  }
}

#' @title spectrum.tic
#'
#' @description calculates the total ion current (TIC) of a spectrum. It's nothing
#'  more than a sum of the intensities present. The spectrum is present in an msInfo
#'  object. Can be used stand-alone or as part of a \link[dataInfo]{dataFunctions} object.
#'
#' @param data msInfo object containing the spectrum with index ...  or id ...
#' @param index index of the spectrum to be plotted
#' @param id id of the spectrum to be plotted, overrides index
#' @param ... to pass additional arguments on to the function. Redundant in this
#'  specific function.
#'
#' @returns a numeric vector
#'
#' @examples
#' msR <- msInfo$new()
#' demoFile <- fs::path_package("extdata", "spec2.csv", package = "MS.Analysis")
#' result <- read.table(demoFile, sep = ",", header = TRUE)
#' msR$spectra$add(
#'   dataElements = dataInfo::readDataFrame(dataframes = list(result))
#' )
#' spectrum.tic(data = msR$spectra)
#'
#' @export
spectrum.tic <- function(data, index = 1, id = NA, ...){
  index <- data$getIndex(index = index, id = id)
  if (!identical(index, NA)){
    return(sum(data$data[[index]]$intensity, na.rm = TRUE))
  } else {
    return(NA)
  }
}

#' @title spectrum.basepeak.mz
#'
#' @description gets the m/z of the highest intensity position in a spectrum.
#'  The spectrum is present in an msInfo object. More meant for centroided spectra,
#'  but will work with profile data too. Can be used stand-alone or as part
#'  of a \link[dataInfo]{dataFunctions} object.
#'
#' @param data msInfo object containing the spectrum with index ...  or id ...
#' @param index index of the spectrum to be plotted
#' @param id id of the spectrum to be plotted, overrides index
#' @param ... to pass additional arguments on to the function. Redundant in this
#'  specific function.
#'
#' @returns a numeric vector
#'
#' @export
spectrum.basepeak.mz <- function(data, index = 1, id = NA, ...){
  index <- data$getIndex(index = index, id = id)
  if (!identical(index, NA)){
    return(data$data[[index]]$mz[which.max(data$data[[index]]$intensity)])
  } else {
    return(NA)
  }
}

#' @title spectrum.basepeak.intensity
#'
#' @description gets the highest intensity position in a spectrum. The spectrum
#'  is present in an msInfo object. More meant for centroided spectra, but will
#'  work with profile data too. Can be used stand-alone or as part of a
#'  \link[dataInfo]{dataFunctions} object.
#'
#' @param data msInfo object containing the spectrum with index ...  or id ...
#' @param index index of the spectrum to be plotted
#' @param id id of the spectrum to be plotted, overrides index
#' @param ... to pass additional arguments on to the function. Redundant in this
#'  specific function.
#'
#' @returns a numeric vector
#'
#' @export
spectrum.basepeak.intensity <- function(data, index = 1, id = NA, ...){
  index <- data$getIndex(index = index, id = id)
  if (!identical(index, NA)){
    return(max(data$data[[index]]$intensity, na.rm = TRUE))
  } else {
    return(NA)
  }
}

#' @title spectrum.mz.intensity
#'
#' @description gets the intensity at a certain m/z position (or m/z range) in
#'  a spectrum. The spectrum is present in an msInfo object. More meant for
#'  centroided spectra, but will work with profile data too. Can be used
#'  stand-alone or as part of a \link[dataInfo]{dataFunctions} object.
#'
#' @param data msInfo object containing the spectrum with index ...  or id ...
#' @param index index of the spectrum to be plotted
#' @param mz numeric vector: the m/z for which the intensity is to be retrieved
#' @param mzRange one or two element numeric vector. If NA then it will be set
#'  at c(0,0). A single element vector will have it's value used for both parts
#'  (... below and ... above the m/z value).
#' @param intensityFunction function that determines how to deal with a set of
#'  intensities present in mz +/- mzRange. Default is sum; other possibilities
#'  are e.g. mean, max, min, etc.
#' @param id id of the spectrum to be plotted, overrides index
#' @param ... to pass additional arguments on to the function. Redundant in this
#'  specific function.
#'
#' @returns a numeric vector
#'
#' @export
spectrum.mz.intensity <- function(data, index = 1, id = NA,
                                 mz = NA, mzRange = NA, intensityFunction = sum, ...){
  index <- data$getIndex(index = index, id = id)
  if (!identical(index, NA)){
    if (!is.na(mz)){
      if (identical(mzRange, NA)){
        mzRange <- c(0,0)
      } else {
        if (length(mzRange) == 1){
          mzRange <- c(mzRange, mzRange)
        }
      }
      theSum <- intensityFunction(data$data[[index]]$intensity[(data$data[[index]]$mz >= (mz-mzRange[1])) &
                                     (data$data[[index]]$mz <= (mz+mzRange[2]))],
                    na.rm = TRUE)
      return(theSum)
    }
  }
  return(NA)
}

# ---- Chromatogram specific functions ----

#' @title chromatogram.plot
#'
#' @description plots a chromatogram present in an msInfo object. Can be used
#'  stand-alone or as part of a \link[dataInfo]{dataFunctions} object.
#'
#' @param data msInfo object containing the chromatogram with index ...  or id ...
#' @param index index of the chromatogram to be plotted
#' @param id id of the chromatogram to be plotted, overrides index
#' @param ... to pass additional arguments on to \link[MS.Analysis]{plotChromatogram}
#'  which does the actual plotting.
#'
#' @returns a ggplot object or NA
#'
#' @examples
#' msR <- msInfo$new()
#' demoFile <- fs::path_package("extdata", "Data0001.CSV", package = "MS.Analysis")
#' result <- readLines(demoFile, n = 9092)
#' result <- read.table(text = result, sep = ",", header = FALSE)[, 2:3]
#' colnames(result) <- c("rt", "intensity")
#' msR$chromatograms$add(
#'   dataElements = dataInfo::readDataFrame(dataframes = list(result))
#' )
#' chromatogram.plot(data = msR$chromatograms, id = 1)
#'
#' @export
chromatogram.plot <- function(data, index = 1, id = NA, ...){
  index <- data$getIndex(index = index, id = id)
  if (!identical(index, NA)){
    plotChromatogram(data$data[[index]], ...)
  } else {
    return(NA)
  }
}

#' @title chromatogram.plot.overlay
#'
#' @description plots an overlay of chromatograms present in an msInfo object.
#'  Can be used stand-alone or as part of a \link[dataInfo]{dataFunctions} object.
#'
#' @param data msInfo object containing the chromatograms with index ...  or id ...
#' @param index index of the chromatograms to be plotted
#' @param id id of the chromatograms to be plotted, overrides index
#' @param ... to pass additional arguments on to \link[MS.Analysis]{plotChromatogramOverlay}
#'  which does the actual plotting
#'
#' @returns a ggplot object or NA
#'
#' @examples
#' msR <- msInfo$new()
#' demoFile <- fs::path_package("extdata", "Data0001.CSV", package = "MS.Analysis")
#' result <- readLines(demoFile, n = 9092)
#' result <- read.table(text = result, sep = ",", header = FALSE)[, 2:3]
#' colnames(result) <- c("rt", "intensity")
#' msR$chromatograms$add(
#'   dataElements = dataInfo::readDataFrame(dataframes = list(result))
#' )
#' demoFile <- fs::path_package("extdata", "chrom1.csv", package = "MS.Analysis")
#' result <- read.table(demoFile, sep = ",", header = TRUE)
#' msR$chromatograms$add(
#'   dataElements = dataInfo::readDataFrame(dataframes = list(result))
#' )
#' chromatogram.plot.overlay(
#'   data = msR$chromatograms,
#'   id = 1:2,
#'   chromatogramColors = c("blue", "red")
#' )
#' @export
chromatogram.plot.overlay <- function(data, index = 1, id = NA, ...){
  index <- data$getIndex(index = index, id = id)
  if (!identical(index, NA)){
    plotChromatogramOverlay(chromatogramList = data$data[index], ...)
  } else {
    return(NA)
  }
}

#' @title chromatogram.plot.sum
#'
#' @description sums the selected chromatograms and plots the area, while one
#'  chromatogram is used as a reference of sorts. Can be used stand-alone or as
#'  part of a \link[dataInfo]{dataFunctions} object.
#'
#' @param data msInfo object containing the chromatograms with index ...  or id ...
#' @param index index of the chromatograms to be used
#' @param id id of the chromatograms to be used, overrides index
#' @param ... to pass additional arguments on to \link[MS.Analysis]{plotChromatogramSum}
#'  which does the actual plotting
#'
#' @returns a ggplot object or NA
#'
#' @export
chromatogram.plot.sum <- function(data, index = 1, id = NA, ...){
  index <- data$getIndex(index = index, id = id)
  if (!identical(index, NA)){
    plotChromatogramSum(chromatogramList = data$data[index], ...)
  } else {
    return(NA)
  }
}

#' @title chromatogram.findPeaks
#'
#' @description performs peak detection on chromatogram present in an msInfo object.
#'  Can be used stand-alone or as part of a \link[dataInfo]{dataFunctions} object.
#'
#' @param data msInfo object containing the chromatogram with index ...  or id ...
#' @param index index of the chromatogram on which to do peak detection
#' @param id id of the chromatogram to perform peak detection on, overrides index
#' @param chromatogramDetectPeaksMethod function to use for peak detection. If NA (default)
#'  then the function will use \link[MS.Analysis]{chromatogramFindPeaks}
#' @param ... to pass additional arguments on to \link[MS.Analysis]{chromatogramFindPeaks}
#'  (or other function(s)) which does the actual peak detection
#'
#' @returns a data.frame or NA
#'
#' @examples
#' msR <- msInfo$new()
#' demoFile <- fs::path_package("extdata", "chrom1.csv", package = "MS.Analysis")
#' result <- read.table(demoFile, sep = ",", header = TRUE)
#' msR$chromatograms$add(
#'   dataElements = dataInfo::readDataFrame(dataframes = list(result))
#' )
#' chromatogram.findPeaks(data = msR$chromatograms, id = 1, signalNoiseRatio = 5)
#'
#' @export
chromatogram.findPeaks <- function(data, index = 1, id = NA,
                                  chromatogramDetectPeaksMethod = NA, ...){
  index <- data$getIndex(index = index, id = id)
  if (!identical(index, NA)){
    if (identical(chromatogramDetectPeaksMethod, NA)){
      return(chromatogramFindPeaks(trace = data$data[[index]], ...))
    } else {
      return(chromatogramDetectPeaksMethod(trace = data$data[[index]], ...))
    }
  } else {
    return(NA)
  }
}

#' @title chromatogram.smooth
#'
#' @description smooths a chromatogram present in an msInfo object.
#'  Can be used stand-alone or as part of a \link[dataInfo]{dataFunctions} object.
#'
#' @param data msInfo object containing the chromatogram with index ...  or id ...
#' @param index index of the chromatogram on which to do smoothing
#' @param id id of the chromatogram to perform smoothing on, overrides index
#' @param smoothFunction function to use for smoothing. Default is \link[MS.Analysis]{smoothFunction.loess}
#' @param returnInfo if the data is to be returned as a function that generates
#'  a dataElement object (which can be used to put in an dataInfo object) via
#'  the 'readData' function)
#' @param ... to pass additional arguments on to smoothing function
#'
#' @returns a data.frame of the smoothed data or a dataElement object
#'
#' @export
chromatogram.smooth <- function(data, index = 1, id = NA,
                               smoothFunction = smoothFunction.loess(x = "rt"),
                               returnInfo = FALSE, ...){
  index <- data$getIndex(index = index, id = id)
  if (!identical(index, NA)){
    if (identical(smoothFunction, NA)){
      result <- data$data[[index]]
    } else {
      result <- smoothData(data$data[[index]],
                           smoothFunction = smoothFunction,
                           ...)
    }
    if (returnInfo){
      return(readData(
        dataframe = result,
        info = append(as.list(data$info[index,]), list(processing = "smoothing"))
      ))
    }
    return(result)
  } else {
    return(NA)
  }
}

#' @title chromatogram.intensity.max
#'
#' @description finds the maximum intensity in a chromatogram present in an msInfo object.
#'  Can be used stand-alone or as part of a \link[dataInfo]{dataFunctions} object.
#'
#' @param data msInfo object containing the chromatogram with index ...  or id ...
#' @param index index of the chromatogram
#' @param id id of the chromatogram, overrides index
#' @param ... to pass additional arguments on to the function. Redundant in this
#'  specific function.
#'
#' @returns numeric vector
#'
#' @examples
#' msR <- msInfo$new()
#' demoFile <- fs::path_package("extdata", "chrom1.csv", package = "MS.Analysis")
#' result <- read.table(demoFile, sep = ",", header = TRUE)
#' msR$chromatograms$add(
#'   dataElements = dataInfo::readDataFrame(dataframes = list(result))
#' )
#' chromatogram.intensity.max(
#'   data = msR$chromatograms,
#'   id = 1,
#'   signalNoiseRatio = 5
#' )
#'
#' @export
chromatogram.intensity.max <- function(data, index = 1, id = NA, ...){
  index <- data$getIndex(index = index, id = id)
  if (!identical(index, NA)){
    return(max(data$data[[index]]$intensity, na.rm = TRUE))
  } else {
    return(NA)
  }
}

# ---- msInfo ----

#' @title msInfo
#'
#' @description
#'  R6 Class dealing with mass spectrometry data in an organised manner. This class
#'   is a descendant of datalist. In principle it holds 3 dataInfo objects which
#'   represent collections of chromatograms, spectra & files respectively
#'
#' @note it can be expanded to contain more items obviously, but this is best done
#'  via a descendant
#'
#' @examples
#' # initailize msInfo object
#' msR <- msInfo$new()
#' # Chromatograms
#' demoFile <- fs::path_package("extdata", "Data0001.CSV", package = "MS.Analysis")
#' result <- readLines(demoFile, n = 9092)
#' result <- read.table(text = result, sep = ",", header = FALSE)[, 2:3]
#' colnames(result) <- c("rt", "intensity")
#' msR$chromatograms$add(
#'   dataElements = dataInfo::readDataFrame(dataframes = list(result))
#' )
#' demoFile <- fs::path_package("extdata", "chrom1.csv", package = "MS.Analysis")
#' result <- read.table(demoFile, sep = ",", header = TRUE)
#' msR$chromatograms$add(
#'   dataElements = dataInfo::readDataFrame(dataframes = list(result))
#' )
#' msR
#' msR$chromatograms
#' msR$chromatograms$info
#' msR$chromatograms$item(1) |> head()
#' msR$chromatograms$raw[[1]]$data |> head()
#' msR$chromatograms$raw[[1]]$info
#' msR$chromatograms$raw[[1]]
#' msR$do(whichElement = "chromatograms", whichFunction = "plot", id = 1)
#' msR$do(whichElement = "chromatograms", whichFunction = "plot", id = 2)
#' msR$do(
#'   "chromatograms",
#'   "plot.overlay",
#'   id = 1:2,
#'   chromatogramColors = c("blue", "red")
#' )
#' msR$do("chromatograms", "intensity.max")
#' msR$do("chromatograms", "intensity.max", id = 2)
#' # Spectra
#' demoFile <- fs::path_package("extdata", "spec1.csv", package = "MS.Analysis")
#' result <- read.table(demoFile, sep = ",", header = TRUE)
#' msR$spectra$add(
#'   dataElements = dataInfo::readDataFrame(dataframes = list(result))
#' )
#' demoFile <- fs::path_package("extdata", "spec2.csv", package = "MS.Analysis")
#' result <- read.table(demoFile, sep = ",", header = TRUE)
#' msR$spectra$add(
#'   dataElements = dataInfo::readDataFrame(dataframes = list(result))
#' )
#' msR
#' msR$spectra$info
#' msR$spectra$info$centroided <- c(TRUE, FALSE)
#' msR$spectra$info
#' msR$do("spectra", "plot")
#' msR$do("spectra", "plot", id = 2)
#' msR$do("spectra", "findPeaks")
#' msR$do("spectra", "findPeaks", id = 2)
#' purrr::map_dbl(msR$spectra$info$id, ~ msR$do("spectra", "tic", id = .x))
#'
#' @export
msInfo <- R6::R6Class(
  "msInfo",
  inherit = dataInfo::dataList,
  private = list(
  ),
  public = list(
    #' @description create a new info object
    #'
    #' @param name character vector. Name of this object (optional)
    #' @param dataObjects by default a set of dataInfo objects named 'files',
    #'  'chromatograms' & 'spectra'. Note: the files object is not defined as
    #'  to what it will contain. This can be peak lists, information on the
    #'  spectra in the raw (Thermo Scientific) files, etc etc. It's not obligatory
    #'  to use the default dataInfo objects. However, if the default dataInfo objects
    #'  are not present, then adding the regular dataFunctions will fail. It is
    #'  of course possible to have non-standard (extra) dataInfo objects.
    #' @param initializeFunctions if TRUE (default) then a set of 'standard' dataFunctions
    #'  are added to the dataInfo objects. When FALSE, then it doesn't happen
    #' @param clone logical value, defines whether the dataElements to be added
    #'  are to be 'cloned'. Default is TRUE to prevent odd situations, option
    #'  may be removed in the future
    #'
    #' @returns a new msInfo object
    #' @export
    initialize = function(name = "", initializeFunctions = TRUE, clone = TRUE){
      super$initialize(name = name,
                       dataObjects = list(dataInfo::dataInfo$new(name = "files"),
                                          dataInfo::dataInfo$new(name = "chromatograms"),
                                          dataInfo::dataInfo$new(name = "spectra")),
                       clone = clone)
      if (initializeFunctions){
        self$files$dos <- dataInfo::dataFunctions$new()
        self$spectra$dos <- dataInfo::dataFunctions$new(functions = list(spectrum.plot,
                                                          spectrum.plot.overlay,
                                                          spectrum.plot.mirror,
                                                          spectrum.findPeaks,
                                                          spectrum.tic,
                                                          spectrum.basepeak.mz,
                                                          spectrum.basepeak.intensity,
                                                          spectrum.mz.intensity),
                                              names = c("plot",
                                                       "plot.overlay",
                                                       "plot.mirror",
                                                       "findPeaks",
                                                       "tic",
                                                       "basepeak.mz",
                                                       "basepeak.intensity",
                                                       "mz.intensity"))
        self$chromatograms$dos <- dataInfo::dataFunctions$new(functions = list(chromatogram.plot,
                                                                chromatogram.plot.overlay,
                                                                chromatogram.plot.sum,
                                                                chromatogram.findPeaks,
                                                                chromatogram.smooth,
                                                                chromatogram.intensity.max),
                                                    names = c("plot",
                                                             "plot.overlay",
                                                             "plot.sum",
                                                             "findPeaks",
                                                             "smooth",
                                                             "intensity.max"))
      }
      invisible(self)
    }
  ),
  active = list(
    #' @field chromatograms provides access to the chromatograms dataInfo object
    chromatograms = function(value){
      if (missing(value)){
        return(private$data_[["chromatograms"]])
      } else {
        private$data_[["chromatograms"]] <- value
      }
    },
    #' @field spectra provides access to the dataInfo object
    spectra = function(value){
      if (missing(value)){
        return(private$data_[["spectra"]])
      } else {
        private$data_[["spectra"]] <- value
      }
    },
    #' @field files provides access to the files dataInfo object
    files = function(value){
      if (missing(value)){
        return(private$data_[["files"]])
      } else {
        private$data_[["files"]] <- value
      }
    }
  )
)
