# to prevent warnings etc with RMD check
utils::globalVariables(c("peak_intensity", "peak_rt", "perc",
                         "readData", "rt", "iterate", "left", "right"))

# ---- Smoothing ----

#' @title smoothFunction.loess
#'
#' @description function factory for generating loess smoothing functions to be
#'  applied to data in a data.frame
#'
#' @param x character vector that defines defines one of two column names to be
#'  used in the \link[stats]{loess} function
#' @param y character vector that defines defines the second of two column names
#'  to be used in the \link[stats]{loess} function
#' @param formula the default is y~x, but can be transformed in anything allowed
#'  by the \link[stats]{loess} function
#' @param span alpha parameter than controls the amount of smoothing (see
#'  \link[stats]{loess} for more info). Note: cannot be too low
#'
#' @note This is more of an example function factory. There is a lot possible
#'
#' @return a function that will smoothen the data.frame provided by the argument
#' data. Additional parameters for the smoothing can be passed on by ...
#' argument
#'
#' @examples
#' set.seed(1234)
#' df <- data.frame(
#'   x = seq(0, 10, by = 0.1)
#' )
#' df$y <- 1+5*sin(df$x)+rnorm(101)
#' plot(df, type = "l")
#' plot(smoothFunction.loess(x = "x", y = "y")(df), type = "l")
#' plot(smoothFunction.loess(x = "x", y = "y", span = 0.5)(df), type = "l")
#' plot(smoothFunction.loess(x = "x", y = "y")(df, degree = 1), type = "l")
#'
#' @export
smoothFunction.loess <- function(
  x = "mz",
  y = "intensity",
  formula = paste(c(y, x), collapse = "~"),
  span = 0.1
) {
  force(x)
  force(y)
  force(formula)
  force(span)
  function(data, ...) {
    tempObj <- stats::loess(
      data = as.data.frame(data),
      formula = formula,
      span = span,
      ...
    )
    tempObj <- data.frame(one = unname(unlist(data[, x])), two = tempObj$fitted)
    colnames(tempObj) <- c(x, y)
    return(tempObj)
  }
}

#' @title smoothData
#'
#' @description Actual smoothing function: uses the smoothFunction parameter to
#'  smoothen the data
#'
#' @note by default the smoothing function moves along the data using 'interval'
#'  data points and then shifting 'step' data points for the next smoothing. It
#'  more or less is the idea of a moving average
#'
#' @param data a data.frame with data to be smoothed
#' @param smoothFunction specifies the smoothing function to be used. Uses the
#'  standard \link[MS.Analysis]{smoothFunction.loess} function by default.
#'  Important is to pass the correct column names onto the smoothing function
#' @param interval specifies how many data points are to be used per smoothing
#'  action; default = 100, if NA then will be set to the number of rows of the
#'  data
#' @param step step with which to shift the window of datapoints to be used
#'  after a smoothing action. Default = 10, if interval is NA, then the steps
#'  are set to 1
#' @param keepMinimumZero logical vector, default: TRUE. This prevents
#'  smoothed data points from becoming less than zero.
#' @param verbose logical vector, default FALSE. If TRUE, then the function
#'  shows a text progress bar
#' @param returnInfo if the data is to be returned as a function that generates
#'  a dataElement object (which can be used to put in an dataInfo object) via
#'  the 'readData' function
#' @param xtraInfo extra info to be added to the info part in the form of a
#'  named list
#'
#' @returns a data.frame of the smoothed data or a dataElement object
#'
#' @examples
#' set.seed(1234)
#' df <- data.frame(
#'   x = seq(0, 25, by = 0.1)
#' )
#' df$y <- 1 + 5 * sin(df$x) + rnorm(251)
#' plot(df, type = "l")
#' plot(smoothFunction.loess(x = "x", y = "y")(df), type = "l")
#' plot(
#'   smoothData(
#'     data = df,
#'     smoothFunction = smoothFunction.loess(x = "x", y = "y")
#'   ),
#'   type = "l"
#' )
#' plot(
#'   smoothData(
#'     data = df,
#'     smoothFunction = smoothFunction.loess(x = "x", y = "y", span = 0.2),
#'     interval = 40,
#'     step = 5
#'   ),
#'   type = "l"
#' )
#' plot(
#'   smoothData(
#'     data = df,
#'     smoothFunction = smoothFunction.loess(x = "x", y = "y", span = 0.2),
#'     interval = 40,
#'     step = 5,
#'     keepMinimumZero = FALSE
#'   ),
#'   type = "l"
#' )
#' plot(
#'   smoothData(
#'     data = df,
#'     smoothFunction = smoothFunction.loess(x = "x", y = "y", span = 0.2),
#'     interval = 40,
#'     step = 20
#'   ),
#'   type = "l"
#' )
#' plot(
#'   smoothData(
#'     data = df,
#'     smoothFunction = smoothFunction.loess(x = "x", y = "y", span = 0.2),
#'     interval = 100,
#'     step = 50,
#'     keepMinimumZero = FALSE
#'   ),
#'   type = "l"
#' )
#' result <- smoothData(
#'   data = df,
#'   smoothFunction = smoothFunction.loess(x = "x", y = "y", span = 0.2),
#'   interval = 100,
#'   step = 50,
#'   keepMinimumZero = FALSE,
#'   returnInfo = TRUE
#' )()
#' result
#' result$data |> head()
#' plot(result$data, type = "l")
#'
#' @export
smoothData <- function(
  data,
  smoothFunction = smoothFunction.loess(),
  interval = 100,
  step = 10,
  keepMinimumZero = TRUE,
  verbose = FALSE,
  returnInfo = FALSE,
  xtraInfo = list()
) {
  if (identical(interval, NA)) {
    interval <- nrow(data)
    step <- 1
  }
  if (nrow(data) > interval) {
    nSteps <- ((nrow(data) - interval) %/% step) +
      as.integer((nrow((data) - interval) %% step) != 0) +
      1
  } else {
    nSteps = 1
  }
  if (verbose) {
    progBar <- utils::txtProgressBar(min = 1, max = nSteps, style = 3)
  }
  for (counter in 1:nSteps) {
    firstE <- ((counter - 1) * step) + 1
    lastE <- firstE + interval - 1
    lastE <- ifelse(lastE > nrow(data), nrow(data), lastE)
    newData <- smoothFunction(data[firstE:lastE, ])
    if (keepMinimumZero) {
      newData[(newData[, 2] < 0) | (is.na(newData[, 2])), 2] <- 0
    }
    data[firstE:lastE, ] <- newData
    if (verbose) {
      utils::setTxtProgressBar(progBar, counter)
    }
  }
  if (verbose) {
    cat("\n")
  }
  if (!returnInfo) {
    return(data)
  } else {
    return(dataInfo::readData(
      dataframe = data,
      columns = NA,
      columnNames = NA,
      rowNames = NULL,
      info = append(list(edit = "smoothing"), xtraInfo)
    ))
  }
}

# ---- chromatograms - peakdetection ----

#' @title chromatogramFindPeaks
#'
#' @description peak detection for chromatograms
#'
#' @note originally meant for HPLC-MS chromatograms, but should also work with
#'  other ypes of chromatograms
#'
#' @note internally the function \link[MALDIquant]{detectPeaks} which does a noise estimation
#'  via \link[stats]{supsmu} ("SuperSmoother") or \link[stats]{mad} ("MAD") and
#'  attemps to find local maxima via C-code in the package itself through the
#'   'y'-values and the 'halfWindowSize' ('span' argument in this function)
#'
#' @param trace data.frame with the data. Please note that the function assumes
#' the first column of trace to be the x-values and the second column if trace to be the y-values.
#' @param span halfWindowSize parameter of the C_localMaxima function in detectPeaks.
#'  Essentially seems to be the number of data points. If too narrow the function will
#'  fail
#' @param smoothing used in noise estimation, two options: "SuperSmoother" or "MAD"
#' @param signalNoiseRatio required signal to noise ratio for a peak to be annotated
#'  as such
#' @param rtLimits two element numeric vector which is the retention time window
#'  between which the returned peaks should fall (default NA, not used). Note that
#'  this happens AFTER peak detection
#' @param limitNr return the limitNr highest or lowest intensity peaks. Default is NA,
#'  not used
#' @param limitPercentage  Default is NA, not used. If a numeric value than it will
#'  cut out any peaks from the peak list that are below limitPercentage of the highest
#'  intensity peak (set at 100%)
#' @param highest default is TRUE. if the limitNr argument is used then this argument determines
#'  if it's the highest or the lowest intensity peaks that will be returned
#' @param returnInfo if the data is to be returned as a function that generates a list of info
#'  & data (which  can be used to put in an info object) via the 'readData' function
#' @param xtraInfo extra info to be added to the info part in the form of a named list
#'
#' @returns a data.frame of the smoothed data or list of info & data
#'
#' @note the returned data.frame contains three empty columns: left, right & area. These
#'  exist for the findPeakArea functions
#'
#' @examples
#' demoFile <- fs::path_package("extdata", "Data0001.CSV", package = "MS.Analysis")
#' result <- readLines(demoFile, n = 9092)
#' result <- read.table(text = result, sep = ",", header = FALSE)[, 2:3]
#' colnames(result) <- c("rt", "intensity")
#' result |> head()
#' plot(result, type = "l")
#' peaks <- chromatogramFindPeaks(
#'   trace = result,
#'   smoothing = NA,
#'   span = 1,
#'   signalNoiseRatio = 5
#' )
#' peaks |> head()
#' text(
#'   peaks$peak_rt,
#'   peaks$peak_intensity,
#'   formatMinimumDigits(1)(peaks$peak_rt),
#'   cex = 0.65,
#'   pos = 3,
#'   col = "red"
#' )
#' # generate dataElement in stead of data.frame with peak info
#' peaks <- chromatogramFindPeaks(
#'   trace = result,
#'   smoothing = NA,
#'   span = 1,
#'   signalNoiseRatio = 5,
#'   returnInfo = TRUE
#' )
#' peaks <- peaks()
#' peaks
#' class(peaks)
#' peaks$info
#' peaks$data |> head()
#' peaks <- chromatogramFindPeaks(
#'   trace = result,
#'   smoothing = NA,
#'   span = 1,
#'   signalNoiseRatio = 10
#' )
#' plot(result, type = "l")
#' text(
#'   peaks$peak_rt,
#'   peaks$peak_intensity,
#'   formatMinimumDigits(1)(peaks$peak_rt),
#'   cex = 0.65,
#'   pos = 3,
#'   col = "red"
#' )
#' peaks <- chromatogramFindPeaks(
#'   trace = result,
#'   smoothing = NA,
#'   span = 1,
#'   signalNoiseRatio = 5,
#'   limitNr = 10
#' )
#' plot(result, type = "l", xlim = c(8, 15))
#' text(
#'   peaks$peak_rt,
#'   peaks$peak_intensity,
#'   formatMinimumDigits(1)(peaks$peak_rt),
#'   cex = 0.65,
#'   pos = 3,
#'   col = "red"
#' )
#' peaks <- chromatogramFindPeaks(
#'   trace = result,
#'   smoothing = NA,
#'   span = 1,
#'   signalNoiseRatio = 1,
#'   rtLimits = c(15, 20)
#' )
#' plot(result, type = "l", xlim = c(15, 20))
#' text(
#'   peaks$peak_rt,
#'   peaks$peak_intensity,
#'   formatMinimumDigits(1)(peaks$peak_rt),
#'   cex = 0.65,
#'   pos = 3,
#'   col = "red"
#' )
#' peaks <- chromatogramFindPeaks(
#'   trace = result,
#'   smoothing = NA,
#'   span = 2,
#'   signalNoiseRatio = 2.5,
#'   rtLimits = c(15, 20)
#' )
#' plot(result, type = "l", xlim = c(15, 20))
#' text(
#'   peaks$peak_rt,
#'   peaks$peak_intensity,
#'   formatMinimumDigits(1)(peaks$peak_rt),
#'   cex = 0.65,
#'   pos = 3,
#'   col = "red"
#' )
#' @export
chromatogramFindPeaks <- function(
  trace,
  span = 5,
  smoothing = "SuperSmoother",
  signalNoiseRatio = 15,
  rtLimits = NA,
  limitNr = NA,
  limitPercentage = NA,
  highest = TRUE,
  returnInfo = FALSE,
  xtraInfo = list()
) {
  if (!identical(smoothing, NA)) {
    rts <- MALDIquant::detectPeaks(
      MALDIquant::createMassSpectrum(
        mass = trace[, 1],
        intensity = trace[, 2]
      ),
      halfWindowSize = span,
      method = smoothing,
      SNR = signalNoiseRatio
    )
  } else {
    rts <- MALDIquant::detectPeaks(
      MALDIquant::createMassSpectrum(
        mass = trace[, 1],
        intensity = trace[, 2]
      ),
      halfWindowSize = span,
      SNR = signalNoiseRatio
    )
  }
  tempdf <- data.frame(peak_rt = rts@mass, peak_intensity = rts@intensity)
  if (!identical(rtLimits, NA)) {
    tempdf <- tempdf %>% filter(peak_rt >= rtLimits[1], peak_rt <= rtLimits[2])
  }
  if (!identical(limitPercentage, NA)) {
    tempdf <- tempdf %>%
      dplyr::mutate(perc = (peak_intensity / max(peak_intensity)) * 100) %>%
      dplyr::filter(perc >= limitPercentage) %>%
      dplyr::select(-perc)
  }
  if (!identical(limitNr, NA)) {
    if (highest) {
      tempdf <- tempdf %>%
        arrange(desc(peak_intensity)) %>%
        slice(1:limitNr)
    } else {
      tempdf <- tempdf %>%
        arrange(peak_intensity) %>%
        slice(1:limitNr)
    }
  }
  tempdf$left <- rep(as.numeric(NA), nrow(tempdf))
  tempdf$right <- rep(as.numeric(NA), nrow(tempdf))
  tempdf$area <- rep(as.numeric(NA), nrow(tempdf))
  if (!returnInfo) {
    return(tempdf)
  } else {
    return(dataInfo::readData(
      dataframe = tempdf,
      columns = NA,
      columnNames = NA,
      rowNames = NULL,
      info = append(
        list(
          span = as.character(span),
          smoothing = smoothing,
          signalNoiseRatio = signalNoiseRatio,
          rtLimits = ifelse(
            identical(rtLimits, NA),
            NA,
            paste(rtLimits, collapse = "-")
          ),
          limitNr = limitNr,
          highest = highest,
          source = "chromatogram"
        ),
        xtraInfo
      )
    ))
  }
}

#' @title chromatogramFindPeaks.General
#'
#' @description function factory for peak detection for chromatograms, creates a
#'  function to do peak detection in a chromatogram.
#'
#' @note the generated function takes two arguments: 'trace' (data.frame) and
#'  'xtraInfo' (list). See \link[MS.Analysis]{chromatogramFindPeaks} for more info
#'
#' @param span halfWindowSize parameter of the C_localMaxima function in detectPeaks.
#'  Essentially seems to be the number of data points. If too narrow the function will
#'  fail
#' @param smoothing used in noise estimation, two options: "SuperSmoother" or "MAD"
#' @param signalNoiseRatio required signal to noise ratio for a peak to be annotated
#'  as such
#' @param rtLimits two element numeric vector which is the retention time window
#'  between which the returned peaks should fall (default NA, not used). Note that
#'  this happens AFTER peak detection
#' @param limitNr return the limitNr highest or lowest intensity peaks. Default is NA,
#'  not used
#' @param limitPercentage  Default is NA, not used. If a numeric value than it will
#'  cut out any peaks from the peak list that are below limitPercentage of the highest
#'  intensity peak (set at 100%)
#' @param highest default is TRUE. if the limitNr argument is used then this argument determines
#'  if it's the highest or the lowest intensity peaks that will be returned
#' @param returnInfo if the data is to be returned as a function that generates a list of info
#'  & data (which  can be used to put in an info object) via the 'readData' function
#'
#' @return a function that generates a function to create peak lists (either in
#'  data.frame or dataElement format
#'
#' @examples
#' demoFile <- fs::path_package("extdata", "Data0001.CSV", package = "MS.Analysis")
#' result <- readLines(demoFile, n = 9092)
#' result <- read.table(text = result, sep = ",", header = FALSE)[, 2:3]
#' colnames(result) <- c("rt", "intensity")
#' result |> head()
#' plot(result, type = "l")
#' peaks <- chromatogramFindPeaks.General(
#'   smoothing = NA,
#'   span = 1,
#'   signalNoiseRatio = 5
#' )
#' peaks(trace = result) |> head()
#' peaks <- peaks(trace = result)
#' text(
#'   peaks$peak_rt,
#'   peaks$peak_intensity,
#'   formatMinimumDigits(1)(peaks$peak_rt),
#'   cex = 0.65,
#'   pos = 3,
#'   col = "red"
#' )
#'
#' @export
chromatogramFindPeaks.General <- function(
  span = 5,
  smoothing = "SuperSmoother",
  signalNoiseRatio = 15,
  rtLimits = NA,
  limitNr = NA,
  limitPercentage = NA,
  highest = TRUE,
  returnInfo = FALSE
) {
  force(span)
  force(smoothing)
  force(signalNoiseRatio)
  force(rtLimits)
  force(limitNr)
  force(limitPercentage)
  force(highest)
  force(returnInfo)
  function(trace, xtraInfo = list()) {
    return(chromatogramFindPeaks(
      trace = trace,
      span = span,
      smoothing = smoothing,
      signalNoiseRatio = signalNoiseRatio,
      rtLimits = rtLimits,
      limitNr = limitNr,
      limitPercentage = limitPercentage,
      highest = highest,
      returnInfo = returnInfo,
      xtraInfo = xtraInfo
    ))
  }
}

#' @title chromatogramPeaks.FixedEdges
#'
#' @description set fixed left, right limits in the peak table. Rather simple
#'  convenience function. Can be used for simple peak area determination where
#'  chromatographic peaks are relatively far away from each other
#'
#' @note The left/right values are needed as limits to calculate the peak area
#'  (in another function). The method used in this function is obviously quite
#'  crude and will not be appropiate for a lot of (more complex) peaks/chromatograms.
#'  For those we need more advanced ways of determining the beginning and end of
#'  a chromatographic peak
#'
#' @param peaks peak table data.frame as coming from chromatogramFindPeaks function.
#'  The data.frame needs to have at least the peak_rt column (which shows the retention
#'  time of the peak)
#' @param edges 2-element numeric vector specifying the lower limit and the upper limit
#'  for the peak area calculation (other function). They're relative numbers:
#'  peak_rt+edges til peak_rt$edges
#'
#' @return the peak table data.frame with left/right columns
#'
#' @examples
#' demoFile <- fs::path_package("extdata", "Data0001.CSV", package = "MS.Analysis")
#' result <- readLines(demoFile, n = 9092)
#' result <- read.table(text = result, sep = ",", header = FALSE)[, 2:3]
#' colnames(result) <- c("rt", "intensity")
#' result |> head()
#' plot(result, type = "l")
#' peaks <- data.frame(
#'   peak_rt = c(11.074, 12.133, 17.769, 19.515),
#'   peak_intensity = c(145412448, 151731744, 99109632, 70294472),
#'   left = NA,
#'   right = NA,
#'   area = NA
#' )
#' peaks
#' peaks <- peaks |> chromatogramPeaks.FixedEdges()
#' peaks
#' plot(result, type = "l", xlim = c(10, 13), ylim = c(0, 2E8))
#' text(
#'   peaks$peak_rt[1:2],
#'   peaks$peak_intensity[1:2],
#'   formatMinimumDigits(1)(peaks$peak_rt[1:2]),
#'   cex = 0.65,
#'   pos = 3,
#'   col = "red"
#' )
#' abline(
#'   v = peaks$peak_rt[1:2],
#'   col = scales::alpha(rgb(1, 0, 0), 0.75),
#'   lty = "dotted"
#' )
#' abline(
#'   v = peaks$left[1:2],
#'   col = scales::alpha(rgb(0, 0, 1), 0.75),
#'   lty = "dashed"
#' )
#' abline(
#'   v = peaks$right[1:2],
#'   col = scales::alpha(rgb(0, 0, 1), 0.75),
#'   lty = "dashed"
#' )
#'
#' @export
chromatogramPeaks.FixedEdges <- function(peaks, edges = c(0.075, 0.15)) {
  return(
    peaks %>%
      mutate(left = peaks$peak_rt - edges[1], right = peaks$peak_rt + edges[2])
  )
}

#' @title chromatogramPeaks.FindAreas
#'
#' @description Calculates the peak areas on the basis of peak_rt, the left &
#'  right (rt) cutoff and the trace (chromatogram) itself
#'
#' @note internally the function \link[DescTools]{AUC} is used. Some of the text
#'  in the help of the DescTools function is used here
#'
#' @param peaks peak table with at least peak_rt, left & right columns for all peaks
#'  to be integrated
#' @param trace data.frame (rt, intensity) of the chromatogram which has the peaks
#' @param auc specifies the method name for the interpolation. Options are: "trapezoid",
#' "step", "spline" & "linear"
#' @param absoluteArea A logical value that determines if negative areas should
#'  be added to the total area under the curve.
#' @param subDivisions an integer telling how many subdivisions should be used for
#'  integrate (for non-linear approximations). Ignored if method is not spline.
#' @param na.rm 	logical, indicating whether NA values should be stripped before the
#'  computation proceeds. In this case only the complete.cases of x and y will be
#'  used. Default is FALSE
#'
#' @return a peak table data.frame with an area column
#'
#' @examples
#' demoFile <- fs::path_package("extdata", "Data0001.CSV", package = "MS.Analysis")
#' result <- readLines(demoFile, n = 9092)
#' result <- read.table(text = result, sep = ",", header = FALSE)[, 2:3]
#' colnames(result) <- c("rt", "intensity")
#' result |> head()
#' plot(result, type = "l")
#' peaks <- data.frame(
#'   peak_rt = c(11.074, 12.133, 17.769, 19.515),
#'   peak_intensity = c(145412448, 151731744, 99109632, 70294472),
#'   left = NA,
#'   right = NA,
#'   area = NA
#' )
#' peaks <- peaks |> chromatogramPeaks.FixedEdges()
#' peaks
#' plot(result, type = "l", xlim = c(10, 21), ylim = c(0, 2E8))
#' text(
#'   peaks$peak_rt,
#'   peaks$peak_intensity,
#'   formatMinimumDigits(1)(peaks$peak_rt),
#'   cex = 0.65,
#'   pos = 3,
#'   col = "red"
#' )
#' abline(
#'   v = peaks$peak_rt,
#'   col = scales::alpha(rgb(1, 0, 0), 0.75),
#'   lty = "dotted"
#' )
#' abline(
#'   v = peaks$left,
#'   col = scales::alpha(rgb(0, 0, 1), 0.75),
#'   lty = "dashed"
#' )
#' abline(
#'   v = peaks$right,
#'   col = scales::alpha(rgb(0, 0, 1), 0.75),
#'   lty = "dashed"
#' )
#' chromatogramPeaks.FindAreas(peaks = peaks,
#'                             trace = result)
#'
#' @export
chromatogramPeaks.FindAreas <- function(
  peaks,
  trace,
  auc = "trapezoid",
  absoluteArea = FALSE,
  subDivisions = 100,
  na.rm = FALSE
) {
  peaks$area <- NA
  for (counter in 1:(nrow(peaks))) {
    if (!is.na(peaks$left[counter]) & !is.na(peaks$right[counter])) {
      traceS <- trace %>%
        dplyr::filter(rt >= peaks$left[counter], rt <= peaks$right[counter])
      peaks$area[counter] <- DescTools::AUC(
        x = traceS$rt,
        y = traceS$intensity,
        method = auc,
        absolutearea = absoluteArea,
        subdivisions = subDivisions,
        na.rm = na.rm
      )
    } else {
      peaks$area[counter] <- NA
    }
  }
  return(peaks)
}

#' @title chromatogramFindAreas.Iterative
#'
#' @description Calculates the peak areas on the basis of peak_rt, the left &
#'  right (rt) cutoff and the trace (chromatogram) itself, but also attempts to
#'  change the retention time limits in a structured manner. This is done to
#'  adjust for unusual peak shapes (tailing, fronting, etc)
#'
#' @note The left limit of the peak is adjusted first, after that the right limit
#'
#' @param peaks peak table with at least peak_rt for all peaks
#' @param trace data.frame (rt, intensity) of the chromatogram which has the peaks
#' @param initialEdges 2-element numeric vector specifying the lower limit and the
#'   upper limit for the peak area calculation (other function).  They're relative
#'   numbers: peak_rt+edges til peak_rt$edges
#' @param auc specifies the method name for the interpolation. Options are:
#'  "trapezoid", "step", "spline" & "linear"
#' @param absoluteArea A logical value that determines if negative areas should
#'  be added to the total area under the curve.
#' @param subDivisions an integer telling how many subdivisions should be used for
#'  integrate (for non-linear approximations). Ignored if method is not spline.
#' @param na.rm	logical, indicating whether NA values should be stripped before
#'  the computation proceeds. In this case only the complete.cases of x and y
#'  will be used. Defaults is FALSE
#' @param maxIterations 2 element integer vector: maximum number of iterations
#'  per side
#' @param iterationCutOff 2 elements numeric vector (first is for the left limit,
#'  second is for the right limit). Defines the amount of change in area that is
#'  to be considered still relevant to continue iterating (0.05 means an increase
#'  of 5%)
#' @param change 2 element numeric vector: amount of change to use per iteration
#'  per side. The value for the left side will be subtracted from the initial
#'  limit, the value for the right side will be added to the initial limit
#' @param includeIterateCounts logical vector. If TRUE that the iteration counts
#'  are included in the resulting peak. Default is FALSE. For debugging/testing
#'  purposes
#' @param maxLeftRight default is NA, if not NA, then a two-element numeric
#'  vector is expected where the first one is the maximum distance between the
#'  left (rt) value and the peak maximum and the second one is the maximum
#'  distance between the right (rt) value and the peak maximum
#'
#' @return the peak table data.frame with extra columns containing the new (area
#'  related) info.
#'
#' @note gives variable result with complex chromatograms with peaks close together
#'  or even overlapping
#'
#' @examples
#'
#' demoFile <- fs::path_package("extdata", "Data0001.CSV", package = "MS.Analysis")
#' result <- readLines(demoFile, n = 9092)
#' result <- read.table(text = result, sep = ",", header = FALSE)[, 2:3]
#' colnames(result) <- c("rt", "intensity")
#' result |> head()
#' plot(result, type = "l")
#' peaks <- data.frame(
#'   peak_rt = c(11.074, 12.133, 17.769, 19.515),
#'   peak_intensity = c(145412448, 151731744, 99109632, 70294472),
#'   left = NA,
#'   right = NA,
#'   area = NA
#' )
#' peaks <- chromatogramPeaks.FindAreas.Iterative(
#'   peaks = peaks,
#'   trace = result,
#'   initialEdges = c(0.1,0.1),
#'   change = c(0.02, 0.02),
#'   includeIterateCounts = TRUE
#' )
#' peaks
#' plot(result, type = "l", xlim = c(10, 21), ylim = c(0, 2E8))
#' text(
#'   peaks$peak_rt,
#'   peaks$peak_intensity,
#'   formatMinimumDigits(1)(peaks$peak_rt),
#'   cex = 0.65,
#'   pos = 3,
#'   col = "red"
#' )
#' abline(
#'   v = peaks$peak_rt,
#'   col = scales::alpha(rgb(1, 0, 0), 0.75),
#'   lty = "dotted"
#' )
#' abline(
#'   v = peaks$left,
#'   col = scales::alpha(rgb(0, 0, 1), 0.75),
#'   lty = "dashed"
#' )
#' abline(
#'   v = peaks$right,
#'   col = scales::alpha(rgb(0, 0, 1), 0.75),
#'   lty = "dashed"
#' )
#'
#' @export
chromatogramPeaks.FindAreas.Iterative <- function(
  peaks,
  trace,
  initialEdges = c(0.075, 0.15),
  auc = "trapezoid",
  absoluteArea = FALSE,
  subDivisions = 100,
  na.rm = FALSE,
  maxIterations = c(10L, 10L),
  iterationCutOff = c(0.05, 0.05),
  change = c(0.1, 0.1),
  includeIterateCounts = FALSE,
  maxLeftRight = NA
) {
  # baseline
  peaks <- chromatogramPeaks.FixedEdges(peaks = peaks, edges = initialEdges)
  peaks <- chromatogramPeaks.FindAreas(
    peaks = peaks,
    trace = trace,
    auc = auc,
    absoluteArea = absoluteArea,
    subDivisions = subDivisions,
    na.rm = na.rm
  )
  # iterate left
  peaks$iterate <- TRUE
  peaks$iterateLeftCount <- 1
  while (sum(peaks$iterate) > 0) {
    for (counter in 1:nrow(peaks)) {
      if (peaks$iterate[counter]) {
        peaks$left[counter] <- peaks$left[counter] - change[1]
        peaks2 <- chromatogramPeaks.FindAreas(
          peaks = peaks[counter, ],
          trace = trace,
          auc = auc,
          absoluteArea = absoluteArea,
          subDivisions = subDivisions,
          na.rm = na.rm
        )
        if (
          ((peaks2$area[1] - peaks$area[counter]) / peaks$area[counter]) >
            iterationCutOff[1]
        ) {
          peaks$area[counter] <- peaks2$area[1]
          peaks$iterateLeftCount[counter] <- peaks$iterateLeftCount[counter] + 1
          if (peaks$iterateLeftCount[counter] >= maxIterations[1]) {
            peaks$iterate[counter] <- FALSE
          }
        } else {
          peaks$left[counter] <- peaks$left[counter] + change[1]
          peaks$iterate[counter] <- FALSE
        }
      }
    }
  }
  # iterate right
  peaks$iterate <- TRUE
  peaks$iterateRightCount <- 1
  while (sum(peaks$iterate) > 0) {
    for (counter in 1:nrow(peaks)) {
      if (peaks$iterate[counter]) {
        peaks$right[counter] <- peaks$right[counter] + change[2]
        peaks2 <- chromatogramPeaks.FindAreas(
          peaks = peaks[counter, ],
          trace = trace,
          auc = auc,
          absoluteArea = absoluteArea,
          subDivisions = subDivisions,
          na.rm = na.rm
        )
        if (
          ((peaks2$area[1] - peaks$area[counter]) / peaks$area[counter]) >
            iterationCutOff[2]
        ) {
          peaks$area[counter] <- peaks2$area[1]
          peaks$iterateRightCount[counter] <- peaks$iterateRightCount[counter] +
            1
          if (peaks$iterateRightCount[counter] >= maxIterations[2]) {
            peaks$iterate[counter] <- FALSE
          }
        } else {
          peaks$right[counter] <- peaks$right[counter] - change[2]
          peaks$iterate[counter] <- FALSE
        }
      }
    }
  }
  peaks <- peaks %>% dplyr::select(-iterate)
  if (!includeIterateCounts) {
    peaks <- peaks %>% dplyr::select(-starts_with("iterate"))
  }
  if (!identical(maxLeftRight, NA)) {
    peaks <- peaks %>%
      dplyr::mutate(
        left = max(left, peak_rt - maxLeftRight[1], na.rm = T),
        right = min(right, peak_rt + maxLeftRight[2], na.rm = T)
      )
    peaks <- chromatogramPeaks.FindAreas(
      peaks,
      trace = trace,
      auc = auc,
      absoluteArea = absoluteArea,
      subDivisions = subDivisions,
      na.rm = na.rm
    )
  }
  return(peaks)
}

#' @title chromatogramPeaks.FindAreas.SideIterative
#'
#' @description Calculates the peak areas on the basis of peak_rt, the left &
#'  right (rt) cutoff and the trace (chromatogram) itself, but also attempts to
#'  change the retention time limits in a structured manner. This is done to
#'  adjust for unusual peak shapes (tailing, fronting, etc)
#'
#' @note Both the left and the right limits of the peak are adjusted; each side
#'  gets adjusted in both directions (first left, then right for left limit/
#'  first right, then left for the right limit)
#'
#' @param peaks peak table with at least peak_rt for all peaks
#' @param trace data.frame (rt, intensity) of the chromatogram which has the peaks
#' @param initialEdges 2-element numeric vector specifying the lower limit and the
#'   upper limit for the peak area calculation (other function).  They're relative
#'   numbers: peak_rt+edges til peak_rt$edges
#' @param auc specifies the method name for the interpolation. Options are:
#'  "trapezoid", "step", "spline" & "linear"
#' @param absoluteArea A logical value that determines if negative areas should
#'  be added to the total area under the curve.
#' @param subDivisions an integer telling how many subdivisions should be used for
#'  integrate (for non-linear approximations). Ignored if method is not spline.
#' @param na.rm	logical, indicating whether NA values should be stripped before
#'  the computation proceeds. In this case only the complete.cases of x and y
#'  will be used. Defaults is FALSE
#' @param maxIterations 2 element integer vector: maximum number of iterations
#'  per side
#' @param iterationCutOff 2 elements numeric vector (first is for the left limit,
#'  second is for the right limit). Defines the amount of change in area that is
#'  to be considered still relevant to continue iterating (0.05 means an increase
#'  of 5%)
#' @param iterationChange 2 element numeric vector: amount of change to use per
#'  iteration per side. The value for the left side will be subtracted from the
#'  initial limit, the value for the right side will be added to the initial limit
#' @param includeIterateCounts logical vector. If TRUE that the iteration counts
#'  are included in the resulting peak. Default is FALSE. For debugging/testing
#'  purposes
#' @param maxLeftRight default is NA, if not NA, then a two-element numeric
#'  vector is expected where the first one is the maximum distance between the
#'  left (rt) value and the peak maximum and the second one is the maximum
#'  distance between the right (rt) value and the peak maximum
#' @param maxIterations 2 element named (left, right) integer vector: maximum
#'  number of iterations per side
#' @param iterationCutOff 2 element named (left, right) numeric vector (first
#'  is for the left limit, second is for the right limit). Defines the amount
#'  of change in area that is to be considered still relevant to continue
#'  iterating (0.001 means an increase of 0.1%)
#' @param iterationChange 2 element named (left, right) numeric vector: amount
#'  of change to use per iteration per side. The value for the left side will
#'  be subtracted from the initial limit, the value for the right side will be
#'  added to the initial limit
#' @param includeIterateCounts logical vector. If TRUE that the iteration counts
#'  are included in the resulting peak. Default is FALSE. For debugging/testing
#'  purposes
#' @param sides 2 element character vector which defines the order of adjustment
#'  of the sides. Default is c("left","right")
#' @param orderDirection a 2 named (left, right) element list of 2 element
#'  character vectors which specify for each side the order or adjustment.
#' @param maxLeftRight default is NA, if not NA, then a two-element numeric
#'  vector is expected where the first one is the maximum distance between the
#'  left (rt) value and the peak maximum and the second one is the maximum
#'  distance between the right (rt) value and the peak maximum
#'
#' @return the peak table data.frame with extra columns containing the new (area
#'  related) info.
#'
#' @note gives variable result with complex chromatograms with peaks close together
#'  or even overlapping
#'
#' @examples
#'
#' demoFile <- fs::path_package("extdata", "Data0001.CSV", package = "MS.Analysis")
#' result <- readLines(demoFile, n = 9092)
#' result <- read.table(text = result, sep = ",", header = FALSE)[, 2:3]
#' colnames(result) <- c("rt", "intensity")
#' result |> head()
#' plot(result, type = "l")
#' peaks <- data.frame(
#'   peak_rt = c(11.074, 12.133, 17.769, 19.515),
#'   peak_intensity = c(145412448, 151731744, 99109632, 70294472),
#'   left = NA,
#'   right = NA,
#'   area = NA
#' )
#' peaks <- chromatogramPeaks.FindAreas.SideIterative(
#'   peaks = peaks,
#'   trace = result,
#'   includeIterateCounts = TRUE,
#'   initialEdges = c(0.1, 0.1),
#'   iterationCutOff = c(left = 0.01, right = 0.01),
#'   iterationChange = c(left = 0.01, right = 0.01),
#' )
#' peaks
#' plot(result, type = "l", xlim = c(10, 21), ylim = c(0, 2E8))
#' text(
#'   peaks$peak_rt,
#'   peaks$peak_intensity,
#'   formatMinimumDigits(1)(peaks$peak_rt),
#'   cex = 0.65,
#'   pos = 3,
#'   col = "red"
#' )
#' abline(
#'   v = peaks$peak_rt,
#'   col = scales::alpha(rgb(1, 0, 0), 0.75),
#'   lty = "dotted"
#' )
#' abline(
#'   v = peaks$left,
#'   col = scales::alpha(rgb(0, 0, 1), 0.75),
#'   lty = "dashed"
#' )
#' abline(
#'   v = peaks$right,
#'   col = scales::alpha(rgb(0, 0, 1), 0.75),
#'   lty = "dashed"
#' )
#'
#' @export
chromatogramPeaks.FindAreas.SideIterative <- function(
  peaks,
  trace,
  initialEdges = c(0.075, 0.15),
  auc = "trapezoid",
  absoluteArea = FALSE,
  subDivisions = 100,
  na.rm = FALSE,
  maxIterations = c(left = 25, right = 25),
  iterationCutOff = c(left = 0.001, right = 0.001),
  iterationChange = c(left = 0.1, right = 0.1),
  includeIterateCounts = FALSE,
  sides = c("left", "right"),
  orderDirection = list(left = c("left", "right"), right = c("right", "left")),
  maxLeftRight = NA
) {
  # baseline
  peaks <- chromatogramPeaks.FixedEdges(peaks = peaks, edges = initialEdges)
  peaks <- chromatogramPeaks.FindAreas(
    peaks = peaks,
    trace = trace,
    auc = auc,
    absoluteArea = absoluteArea,
    subDivisions = subDivisions,
    na.rm = na.rm
  )
  iterationChange[["left"]] <- -iterationChange[["left"]]
  for (sidesCount in 1:length(sides)) {
    # usually first left, then right
    for (sideOrderCount in 1:length(orderDirection[[sides[sidesCount]]])) {
      peaks$iterate <- 0 # note: iterate = 0, keep on iterating, iterate = 1, iterate max reached, iterate > 1 do not do anything
      peaks$iterateSideCount <- 0
      while (sum(peaks$iterate == 0) > 0) {
        peaks[, sides[sidesCount]] <- peaks[, sides[sidesCount]] +
          ((peaks$iterate == 0) *
            iterationChange[[orderDirection[[sidesCount]][[sideOrderCount]]]])
        peaks2 <- chromatogramPeaks.FindAreas(
          peaks = peaks,
          trace = trace,
          auc = auc,
          absoluteArea = absoluteArea,
          subDivisions = subDivisions,
          na.rm = na.rm
        )
        peaks$iterateSideCount <- peaks$iterateSideCount + (peaks$iterate == 0)
        peaks$iterate <- peaks$iterate +
          as.integer(
            !((peaks2$area >
              (peaks$area *
                (1 +
                  iterationCutOff[[orderDirection[[sidesCount]][[
                    sideOrderCount
                  ]]]]))) &
              (peaks$iterateSideCount <
                maxIterations[[orderDirection[[sidesCount]][[
                  sideOrderCount
                ]]]]))
          )
        peaks[, sides[sidesCount]] <- ifelse(
          (peaks$iterate == 0),
          peaks[, sides[sidesCount]],
          ifelse(
            (peaks$iterate > 1),
            peaks[, sides[sidesCount]],
            peaks[, sides[sidesCount]] -
              (iterationChange[[orderDirection[[sidesCount]][[
                sideOrderCount
              ]]]])
          )
        )
        peaks$area <- ifelse(
          peaks2$area >
            (peaks$area *
              (1 +
                iterationCutOff[[orderDirection[[sidesCount]][[
                  sideOrderCount
                ]]]])),
          peaks2$area,
          peaks$area
        )
      }
      colnames(peaks)[which(colnames(peaks) == "iterateSideCount")] <- paste(
        c(
          "iterate_",
          sides[sidesCount],
          "_",
          orderDirection[[sidesCount]][sideOrderCount]
        ),
        collapse = ""
      )
    }
  }
  if (!includeIterateCounts) {
    peaks <- peaks %>% dplyr::select(-starts_with("iterate"))
  } else {
    peaks <- peaks %>% dplyr::select(-c("iterate"))
  }
  if (!identical(maxLeftRight, NA)) {
    peaks <- peaks %>%
      dplyr::mutate(
        left = max(left, peak_rt - maxLeftRight[1], na.rm = T),
        right = min(right, peak_rt + maxLeftRight[2], na.rm = T)
      )
    peaks <- chromatogramPeaks.FindAreas(
      peaks,
      trace = trace,
      auc = auc,
      absoluteArea = absoluteArea,
      subDivisions = subDivisions,
      na.rm = na.rm
    )
  }
  return(peaks)
}

# ---- spectra - peakdetection ----

#' @title spectrumDetectPeaks
#'
#' @description peak detection for spectra
#'
#' @note originally meant for MALDI spectra, but should also work with other
#'  types of spectra
#'
#' @note internally the function MALDIquant::detecPeaks which does a noise
#'  estimation via \link[stats]{supsmu} ("SuperSmoother") or \link[stats]{mad}
#'  ("MAD") and attemps to find local maxima via C-code in the package itself
#'  through the 'y'-values and the 'halfWindowSize' ('span' argument in this
#'  function)
#'
#' @param spectrum data.frame with the spectra data (columns: mz & intensity )
#' @param halfWindowSize halfWindowSize parameter of the C_localMaxima function in
#'  detectPeaks. Essentially seems to be the number of data points. If too narrow
#'  the function will fail
#' @param smoothing smoothing used in noise estimation, two options:
#'  "SuperSmoother" or "MAD"
#' @param span see \link[stats]{supsmu} (Friednan's SuperSmoother)
#' @param signalNoiseRatio required signal to noise ratio for a peak to be
#'  annotated as such. Does not work for centoid(ed) data
#' @param intensityPercentage logical vector which defines whether intensities
#'  should be transformed to a percentage of the highest intensity. Default is
#'  FALSE
#' @param mzLimits two element numeric vector which is the m/z window between
#'  which the returned peaks should fall (default NA, not used). Note that this
#'  happens AFTER peak detection
#' @param limitNr return the limitNr highest or lowest intensity peaks. Default
#'  is NA (= not used)
#' @param limitPercentage  Default is NA, not used. If a numeric value than it will
#'  cut out any peaks from the peak list that are below limitPercentage of the
#'  highest intensity peak (set at 100%)
#' @param highest default is TRUE. if the limitNr argument is used then this
#'  argument determines if it's the highest or the lowest intensity peaks that
#'   will be returned
#' @param returnInfo if the data is to be returned as a function that generates
#'  a dataElement object (which  can be used to put in an info object)
#' @param xtraInfo extra info to be added to the info part in the form of a named list
#'
#' @return the peak table data.frame or a dataElement object
#'
#' @examples
#' demoFile <- fs::path_package("extdata", "spec1.csv", package = "MS.Analysis")
#' centroidSpectrum <- read.table(demoFile, sep = ",", header = TRUE)
#' plot(centroidSpectrum, type = "h", ylim = c(0,1.5E7))
#'
#' peaks <- spectrumDetectPeaks(centroidSpectrum, halfWindowSize = 3, limitNr = 10)
#' peaks
#' text(
#'   peaks$mz,
#'   peaks$intensity,
#'   formatMinimumDigits(2)(peaks$mz),
#'   cex = 0.65,
#'   pos = 3,
#'   col = "red"
#' )
#'
#' demoFile <- fs::path_package("extdata", "spec2.csv", package = "MS.Analysis")
#' profileSpectrum <- read.table(demoFile, sep = ",", header = TRUE)
#' plot(profileSpectrum, type = "l", ylim = c(0,1.5E7))
#'
#' peaks <- spectrumDetectPeaks(profileSpectrum, halfWindowSize = 50, limitNr = 7)
#' peaks
#' text(
#'   peaks$mz,
#'   peaks$intensity,
#'   formatMinimumDigits(2)(peaks$mz),
#'   cex = 0.65,
#'   pos = 3,
#'   col = "red"
#' )
#'
#' @export
spectrumDetectPeaks <- function(
  spectrum,
  halfWindowSize = 50,
  smoothing = "MAD",
  span = "cv",
  signalNoiseRatio = 2, # doesn't work with profile data
  intensityPercentage = FALSE,
  mzLimits = NA,
  limitNr = NA,
  limitPercentage = NA,
  highest = TRUE,
  returnInfo = FALSE,
  xtraInfo = list()
) {
  if (intensityPercentage) {
    spectrum$intensity <- (spectrum$intensity / max(spectrum$intensity)) *
      100
  }
  rts <- MALDIquant::detectPeaks(
    MALDIquant::createMassSpectrum(
      mass = spectrum$mz,
      intensity = spectrum$intensity
    ),
    halfWindowSize = halfWindowSize,
    span = span,
    method = smoothing,
    SNR = signalNoiseRatio
  )
  tempdf <- data.frame(mz = rts@mass, intensity = rts@intensity, snr = rts@snr)
  if (!identical(mzLimits, NA)) {
    tempdf <- tempdf %>% filter(mz >= mzLimits[1], mz <= mzLimits[2])
  }
  if (!identical(limitPercentage, NA)) {
    tempdf <- tempdf %>%
      dplyr::mutate(perc = (intensity / max(intensity)) * 100) %>%
      dplyr::filter(perc >= limitPercentage) %>%
      dplyr::select(-perc)
  }
  if (!identical(limitNr, NA)) {
    if (highest) {
      tempdf <- tempdf %>%
        arrange(desc(intensity)) %>%
        slice(1:limitNr)
    } else {
      tempdf <- tempdf %>%
        arrange(intensity) %>%
        slice(1:limitNr)
    }
  }
  if (!returnInfo) {
    return(tempdf)
  } else {
    return(dataInfo::readData(
      dataframe = tempdf,
      columns = NA,
      columnNames = NA,
      rowNames = NULL,
      info = append(
        list(
          span = span,
          halfWindowSize = halfWindowSize,
          signalNoiseRatio = signalNoiseRatio,
          smoothing = smoothing,
          intensityPercentage = intensityPercentage,
          source = "spectrum"
        ),
        xtraInfo
      )
    ))
  }
}

#' @title spectrumDetectPeaks.General
#'
#' @description function factory to generate a peak detection function for spectra
#'
#' @param halfWindowSize halfWindowSize parameter of the C_localMaxima function in
#'  detectPeaks. Essentially seems to be the number of data points. If too narrow
#'  the function will fail
#' @param smoothing smoothing used in noise estimation, two options:
#'  "SuperSmoother" or "MAD"
#' @param span see \link[stats]{supsmu} (Friednan's SuperSmoother)
#' @param signalNoiseRatio required signal to noise ratio for a peak to be
#'  annotated as such.
#' @param intensityPercentage logical vector which defines whether intensities
#'  should be transformed to a percentage of the highest intensity. Default is
#'  FALSE
#' @param mzLimits two element numeric vector which is the m/z window between
#'  which the returned peaks should fall (default NA, not used). Note that this
#'  happens AFTER peak detection
#' @param limitNr return the limitNr highest or lowest intensity peaks. Default
#'  is NA (= not used)
#' @param limitPercentage  Default is NA, not used. If a numeric value than it will
#'  cut out any peaks from the peak list that are below limitPercentage of the
#'  highest intensity peak (set at 100%)
#' @param highest default is TRUE. if the limitNr argument is used then this
#'  argument determines if it's the highest or the lowest intensity peaks that
#'   will be returned
#' @param returnInfo if the data is to be returned as a function that generates
#'  a dataElement object (which  can be used to put in an info object)
#' @param xtraInfo extra info to be added to the info part in the form of a named list
#'
#' @return a function that can do peak detection in a spectrum
#'
#' @note the function generated has only one argument: 'spectrum', which is
#'  data.frame with spectrum data
#'
#' @examples
#' demoFile <- fs::path_package("extdata", "spec1.csv", package = "MS.Analysis")
#' centroidSpectrum <- read.table(demoFile, sep = ",", header = TRUE)
#' plot(centroidSpectrum, type = "h", ylim = c(0, 1.5E7))
#'
#' peaks <- spectrumDetectPeaks.General(halfWindowSize = 3, limitNr = 10)(
#'   centroidSpectrum
#' )
#' peaks
#' text(
#'   peaks$mz,
#'   peaks$intensity,
#'   formatMinimumDigits(2)(peaks$mz),
#'   cex = 0.65,
#'   pos = 3,
#'   col = "red"
#' )
#'
#' demoFile <- fs::path_package("extdata", "spec2.csv", package = "MS.Analysis")
#' profileSpectrum <- read.table(demoFile, sep = ",", header = TRUE)
#' plot(profileSpectrum, type = "l", ylim = c(0, 1.5E7))
#'
#' peaks <- spectrumDetectPeaks.General(halfWindowSize = 50, limitNr = 7)(
#'   profileSpectrum
#' )
#' peaks
#' text(
#'   peaks$mz,
#'   peaks$intensity,
#'   formatMinimumDigits(2)(peaks$mz),
#'   cex = 0.65,
#'   pos = 3,
#'   col = "red"
#' )
#'
#' @export
spectrumDetectPeaks.General <- function(
  halfWindowSize = 50,
  smoothing = "MAD",
  span = "cv",
  signalNoiseRatio = 2,
  intensityPercentage = FALSE,
  mzLimits = NA,
  limitNr = NA,
  limitPercentage = NA,
  highest = TRUE,
  returnInfo = FALSE,
  xtraInfo = list()
) {
  force(halfWindowSize)
  force(smoothing)
  force(span)
  force(signalNoiseRatio)
  force(intensityPercentage)
  force(mzLimits)
  force(limitNr)
  force(limitPercentage)
  force(highest)
  force(returnInfo)
  force(xtraInfo)
  function(spectrum) {
    return(spectrumDetectPeaks(
      spectrum,
      halfWindowSize = halfWindowSize,
      smoothing = smoothing,
      span = span,
      intensityPercentage = intensityPercentage,
      mzLimits = mzLimits,
      limitNr = limitNr,
      limitPercentage = limitPercentage,
      highest = highest,
      returnInfo = returnInfo,
      xtraInfo = xtraInfo
    ))
  }
}

#' @title spectrumDetectPeaks.Profile
#'
#' @description function factory to generate a smoothing function for profile type
#'  spectra
#'
#' @note essentially spectrumDetectPeaks.General with different presets/defaults
#'  which 'usually' work for profile data
#'
#' @param halfWindowSize halfWindowSize parameter of the C_localMaxima function in
#'  detectPeaks. Essentially seems to be the number of data points. If too narrow
#'  the function will fail
#' @param smoothing smoothing used in noise estimation, two options:
#'  "SuperSmoother" or "MAD"
#' @param span see \link[stats]{supsmu} (Friednan's SuperSmoother)
#' @param intensityPercentage logical vector which defines whether intensities
#'  should be transformed to a percentage of the highest intensity. Default is
#'  FALSE
#' @param mzLimits two element numeric vector which is the m/z window between
#'  which the returned peaks should fall (default NA, not used). Note that this
#'  happens AFTER peak detection
#' @param limitNr return the limitNr highest or lowest intensity peaks. Default
#'  is NA (= not used)
#' @param limitPercentage  Default is NA, not used. If a numeric value than it will
#'  cut out any peaks from the peak list that are below limitPercentage of the
#'  highest intensity peak (set at 100%)
#' @param highest default is TRUE. if the limitNr argument is used then this
#'  argument determines if it's the highest or the lowest intensity peaks that
#'   will be returned
#' @param returnInfo if the data is to be returned as a function that generates
#'  a dataElement object (which  can be used to put in an info object)
#' @param xtraInfo extra info to be added to the info part in the form of a
#'  named list
#'
#' @return a function that can do peak detection in a spectrum
#'
#' @note the function generated has only one argument: 'spectrum', which is
#'  data.frame with spectrum data
#'
#' @examples
#' demoFile <- fs::path_package("extdata", "spec2.csv", package = "MS.Analysis")
#' profileSpectrum <- read.table(demoFile, sep = ",", header = TRUE)
#' plot(profileSpectrum, type = "l", ylim = c(0, 1.5E7))
#'
#' peaks <- spectrumDetectPeaks.Profile(limitNr = 7)(profileSpectrum)
#' peaks
#' text(
#'   peaks$mz,
#'   peaks$intensity,
#'   formatMinimumDigits(2)(peaks$mz),
#'   cex = 0.65,
#'   pos = 3,
#'   col = "red"
#' )
#'
#' @export
spectrumDetectPeaks.Profile <- function(
  halfWindowSize = 50,
  smoothing = "MAD",
  span = "cv",
  intensityPercentage = FALSE,
  mzLimits = NA,
  limitNr = NA,
  limitPercentage = NA,
  highest = TRUE,
  returnInfo = FALSE,
  xtraInfo = list()
) {
  force(halfWindowSize)
  force(smoothing)
  force(span)
  force(intensityPercentage)
  force(mzLimits)
  force(limitNr)
  force(limitPercentage)
  force(highest)
  force(returnInfo)
  force(xtraInfo)
  function(spectrum) {
    return(spectrumDetectPeaks(
      spectrum,
      halfWindowSize = halfWindowSize,
      smoothing = smoothing,
      span = span,
      intensityPercentage = intensityPercentage,
      mzLimits = mzLimits,
      limitNr = limitNr,
      limitPercentage = limitPercentage,
      highest = highest,
      returnInfo = returnInfo,
      xtraInfo = xtraInfo
    ))
  }
}

#' @title spectrumDetectPeaks.Centroid
#'
#' @description function factory to generate a smoothing function for centroid(ed)
#'  type spectra
#'
#' @note essentially spectrumDetectPeaks.General with different presets/defaults
#'  which 'usually' work for centroid(ed) data
#'
#' @param halfWindowSize halfWindowSize parameter of the C_localMaxima function in
#'  detectPeaks. Essentially seems to be the number of data points. If too narrow
#'  the function will fail
#' @param smoothing smoothing used in noise estimation, two options:
#'  "SuperSmoother" or "MAD"
#' @param span see \link[stats]{supsmu} (Friednan's SuperSmoother)
#' @param signalNoiseRatio required signal to noise ratio for a peak to be
#'  annotated as such. Does not work for centoid(ed) data
#' @param intensityPercentage logical vector which defines whether intensities
#'  should be transformed to a percentage of the highest intensity. Default is
#'  FALSE
#' @param mzLimits two element numeric vector which is the m/z window between
#'  which the returned peaks should fall (default NA, not used). Note that this
#'  happens AFTER peak detection
#' @param limitNr return the limitNr highest or lowest intensity peaks. Default
#'  is NA (= not used)
#' @param limitPercentage  Default is NA, not used. If a numeric value than it will
#'  cut out any peaks from the peak list that are below limitPercentage of the
#'  highest intensity peak (set at 100%)
#' @param highest default is TRUE. if the limitNr argument is used then this
#'  argument determines if it's the highest or the lowest intensity peaks that
#'   will be returned
#' @param returnInfo if the data is to be returned as a function that generates
#'  a dataElement object (which  can be used to put in an info object)
#' @param xtraInfo extra info to be added to the info part in the form of a
#'  named list
#'
#' @return a function that can do peak detection in a spectrum
#'
#' @note the function generated has only one argument: 'spectrum', which is
#'  data.frame with spectrum data
#'
#' @examples
#' demoFile <- fs::path_package("extdata", "spec1.csv", package = "MS.Analysis")
#' centroidSpectrum <- read.table(demoFile, sep = ",", header = TRUE)
#' plot(centroidSpectrum, type = "h", ylim = c(0, 1.5E7))
#'
#' peaks <- spectrumDetectPeaks.Centroid()(
#'   centroidSpectrum
#' )
#' peaks
#' text(
#'   peaks$mz,
#'   peaks$intensity,
#'   formatMinimumDigits(2)(peaks$mz),
#'   cex = 0.65,
#'   pos = 3,
#'   col = "red"
#' )
#'
#' @export
spectrumDetectPeaks.Centroid <- function(
  halfWindowSize = 10,
  smoothing = "MAD",
  span = "cv",
  signalNoiseRatio = 1,
  intensityPercentage = FALSE,
  mzLimits = NA,
  limitNr = NA,
  limitPercentage = NA,
  highest = TRUE,
  returnInfo = FALSE,
  xtraInfo = list()
) {
  force(halfWindowSize)
  force(smoothing)
  force(span)
  force(signalNoiseRatio)
  force(intensityPercentage)
  force(mzLimits)
  force(limitNr)
  force(limitPercentage)
  force(highest)
  force(returnInfo)
  force(xtraInfo)
  function(spectrum) {
    return(spectrumDetectPeaks(
      spectrum,
      halfWindowSize = halfWindowSize,
      smoothing = smoothing,
      span = span,
      signalNoiseRatio = signalNoiseRatio,
      intensityPercentage = intensityPercentage,
      mzLimits = mzLimits,
      limitNr = limitNr,
      limitPercentage = limitPercentage,
      highest = highest,
      returnInfo = returnInfo,
      xtraInfo = xtraInfo
    ))
  }
}
