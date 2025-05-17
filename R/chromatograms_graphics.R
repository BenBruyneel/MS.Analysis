#' @title plotChromatogram
#'
#' @description Creates a plot of a chromatogram, rt (retention time) will be on
#'  the x-axis, intensity on the y-axis
#'
#' @param chromatogram a data.frame with "rt" & "intensity" columns
#' @param rtLimits two element numeric vector. Essentially the limits of the x-axis
#'  (rt, retention time)
#' @param intensityLimits two element numeric vector. Essentially the limits of the
#'  y-axis (intensity)
#' @param incrScaleIntensity numeric value which specifies the factor with which
#'  to increase the y-axis limit
#' @param scaleIntensityLocal logical vector which determines whether to use global
#'  intensities (FALSE) or only the intensities of m/z's which fall within the mzLimits
#'  (TRUE, default)
#' @param intensityPercentage Whether to display the intensity axis in percentages
#'  (default FALSE)
#' @param rtLabelFormat defines the format of the rt (x) axis labels.
#'  See eg \link[MS.Analysis]{formatDigits}. Default is \link[ggplot2]{waiver}
#'  which ensures 'standard' formatting
#' @param intensityLabelFormat defines the format of the intensity (y) axis labels.
#'  See eg \link[MS.Analysis]{formatDigits}
#' @param lineType determines linetype of the lines used for plotting the chromatogram
#' @param lineAlpha determines alpha of the lines used for plotting the chromatogram
#' @param lineColor determines color of the lines used for plotting the chromatogram
#' @param lineWidth determines linewidth of the lines used for plotting the chromatogram
#' @param areaFill default is NA. If not NA, then should be color that the area
#'  under the chromatogram is given. See example.
#' @param areaAlpha the alpha of the filled area under the chromatogram, default
#'  is 0.5
#' @param rtUnits together with rtTitle argument: label for x-axis
#' @param rtTitle together with rtUnits argument: label for x-axis
#' @param intensityTitle defines the label for y-axis (intensity)
#' @param labels supposed to be a data.frame like coming from
#'  \link[MS.Analysis]{chromatogramFindPeaks}. This may change in the future
#' @param labelsLabel defines which column of the labels data.frame to use as
#'  label (default is 'peak_rt')
#' @param labelsLabelFormat defines the format of the labels
#' @param labelCutOffValue defines which column of the labels data.frame to use
#'  for the labelCutOff argument
#' @param labelCutOff numeric vector that defines at which intensity level, peak
#'  labels should not be shown. Default is 0.01 which means that below 1% of
#'  maximum intensity the labels won't be shown. Note: this assumes the column
#'  chosen by labelCutOffValue is intensity related
#' @param labelOverlap logical vector that sets the "check_overlap" argument of
#'  \link[ggplot2]{geom_text} which is used internally for labels
#' @param labelSize sets the size of the label text
#' @param labelColor sets the color of the label text
#' @param labelAngle sets the angle of the label text
#' @param labelNudge_x determines how much the label should be shifted (nudged)
#'  horizontally
#' @param labelNudge_y determines how much the label should be shifted (nudged)
#'  vertically
#' @param annotateRt either NA or a list of annotation objects to be used, see
#'  \link[MS.Analysis]{chromatogramAnnotation}
#' @param chromatogramTitle sets the title of the chromatogram
#' @param chromatogramSubtitle sets the subtitle of the chromatogram
#' @param chromatogramCaption sets the caption of the chromatogram
#' @param plot.margins.default sets whether default ggplot margins are to be used
#'  (default = FALSE)
#' @param plot.margins sets the plot margin sizes/widths (4 element integer vector).
#'  Default is c(5,15,5,5)
#' @param plot.margins.units sets the units for the margin sizes/widths. Default
#'  is "point"
#' @param generalTextSize factor with which to increase or decrease the size of
#'  all text elements used. Default is NA
#' @param generalLineWidth factor with which to increase or decrease the width of
#'  all line elements used. Default is NA
#' @param overrideLineWidth if the lineWidth argument is not used (NA), then if
#'  this argument is TRUE line width 0.5 x generalLineWidth is used, if FALSE then
#'  the line width will be set to 0.25 x generalLineWidth
#'
#' @return a ggplot object
#'
#' @examples
#' demoFile <- fs::path_package("extdata", "Data0001.CSV", package = "MS.Analysis")
#' result <- readLines(demoFile, n = 9092)
#' result <- read.table(text = result, sep = ",", header = FALSE)[, 2:3]
#' colnames(result) <- c("rt", "intensity")
#' plotChromatogram(chromatogram = result)
#' plotChromatogram(chromatogram = result, intensityPercentage = TRUE)
#' plotChromatogram(chromatogram = result, rtLimits = c(3, 7))
#' plotChromatogram(
#'   chromatogram = result,
#'   labels = chromatogramFindPeaks.General(
#'     smoothing = NA,
#'     span = 1,
#'     signalNoiseRatio = 8
#'   )(result),
#'   labelColor = "red"
#' )
#' plotChromatogram(
#'   chromatogram = result,
#'   rtLimits = c(3, 7),
#'   labels = chromatogramFindPeaks.General(
#'     smoothing = NA,
#'     span = 1,
#'     signalNoiseRatio = 1
#'   )(result),
#'   labelColor = "red",
#'   lineColor = "blue"
#' )
#' plotChromatogram(
#' chromatogram = result,
#' rtLimits = c(3, 7),
#' areaFill = "red",
#' areaAlpha = 0.5
#' )
#'
#' @export
plotChromatogram <- function(chromatogram,
                             rtLimits = NULL, intensityLimits = NULL,
                             incrScaleIntensity = 0.05, scaleIntensityLocal = TRUE,
                             intensityPercentage = FALSE,
                             rtLabelFormat = formatDigits(1),
                             intensityLabelFormat = ifelse(intensityPercentage,
                                                           formatDigits(0),
                                                           formatScientificDigits(4)),
                             lineType = "solid", lineAlpha = 1,
                             lineColor = "black", lineWidth = NA,
                             areaFill = NA, areaAlpha = 0.5,
                             rtUnits = "(mins)",
                             rtTitle = paste0("rt ", rtUnits),
                             intensityTitle = ifelse(intensityPercentage,
                                                     "Intensity (%)",
                                                     "Intensity"),
                             labels = NA,
                             labelsLabel = "peak_rt",
                             labelsLabelFormat = ifelse(labelsLabel == "peak_rt",
                                                        formatDigits(2),
                                                        ifelse(labelsLabel == "area",
                                                               formatScientificDigits(4),
                                                               ifelse(labelsLabel == "peak_intensity",
                                                                      formatScientificDigits(3),
                                                                      formatDigitsWaiver))),
                             labelCutOffValue = ifelse(labelsLabel == "peak_rt",
                                                       "peak_intensity",
                                                       labelsLabel),
                             labelCutOff = 0.01,
                             labelOverlap = FALSE, labelSize = 3, labelColor = "black",
                             labelAngle = 25,
                             labelNudge_x = ifelse(labelsLabel == "peak_rt",
                                                   0.01,
                                                   0.02),
                             labelNudge_y = ifelse(labelsLabel == "peak_rt",
                                                   0.02,
                                                   0.025),
                             annotateRt = NULL,
                             chromatogramTitle = ggplot2::waiver(),
                             chromatogramSubtitle = ggplot2::waiver(),
                             chromatogramCaption = ggplot2::waiver(),
                             plot.margins.default = FALSE,
                             plot.margins = c(5,15,5,5),
                             plot.margins.units = "points",
                             generalTextSize = NA,
                             generalLineWidth = NA, overrideLineWidth = FALSE){
  generalTextSize <- ifelse(is.na(generalTextSize),
                            10,
                            generalTextSize)
  generalLineWidth <- ifelse(is.na(generalLineWidth),
                             1,
                             generalLineWidth)
  lineWidth <- ifelse(is.na(lineWidth),
                      ifelse(overrideLineWidth, 0.5,  0.25),
                      lineWidth) * generalLineWidth
  if (intensityPercentage){
    yMaxStored <- max(chromatogram$intensity)
    chromatogram$intensity <- (chromatogram$intensity/yMaxStored)*100
  }
  if ((!scaleIntensityLocal) | (is.null(rtLimits))){
    maxY <- max(chromatogram$intensity)
  } else {
    maxY <- max(chromatogram[(chromatogram$rt >= rtLimits[1]) & (chromatogram$rt <= rtLimits[2]),]$intensity)
  }
  if (!is.null(rtLimits)){
    rtRangeSize <- rtLimits[2] - rtLimits[1]
  } else {
    rtRangeSize <- max(chromatogram$rt) - min(chromatogram$rt)
  }
  maxY <- (1+incrScaleIntensity) * maxY
  if (!identical(labels, NA)){
    if (nrow(labels) < 1){
      labels <- NA
    }
  }
  g <- ggplot2::ggplot(chromatogram, ggplot2::aes(x = rt, y = intensity))
  if (!is.na(areaFill)){
    g <- g + ggplot2::geom_area(fill = areaFill, alpha = areaAlpha)
  }
  if (!is.na(lineColor)){
    g <- g + ggplot2::geom_line(linetype = lineType, alpha = lineAlpha,
                                color = lineColor, linewidth = lineWidth)
  }
  if (!identical(labels, NA)){
    if (!is.na(labelCutOff)){
      if (labelCutOffValue == "peak_rt"){
        labels <- labels %>% dplyr::filter(peak_intensity > (labelCutOff*maxY))
      } else {
        if (is.numeric(labels[, labelCutOffValue])){
          if (labelCutOff < 1){
            labels <- labels[labels[, labelCutOffValue] > (labelCutOff*max(labels[,labelCutOffValue])),]
          } else {
            labels <- labels[labels[, labelCutOffValue] > labelCutOff]
          }
        }
      }
    }
    if (nrow(labels)>0){
      labels$actualLabels <- labelsLabelFormat(labels[,labelsLabel])
      g <- g + ggplot2::geom_text(data = labels, ggplot2::aes(x = peak_rt, y = peak_intensity, label = actualLabels), angle = labelAngle,
                                  color = labelColor, check_overlap = labelOverlap, size = labelSize,
                                  nudge_y = labelNudge_y*maxY, nudge_x = labelNudge_x*rtRangeSize)
    }
  }
  if (!is.null(rtLimits)){
    g <- g + ggplot2::scale_x_continuous(expand = c(0,0),limits = rtLimits, labels = rtLabelFormat)
  } else {
    g <- g + ggplot2::scale_x_continuous(labels = rtLabelFormat)
  }
  if (!is.null(intensityLimits)){
    g <- g + ggplot2::scale_y_continuous(expand = c(0,0), limits = intensityLimits, labels = intensityLabelFormat)
  } else {
    g <- g + ggplot2::scale_y_continuous(expand = c(0,0), limits = c(0,maxY), labels = intensityLabelFormat)
  }
  if (!is.null(annotateRt)){
    for (i in 1:length(annotateRt)){
      g <- annotateRt[[i]]$draw(graphObject = g, maxY = maxY, intensityPercentage = intensityPercentage)
    }
  }
  g <- g + ggplot2::labs(title = chromatogramTitle, subtitle = chromatogramSubtitle, caption = chromatogramCaption,
                         x = rtTitle, y = intensityTitle)
  g <- g + ggplot2::theme_classic()
  if (!plot.margins.default){
    g <- g + ggplot2::theme(plot.margin = ggplot2::unit(plot.margins, plot.margins.units))
  }
  g <- g + ggplot2::theme(
    axis.title.x = ifelseProper(is.na(rtTitle),
                                ggplot2::waiver(),
                                ggplot2::element_text(size = generalTextSize)),
    axis.title.y = ifelseProper(is.na(intensityTitle),
                                ggplot2::waiver(),
                                ggplot2::element_text(size = generalTextSize)),
    axis.ticks = ggplot2::element_line(linewidth = generalLineWidth*.5),
    text = ggplot2::element_text(size = generalTextSize),
    line = ggplot2::element_line(linewidth = generalLineWidth*.5),
    axis.text = ggplot2::element_text(size = generalTextSize)
  )
  return(g)
}

#' @title plotChromatogramOverlay
#'
#' @description Creates a overlay plot of a list of chromatograms, rt (retention
#'  time) will be on the x-axis, intensity on the y-axis.
#'
#' @param chromatogramList list of data.frame's (chromatograms)
#' @param chromatogramColors vector specifying the colors of the different chromatograms,
#'  eg c("red","black"). Length 1 or same length as chromatogramList argument
#' @param chromatogramLineTypes vector specifying the linetypes of the different chromatograms.
#'  Length 1 or same length as chromatogramList argument
#' @param chromatogramAlphas vector specifying the alphas of the different chromatograms.
#'  Length 1 or same length as chromatogramList argument
#' @param chromatogramWidths vector specifying the linewidths of the different chromatograms.
#'  Length 1 or same length as chromatogramList argument
#' @param chromatogramAreaFills vector specifying the area fill colors (for area
#'  under the chromatograms) of the different chromatograms. Length 1 or same
#'  length as chromatogramList argument under the chromatogram is given. See
#'  example.
#' @param chromatogramAreaAlphas vector specifying the area fill alphas (for area
#'  under the chromatograms) of the different chromatograms. Length 1 or same
#'  length as chromatogramList argument under the chromatogram is given. See
#'  example.
#' @param rtLimits two element numeric vector. Essentially the limits of the x-axis
#'  (rt, retention time)
#' @param intensityLimits two element numeric vector. Essentially the limits of the
#'  y-axis (intensity)
#' @param incrScaleIntensity numeric value which specifies the factor with which to
#'  increase the y-axis limit
#' @param scaleIntensityLocal logical vector which determines whether to use global
#'  intensities (FALSE) or only the intensities of m/z's which fall within the
#'  rtLimits (TRUE, default)
#' @param intensityPercentage Whether to display the intensity axis in percentages
#'  (default FALSE)
#' @param rtLabelFormat defines the format of the rt (x) axis labels.
#'  See eg \link[MS.Analysis]{formatDigits}. Default is \link[ggplot2]{waiver}
#'  which ensures 'standard' formatting
#' @param intensityLabelFormat defines the format of the intensity (y) axis labels.
#'  See eg \link[MS.Analysis]{formatDigits}
#' @param annotateRt either NA or a list of annotation objects to be used, see
#'  \link[MS.Analysis]{chromatogramAnnotation}
#' @param rtUnits together with rtTitle argument: label for x-axis
#' @param rtTitle together with rtUnits argument: label for x-axis
#' @param intensityTitle defines the label for y-axis (intensity)
#' @param chromatogramTitle sets the title of the chromatogram
#' @param chromatogramSubtitle sets the subtitle of the chromatogram
#' @param chromatogramCaption sets the caption of the chromatogram
#' @param plot.margins.default sets whether default ggplot margins are to be used
#'  (default = FALSE)
#' @param plot.margins sets the plot margin sizes/widths (4 element integer vector).
#'  Default is c(5,15,5,5)
#' @param plot.margins.units sets the units for the margin sizes/widths. Default
#'  is "point"
#' @param generalTextSize factor with which to increase or decrease the size of
#'  all text elements used. Default is NA
#' @param generalLineWidth factor with which to increase or decrease the width of
#'  all line elements used. Default is NA
#' @param overrideLineWidth if the lineWidth argument is not used (NA), then if
#'  this argument is TRUE line width 0.5 x generalLineWidth is used, if FALSE then
#'  the line width will be set to 0.25 x generalLineWidth
#'
#' @return a ggplot object
#'
#' @examples
#' demoFile <- fs::path_package("extdata", "Data0001.CSV", package = "MS.Analysis")
#' result <- readLines(demoFile, n = 9092)
#' result <- read.table(text = result, sep = ",", header = FALSE)[, 2:3]
#' colnames(result) <- c("rt", "intensity")
#' result2 <- result
#' result2$rt <- result2$rt + 1
#' result2$intensity <- result2$intensity * 1.25
#' plotChromatogramOverlay(
#'   list(result, result2),
#'   chromatogramColors = c("black", "red")
#' )
#' plotChromatogramOverlay(
#'   list(result, result2),
#'   chromatogramColors = c("black", "red"),
#'   rtLimits = c(3, 7)
#' )
#' plotChromatogramOverlay(
#'   list(result, result2),
#'   chromatogramColors = c("black", "red"),
#'   chromatogramAreaFills = c("black", "red"),
#'   rtLimits = c(3, 7)
#' )
#'
#'
#' @export
plotChromatogramOverlay <- function(chromatogramList,
                                    chromatogramColors = "black",
                                    chromatogramLineTypes = "solid",
                                    chromatogramAlphas = 1,
                                    chromatogramWidths = NA,
                                    chromatogramAreaFills = NA,
                                    chromatogramAreaAlphas = 0.5,
                                    rtLimits = NULL, intensityLimits = NULL,
                                    incrScaleIntensity = 0.05, scaleIntensityLocal = TRUE,
                                    intensityPercentage = FALSE,
                                    rtLabelFormat = formatDigits(1),
                                    intensityLabelFormat = ifelse(intensityPercentage,
                                                                  formatDigits(0),
                                                                  formatScientificDigits(4)),
                                    annotateRt = NULL,
                                    rtUnits = "(mins)",
                                    rtTitle = paste0("rt ", rtUnits),
                                    intensityTitle = ifelse(intensityPercentage,
                                                            "Intensity (%)",
                                                            "Intensity"),
                                    chromatogramTitle = ggplot2::waiver(),
                                    chromatogramSubtitle = ggplot2::waiver(),
                                    chromatogramCaption = ggplot2::waiver(),
                                    plot.margins.default = FALSE,
                                    plot.margins = c(5,15,5,5),
                                    plot.margins.units = "points",
                                    generalTextSize = NA,
                                    generalLineWidth = NA, overrideLineWidth = FALSE){
  generalTextSize <- ifelse(is.na(generalTextSize),
                            10,
                            generalTextSize)
  generalLineWidth <- ifelse(is.na(generalLineWidth),
                             1,
                             generalLineWidth)
  if (length(chromatogramColors) == 1){
    chromatogramColors <- rep(chromatogramColors, length(chromatogramList))
  }
  if (length(chromatogramLineTypes) == 1){
    chromatogramLineTypes <- rep(chromatogramLineTypes, length(chromatogramList))
  }
  if (length(chromatogramAlphas) == 1){
    chromatogramAlphas <- rep(chromatogramAlphas, length(chromatogramList))
  }
  if (length(chromatogramWidths) == 1){
    chromatogramWidths <- rep(chromatogramWidths, length(chromatogramList))
  }
  if (length(chromatogramAreaFills) == 1){
    chromatogramAreaFills <- rep(chromatogramAreaFills, length(chromatogramList))
  }
  if (length(chromatogramAreaAlphas) == 1){
    chromatogramAreaAlphas <- rep(chromatogramAreaAlphas, length(chromatogramList))
  }
  chromatogramWidths <- map_dbl(chromatogramWidths,
                                ~ifelse(is.na(.x),
                                        ifelse(overrideLineWidth, 0.5,  0.25),
                                        .x) * generalLineWidth)
  yMaxStored <- max(chromatogramList[[1]]$intensity)
  if (intensityPercentage){
    chromatogramList[[1]]$intensity <- (chromatogramList[[1]]$intensity/yMaxStored)*100
  }
  if (length(chromatogramList) > 1){
    for (counter in 2:length(chromatogramList)){
      yMaxStored2 <- max(chromatogramList[[counter]]$intensity)
      if (intensityPercentage){
        chromatogramList[[counter]]$intensity <- (chromatogramList[[counter]]$intensity/yMaxStored2)*100
      }
      yMaxStored <- max(yMaxStored, yMaxStored2)
    }
  }
  if ((!scaleIntensityLocal) | (is.null(rtLimits))){
    maxY <- yMaxStored
  } else {
    maxY <- max(chromatogramList[[1]][(chromatogramList[[1]]$rt >= rtLimits[1]) & (chromatogramList[[1]]$rt <= rtLimits[2]),]$intensity)
    if (length(chromatogramList) > 1){
      for (counter in 2:length(chromatogramList)){
        maxY <- max(c(maxY, max(chromatogramList[[counter]][(chromatogramList[[counter]]$rt >= rtLimits[1]) & (chromatogramList[[counter]]$rt <= rtLimits[2]),]$intensity)))
      }
    }
  }
  if (!is.null(rtLimits)){
    minRt <- rtLimits[1]
    maxRt <- rtLimits[2]
  } else {
    maxRt <- max(chromatogramList[[1]]$rt)
    minRt <- min(chromatogramList[[1]]$rt)
    if (length(chromatogramList) > 1){
      for (counter in 2:length(chromatogramList)){
        maxRt <- max(c(maxRt, max(chromatogramList[[counter]]$rt)))
        minRt <- min(c(minRt, min(chromatogramList[[counter]]$rt)))
      }
    }
  }
  rtRangeSize <- maxRt - minRt
  maxY <- (1+incrScaleIntensity) * maxY
  g <- ggplot2::ggplot()
  for (counter in 1:length(chromatogramList)){
    if (!is.na(chromatogramAreaFills[counter])){
      g <- g + ggplot2::geom_area(data = chromatogramList[[counter]], ggplot2::aes(x = rt, y = intensity),
                                  fill = chromatogramAreaFills[counter],
                                  alpha = chromatogramAreaAlphas[counter])
    }
    if (!is.na(chromatogramColors[counter])){
      g <- g + ggplot2::geom_line(data = chromatogramList[[counter]], ggplot2::aes(x = rt, y = intensity),
                                  color = chromatogramColors[counter],
                                  alpha = chromatogramAlphas[counter],
                                  linetype = chromatogramLineTypes[counter],
                                  linewidth = chromatogramWidths[counter])
    }
  }
  if (!is.null(rtLimits)){
    g <- g + ggplot2::scale_x_continuous(expand = c(0,0),limits = rtLimits, labels = rtLabelFormat)
  } else {
    g <- g + ggplot2::scale_x_continuous(labels = rtLabelFormat)
  }
  if (!is.null(intensityLimits)){
    g <- g + ggplot2::scale_y_continuous(expand = c(0,0), limits = intensityLimits, labels = intensityLabelFormat)
  } else {
    g <- g + ggplot2::scale_y_continuous(expand = c(0,0), limits = c(0,maxY), labels = intensityLabelFormat)
  }
  if (!is.null(annotateRt)){
    for (i in 1:length(annotateRt)){
      g <- annotateRt[[i]]$draw(graphObject = g, maxY = maxY, intensityPercentage = intensityPercentage)
    }
  }
  g <- g + ggplot2::labs(title = chromatogramTitle, subtitle = chromatogramSubtitle, caption = chromatogramCaption,
                         x = rtTitle, y = intensityTitle)
  g <- g + ggplot2::theme_classic()
  if (!plot.margins.default){
    g <- g + ggplot2::theme(plot.margin = ggplot2::unit(plot.margins, plot.margins.units))
  }
  g <- g + ggplot2::theme(
    axis.title.x = ifelseProper(is.na(rtTitle),
                                ggplot2::waiver(),
                                ggplot2::element_text(size = generalTextSize)),
    axis.title.y = ifelseProper(is.na(intensityTitle),
                                ggplot2::waiver(),
                                ggplot2::element_text(size = generalTextSize)),
    axis.ticks = ggplot2::element_line(linewidth = generalLineWidth*.5),
    text = ggplot2::element_text(size = generalTextSize),
    line = ggplot2::element_line(linewidth = generalLineWidth*.5),
    axis.text = ggplot2::element_text(size = generalTextSize)
  )
  return(g)
}

#' @title plotChromatogramSum
#'
#' @description sums a number of chromatograms and plots the area, while one
#'  chromatogram is used as a reference of sorts. See example. A use case for
#'  this a number of extracted ion chromatograms overlayed on top of a base peak
#'  chromatogram.
#'
#' @note all chromatograms MUST have exact same retention time data points
#'  (x-scale). The first chromatogram on the list will be plotted w/o fill,
#'  is e.g. BPC chromatogram, while the others (2 - ..) are summed and then used
#'  for fill
#'
#' @note code is not (yet) optimized and can be slow when dealing with many
#'  chromatograms with many data points each
#'
#' @param chromatogramList list of chromatograms (data.frame's with column rt &
#'  intensity). First chromatogram is the reference, the others are summed
#' @param chromatogramColor color for the reference chromatogram
#' @param chromatogramLineType linetype for the reference chromatogram
#' @param chromatogramAlpha alpha for the reference chromatogram
#' @param chromatogramWidth linewidth for the reference chromatogram
#' @param chromatogramAreaFill fill color for area under summed chromatograms
#' @param chromatogramAreaAlpha fill alpha for area under summed chromatograms
#' @param ... for additional parameters used in \link[MS.Analysis]{plotChromatogramOverlay}
#'
#' @returns a ggplot object
#'
#' @examples
#' demoFile <- fs::path_package("extdata", "Data0001.CSV", package = "MS.Analysis")
#' result <- readLines(demoFile, n = 9092)
#' result <- read.table(text = result, sep = ",", header = FALSE)[, 2:3]
#' colnames(result) <- c("rt", "intensity")
#' result <- list(result)
#' plotChromatogram(result[[1]], rtLimits = c(3,7))
#' # create parts of chromatogram imitating EIC's taking first chromatogram as bpc
#' result[[2]] <- result[[1]]
#' result[[2]]$intensity[(result[[2]]$rt <= 4.9 | result[[2]]$rt >= 5.32)] <- 0
#' result[[3]] <- result[[1]]
#' result[[3]]$intensity[(result[[3]]$rt <= 6.5 | result[[3]]$rt >= 6.7)] <- 0
#' plotChromatogramOverlay(chromatogramList = result,
#'                         chromatogramColors = c("black","red","red"),
#'                         rtLimits = c(3,7))
#' plotChromatogramSum(chromatogramList = result,
#'                     rtLimits = c(3,7))
#'
#' @export
plotChromatogramSum <- function(chromatogramList,
                                chromatogramColor = "black",
                                chromatogramLineType = "solid",
                                chromatogramAlpha = 1,
                                chromatogramWidth = 0.25,
                                chromatogramAreaFill = "red",
                                chromatogramAreaAlpha = 0.5,
                                ...){
  fillList <- chromatogramList[[2]]
  if (length(chromatogramList)>2){
    for (counter in 3:length(chromatogramList)){
      fillList$intensity <- fillList$intensity + chromatogramList[[counter]]$intensity
    }
  }
  plotChromatogramOverlay(chromatogramList = list(fillList, chromatogramList[[1]]),
                          chromatogramColors = c(NA, chromatogramColor),
                          chromatogramLineTypes = chromatogramLineType,
                          chromatogramAlphas = chromatogramAlpha,
                          chromatogramWidths = chromatogramWidth,
                          chromatogramAreaFills = c(chromatogramAreaFill, NA),
                          chromatogramAreaAlphas = c(chromatogramAreaAlpha, chromatogramAlpha),
                          ...)
}
