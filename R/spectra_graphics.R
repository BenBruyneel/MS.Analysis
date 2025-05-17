# ---- General - Non R6 ----

# to prevent warnings etc with RMD check
utils::globalVariables(c("intensity", "mz", "mzGroup", "mzG", "label", "actualLabels"))

#' @title reduceSpectrumLabelDF
#'
#' @description function to reduce number of rows to the top or bottom set.
#'  Specific for spectrum labels
#'
#' @param spectrumLabelDF data.frame with columns: mz, intensity & label
#' @param maxIntensity logical vector: if TRUE then the top 'number' of
#'  intensities will be returned. If FALSE, then the numbers will be reduced based
#'  on how close they are togther. The 'mzClosness' argument together with the
#'  'number' argument determine how many rows will be left
#' @param number the 'number' of top intensity rows to be returned
#' @param mzCloseness determines how the mz's values are grouped after which the
#'  m/z's with the highest value within the groups are returned
#'
#' @return data.frame (same structure as spectrumLabelDF)
#'
#' @note may be removed from export in future
#'
#' @examples
#' # create spectrum label data.frame
#' demoSpec <- fs::path_package("extdata", "spec1.csv", package = "MS.Analysis")
#' specData <- utils::read.table(file = demoSpec, header = TRUE, sep = ",")
#' specData$label <- as.character(specData$mz)
#' specData |> head()
#' specData |>
#'  dplyr::filter(mz > 150, mz < 180) |>
#'  head(20)
#' reduceSpectrumLabelDF(
#'   specData,
#'   number = 1,
#'   maxIntensity = FALSE,
#'   mzCloseness = 1
#' ) |>
#'   dplyr::filter(mz > 150, mz < 180) |>
#'   head(20)
#'
#' reduceSpectrumLabelDF(
#'   specData,
#'   number = 1,
#'   maxIntensity = FALSE,
#'   mzCloseness = log10(2)
#' ) |>
#'   dplyr::filter(mz > 150, mz < 180) |>
#'   head(20)
#'
#'
#' @export
reduceSpectrumLabelDF <- function(
  spectrumLabelDF,
  maxIntensity = TRUE,
  number = 1,
  mzCloseness = 1
) {
  if (maxIntensity) {
    return(
      spectrumLabelDF %>%
        dplyr::arrange(desc(intensity)) %>%
        dplyr::slice(1:number)
    )
  } else {
    return(
      spectrumLabelDF %>%
        dplyr::mutate(
          mzGroup = round(mz / (10^mzCloseness), digits = 0)
        ) %>%
        dplyr::group_by(mzGroup) %>%
        dplyr::arrange(desc(intensity)) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select(-mzGroup)
    )
  }
}

#' @title spectrumLabels
#'
#' @description function to generate a data.frame of labels (each row is a
#'  separate label). Specific for spectrum labels
#'
#' @param dataframe data.frame (with columns: mz, intensity, label). If this parameter
#'  is not NA, it will override mz, intensity arguments
#' @param mz numeric vector specifying the m/z's of the labels
#' @param intensity numeric vector specifying the intensity of the labels
#' @param label function that specifies the formatting of the label, example is
#'  formatDigits(4) (default)
#' @param top integer vector. If not NA only show labels with top highest
#'  intensities
#' @param mzAccuracy integer vector, can be both positive and negative or NA. If
#'  NA then this is not used. Values will group together 10^mzAccuracy values
#'  and show only the highest one. Eg mzAccuracy NA: spectrumLabels(mz =
#'  c(100,100.0001)), nothing will happen to labels; spectrumLabels(mz =
#'  c(100,101.0001), mzAccuracy = 0) nothing will happen, but with value 1
#'  only the highest intensity (or lowest m/z) will still be in data.frame
#'  after the function. In case of spectrumLabels(mz = c(100,100.0001),
#'  mzAccuracy = -4), both values will still be there, if value is 3 then only
#'  100.0000 is there
#' @param maxIntensity if TRUE then the top 'number' of intensities will be
#'  returned.
#'
#' @return data.frame (with columns: mz, intensity, label)
#'
#' @examples
#' demoSpec <- fs::path_package("extdata", "spec1.csv", package = "MS.Analysis")
#' specData <- utils::read.table(file = demoSpec, header = TRUE, sep = ",")
#' spectrumLabels(specData)
#' spectrumLabels(specData, mzAccuracy = 1)
#' spectrumLabels(specData, mzAccuracy = 1, maxIntensity = TRUE, top = 10)
#'
#' @export
spectrumLabels <- function(
  dataframe = NA,
  mz = NA,
  intensity = NA,
  label = dataInfo::formatDigits(4),
  top = NA,
  mzAccuracy = NA,
  maxIntensity = TRUE
) {
  if (identical(mz, NA) & identical(intensity, NA) & identical(dataframe, NA)) {
    return(data.frame(
      mz = as.numeric(),
      intensity = as.numeric(),
      label = as.character()
    ))
  } else {
    if (!identical(dataframe, NA)) {
      mz = dataframe$mz
      intensity = dataframe$intensity
    }
  }
  if (dataInfo::is.Class(label, "function")) {
    if (!identical(mz, NA)) {
      dfLabels <- data.frame(mz = mz, intensity = intensity, label = label(mz))
    } else {
      dfLabels <- data.frame(
        mz = mz,
        intensity = intensity,
        label = as.character(NA)
      )
    }
  } else {
    dfLabels <- data.frame(mz = mz, intensity = intensity, label = label)
  }
  if (!is.na(mzAccuracy)) {
    dfLabels <- dfLabels %>%
      dplyr::arrange(mz) %>%
      dplyr::mutate(mzG = round(mz / (10^mzAccuracy))) %>%
      dplyr::group_by(mzG) %>%
      dplyr::group_split()
    dfLabels <- map_df(
      dfLabels,
      ~ reduceSpectrumLabelDF(.x, maxIntensity = maxIntensity, number = 1)
    ) %>%
      dplyr::select(-mzG)
  }
  if (!is.na(top)) {
    dfLabels <- dfLabels %>%
      arrange(desc(intensity)) %>%
      slice(1:top)
  }
  return(dfLabels)
}

#' @title plotSpectrum
#'
#' @description Creates a plot of a spectrum, m/z will be on the x-axis,
#'  intensity on the y-axis
#'
#' @param spectrum a data.frame with "mz" & "intensity" columns
#' @param mzLimits two element numeric vector. Essentially the limits of the x-axis
#'  (m/z)
#' @param intensityLimits two element numeric vector. Essentially the limits of the
#'  y-axis (intensity)
#' @param incrScaleIntensity numeric value which specifies the factor with which to
#'  increase the y-axis limit
#' @param scaleIntensityLocal logical vector which determines whether to use global
#'  intensities (FALSE) or only the intensities of m/z's which fall within the
#'  mzLimits (TRUE, default) for determining the maximum intensity on the y-axis.
#'  This does nothing when using intensityLimits
#' @param intensityPercentage Whether to display the intensity axis in percentages
#'  (default FALSE)
#' @param mzLabelFormat defines the format of the m/z (x) axis labels.
#'  See eg \link[MS.Analysis]{formatDigits}. Default is \link[ggplot2]{waiver}
#'  which ensures 'standard' formatting
#' @param intensityLabelFormat defines the format of the intensity (y) axis labels.
#'  See eg \link[MS.Analysis]{formatDigits}
#' @param centroidPlot logical vector that defines whether the plot should be centroid
#'  -like (\link[ggplot2]{geom_segment} is used) or profile-like (\link[ggplot2]{geom_line}
#'  is used)
#' @param cutOff numeric vector that defines at which intensity level, below which
#'  peaks should not be shown. Default is 0.01 which means that below 1% of maximum
#'  intensity the peaks won't be shown. This only works for centroid(ed) spectra.
#' @param labelFormat defines the format of the peak labels. See e.g. \link[MS.Analysis]{formatDigits}
#' @param spectrumDetectPeaksMethod defines the spectrumDetectPeaks Method to be used.
#'  Works via function factories such as spectrumDetectPeaks.Centroid. By default
#'  uses spectrumDetectPeaks.Centroid or spectrumDetectPeaks.Profile depending on
#'  the centroidPlot argument
#' @param labels should be the result of a spectrumLabels function or NA. By default
#'  attempts to use the spectrumDetectPeaksMethod together with the labelFormat arguments.
#' @param returnPeaks default is FALSE. If TRUE, then a list object is returned of which
#'  element 1 is the peak list (detected or provided) and element 2 is the ggplot object
#'  of the spectrum
#' @param returnAllPeaks default is FALSE. If TRUE then the whole peak list is returned.
#'  If FALSE then only the peaks visible in the spectrum plot are returned. Argument is
#'  ignored if returnPeaks is FALSE
#' @param labelCutOff numeric vector that defines at which intensity level, peak labels
#'  should not be shown. Default is 0.01 which means that below 1% of maximum intensity
#'  the labels won't be shown
#' @param labelOverlap logical vector that sets the "check_overlap" argument of
#'  \link[ggplot2]{geom_text} which is used internally for displaying labels
#' @param labelSize sets the size of the label text
#' @param labelAlpha sets the alpha of the label text
#' @param labelColor sets the color of the label text
#' @param labelAngle sets the angle of the label text
#' @param labelNudge_x determines how much the label should be shifted (nudged)
#'  horizontally
#' @param labelNudge_y determines how much the label should be shifted (nudged)
#'  vertically
#' @param annotateMz either NA or a list of annotation objects to be used, see
#'  \link[MS.Analysis]{spectrumAnnotation}
#' @param lineAlpha determines alpha of the lines used for plotting the spectrum
#' @param lineColor determines color of the lines used for plotting the spectrum
#' @param lineWidth determines width of the lines used for plotting the spectrum
#' @param lineType determines linetype of the lines used for plotting the spectrum
#' @param plot.margins.default sets whether default ggplot margins are to be used
#'  (default = FALSE)
#' @param plot.margins sets the plot margin sizes/widths (4 element integer vector).
#'  Default is c(5,15,5,5)
#' @param plot.margins.units sets the units for the margin sizes/widths. Default
#'  is "point"
#' @param mzTitle defines the label for the x-axis (m/z)
#' @param intensityTitle defines the label for y-axis (intensity)
#' @param spectrumTitle sets title of the spectrum
#' @param spectrumSubtitle sets subtitle of the spectrum
#' @param spectrumCaption sets caption of the spectrum
#' @param generalTextSize factor with which to increase or decrease the size of
#'  all text elements used, except for annotateMz elements. Default is NA
#' @param generalLineWidth factor with which to increase or decrease the width of
#'  all line elements used, except for annotateMz elements. Default is NA
#' @param overrideLineWidth if the lineWidth argument is not used (NA), then if
#'  this argument is TRUE line width 0.5 x generalLineWidth is used, if FALSE then
#'  the line width will be set to 0.25 x generalLineWidth
#'
#' @return a ggplot object or list of peak list (data.frame) + ggplot object
#'
#' @examples
#' demoFile <- fs::path_package("extdata", "spec1.csv", package = "MS.Analysis")
#' centroidSpectrum <- read.table(demoFile, sep = ",", header = TRUE)
#' plotSpectrum(centroidSpectrum, centroidPlot = TRUE)
#' plotSpectrum(
#'   centroidSpectrum,
#'   centroidPlot = TRUE,
#'   mzLimits = c(100, 500),
#'   labelColor = "red",
#'   intensityPercentage = TRUE
#' )
#' plotSpectrum(
#'   centroidSpectrum,
#'   centroidPlot = TRUE,
#'   mzLimits = c(100, 500),
#'   labelColor = "red",
#'   intensityPercentage = TRUE,
#'   cutOff = 0.05
#' )
#' demoFile <- fs::path_package("extdata", "spec2.csv", package = "MS.Analysis")
#' profileSpectrum <- read.table(demoFile, sep = ",", header = TRUE)
#' plotSpectrum(profileSpectrum, labelCutOff = 0.025)
#' plotSpectrum(
#'   profileSpectrum,
#'   labelCutOff = 0.05,
#'   mzLimits = c(550, 1000),
#'   intensityPercentage = TRUE,
#'   scaleIntensityLocal = TRUE
#' )
#' plotSpectrum(
#'   profileSpectrum,
#'   labelCutOff = 0.05,
#'   mzLimits = c(550, 1000),
#'   intensityPercentage = TRUE,
#'   scaleIntensityLocal = TRUE,
#'   spectrumDetectPeaksMethod = spectrumDetectPeaks.Profile(halfWindowSize = 25)
#' )
#' result <- plotSpectrum(
#'   profileSpectrum,
#'   labelCutOff = 0.05,
#'   mzLimits = c(550, 1000),
#'   intensityPercentage = TRUE,
#'   scaleIntensityLocal = TRUE,
#'   returnPeaks = TRUE,
#'   returnAllPeaks = TRUE
#' )
#' result$graph
#' result$peaks
#' plotSpectrum(
#'   profileSpectrum,
#'   labelCutOff = 0.05,
#'   mzLimits = c(550, 1000),
#'   intensityPercentage = TRUE,
#'   scaleIntensityLocal = TRUE,
#'   spectrumDetectPeaksMethod = spectrumDetectPeaks.Profile(halfWindowSize = 25),
#'   generalLineWidth = 2,
#'   generalTextSize = 20
#' )
#'
#' @export
plotSpectrum <- function(spectrum,
                         mzLimits = NULL, intensityLimits = NULL,
                         incrScaleIntensity = 0.05, scaleIntensityLocal = TRUE,
                         intensityPercentage = FALSE,
                         mzLabelFormat = ggplot2::waiver(),
                         intensityLabelFormat = ifelse(intensityPercentage,
                                                       formatDigits(0),
                                                       formatScientificDigits(4)),
                         centroidPlot = FALSE, cutOff = 0.01,
                         labelFormat = formatDigitsLargeNumbers(4),
                         spectrumDetectPeaksMethod = ifelseProper(centroidPlot,
                                                                  spectrumDetectPeaks.Centroid(),
                                                                  spectrumDetectPeaks.Profile()),
                         labels = spectrumLabels(spectrumDetectPeaksMethod(spectrum), label = labelFormat),
                         returnPeaks = FALSE, returnAllPeaks = FALSE,
                         labelCutOff = ifelse(is.na(cutOff),
                                              0.01,
                                              cutOff),
                         labelOverlap = FALSE, labelSize = NA, labelColor = "black", labelAlpha = 1,
                         labelAngle = 0, labelNudge_x = 0.02, labelNudge_y = 0.01,
                         annotateMz = NULL,
                         lineAlpha = 1,
                         lineColor =  "black",
                         lineWidth = NA,
                         lineType = "solid",
                         plot.margins.default = FALSE,
                         plot.margins = c(5,15,5,5),
                         plot.margins.units = "points",
                         mzTitle = "m/z",
                         intensityTitle = ifelse(intensityPercentage,
                                                 "Intensity (%)",
                                                 "Intensity"),
                         spectrumTitle = ggplot2::waiver(),
                         spectrumSubtitle = ggplot2::waiver(),
                         spectrumCaption = ggplot2::waiver(),
                         generalTextSize = NA,
                         generalLineWidth = NA, overrideLineWidth = FALSE){
  generalTextSize <- ifelse(is.na(generalTextSize),
                            10,
                            generalTextSize)
  generalLineWidth <- ifelse(is.na(generalLineWidth),
                             1,
                             generalLineWidth)
  labelSize <- ifelse(is.na(labelSize),
                      3,
                      labelSize)*(generalTextSize/10)
  lineWidth <- ifelse(is.na(lineWidth),
                      ifelse(overrideLineWidth, 0.5,  0.25),
                      lineWidth) * generalLineWidth
  yMaxStored <- max(spectrum$intensity)
  if (intensityPercentage){
    spectrum$intensity <- (spectrum$intensity/yMaxStored)*100
  }
  if (!is.null(labels)){
    if (!identical(labels, NA)){
      if (nrow(labels) < 1){
        labels <- NULL
      }
    } else {
      labels <- NULL
    }
  }
  if ((!scaleIntensityLocal) | (is.null(mzLimits))){
    maxY <- max(spectrum$intensity)
  } else {
    maxY <- max(spectrum[(spectrum$mz >= mzLimits[1]) & (spectrum$mz <= mzLimits[2]),]$intensity)
  }
  if (!is.null(mzLimits)){
    mzRangeSize <- mzLimits[2] - mzLimits[1]
  } else {
    mzRangeSize <- max(spectrum$mz) - min(spectrum$mz)
  }
  if (centroidPlot & !is.na(cutOff)){
    spectrum <- spectrum[spectrum[,2] >= (cutOff*maxY),]
  }
  maxY <- (1+incrScaleIntensity) * maxY
  if (centroidPlot) {
    g <- ggplot2::ggplot(spectrum, ggplot2::aes(x = mz, xend = mz, y = 0, yend= intensity, label = mz))
    g <- g + ggplot2::geom_segment(alpha = lineAlpha, color = lineColor, linewidth = lineWidth, linetype = lineType)
  } else {
    g <- ggplot2::ggplot(spectrum, ggplot2::aes(x = mz, y = intensity, label = mz))
    g <- g + ggplot2::geom_line(alpha = lineAlpha, color = lineColor, linewidth = lineWidth, linetype = lineType)
  }
  g <- g + ggplot2::xlab(mzTitle) + ggplot2::ylab(intensityTitle)
  if (!is.null(labels)){
    if (!is.na(labelCutOff)){
      labels <- labels %>% dplyr::filter(intensity > (labelCutOff*maxY))
    }
    g <- g + ggplot2::geom_text(data = labels, ggplot2::aes(x = mz, y = intensity, label = label), angle = labelAngle, color = labelColor, alpha = labelAlpha,
                       check_overlap = labelOverlap, size = labelSize, nudge_y = labelNudge_y*maxY, nudge_x = labelNudge_x*mzRangeSize)
  }
  if (!is.null(mzLimits)){
    g <- g + ggplot2::scale_x_continuous(expand = c(0,0),limits = mzLimits, labels = mzLabelFormat)
  } else {
    g <- g + ggplot2::scale_x_continuous(labels = mzLabelFormat)
  }
  if (!is.null(intensityLimits)){
    g <- g + ggplot2::scale_y_continuous(expand = c(0,0), limits = c(intensityLimits[1], intensityLimits[2]*(1+incrScaleIntensity)), labels = intensityLabelFormat)
  } else {
    g <- g + ggplot2::scale_y_continuous(expand = c(0,0), limits = c(0,maxY), labels = intensityLabelFormat)
  }
  if (!is.null(annotateMz)){
    for (i in 1:length(annotateMz)){
      g <- annotateMz[[i]]$draw(graphObject = g, maxY = maxY, yMax = yMaxStored, intensityPercentage = intensityPercentage)
    }
  }
  g <- g + ggplot2::theme_classic()
  if (!plot.margins.default){
    g <- g + ggplot2::theme(plot.margin = ggplot2::unit(plot.margins, plot.margins.units))
  }
  g <- g + ggplot2::labs(title = spectrumTitle, subtitle = spectrumSubtitle, caption = spectrumCaption,
                x = mzTitle, y = intensityTitle)
  g <- g + ggplot2::theme(
    axis.title.x = ifelseProper(is.na(mzTitle),
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
  if (returnPeaks){
    if (!is.null(labels)){
      if (!returnAllPeaks & !is.null(mzLimits)){
        labels <- labels %>% dplyr::filter(mz >= mzLimits[1],
                                           mz <= mzLimits[2])
      }
      if (!identical(mzTitle, NA)){
        colnames(labels)[1] <- mzTitle
      }
      return(list(graph = g, peaks = labels %>% dplyr::select(-label)))
    }
  }
  return(g)
}

#' @title plotSpectrumOverlay
#'
#' @description Creates a overlay plot of a list of spectra, m/z will be on the
#'  x-axis, intensity on the y-axis
#'
#' @param spectrumList list of data.frame's (spectra)
#' @param spectrumColors vector specifying the colors of the different spectra,
#'  eg c("red","black"). Length 1 or same length as spectrumList argument
#' @param spectrumLineTypes vector specifying the linetypes of the different spectra.
#'  Length 1 or same length as spectrumList argument
#' @param spectrumAlphas vector specifying the alphas of the different spectra.
#'  Length 1 or same length as spectrumList argument
#' @param spectrumWidths vector specifying the linewidths of the different spectra.
#'  Length 1 or same length as spectrumList argument
#' @param mzLimits two element numeric vector. Essentially the limits of the x-axis
#'  (m/z)
#' @param intensityLimits two element numeric vector. Essentially the limits of the
#'  y-axis (intensity)
#' @param incrScaleIntensity numeric value which specifies the factor with which to
#'  increase the y-axis limit
#' @param scaleIntensityLocal logical vector which determines whether to use global
#'  intensities (FALSE) or only the intensities of m/z's which fall within the mzLimits
#'  (TRUE, default)
#' @param intensityPercentage Whether to display the intensity axis in percentages
#'  (default FALSE)
#' @param mzLabelFormat defines the format of the m/z (x) axis labels.
#'  See eg \link[MS.Analysis]{formatDigits}. Default is \link[ggplot2]{waiver} which
#'  ensures 'standard' formatting
#' @param intensityLabelFormat defines the format of the intensity (y) axis labels.
#'  See eg \link[MS.Analysis]{formatDigits}
#' @param centroidPlot logical vector that defines whether the plot should be centroid
#'  -like (\link[ggplot2]{geom_segment} is used) or profile-like (\link[ggplot2]{geom_line}
#'  is used)
#' @param cutOff numeric vector that defines at which intensity level, below which
#'  peaks should not be shown. Default is 0.01 which means that below 1% of maximum
#'  intensity the peaks won't be shown. Currently only works for centroid(ed) spectra.
#' @param annotateMz either NA or a list of annotation objects to be used, see
#'  \link[MS.Analysis]{spectrumAnnotation}
#' @param plot.margins.default sets whether default ggplot margins are to be used
#'  (default = FALSE)
#' @param plot.margins sets the plot margin sizes/widths (4 element integer vector).
#'  Default is c(5,15,5,5)
#' @param plot.margins.units sets the units for the margin sizes/widths. Default
#'  is "point"
#' @param mzTitle defines the label for the x-axis (m/z)
#' @param intensityTitle defines the label for y-axis (intensity)
#' @param spectrumTitle sets title of the spectrum
#' @param spectrumSubtitle sets subtitle of the spectrum
#' @param spectrumCaption sets caption of the spectrum
#' @param generalTextSize factor with which to increase or decrease the size of
#'  all text elements used, except for annotateMz elements. Default is NA
#' @param generalLineWidth factor with which to increase or decrease the width of
#'  all line elements used, except for annotateMz elements. Default is NA
#' @param overrideLineWidth if the lineWidth argument is not used (NA), then if
#'  this argument is TRUE line width 0.5 x generalLineWidth is used, if FALSE then
#'  the line width will be set to 0.25 x generalLineWidth
#'
#' @return a ggplot object
#'
#' @examples
#' demoFile <- fs::path_package("extdata", "spec2.csv", package = "MS.Analysis")
#' profileSpectrum <- read.table(demoFile, sep = ",", header = TRUE)
#'
#' plotSpectrum(profileSpectrum, labelCutOff = 0.025)
#' profileSpectrum2 <- profileSpectrum
#' profileSpectrum2$mz <- profileSpectrum2$mz + 10
#' profileSpectrum2$intensity <- profileSpectrum2$intensity + 5E6
#' plotSpectrumOverlay(list(profileSpectrum, profileSpectrum2),
#'                     spectrumColors = c("black", "red"))
#' plotSpectrumOverlay(list(profileSpectrum, profileSpectrum2),
#'                     spectrumColors = c("black", "red"), mzLimits = c(500, 600))
#'
#' @export
plotSpectrumOverlay <- function(spectrumList,
                                spectrumColors = "black",
                                spectrumLineTypes = "solid",
                                spectrumAlphas = 1,
                                spectrumWidths = NA,
                                mzLimits = NULL, intensityLimits = NULL,
                                incrScaleIntensity = 0.05, scaleIntensityLocal = TRUE,
                                intensityPercentage = FALSE,
                                mzLabelFormat = ggplot2::waiver(),
                                intensityLabelFormat = ifelse(intensityPercentage,
                                                              formatDigits(0),
                                                              formatScientificDigits(4)),
                                centroidPlot = FALSE, cutOff = 0.01,
                                annotateMz = NULL,
                                plot.margins.default = FALSE,
                                plot.margins = c(5,15,5,5),
                                plot.margins.units = "points",
                                mzTitle = "m/z",
                                intensityTitle = ifelse(intensityPercentage,
                                                        "Intensity (%)",
                                                        "Intensity"),
                                spectrumTitle = ggplot2::waiver(),
                                spectrumSubtitle = ggplot2::waiver(),
                                spectrumCaption = ggplot2::waiver(),
                                generalTextSize = NA,
                                generalLineWidth = NA, overrideLineWidth = FALSE){
  generalTextSize <- ifelse(is.na(generalTextSize),
                            10,
                            generalTextSize)
  generalLineWidth <- ifelse(is.na(generalLineWidth),
                             1,
                             generalLineWidth)
  if (length(spectrumColors) == 1){
    spectrumColors <- rep(spectrumColors, length(spectrumList))
  }
  if (length(spectrumLineTypes) == 1){
    spectrumLineTypes <- rep(spectrumLineTypes, length(spectrumList))
  }
  if (length(spectrumAlphas) == 1){
    spectrumAlphas <- rep(spectrumAlphas, length(spectrumList))
  }
  if (length(spectrumWidths) == 1){
    spectrumWidths <- rep(spectrumWidths, length(spectrumList))
  }
  spectrumWidths <- map_dbl(spectrumWidths,
                            ~ifelse(is.na(.x),
                                    ifelse(overrideLineWidth, 0.5,  0.25),
                                    .x) * generalLineWidth)
  if (length(centroidPlot) == 1){
    centroidPlot <- rep(centroidPlot, length(spectrumList))
  }
  yMaxStored <- max(spectrumList[[1]]$intensity)
  if (intensityPercentage){
    spectrumList[[1]]$intensity <- (spectrumList[[1]]$intensity/yMaxStored)*100
  }
  for (counter in 2:length(spectrumList)){
    yMaxStored2 <- max(spectrumList[[counter]]$intensity)
    if (intensityPercentage){
      spectrumList[[counter]]$intensity <- (spectrumList[[counter]]$intensity/yMaxStored2)*100
    }
    yMaxStored <- max(yMaxStored, yMaxStored2)
  }
  if ((!scaleIntensityLocal) | (is.null(mzLimits))){
    maxY <- yMaxStored
  } else {
    maxY <- max(spectrumList[[1]][(spectrumList[[1]]$mz >= mzLimits[1]) & (spectrumList[[1]]$mz <= mzLimits[2]),]$intensity)
    for (counter in 2:length(spectrumList)){
      maxY <- max(c(maxY, max(spectrumList[[counter]][(spectrumList[[counter]]$mz >= mzLimits[1]) & (spectrumList[[counter]]$mz <= mzLimits[2]),]$intensity)))
    }
  }
  if (!is.null(mzLimits)){
    minMz <- mzLimits[1]
    maxMz <- mzLimits[2]
  } else {
    maxMz <- max(spectrumList[[1]]$mz)
    minMz <- min(spectrumList[[1]]$mz)
    for (counter in 2:length(spectrumList)){
      maxMz <- max(c(maxMz, max(spectrumList[[counter]]$mz)))
      minMz <- min(c(minMz, min(spectrumList[[counter]]$mz)))
    }
  }
  mzRangeSize <- maxMz - minMz
  for (counter in 1:length(spectrumList)){
    if (centroidPlot[counter] & !is.na(cutOff)){
      spectrumList[[counter]] <- spectrumList[[counter]][spectrumList[[counter]][,2] >= (cutOff*maxY),]
    }
  }
  maxY <- (1+incrScaleIntensity) * maxY
  g <- ggplot2::ggplot()
  for (counter in 1:length(spectrumList)){
    if (centroidPlot[counter]) {
      g <- g + ggplot2::geom_segment(data = spectrumList[[counter]], ggplot2::aes(x = mz, xend = mz, y = 0, yend = intensity),
                            color = spectrumColors[counter],
                            alpha = spectrumAlphas[counter],
                            linetype = spectrumLineTypes[counter],
                            linewidth = spectrumWidths[counter])
    } else {
      g <- g + ggplot2::geom_line(data = spectrumList[[counter]], ggplot2::aes(x = mz, y = intensity),
                         color = spectrumColors[counter],
                         alpha = spectrumAlphas[counter],
                         linetype = spectrumLineTypes[counter],
                         linewidth = spectrumWidths[counter])
    }
  }
  if (!is.null(mzLimits)){
    g <- g + ggplot2::scale_x_continuous(expand = c(0,0),limits = mzLimits, labels = mzLabelFormat)
  } else {
    g <- g + ggplot2::scale_x_continuous(labels = mzLabelFormat)
  }
  if (!is.null(intensityLimits)){
    g <- g + ggplot2::scale_y_continuous(expand = c(0,0), limits = intensityLimits)
  } else {
    g <- g + ggplot2::scale_y_continuous(expand = c(0,0), limits = c(0,maxY))
  }
  if (!is.null(annotateMz)){
    for (i in 1:length(annotateMz)){
      g <- annotateMz[[i]]$draw(graphObject = g, maxY = maxY, intensityPercentage = intensityPercentage)
    }
  }
  g <- g + ggplot2::theme_classic()
  if (!plot.margins.default){
    g <- g + ggplot2::theme(plot.margin = ggplot2::unit(plot.margins, plot.margins.units))
  }
  g <- g + ggplot2::labs(title = spectrumTitle, subtitle = spectrumSubtitle, caption = spectrumCaption,
                x = mzTitle, y = intensityTitle)
  g <- g + ggplot2::theme(
    axis.title.x = ifelseProper(is.na(mzTitle),
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

#' @title plotSpectrumMirror
#'
#' @description Creates a mirror plot of a list of spectra, m/z will be on the
#'  x-axis, intensity on the y-axis
#'
#' @param spectrumList list of data.frame's (spectra). Note the list can only be
#'  length 2.
#' @param spectrumColors vector specifying the colors of the different spectra,
#'  eg c("red","black"). Length 1 or 2
#' @param spectrumLineTypes vector specifying the linetypes of the different spectra.
#'  Length 1 or 2
#' @param spectrumAlphas vector specifying the alphas of the different spectra.
#'  Length 1 or 2
#' @param spectrumWidths vector specifying the linewidths of the different spectra.
#'  Length 1 or 2
#' @param mzLimits two element numeric vector. Essentially the limits of the x-axis
#'  (m/z)
#' @param intensityLimits two element numeric vector. Essentially the limits of the
#'  y-axis (intensity)
#' @param incrScaleIntensity numeric value which specifies the factor with which to
#'  increase the y-axis limit
#' @param scaleIntensityLocal logical vector which determines whether to use global
#'  intensities (FALSE) or only the intensities of m/z's which fall within the mzLimits
#'  (TRUE, default)
#' @param intensityPercentage Whether to display the intensity axis in percentages
#'  (default FALSE)
#' @param mzLabelFormat defines the format of the m/z (x) axis labels.
#'  See eg \link[MS.Analysis]{formatDigits}. Default is \link[ggplot2]{waiver}
#'  which ensures 'standard' formatting
#' @param intensityLabelFormat defines the format of the intensity (y) axis labels.
#'  See eg \link[MS.Analysis]{formatDigits}
#' @param centroidPlot logical vector that defines whether the plot should be
#'  centroid-like (\link[ggplot2]{geom_segment} is used) or profile-like (\link[ggplot2]{geom_line} is used)
#' @param cutOff numeric vector that defines at which intensity level, below which
#'  peaks should not be shown. Default is 0.01 which means that below 1% of maximum
#'  intensity the peaks won't be shown. This only works for centroid(ed) spectra.
#' @param annotateMz either NA or a list of annotation objects to be used, see
#'  \link[MS.Analysis]{spectrumAnnotation}
#' @param plot.margins.default sets whether default ggplot margins are to be used
#'  (default = FALSE)
#' @param plot.margins sets the plot margin sizes/widths (4 element integer vector).
#'  Default is c(5,15,5,5)
#' @param plot.margins.units sets the units for the margin sizes/widths. Default
#'  is "point"
#' @param mzTitle defines the label for the x-axis (m/z)
#' @param intensityTitle defines the label for y-axis (intensity)
#' @param spectrumTitle sets title of the spectrum
#' @param spectrumSubtitle sets subtitle of the spectrum
#' @param spectrumCaption sets caption of the spectrum
#' @param generalTextSize factor with which to increase or decrease the size of
#'  all text elements used, except for annotateMz elements. Default is NA
#' @param generalLineWidth factor with which to increase or decrease the width of
#'  all line elements used, except for annotateMz elements. Default is NA
#' @param overrideLineWidth if the lineWidth argument is not used (NA), then if
#'  this argument is TRUE line width 0.5 x generalLineWidth is used, if FALSE then
#'  the line width will be set to 0.25 x generalLineWidth
#'
#' @return a ggplot object
#'
#' @examples
#' demoFile <- fs::path_package("extdata", "spec2.csv", package = "MS.Analysis")
#' profileSpectrum <- read.table(demoFile, sep = ",", header = TRUE)
#'
#' profileSpectrum2 <- profileSpectrum
#' profileSpectrum2$mz <- profileSpectrum2$mz + 15
#' profileSpectrum2$intensity <- profileSpectrum2$intensity * 1.5
#' plotSpectrumMirror(
#'   list(profileSpectrum, profileSpectrum2),
#'   spectrumColors = c("black", "red")
#' )
#' plotSpectrumMirror(
#'   list(profileSpectrum, profileSpectrum2),
#'   spectrumColors = c("black", "red"),
#'   intensityLimits = c(-2E7, 2E7)
#' )
#' plotSpectrumMirror(
#'   list(profileSpectrum, profileSpectrum2),
#'   spectrumColors = c("black", "red"),
#'   intensityPercentage = TRUE
#' )
#' plotSpectrumMirror(
#'   list(profileSpectrum, profileSpectrum2),
#'   spectrumColors = c("black", "red"),
#'   intensityPercentage = TRUE,
#'   mzLimits = c(525, 575)
#' )
#'
#' @export
plotSpectrumMirror <- function(spectrumList,
                               spectrumColors = "black",
                               spectrumLineTypes = "solid",
                               spectrumAlphas = 1,
                               spectrumWidths = NA,
                               mzLimits = NULL, intensityLimits = NULL,
                               incrScaleIntensity = 0.05, scaleIntensityLocal = TRUE,
                               intensityPercentage = FALSE,
                               mzLabelFormat = ggplot2::waiver(),
                               intensityLabelFormat = ifelse(intensityPercentage,
                                                             formatDigits(0),
                                                             formatScientificDigits(4)),
                               centroidPlot = FALSE, cutOff = 0.01,
                               annotateMz = NULL,
                               plot.margins.default = FALSE,
                               plot.margins = c(5,15,5,5),
                               plot.margins.units = "points",
                               mzTitle = "m/z",
                               intensityTitle = ifelse(intensityPercentage,
                                                       "Intensity (%)",
                                                       "Intensity"),
                               spectrumTitle = ggplot2::waiver(),
                               spectrumSubtitle = ggplot2::waiver(),
                               spectrumCaption = ggplot2::waiver(),
                               generalTextSize = NA,
                               generalLineWidth = NA, overrideLineWidth = FALSE){
  generalTextSize <- ifelse(is.na(generalTextSize),
                            10,
                            generalTextSize)
  generalLineWidth <- ifelse(is.na(generalLineWidth),
                             1,
                             generalLineWidth)
  if (length(spectrumList) != 2){
    return(NA)
  }
  if (length(spectrumColors) == 1){
    spectrumColors <- rep(spectrumColors, length(spectrumList))
  }
  if (length(spectrumLineTypes) == 1){
    spectrumLineTypes <- rep(spectrumLineTypes, length(spectrumList))
  }
  if (length(spectrumAlphas) == 1){
    spectrumAlphas <- rep(spectrumAlphas, length(spectrumList))
  }
  if (length(spectrumWidths) == 1){
    spectrumWidths <- rep(spectrumWidths, length(spectrumList))
  }
  if (length(centroidPlot) == 1){
    centroidPlot <- rep(centroidPlot, length(spectrumList))
  }
  spectrumWidths <- map_dbl(spectrumWidths,
                            ~ifelse(is.na(.x),
                                    ifelse(overrideLineWidth, 0.5,  0.25),
                                    .x) * generalLineWidth)
  spectrumList[[2]]$intensity <- -spectrumList[[2]]$intensity
  yMinStored <- min(spectrumList[[2]]$intensity, na.rm = T)
  yMaxStored <- max(spectrumList[[1]]$intensity, na.rm = T)
  if (intensityPercentage){
    spectrumList[[1]]$intensity <- (spectrumList[[1]]$intensity/yMaxStored)*100
    spectrumList[[2]]$intensity <- -(spectrumList[[2]]$intensity/yMinStored)*100
  }
  maxY <- as.numeric()
  if ((!scaleIntensityLocal) | (is.null(mzLimits))){
    maxY[1] <- yMaxStored
    maxY[2] <- yMinStored
  } else {
    maxY[1] <- max(spectrumList[[1]][(spectrumList[[1]]$mz >= mzLimits[1]) & (spectrumList[[1]]$mz <= mzLimits[2]),]$intensity)
    maxY[2] <- min(spectrumList[[2]][(spectrumList[[2]]$mz >= mzLimits[1]) & (spectrumList[[2]]$mz <= mzLimits[2]),]$intensity)
  }
  if (!is.null(mzLimits)){
    minMz <- mzLimits[1]
    maxMz <- mzLimits[2]
  } else {
    maxMz <- max(c(max(spectrumList[[1]]$mz, na.rm = TRUE), max(spectrumList[[2]]$mz, na.rm = TRUE)), na.rm = T)
    minMz <- min(c(min(spectrumList[[1]]$mz, na.rm = TRUE), min(spectrumList[[2]]$mz, na.rm = TRUE)), na.rm = T)
  }
  mzRangeSize <- maxMz - minMz
  for (counter in 1:2){
    if (centroidPlot[counter] & !is.na(cutOff)){
      spectrumList[[counter]] <- spectrumList[[counter]][spectrumList[[counter]][,2] >= (cutOff*maxY[counter]),]
    }
  }
  maxY <- (1+incrScaleIntensity) * maxY
  g <- ggplot2::ggplot()
  for (counter in 1:length(spectrumList)){
    if (centroidPlot[counter]) {
      g <- g + ggplot2::geom_segment(data = spectrumList[[counter]], ggplot2::aes(x = mz, xend = mz, y = 0, yend = intensity),
                            color = spectrumColors[counter],
                            alpha = spectrumAlphas[counter],
                            linetype = spectrumLineTypes[counter],
                            linewidth = spectrumWidths[counter])
    } else {
      g <- g + ggplot2::geom_line(data = spectrumList[[counter]], ggplot2::aes(x = mz, y = intensity),
                         color = spectrumColors[counter],
                         alpha = spectrumAlphas[counter],
                         linetype = spectrumLineTypes[counter],
                         linewidth = spectrumWidths[counter])
    }
  }
  if (!is.null(mzLimits)){
    g <- g + ggplot2::scale_x_continuous(expand = c(0,0),limits = mzLimits, labels = mzLabelFormat)
  } else {
    g <- g + ggplot2::scale_x_continuous(labels = mzLabelFormat)
  }
  if (!is.null(intensityLimits)){
    g <- g + ggplot2::scale_y_continuous(expand = c(0,0), limits = c(intensityLimits[1]*(1+incrScaleIntensity), intensityLimits[2]*(1+incrScaleIntensity)))
  } else {
    if (!intensityPercentage){
      g <- g + ggplot2::scale_y_continuous(expand = c(0,0), limits = c(maxY[2], maxY[1]))
    } else {
      g <- g + ggplot2::scale_y_continuous(expand = c(0,0), limits = c(-100, 100))
    }
  }
  if (!is.null(annotateMz)){
    for (i in 1:length(annotateMz)){
      g <- annotateMz[[i]]$draw(graphObject = g, minY = maxY[2], maxY = maxY[1], intensityPercentage = intensityPercentage)
    }
  }
  g <- g + ggplot2::theme_classic()
  if (!plot.margins.default){
    g <- g + ggplot2::theme(plot.margin = ggplot2::unit(plot.margins, plot.margins.units))
  }
  g <- g + ggplot2::labs(title = spectrumTitle, subtitle = spectrumSubtitle, caption = spectrumCaption,
                x = mzTitle, y = intensityTitle)
  g <- g + ggplot2::theme(
    axis.title.x = ifelseProper(is.na(mzTitle),
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
