# library(jsonlite)
# library(jsonify)
# library(ggtext)

# ---- ion series ----

#' @title createIonSeries
#'
#' @description function to generate a series of labels for ions
#'
#' @note all arguments, except where indicated, are character vectors
#'
#' @param whichSeries letter or name assigned to the series, eg y for y-ions,
#'  b for b-ions, etc
#' @param theSeries integer vector, eg 1:10 to specify the numbers of the ion series
#' @param whichCharge integer vector, gives the charge state (1,2,3,...)
#' @param positive logical vector. If TRUE than positive ions series is generated,
#'  else negative ion series
#' @param chargeConnectorPre character vector placed between the charge and the
#'  chargeConnector
#' @param chargeConnector character vector to connect the name + charge to the
#'  ion series number
#' @param chargeConnectorPost character vector placed between the chargeConnector
#'  and the ion series number
#' @param seriesConnectorPre character vector placed just before the series
#'  number
#' @param seriesConnectorPost character vector placed just after the series
#'  number
#'
#' @return a character vector or NA
#'
#' @examples
#' createIonSeries(theSeries = 1:5)
#' createIonSeries(whichSeries = "b", theSeries = 1:5, whichCharge = 2)
#'
#' @export
createIonSeries <- function(whichSeries = "y",  theSeries = NA, whichCharge = 1, positive = TRUE,
                            chargeConnectorPre = "", chargeConnector = "", chargeConnectorPost = "",
                            seriesConnectorPre = "_", seriesConnectorPost = ""){
  if (!identical(theSeries,NA)){
    whichCharge <- ifelse(whichCharge == 1,
                          ifelse(is.na(positive),
                                 "",
                                 ifelse(positive,
                                        "+","-")),
                          paste(c(as.character(whichCharge), chargeConnector, ifelse(is.na(positive),
                                                                                     "",
                                                                                     ifelse(positive,
                                                                                            "+","-"))), collapse = ""))
    return(unlist(lapply(theSeries,function(x){paste(c(whichSeries,
                                                       chargeConnectorPre, whichCharge, chargeConnectorPost,
                                                       seriesConnectorPre, as.character(x), seriesConnectorPost), collapse = "")})))
  } else {
    return(NA)
  }
}

#' @title createIonSeriesMD
#'
#' @description function to generate a series of markdown/html-style labels for
#'  ions
#'
#' @note essentially a wrapper for \link[MS.Analysis]{createIonSeries}
#'
#' @param whichSeries letter or name assigned to the series, eg y for y-ions,
#'  b for b-ions, etc
#' @param theSeries integer vector, eg 1:10 to specify the numbers of the ion series
#' @param whichCharge integer vector, gives the charge state (1,2,3,...)
#' @param positive logical vector. If TRUE than positive ions series is generated,
#'  else negative ion series
#'
#' @returns a character vector or NA
#'
#' @examples
#' createIonSeriesMD(theSeries = 1:5)
#' createIonSeriesMD(whichSeries = "b", theSeries = 1:5, whichCharge = 2)
#'
#' @export
createIonSeriesMD <- function(whichSeries = "y",  theSeries = NA, whichCharge = 1, positive = TRUE){
  if (!identical(theSeries,NA)){
    chargeConnectorPre <- "<sup>"
    chargeConnector <- ""
    chargeConnectorPost <- "</sup>"
    seriesConnectorPre <- "<sub>"
    seriesConnectorPost <- "</sub>"
    whichCharge <- ifelse(whichCharge == 1,
                          ifelse(is.na(positive),
                                 "",
                                 ifelse(positive,
                                        "+","-")),
                          paste(c(as.character(whichCharge), chargeConnector, ifelse(is.na(positive),
                                                                                     "",
                                                                                     ifelse(positive,
                                                                                            "+","-"))), collapse = ""))
    return(unlist(lapply(theSeries,function(x){paste(c(whichSeries,
                                                       seriesConnectorPre, as.character(x), seriesConnectorPost,
                                                       chargeConnectorPre, whichCharge, chargeConnectorPost), collapse = "")})))
  } else {
    return(NA)
  }
}

# ---- annotation ----

#' @title annotation
#'
#' @description
#'  R6 Class base for dealing with annotations in graphs, originally intended for
#'  semi-automatic peptide spectrum annotation (y- & b-ions)
#'
#' @examples
#' demoFile <- fs::path_package("extdata", "spec1.csv", package = "MS.Analysis")
#' centroidSpectrum <- read.table(demoFile, sep = ",", header = TRUE)
#' plotSpectrum(
#'   centroidSpectrum,
#'   centroidPlot = TRUE,
#'   mzLimits = c(100, 1000),
#'   labelColor = "red",
#'   intensityPercentage = TRUE
#' )
#' thePeptide <- massSpectrometryR::peptide$new(sequence = "LGGNEQVTR")
#' thePeptide$fragments()$yions
#' theAnnotation <- annotation$new(
#'   nameX = "mz",
#'   nameY = "intensity",
#'   x = thePeptide$fragments()$yions,
#'   labelX = createIonSeries(theSeries = 1:(thePeptide$length-1))
#' )
#'
#' theAnnotation
#' plotSpectrum(
#'   centroidSpectrum,
#'   centroidPlot = TRUE,
#'   mzLimits = c(100, 1000),
#'   labelColor = "red",
#'   intensityPercentage = TRUE,
#'   incrScaleIntensity = 0.25,
#'   annotateMz = list(theAnnotation)
#' )
#' theAnnotation$color <- "blue"
#' plotSpectrum(
#'   centroidSpectrum,
#'   centroidPlot = TRUE,
#'   mzLimits = c(100, 1000),
#'   labelColor = "red",
#'   intensityPercentage = TRUE,
#'   incrScaleIntensity = 0.25,
#'   annotateMz = list(theAnnotation)
#' )
#' theAnnotation$check(dataframe = centroidSpectrum)
#' theAnnotation
#' plotSpectrum(
#'   centroidSpectrum,
#'   centroidPlot = TRUE,
#'   mzLimits = c(100, 1000),
#'   labelColor = "red",
#'   intensityPercentage = TRUE,
#'   incrScaleIntensity = 0.25,
#'   annotateMz = list(theAnnotation)
#' )
#' theAnnotation$labelType <- 2
#' plotSpectrum(
#'   centroidSpectrum,
#'   centroidPlot = TRUE,
#'   mzLimits = c(100, 1000),
#'   labelColor = "red",
#'   intensityPercentage = TRUE,
#'   incrScaleIntensity = 0.25,
#'   annotateMz = list(theAnnotation)
#' )
#' theAnnotation$labelType <- 3
#' plotSpectrum(
#'   centroidSpectrum,
#'   centroidPlot = TRUE,
#'   mzLimits = c(100, 1000),
#'   labelColor = "red",
#'   intensityPercentage = TRUE,
#'   incrScaleIntensity = 0.5,
#'   annotateMz = list(theAnnotation)
#' )
#' # using markdown/html-type labels
#' theAnnotation <- annotation$new(
#' nameX = "mz",
#' nameY = "intensity",
#' x = thePeptide$fragments()$yions,
#' labelX = createIonSeriesMD(theSeries = 1:(thePeptide$length-1))
#' )
#' theAnnotation$color <- "blue"
#' theAnnotation$check(dataframe = centroidSpectrum)
#' theAnnotation
#' plotSpectrum(
#'   centroidSpectrum,
#'   centroidPlot = TRUE,
#'   mzLimits = c(100, 1000),
#'   labelColor = "red",
#'   intensityPercentage = TRUE,
#'   incrScaleIntensity = 0.25,
#'   annotateMz = list(theAnnotation)
#' )
#'
#' @export
annotation <- R6::R6Class("annotation",
                          private = list(
                            name_ = NA,
                            # internal variable for storing the (optional) name
                            #  for the annotation object

                            #' @description internal function to get all the (public)
                            #'  variables into a list (for saving purposes)
                            #'
                            #' @return a named list
                            getList = function(){
                              return(list(
                                nameX = self$nameX,
                                nameY = self$nameY,
                                x = self$x,
                                labelX = self$labelX,
                                labelNumber = self$labelNumber,
                                labelWhere = self$labelWhere,
                                labelWhereAbsolute = self$labelWhereAbsolute,
                                labelColor = self$labelColor,
                                labelSize = self$labelSize,
                                labelAngle = self$labelAngle,
                                labelConnect = self$labelConnect,
                                labelConnectColor = self$labelConnectColor,
                                labelConnectAlpha = self$labelConnectAlpha,
                                labelBetween = self$labelBetween,
                                labelNudgeY = self$labelNudgeY,
                                axisConnect = self$axisConnect,
                                axisConnectColor = self$axisConnectColor,
                                axisConnectAlpha = self$axisConnectAlpha,
                                axisConnectWidth = self$axisConnectWidth,
                                axisConnectLevel = self$axisConnectLevel,
                                axisConnectWhere = self$axisConnectWhere,
                                axisConnectType = self$axisConnectType))
                            },
                            #' @description internal function to set all the (public) variables
                            #'  from a named list (for loading purposes)
                            setList = function(theList = NA){
                              if (!identical(theList, NA)){
                                self$nameX <- theList$nameX
                                self$nameY <- theList$nameY
                                self$x <- theList$x
                                self$labelX <- theList$labelX
                                self$labelNumber <- theList$labelNumber
                                self$labelWhere <- theList$labelWhere
                                self$labelWhereAbsolute <- theList$labelWhereAbsolute
                                self$labelColor <- theList$labelColor
                                self$labelSize <- theList$labelSize
                                self$labelAngle <- theList$labelAngle
                                self$labelConnect <- theList$labelConnect
                                self$labelConnectColor <- theList$labelConnectColor
                                self$labelConnectAlpha <- theList$labelConnectAlpha
                                self$labelBetween <- labelBetween
                                self$labelNudgeY <- labelNudgeY
                                self$axisConnect <- theList$axisConnect
                                self$axisConnectColor <- theList$axisConnectColor
                                self$axisConnectAlpha <- theList$axisConnectAlpha
                                self$axisConnectWidth <- theList$axisConnectWidth
                                self$axisConnectLevel <- theList$axisConnectLevel
                                self$axisConnectWhere <- theList$axisConnectWhere
                                self$axisConnectType <- theList$axisConnectType
                              }
                            }
                          ),
                          public = list(
                            #' @field nameX character vector specifying the name of the x-axis variable (eg 'mz')
                            nameX = NA,
                            #' @field nameY character vector specifying the name of the y-axis variable (eg 'intensity')
                            nameY = NA,
                            #' @field x numeric vector of the x-values used for the annotation
                            x = NA,
                            #' @field labelX character vector of the labels to be used for the annotation
                            labelX = NA,
                            #' @field labelNumber each label has a asscociated number 1..
                            #'  When a label is removed, the number is also removed. This way we can keep track of
                            #'  which 'original' labels are still there after e.g. 'check'
                            labelNumber = NA,
                            #' @field labelWhere numeric vector: position of the labels (essentially the y-values for the placement of the labels).
                            #'  If labelWhereAbsolute = FALSE, then 1 = maximum y-axis
                            labelWhere = 0.9,
                            #' @field labelWhereAbsolute logical vector: labelWhere positions are absolute?
                            labelWhereAbsolute = FALSE,
                            #' @field labelColor color of the label
                            labelColor = "red",
                            #' @field labelSize size of the label
                            labelSize = 3,
                            #' @field labelAngle angle (degrees) of display for the label
                            labelAngle = 30,
                            #' @field labelConnect logical vector that specifies whether the labels should be
                            #'  'connected' with a line between them
                            labelConnect = TRUE,
                            #' @field labelConnectColor color of the label connect line(s)
                            labelConnectColor = "red",
                            #' @field labelConnectAlpha alpha of the label connect line(s)
                            labelConnectAlpha = 0.25,
                            #' @field labelBetween logical vector, determines whether the labels should be put between the x-values (TRUE) or on the x-values (FALSE).
                            #'  If TRUE then the first label is ignored and only the labels starting from 2 are used
                            labelBetween = FALSE,
                            #' @field labelNudgeY amount with which to increase the labelWhere values. Can be used to up or down the label slightly when displaying
                            labelNudgeY = 0,
                            #' @field axisConnect logical vector that specifies whether the label should be
                            #'  (vertically) connected to the (x-)axis by a line
                            axisConnect = TRUE,
                            #' @field axisConnectColor color of axis connector line
                            axisConnectColor = "red",
                            #' @field axisConnectAlpha alpha of the axis connector line
                            axisConnectAlpha = 0.25,
                            #' @field axisConnectWidth linewidth of the axis connector line
                            axisConnectWidth = 1,
                            #' @field axisConnectLevel NA means connect to axis, if not NA then axisConnectType can be set
                            #'  axisConnectLevel defines to where (how far down) the axisConnect lines should reach
                            axisConnectLevel = NA,
                            #' @field axisConnectWhere this is how high above the connect, is relative to max Y-axis. To place labels close to their peaks,
                            #'  this option should only be set/changed by annotation field labelType
                            axisConnectWhere = 0.05,
                            #' @field axisConnectType if axisConnectLevel is:
                            #'  1: means all the way down
                            #'  2: means up till axisConnectLevel above int of m/z
                            #'  3: overrides all axisConnect Settings, labels put at axisConnectLevel above int of m/z and axisConnect lines
                            #'  are removed (2 & 3 only really work for 'checked') annotations. Since the axisConnectType, axisConnectLevel
                            #'  and labelConnect 'work' together: always use the object's labelType field to change axisConnectType since
                            #'  it will 'take care' of all settings together. AxisConnectLevel is also influenced by the (public) check
                            #'  method (it attempts to place set the values above the m/z's found)
                            axisConnectType = 1,

                            #' @description Initializes an annotation object
                            #'
                            #' @param annotationName set the internal name of the object
                            #' @param nameX sets the name of the x-axis variable (eg m/z or retention time)
                            #' @param nameY sets the name of the y-axis variable (eg intensity)
                            #' @param x x-values (location on x-axis) for the labels
                            #' @param labelX the labels to be used for the annotation. Should have same length
                            #'  as argument 'x'. If NA, then it will be as.character(x)
                            #' @param labelWhere y-axis height where axis labels are to be placed. Expressed as
                            #'  a fraction of the y-axis maximum (default 0.9)
                            #' @param labelWhereAbsolute labelWhere positions are absolute? (default: FALSE)
                            #' @param labelColor sets the color of the labels
                            #' @param labelSize sets the size of the labels
                            #' @param labelAngle sets the angle of display of the labels
                            #' @param labelConnect logical vector which defines if the labels are connected
                            #'  horizontally ('along' the x-axis)
                            #' @param labelConnectColor sets the color of the connection lines between the labels
                            #' @param labelConnectAlpha sets the alpha of the connection lines between the labels
                            #' @param labelBetween logical vector. If TRUE then labels are placed between connection points
                            #' @param labelNudgeY numeric vector, to have the labels drawn a little higher or lower in the final graph
                            #' @param axisConnect specifies if the labels should have axis connect lines
                            #' @param axisConnectColor sets the color of the axis connector line(s)
                            #' @param axisConnectAlpha sets the alpha of the axis connector line(s)
                            #' @param axisConnectWidth sets the width of the axis connector line(s)
                            #' @param axisConnectLevel defines to where (how far down) the axisConnect lines should reach
                            #' @param axisConnectWhere this is how high above the connect, is relative to max Y-axis.
                            #'  To place labels close to their peaks, this option should only be set/changed by
                            #'  annotation field labelType
                            #' @param axisConnectType at this moment can be 1,2 or 3. 1 means connect all the way down,
                            #'  2 means up till axisConnectLevel above int of m/z. 3 = overrides all axisConnect Settings,
                            #'  labels put at axisConnectLevel above int of m/z and axisConnect lines
                            #'
                            #' @returns a new annotation object
                            #'
                            #' @export
                            initialize = function(annotationName = NA,
                                                  nameX = NA, nameY = NA,
                                                  x = NA, labelX = NA,
                                                  labelWhere = 0.9, labelWhereAbsolute = FALSE,
                                                  labelColor = "red", labelSize = 1, labelAngle = 30,
                                                  labelConnect = TRUE, labelConnectColor = "red", labelConnectAlpha = 0.25,
                                                  labelBetween = FALSE, labelNudgeY = 0,
                                                  axisConnect = TRUE, axisConnectColor = "red", axisConnectAlpha = 0.25,
                                                  axisConnectWidth = 0.5, axisConnectLevel = NA, axisConnectWhere = 0.05, axisConnectType = 1){
                              if (!identical(x,NA) & !identical(nameX, NA) & !identical(nameY,NA)){
                                if (length(labelWhere) == 1){
                                  labelWhere <- rep(labelWhere, length(x))
                                } else {
                                  stopifnot(length(labelWhere) == length(x))
                                }
                                if (identical(labels, NA)){
                                  labels = as.character(x)
                                }
                                private$name_ <- annotationName
                                self$nameX <- nameX
                                self$nameY <- nameY
                                self$x <- x
                                self$labelX <- labelX
                                self$labelNumber <- 1:length(labelX)
                                self$labelWhere <- labelWhere
                                self$labelWhereAbsolute <- labelWhereAbsolute
                                self$labelColor <- labelColor
                                self$labelSize <- labelSize
                                self$labelAngle <- labelAngle
                                self$labelConnect <- labelConnect
                                self$labelConnectColor <- labelConnectColor
                                self$labelConnectAlpha <- labelConnectAlpha
                                self$labelBetween <- labelBetween
                                self$labelNudgeY <- labelNudgeY
                                self$axisConnect <- axisConnect
                                self$axisConnectColor <- axisConnectColor
                                self$axisConnectAlpha <- axisConnectAlpha
                                self$axisConnectWidth <- axisConnectWidth
                                self$axisConnectLevel <- axisConnectLevel
                                self$axisConnectWhere <- axisConnectWhere
                                self$axisConnectType <- axisConnectType
                              } else {
                                # do nothing, leave at standard values
                              }
                              invisible()
                            },
                            #' @description
                            #' For printing purposes: prints the name of the annotation (if not NA)
                            #'  and a table of the labels
                            #'
                            #' @note no arguments, the function takes care of printing
                            #'
                            #' @export
                            print = function(){
                              if (!is.na(self$name)){
                                cat(paste(c("Annotation: ",self$name,"\n"), collapse = ""))
                              }
                              print(self$table)
                            },
                            #' @description
                            #'  checks whether the x-axis coordinates have 'sufficient' y-value (intensity) labels
                            #'  if not, then the label is dropped from the object
                            #'
                            #' @param dataframe data.frame with at least the x-column (nameX) and y-column (nameY)
                            #' @param toleranceX two element numeric vecto: x-axis left tolerance (m/z) and right tolerance (mz)
                            #' @param relativeCutOff is the yCutOff relative (fraction of maximum y-value (intensity))
                            #' @param yCutOff y-axis cut off. Y-axis value (intensity) should be above
                            #'
                            #' @returns the object itself (invisible)
                            #'
                            #' @export
                            check = function(dataframe = NULL,
                                             toleranceX = c(0.1, 0.1),
                                             relativeCutOff = TRUE,
                                             yCutOff = ifelse(relativeCutOff, 0.001, 10)){
                              if (!is.null(dataframe)){
                                dataframe <- as.data.frame(dataframe)
                                if (relativeCutOff){
                                  yCutOff <- yCutOff * max(dataframe[,self$nameY], na.rm = TRUE)
                                }
                                if (yCutOff > 0 ){
                                  dataframe <- dataframe[dataframe[,self$nameY] >= yCutOff,]
                                }
                                if (length(self$labelWhere) != length(self$labelX)){
                                  self$labelWhere <- rep(self$labelWhere[1], length(self$labelX))
                                }
                                remove <- rep(FALSE, length(self$x))
                                stayY <- rep(0,length(self$x))
                                for (cntr in 1:length(self$x)){
                                  tempY <- dataframe[
                                    dataframe[, self$nameX] >= (self$x[cntr] - toleranceX[1]) & dataframe[, self$nameX] <= (self$x[cntr] + toleranceX[2]), self$nameY]
                                  if (length(tempY)<1){
                                    remove[cntr] <- TRUE
                                  } else {
                                    maxY <- max(tempY, na.rm = TRUE)
                                    if (maxY < yCutOff){
                                      remove[cntr] <- TRUE
                                    } else {
                                      stayY[cntr] <- maxY
                                    }
                                  }
                                }
                                # remove non-detected labels
                                if (length(remove)>0){
                                  self$x <- self$x[!remove]
                                  self$labelX <- self$labelX[!remove]
                                  self$labelNumber <- self$labelNumber[!remove]
                                  self$labelWhere <- self$labelWhere[!remove]
                                  if (length(self$labelColor)>1){
                                    self$labelColor <- self$labelColor[!remove]
                                  }
                                  # add intensity levels for detected ones
                                  self$axisConnectLevel <- stayY[!remove]
                                }
                              }
                              invisible()
                            },
                            #' @description
                            #'  Adds the annotation graphics to a ggplot object
                            #'
                            #' @note plotSpectrum & plotChromatogram have internal code to use this function but can also be used 'manually'
                            #'
                            #' @param graphObject ggplot object to add the annotation to, easiest if it's the output
                            #'  of plotSpectrum or plotChromatogram
                            #' @param minY minimum intensity value for the ggplot object
                            #' @param maxY maximum intensity value for the ggplot object
                            #' @param yMax when working with percentage y-scales, this argument is the original
                            #'  intensity value of the maximum y-value (not percentage)
                            #' @param intensityPercentage Whether the intensity axis is displayed in percentages
                            #'  (default FALSE)
                            #'
                            #' @returns ggplot object
                            #'
                            #' @export
                            draw = function(graphObject, minY = 0, maxY = NA, yMax = NA, intensityPercentage = FALSE){
                              if (!identical(maxY, NA)){
                                if (self$axisConnect){
                                  if (self$axisConnectType == 1){
                                    graphObject <- graphObject +
                                      ggplot2::annotate("linerange",x = self$x, ymin = rep(minY, length(self$x)),
                                                        ymax = ifelseProper(self$labelWhereAbsolute, self$labelWhere, self$labelWhere * maxY),
                                                        linewidth = self$axisConnectWidth,
                                                        color = self$axisConnectColor, alpha = self$axisConnectAlpha)
                                  } else {   # atm anything other than 1
                                    ymaxs <- ifelseProper(self$labelWhereAbsolute, self$labelWhere, self$labelWhere * maxY)
                                    if (intensityPercentage){
                                      ymins <- ((100*(self$axisConnectLevel/yMax)) + 100*(self$axisConnectWhere))
                                    } else {
                                      ymins <- self$axisConnectLevel + (self$axisConnectWhere * ymaxs)
                                    }
                                    graphObject <- graphObject +
                                      ggplot2::annotate("linerange",x = self$x, ymin = ymins,
                                                        ymax = ymaxs, linewidth = self$axisConnectWidth,
                                                        color = self$axisConnectColor, alpha = self$axisConnectAlpha)
                                  }
                                }
                                if (self$labelConnect){
                                  if (length(self$x) > 1){
                                    graphObject <- graphObject +
                                      ggplot2::annotate("path",x = self$x, y = ifelseProper(self$labelWhereAbsolute, self$labelWhere, self$labelWhere * maxY),
                                                        linewidth = self$axisConnectWidth,
                                                        color = self$labelConnectColor,
                                                        alpha = self$labelConnectAlpha)
                                  }
                                }
                                if (self$axisConnectType == 3){
                                  if (intensityPercentage){
                                    ymaxs <- ((100*(self$axisConnectLevel/yMax)) + 100*(self$axisConnectWhere))
                                  } else {
                                    ymaxs <- self$axisConnectLevel + (self$axisConnectWhere * maxY)
                                  }
                                  rtdf <- data.frame(x = self$x,
                                                     y = ymaxs,
                                                     label = self$labelX,
                                                     size = self$labelSize,
                                                     color = self$labelColor)
                                  for (counter in 1:nrow(rtdf)){
                                    rtdf$label[counter] <-  paste(c("<span style='font-size:",round(9*rtdf$size[counter]),
                                                                    "pt; color:",rtdf$color[counter],"'>",rtdf$label[counter],"</span>"), collapse = "")
                                  }
                                  colnames(rtdf)[1:2] <- c(self$nameX, self$nameY)
                                  graphObject <- graphObject +
                                    ggtext::geom_richtext(data = rtdf, ggplot2::aes(x=!!sym(self$nameX), y= !!sym(self$nameY),
                                                                                    label = label),
                                                          label.size = 0,  angle = self$labelAngle,
                                                          alpha = 0.9, fill = "white", label.color = NA)
                                } else {
                                  if (!self$labelBetween){
                                    theLabelY <- ifelseProper(self$labelWhereAbsolute, self$labelWhere, self$labelWhere * maxY)
                                    theLabelX <- self$x
                                    theLabel <- self$labelX
                                  } else {
                                    theLabelY <- as.numeric()
                                    theLabelX <- as.numeric()
                                    for (counterx in 1:(length(self$labelX)-1)){
                                      theLabelX[counterx] <- mean(c(self$x[counterx], self$x[counterx+1]), na.rm = T)
                                      theLabelY[counterx] <- mean(c(ifelseProper(self$labelWhereAbsolute, self$labelWhere[counterx], self$labelWhere[counterx] * maxY),
                                                                    ifelseProper(self$labelWhereAbsolute, self$labelWhere[counterx+1], self$labelWhere[counterx+1] * maxY)), na.rm = T) + self$labelNudgeY
                                    }
                                    thelabelPos <-    ifelseProper(self$labelWhereAbsolute, self$labelWhere, self$labelWhere * maxY)
                                    theLabel <- self$labelX[-length(self$labelX)]
                                  }
                                  if (!self$labelBetween){
                                    rtdf <- data.frame(x = self$x,
                                                       y = ifelseProper(self$labelWhereAbsolute, self$labelWhere, self$labelWhere * maxY),
                                                       label = self$labelX,
                                                       size = self$labelSize,
                                                       color = self$labelColor)
                                  } else {
                                    rtdf <- data.frame(x = theLabelX,
                                                       y = theLabelY,
                                                       label = theLabel,
                                                       size = self$labelSize,
                                                       color = self$labelColor)
                                  }
                                  for (counter in 1:nrow(rtdf)){
                                    rtdf$label[counter] <-  paste(c("<span style='font-size:",round(9*rtdf$size[counter]),
                                                                    "pt; color:",rtdf$color[counter],"'>",rtdf$label[counter],"</span>"), collapse = "")
                                  }
                                  colnames(rtdf)[1:2] <- c(self$nameX, self$nameY)
                                  graphObject <- graphObject +
                                    ggtext::geom_richtext(data = rtdf, ggplot2::aes(x=!!sym(self$nameX), y= !!sym(self$nameY),
                                                                                    label = label),
                                                          label.size = 0, angle = self$labelAngle,
                                                          alpha = 0.9, fill = "white", label.color = NA)
                                }
                              }
                              return(graphObject )
                            }
                          ),
                          active = list(
                            #' @field name gets/sets the name of the object
                            name = function(value){
                              if (missing(value)){
                                return(private$name_)
                              } else {
                                private$name_ <- value
                              }
                            },
                            #' @field table provides a data.frame of the x-axis values and labels
                            #'  (read only)
                            table = function(value){
                              if (missing(value)){
                                result <- data.frame(mz = self$x,
                                                     intensity = self$axisConnectLevel,
                                                     labelWhere = self$labelWhere,
                                                     label = self$labelX,
                                                     labelNr = self$labelNumber)
                                colnames(result)[1] <- self$nameX
                                colnames(result)[2] <- self$nameY
                                return(result)
                              } else {
                                # do nothing, read only
                              }
                            },
                            #' @field labelNr returns the label numbers
                            #'  (read only)
                            labelNr = function(value){
                              if (missing(value)){
                                return(self$labelNumber)
                              } else {
                                # do nothing, read only
                              }
                            },
                            #' @field  labels gets/sets the labels in the object
                            #' @note use with care, no safeguards (yet)
                            labels = function(value){
                              if (missing(value)){
                                return(self$labelX)
                              } else {
                                self$labelX <- value
                              }
                            },
                            #' @field y gets/sets the y-positions in the object
                            #' @note use with care, no safeguards (yet)
                            y = function(value){
                              if (missing(value)){
                                return(self$axisConnectLevel)
                              } else {
                                self$axisConnectLevel <- value
                              }
                            },
                            #' @field color returns the axisConnectColor, labelColor and labelConnectColor as vector
                            #'  can be set as single value or 3 element vector
                            color = function(value){
                              if (missing(value)){
                                if ((self$axisConnectColor == self$labelColor) & (self$axisConnectColor == self$labelConnectColor)){
                                  return(self$axisConnectColor)
                                } else {
                                  return(c(self$axisConnectColor,
                                           self$labelColor,
                                           self$labelConnectColor))
                                }
                              } else {
                                if (length(value) == 1){
                                  self$axisConnectColor <- value
                                  self$labelColor <- value
                                  self$labelConnectColor <- value
                                } else {
                                  self$axisConnectColor <- value[1]
                                  self$labelColor <- value[2]
                                  self$labelConnectColor <- value[3]
                                }
                              }
                            },
                            #' @field labelType gets/sets the axisConnectType
                            #'  When set it takes care of axisConnect & labelConnect at the same time
                            labelType = function(value){
                              if (missing(value)){
                                return(self$axisConnectType)
                              } else {
                                if (value > 1){
                                  if (!identical(self$axisConnectLevel, NA)){
                                    self$axisConnectType <- value
                                    if (value > 2){
                                      self$axisConnect <- FALSE
                                      self$labelConnect <- FALSE
                                    } else {
                                      self$axisConnect <- TRUE
                                      self$labelConnect <- TRUE
                                    }
                                  } else {
                                    warning("No axis connect level values, axis connect type was not changed.")
                                  }
                                } else {
                                  self$axisConnectType <- value
                                  self$axisConnect <- TRUE
                                  self$labelConnect <- TRUE
                                }
                              }
                            }
                          )
)

# ---- Spectrum Annotation ----

#' @title spectrumAnnotation
#'
#' @description
#'  R6 Class dealing with mass spectrometry annotation data in an organised manner.
#'   This class is a descendant of \link[MS.Analysis]{annotation}
#'
#' @note currently not much more of a convenience 'wrapper' around \link[MS.Analysis]{annotation}
#'
#' @examples
#' demoFile <- fs::path_package("extdata", "spec1.csv", package = "MS.Analysis")
#' centroidSpectrum <- read.table(demoFile, sep = ",", header = TRUE)
#' plotSpectrum(
#'   centroidSpectrum,
#'   centroidPlot = TRUE,
#'   mzLimits = c(100, 1000),
#'   labelColor = "red",
#'   intensityPercentage = TRUE
#' )
#' thePeptide <- massSpectrometryR::peptide$new(sequence = "LGGNEQVTR")
#' thePeptide$fragments()$yions
#' theAnnotation <- spectrumAnnotation$new(
#'   mzs = thePeptide$fragments()$yions,
#'   labels = createIonSeries(theSeries = 1:(thePeptide$length-1))
#' )
#' theAnnotation
#' plotSpectrum(
#'   centroidSpectrum,
#'   centroidPlot = TRUE,
#'   mzLimits = c(100, 1000),
#'   labelColor = "red",
#'   intensityPercentage = TRUE,
#'   incrScaleIntensity = 0.25,
#'   annotateMz = list(theAnnotation)
#' )
#' theAnnotation$color <- "blue"
#' plotSpectrum(
#'   centroidSpectrum,
#'   centroidPlot = TRUE,
#'   mzLimits = c(100, 1000),
#'   labelColor = "red",
#'   intensityPercentage = TRUE,
#'   incrScaleIntensity = 0.25,
#'   annotateMz = list(theAnnotation)
#' )
#' theAnnotation$check(spectrum = centroidSpectrum)
#' theAnnotation
#' plotSpectrum(
#'   centroidSpectrum,
#'   centroidPlot = TRUE,
#'   mzLimits = c(100, 1000),
#'   labelColor = "red",
#'   intensityPercentage = TRUE,
#'   incrScaleIntensity = 0.25,
#'   annotateMz = list(theAnnotation)
#' )
#' theAnnotation$labelType <- 2
#' plotSpectrum(
#'   centroidSpectrum,
#'   centroidPlot = TRUE,
#'   mzLimits = c(100, 1000),
#'   labelColor = "red",
#'   intensityPercentage = TRUE,
#'   incrScaleIntensity = 0.25,
#'   annotateMz = list(theAnnotation)
#' )
#' theAnnotation$labelType <- 3
#' plotSpectrum(
#'   centroidSpectrum,
#'   centroidPlot = TRUE,
#'   mzLimits = c(100, 1000),
#'   labelColor = "red",
#'   intensityPercentage = TRUE,
#'   incrScaleIntensity = 0.5,
#'   annotateMz = list(theAnnotation)
#' )
#' # using markdown/html-type labels
#' theAnnotation <- spectrumAnnotation$new(
#'   mzs = thePeptide$fragments()$yions,
#'   labels = createIonSeriesMD(theSeries = 1:(thePeptide$length-1))
#' )
#' theAnnotation$color <- "blue"
#' theAnnotation$check(spectrum = centroidSpectrum)
#' theAnnotation
#' plotSpectrum(
#'   centroidSpectrum,
#'   centroidPlot = TRUE,
#'   mzLimits = c(100, 1000),
#'   labelColor = "red",
#'   intensityPercentage = TRUE,
#'   incrScaleIntensity = 0.25,
#'   annotateMz = list(theAnnotation)
#' )
#'
#' @export
spectrumAnnotation <- R6::R6Class("spectrumAnnotation",
                                  inherit = annotation,
                                  public = list(
                                    #' @description Initializes an spectrum annotation object
                                    #'
                                    #' @param annotationName set the internal name of the object
                                    #' @param nameX sets the name of the x-axis variable (eg m/z or retention time)
                                    #' @param nameY sets the name of the y-axis variable (eg intensity)
                                    #' @param mzs m/z-values (location on x-axis) for the labels
                                    #' @param labels the labels to be used for the annotation. Should have same length
                                    #'  as argument mzs. If NA, then it will be as.character(mzs)
                                    #' @param levelWhere y-axis height where axis labels are to be placed. Expressed as
                                    #'  a fraction of the y-axis maximum (default 0.9)
                                    #' @param levelWhereAbsolute labelWhere positions are absolute? (default: FALSE)
                                    #' @param labelColor sets the color of the labels
                                    #' @param labelSize sets the size of the labels
                                    #' @param labelAngle sets the angle of display of the labels
                                    #' @param labelConnect logical vector which defines if the labels are connected
                                    #'  horizontally ('along' the x-axis)
                                    #' @param labelConnectColor sets the color of the connection lines between the labels
                                    #' @param labelConnectAlpha sets the alpha of the connection lines between the labels
                                    #' @param labelBetween logical vector. If TRUE then labels are placed between connection points
                                    #' @param labelNudgeY numeric vector, to have the labels drawn a little higher or lower in the final graph
                                    #' @param axisConnect specifies if the labels should have axis connect lines
                                    #' @param axisConnectColor sets the color of the axis connector line(s)
                                    #' @param axisConnectAlpha sets the alpha of the axis connector line(s)
                                    #' @param axisConnectWidth sets the width of the axis connector line(s)
                                    #' @param axisConnectLevel defines to where (how far down) the axisConnect lines should reach
                                    #' @param axisConnectWhere this is how high above the connect, is relative to max Y-axis.
                                    #'  To place labels close to their peaks, this option should only be set/changed by
                                    #'  annotation field labelType
                                    #' @param axisConnectType at this moment can be 1,2 or 3. 1 means connect all the way down,
                                    #'  2 means up till axisConnectLevel above int of m/z. 3 = overrides all axisConnect Settings,
                                    #'  labels put at axisConnectLevel above int of m/z and axisConnect lines
                                    #'
                                    #' @returns a new spectrum annotation object
                                    #'
                                    #' @export
                                    initialize = function(annotationName = NA,
                                                          nameX = "mz", nameY = "intensity",
                                                          mzs = NA, labels = NA,
                                                          levelWhere = 0.9, levelWhereAbsolute = FALSE,
                                                          labelColor = "red", labelSize= 1, labelAngle = 30,
                                                          labelConnect = TRUE, labelConnectColor = "red", labelConnectAlpha = 0.25,
                                                          labelBetween = FALSE, labelNudgeY = 0,
                                                          axisConnect = TRUE, axisConnectColor = "red", axisConnectAlpha = 0.25,
                                                          axisConnectWidth = 0.5, axisConnectLevel = NA, axisConnectWhere = 0.05, axisConnectType = 1){
                                      super$initialize(annotationName = annotationName,
                                                       nameX = nameX, nameY = nameY,
                                                       x = mzs, labelX = labels,
                                                       labelWhere = levelWhere, labelWhereAbsolute = levelWhereAbsolute,
                                                       labelColor = labelColor, labelSize = labelSize, labelAngle = labelAngle,
                                                       labelConnect = labelConnect, labelConnectColor = labelConnectColor, labelConnectAlpha = labelConnectAlpha,
                                                       labelBetween = labelBetween, labelNudgeY = labelNudgeY,
                                                       axisConnect = axisConnect, axisConnectColor = axisConnectColor, axisConnectAlpha = axisConnectAlpha,
                                                       axisConnectWidth = axisConnectWidth, axisConnectLevel = axisConnectLevel, axisConnectWhere = axisConnectWhere,
                                                       axisConnectType = axisConnectType)
                                      invisible()
                                    },
                                    #' @description
                                    #'  checks whether the m/z coordinates have 'sufficient' intensity labels
                                    #'  if not, then the label is dropped from the object
                                    #'
                                    #' @param spectrum data.frame with at least an m/z column and an intensity column
                                    #' @param toleranceLow left tolerance (m/z)
                                    #' @param toleranceHigh right tolerance (m/z)
                                    #' @param relativeCutOff is the intensity CutOff reletive (fraction of maximum intensity)
                                    #' @param intensityCutOff y-axis (intensity) cut off y-axis value (intensity) should be above
                                    #'
                                    #' @returns the object itself (invisible)
                                    #'
                                    #' @export
                                    check = function(spectrum,
                                                     toleranceLow = 0.1, toleranceHigh = toleranceLow,
                                                     relativeCutOff = TRUE, intensityCutOff = ifelse(relativeCutOff, 0.001, 10)){
                                      super$check(dataframe = spectrum,
                                                  toleranceX = c(toleranceLow, toleranceHigh),
                                                  relativeCutOff = relativeCutOff, yCutOff = intensityCutOff)
                                      invisible()
                                    }
                                  ),
                                  active = list(
                                    #' @field mz gets/sets the x values of the labels
                                    #' @note use with care, no safeguards (yet)
                                    mz = function(value){
                                      if (missing(value)){
                                        return(self$x)
                                      } else {
                                        self$x <- value
                                      }
                                    },
                                    #' @field intensity gets/sets the y-positions in the object
                                    #' @note use with care, no safeguards (yet)
                                    intensity = function(value){
                                      if (missing(value)){
                                        return(self$axisConnectLevel)
                                      } else {
                                        self$axisConnectLevel <- value
                                      }
                                    }
                                  )
)

# ---- Chromatogram Annotation ----

#' @title chromatogramAnnotation
#'
#' @description R6 Class dealing with mass spectrometry annotation data in an organised manner.
#'  This class is a descendant of annotation
#'
#' @examples
#' demoFile <- fs::path_package("extdata", "Data0001.CSV", package = "MS.Analysis")
#' result <- readLines(demoFile, n = 9092)
#' result <- read.table(text = result, sep = ",", header = FALSE)[, 2:3]
#' colnames(result) <- c("rt", "intensity")
#' plotChromatogram(chromatogram = result)
#' thePeaks <- chromatogramFindPeaks(result, span = 2, smoothing = NA, signalNoiseRatio = 10)
#' thePeaks |> head()
#' theAnnotation <- chromatogramAnnotation$new(
#'   rts = thePeaks$peak_rt,
#'   labels = formatDigits(2)(thePeaks$peak_rt)
#' )
#' theAnnotation
#' plotChromatogram(chromatogram = result, annotateRt = list(theAnnotation))
#' plotChromatogram(chromatogram = result, rtLimits = c(7, 11), annotateRt = list(theAnnotation),
#'                  incrScaleIntensity = 0.35)
#' theAnnotation$check(chromatogram = result)
#' theAnnotation
#' theAnnotation$labelType <- 2
#' plotChromatogram(chromatogram = result, rtLimits = c(7, 11), annotateRt = list(theAnnotation),
#'                  incrScaleIntensity = 0.35)
#' theAnnotation$labelType <- 3
#' plotChromatogram(chromatogram = result, rtLimits = c(7, 11), annotateRt = list(theAnnotation),
#'                  incrScaleIntensity = 0.35)
#'
#' @export
chromatogramAnnotation <- R6::R6Class("chromatogramAnnotation",
                                      inherit = annotation,
                                      public = list(
                                        #' @description Initializes an spectrum annotation object
                                        #'
                                        #' @param annotationName set the internal name of the object
                                        #' @param nameX sets the name of the x-axis variable (eg m/z or retention time)
                                        #' @param nameY sets the name of the y-axis variable (eg intensity)
                                        #' @param rts rt-values (retention time, location on x-axis) for the labels
                                        #' @param labels the labels to be used for the annotation. Should have same length
                                        #'  as argument rts. If NA, then it will be as.character(rts)
                                        #' @param levelWhere y-axis height where axis labels are to be placed. Expressed as
                                        #'  a fraction of the y-axis maximum (default 0.9)
                                        #' @param levelWhereAbsolute labelWhere positions are absolute? (default: FALSE)
                                        #' @param labelColor sets the color of the labels
                                        #' @param labelSize sets the size of the labels
                                        #' @param labelAngle sets the angle of display of the labels
                                        #' @param labelConnect logical vector which defines if the labels are connected
                                        #'  horizontally ('along' the x-axis)
                                        #' @param labelConnectColor sets the color of the connection lines between the labels
                                        #' @param labelConnectAlpha sets the alpha of the connection lines between the labels
                                        #' @param labelBetween logical vector. If TRUE then labels are placed between connection points
                                        #' @param labelNudgeY numeric vector, to have the labels drawn a little higher or lower in the final graph
                                        #' @param axisConnect specifies if the labels should have axis connect lines
                                        #' @param axisConnectColor sets the color of the axis connector line(s)
                                        #' @param axisConnectAlpha sets the alpha of the axis connector line(s)
                                        #' @param axisConnectWidth sets the width of the axis connector line(s)
                                        #' @param axisConnectLevel defines to where (how far down) the axisConnect lines should reach
                                        #' @param axisConnectWhere this is how high above the connect, is relative to max Y-axis.
                                        #'  To place labels close to their peaks, this option should only be set/changed by
                                        #'  annotation field labelType
                                        #' @param axisConnectType at this moment can be 1,2 or 3. 1 means connect all the way down,
                                        #'  2 means up till axisConnectLevel above int of m/z. 3 = overrides all axisConnect Settings,
                                        #'  labels put at axisConnectLevel above int of m/z and axisConnect lines
                                        #'
                                        #' @return a new spectrum annotation object
                                        initialize = function(annotationName = NA,
                                                              nameX = "rt", nameY = "intensity",
                                                              rts = NA, labels = NA,
                                                              levelWhere = 0.9, levelWhereAbsolute = FALSE,
                                                              labelColor = "red",
                                                              labelSize= 1, labelAngle = 30,
                                                              labelConnect = FALSE, labelConnectColor = "red", labelConnectAlpha = 0.25,
                                                              labelBetween = FALSE, labelNudgeY = TRUE,
                                                              axisConnect = TRUE, axisConnectColor = "red", axisConnectAlpha = 0.25,
                                                              axisConnectWidth = 0.5, axisConnectLevel = NA, axisConnectWhere = 0.05, axisConnectType = 1){
                                          super$initialize(annotationName = annotationName,
                                                           nameX = nameX, nameY = nameY,
                                                           x = rts, labelX = labels,
                                                           labelWhere = levelWhere, labelColor = labelColor,
                                                           labelSize = labelSize, labelAngle = labelAngle,
                                                           labelConnect = labelConnect, labelConnectColor = labelConnectColor, labelConnectAlpha = labelConnectAlpha,
                                                           labelBetween = labelBetween, labelNudgeY = labelNudgeY,
                                                           axisConnect = axisConnect, axisConnectColor = axisConnectColor, axisConnectAlpha = axisConnectAlpha,
                                                           axisConnectWidth = axisConnectWidth, axisConnectLevel = axisConnectLevel, axisConnectWhere = axisConnectWhere,
                                                           axisConnectType = axisConnectType)
                                          invisible()
                                        },
                                        #' @description
                                        #'  checks whether the m/z coordinates have 'sufficient' intensity labels
                                        #'  if not, then the label is dropped from the object
                                        #'
                                        #' @param chromatogram data.frame with at least an m/z column and an intensity column
                                        #' @param toleranceLow left tolerance (m/z)
                                        #' @param toleranceHigh right tolerance (m/z)
                                        #' @param relativeCutOff is the intensity CutOff reletive (fraction of maximum intensity)
                                        #' @param intensityCutOff y-axis (intensity) cut off y-axis value (intensity) should be above
                                        #'
                                        #' @returns the object itself (invisible)
                                        #'
                                        #' @export
                                        check = function(chromatogram,
                                                         toleranceLow = 0.1, toleranceHigh = toleranceLow,
                                                         relativeCutOff = TRUE, intensityCutOff = ifelse(relativeCutOff, 0.001, 10)){
                                          super$check(dataframe = chromatogram,
                                                      toleranceX = c(toleranceLow, toleranceHigh),
                                                      relativeCutOff = relativeCutOff, yCutOff = intensityCutOff)
                                          invisible()
                                        }
                                      ),
                                      active = list(
                                        #' @field rt gets/sets the x values of the labels
                                        rt = function(value){
                                          if (missing(value)){
                                            return(self$x)
                                          } else {
                                            self$x <- value
                                          }
                                        },
                                        #' @field intensity gets/sets the y-positions in the object
                                        #' @note use with care, no safeguards (yet)
                                        intensity = function(value){
                                          if (missing(value)){
                                            return(self$axisConnectLevel)
                                          } else {
                                            self$axisConnectLevel <- value
                                          }
                                        },
                                        #' @field labelType gets/sets the axisConnectType
                                        #'  When set it takes care of axisConnect & labelConnect at the same time
                                        labelType = function(value){
                                          if (missing(value)){
                                            return(self$axisConnectType)
                                          } else {
                                            if (value > 1){
                                              if (!identical(self$axisConnectLevel, NA)){
                                                self$axisConnectType <- value
                                                if (value > 2){
                                                  self$axisConnect <- FALSE
                                                  self$labelConnect <- FALSE
                                                } else {
                                                  self$axisConnect <- TRUE
                                                }
                                              } else {
                                                warning("No axis connect level values, axis connect type was not changed.")
                                              }
                                            } else {
                                              self$axisConnectType <- value
                                              self$axisConnect <- TRUE
                                            }
                                          }
                                        }

                                      )
)
