% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MS_annotation.R
\name{spectrumAnnotation}
\alias{spectrumAnnotation}
\title{spectrumAnnotation}
\value{
a new spectrum annotation object

the object itself (invisible)
}
\description{
R6 Class dealing with mass spectrometry annotation data in an organised manner.
This class is a descendant of \link[MS.Analysis]{annotation}
}
\note{
currently not much more of a convenience 'wrapper' around \link[MS.Analysis]{annotation}

use with care, no safeguards (yet)

use with care, no safeguards (yet)
}
\examples{
demoFile <- fs::path_package("extdata", "spec1.csv", package = "MS.Analysis")
centroidSpectrum <- read.table(demoFile, sep = ",", header = TRUE)
plotSpectrum(
  centroidSpectrum,
  centroidPlot = TRUE,
  mzLimits = c(100, 1000),
  labelColor = "red",
  intensityPercentage = TRUE
)
thePeptide <- massSpectrometryR::peptide$new(sequence = "LGGNEQVTR")
thePeptide$fragments()$yions
theAnnotation <- spectrumAnnotation$new(
  mzs = thePeptide$fragments()$yions,
  labels = createIonSeries(theSeries = 1:(thePeptide$length-1))
)
theAnnotation
plotSpectrum(
  centroidSpectrum,
  centroidPlot = TRUE,
  mzLimits = c(100, 1000),
  labelColor = "red",
  intensityPercentage = TRUE,
  incrScaleIntensity = 0.25,
  annotateMz = list(theAnnotation)
)
theAnnotation$color <- "blue"
plotSpectrum(
  centroidSpectrum,
  centroidPlot = TRUE,
  mzLimits = c(100, 1000),
  labelColor = "red",
  intensityPercentage = TRUE,
  incrScaleIntensity = 0.25,
  annotateMz = list(theAnnotation)
)
theAnnotation$check(spectrum = centroidSpectrum)
theAnnotation
plotSpectrum(
  centroidSpectrum,
  centroidPlot = TRUE,
  mzLimits = c(100, 1000),
  labelColor = "red",
  intensityPercentage = TRUE,
  incrScaleIntensity = 0.25,
  annotateMz = list(theAnnotation)
)
theAnnotation$labelType <- 2
plotSpectrum(
  centroidSpectrum,
  centroidPlot = TRUE,
  mzLimits = c(100, 1000),
  labelColor = "red",
  intensityPercentage = TRUE,
  incrScaleIntensity = 0.25,
  annotateMz = list(theAnnotation)
)
theAnnotation$labelType <- 3
plotSpectrum(
  centroidSpectrum,
  centroidPlot = TRUE,
  mzLimits = c(100, 1000),
  labelColor = "red",
  intensityPercentage = TRUE,
  incrScaleIntensity = 0.5,
  annotateMz = list(theAnnotation)
)
# using markdown/html-type labels
theAnnotation <- spectrumAnnotation$new(
  mzs = thePeptide$fragments()$yions,
  labels = createIonSeriesMD(theSeries = 1:(thePeptide$length-1))
)
theAnnotation$color <- "blue"
theAnnotation$check(spectrum = centroidSpectrum)
theAnnotation
plotSpectrum(
  centroidSpectrum,
  centroidPlot = TRUE,
  mzLimits = c(100, 1000),
  labelColor = "red",
  intensityPercentage = TRUE,
  incrScaleIntensity = 0.25,
  annotateMz = list(theAnnotation)
)

}
\section{Super class}{
\code{\link[MS.Analysis:annotation]{MS.Analysis::annotation}} -> \code{spectrumAnnotation}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{mz}}{gets/sets the x values of the labels}

\item{\code{intensity}}{gets/sets the y-positions in the object}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-spectrumAnnotation-new}{\code{spectrumAnnotation$new()}}
\item \href{#method-spectrumAnnotation-check}{\code{spectrumAnnotation$check()}}
\item \href{#method-spectrumAnnotation-clone}{\code{spectrumAnnotation$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="MS.Analysis" data-topic="annotation" data-id="draw"><a href='../../MS.Analysis/html/annotation.html#method-annotation-draw'><code>MS.Analysis::annotation$draw()</code></a></span></li>
<li><span class="pkg-link" data-pkg="MS.Analysis" data-topic="annotation" data-id="print"><a href='../../MS.Analysis/html/annotation.html#method-annotation-print'><code>MS.Analysis::annotation$print()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-spectrumAnnotation-new"></a>}}
\if{latex}{\out{\hypertarget{method-spectrumAnnotation-new}{}}}
\subsection{Method \code{new()}}{
Initializes an spectrum annotation object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{spectrumAnnotation$new(
  annotationName = NA,
  nameX = "mz",
  nameY = "intensity",
  mzs = NA,
  labels = NA,
  levelWhere = 0.9,
  levelWhereAbsolute = FALSE,
  labelColor = "red",
  labelSize = 1,
  labelAngle = 30,
  labelConnect = TRUE,
  labelConnectColor = "red",
  labelConnectAlpha = 0.25,
  labelBetween = FALSE,
  labelNudgeY = 0,
  axisConnect = TRUE,
  axisConnectColor = "red",
  axisConnectAlpha = 0.25,
  axisConnectWidth = 0.5,
  axisConnectLevel = NA,
  axisConnectWhere = 0.05,
  axisConnectType = 1
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{annotationName}}{set the internal name of the object}

\item{\code{nameX}}{sets the name of the x-axis variable (eg m/z or retention time)}

\item{\code{nameY}}{sets the name of the y-axis variable (eg intensity)}

\item{\code{mzs}}{m/z-values (location on x-axis) for the labels}

\item{\code{labels}}{the labels to be used for the annotation. Should have same length
as argument mzs. If NA, then it will be as.character(mzs)}

\item{\code{levelWhere}}{y-axis height where axis labels are to be placed. Expressed as
a fraction of the y-axis maximum (default 0.9)}

\item{\code{levelWhereAbsolute}}{labelWhere positions are absolute? (default: FALSE)}

\item{\code{labelColor}}{sets the color of the labels}

\item{\code{labelSize}}{sets the size of the labels}

\item{\code{labelAngle}}{sets the angle of display of the labels}

\item{\code{labelConnect}}{logical vector which defines if the labels are connected
horizontally ('along' the x-axis)}

\item{\code{labelConnectColor}}{sets the color of the connection lines between the labels}

\item{\code{labelConnectAlpha}}{sets the alpha of the connection lines between the labels}

\item{\code{labelBetween}}{logical vector. If TRUE then labels are placed between connection points}

\item{\code{labelNudgeY}}{numeric vector, to have the labels drawn a little higher or lower in the final graph}

\item{\code{axisConnect}}{specifies if the labels should have axis connect lines}

\item{\code{axisConnectColor}}{sets the color of the axis connector line(s)}

\item{\code{axisConnectAlpha}}{sets the alpha of the axis connector line(s)}

\item{\code{axisConnectWidth}}{sets the width of the axis connector line(s)}

\item{\code{axisConnectLevel}}{defines to where (how far down) the axisConnect lines should reach}

\item{\code{axisConnectWhere}}{this is how high above the connect, is relative to max Y-axis.
To place labels close to their peaks, this option should only be set/changed by
annotation field labelType}

\item{\code{axisConnectType}}{at this moment can be 1,2 or 3. 1 means connect all the way down,
2 means up till axisConnectLevel above int of m/z. 3 = overrides all axisConnect Settings,
labels put at axisConnectLevel above int of m/z and axisConnect lines}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-spectrumAnnotation-check"></a>}}
\if{latex}{\out{\hypertarget{method-spectrumAnnotation-check}{}}}
\subsection{Method \code{check()}}{
checks whether the m/z coordinates have 'sufficient' intensity labels
if not, then the label is dropped from the object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{spectrumAnnotation$check(
  spectrum,
  toleranceLow = 0.1,
  toleranceHigh = toleranceLow,
  relativeCutOff = TRUE,
  intensityCutOff = ifelse(relativeCutOff, 0.001, 10)
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{spectrum}}{data.frame with at least an m/z column and an intensity column}

\item{\code{toleranceLow}}{left tolerance (m/z)}

\item{\code{toleranceHigh}}{right tolerance (m/z)}

\item{\code{relativeCutOff}}{is the intensity CutOff reletive (fraction of maximum intensity)}

\item{\code{intensityCutOff}}{y-axis (intensity) cut off y-axis value (intensity) should be above}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-spectrumAnnotation-clone"></a>}}
\if{latex}{\out{\hypertarget{method-spectrumAnnotation-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{spectrumAnnotation$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
