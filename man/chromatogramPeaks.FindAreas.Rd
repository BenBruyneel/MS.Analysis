% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MS_functional.R
\name{chromatogramPeaks.FindAreas}
\alias{chromatogramPeaks.FindAreas}
\title{chromatogramPeaks.FindAreas}
\usage{
chromatogramPeaks.FindAreas(
  peaks,
  trace,
  auc = "trapezoid",
  absoluteArea = FALSE,
  subDivisions = 100,
  na.rm = FALSE
)
}
\arguments{
\item{peaks}{peak table with at least peak_rt, left & right columns for all peaks
to be integrated}

\item{trace}{data.frame (rt, intensity) of the chromatogram which has the peaks}

\item{auc}{specifies the method name for the interpolation. Options are: "trapezoid",
"step", "spline" & "linear"}

\item{absoluteArea}{A logical value that determines if negative areas should
be added to the total area under the curve.}

\item{subDivisions}{an integer telling how many subdivisions should be used for
integrate (for non-linear approximations). Ignored if method is not spline.}

\item{na.rm}{logical, indicating whether NA values should be stripped before the
computation proceeds. In this case only the complete.cases of x and y will be
used. Default is FALSE}
}
\value{
a peak table data.frame with an area column
}
\description{
Calculates the peak areas on the basis of peak_rt, the left &
right (rt) cutoff and the trace (chromatogram) itself
}
\note{
internally the function \link[DescTools]{AUC} is used. Some of the text
in the help of the DescTools function is used here
}
\examples{
demoFile <- fs::path_package("extdata", "Data0001.CSV", package = "MS.Analysis")
result <- readLines(demoFile, n = 9092)
result <- read.table(text = result, sep = ",", header = FALSE)[, 2:3]
colnames(result) <- c("rt", "intensity")
result |> head()
plot(result, type = "l")
peaks <- data.frame(
  peak_rt = c(11.074, 12.133, 17.769, 19.515),
  peak_intensity = c(145412448, 151731744, 99109632, 70294472),
  left = NA,
  right = NA,
  area = NA
)
peaks <- peaks |> chromatogramPeaks.FixedEdges()
peaks
plot(result, type = "l", xlim = c(10, 21), ylim = c(0, 2E8))
text(
  peaks$peak_rt,
  peaks$peak_intensity,
  formatMinimumDigits(1)(peaks$peak_rt),
  cex = 0.65,
  pos = 3,
  col = "red"
)
abline(
  v = peaks$peak_rt,
  col = scales::alpha(rgb(1, 0, 0), 0.75),
  lty = "dotted"
)
abline(
  v = peaks$left,
  col = scales::alpha(rgb(0, 0, 1), 0.75),
  lty = "dashed"
)
abline(
  v = peaks$right,
  col = scales::alpha(rgb(0, 0, 1), 0.75),
  lty = "dashed"
)
chromatogramPeaks.FindAreas(peaks = peaks,
                            trace = result)

}
