test_that("reduceSpectrumLabelDF works", {
  demoSpec <- fs::path_package("extdata", "spec1.csv", package = "MS.Analysis")
  specData <- utils::read.table(file = demoSpec, header = TRUE, sep = ",")
  specData$label <- dataInfo::formatDigitsLargeNumbers(4)(specData$mz)
  expect_equal(nrow(specData), 139)
  expect_equal(
    specData %>%
      dplyr::filter(mz > 150, mz < 180) %>%
      nrow(),
    9
  )
  expect_equal(
    reduceSpectrumLabelDF(
      specData,
      number = 1,
      maxIntensity = FALSE,
      mzCloseness = 1
    ) %>%
      dplyr::filter(mz > 150, mz < 180) %>%
      nrow(),
    3
  )
  expect_equal(
    reduceSpectrumLabelDF(
      specData,
      number = 10,
      maxIntensity = TRUE,
      mzCloseness = 1
    ) %>%
      nrow(),
    10
  )
  expect_equal(
    reduceSpectrumLabelDF(
      specData,
      number = 1,
      maxIntensity = FALSE,
      mzCloseness = log10(2)
    ) %>%
      dplyr::filter(mz > 150, mz < 180) %>%
      nrow(),
    4
  )
})

test_that("spectrumLabels works", {
  demoSpec <- fs::path_package("extdata", "spec1.csv", package = "MS.Analysis")
  specData <- utils::read.table(file = demoSpec, header = TRUE, sep = ",")
  expect_equal(dim(spectrumLabels(specData)), c(139, 3))
  expect_equal(dim(spectrumLabels(specData, mzAccuracy = 1)), c(62, 3))
  expect_equal(
    dim(spectrumLabels(
      specData,
      mzAccuracy = 1,
      maxIntensity = TRUE,
      top = 10
    )),
    c(10, 3)
  )
})

# No test for
# - plotSpectrum
# - plotSpectrumOverlay
# - plotSpectrumMirror
#
# If the examples work it should be good

