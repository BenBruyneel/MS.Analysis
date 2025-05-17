test_that("createIonSeries works",{
  expect_equal(createIonSeries(theSeries = 1:5),
               c('y+_1','y+_2','y+_3','y+_4','y+_5'))
  expect_equal(createIonSeries(whichSeries = "b", theSeries = 1:5, whichCharge = 2),
               c('b2+_1','b2+_2','b2+_3','b2+_4','b2+_5'))
})

test_that("createIonSeries works",{
  expect_equal(createIonSeriesMD(theSeries = 1:5),
               c('y<sub>1</sub><sup>+</sup>','y<sub>2</sub><sup>+</sup>','y<sub>3</sub><sup>+</sup>','y<sub>4</sub><sup>+</sup>','y<sub>5</sub><sup>+</sup>'))
  expect_equal(createIonSeriesMD(whichSeries = "b", theSeries = 1:5, whichCharge = 2),
               c('b<sub>1</sub><sup>2+</sup>','b<sub>2</sub><sup>2+</sup>','b<sub>3</sub><sup>2+</sup>','b<sub>4</sub><sup>2+</sup>','b<sub>5</sub><sup>2+</sup>'))
})

# No test for
# - annotation
# - spectrumAnnotation
# - chromatogramAnnotation
#
# If the examples work it should be good
