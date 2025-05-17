test_that("Number formating functions work", {
  # formatDigitsWaiver
  expect_equal(formatDigitsWaiver(1.234), "1.234")
  expect_equal(formatDigitsWaiver(1), "1")
  expect_equal(formatDigitsWaiver(1.234E01), "12.34")
  expect_equal(formatDigitsWaiver(1.234E-01), "0.1234")
  # formatDigits
  expect_equal(formatDigits(5)(pi), "3.14159")
  singleDigit <- formatDigits(2)
  expect_equal(singleDigit(pi), "3.14")
  expect_equal(formatDigits(5)(50000000), "50,000,000.00000")
  # formatMinimumDigits
  expect_equal(formatMinimumDigits(5)(3.14), "3.14000")
  expect_equal(formatMinimumDigits(5)(pi), "3.14159")
  expect_equal(formatMinimumDigits(5)(-0.1), "-0.10000")
  # formatDigitsLargeNumbers
  expect_equal(formatDigits(5)(50000000), "50,000,000.00000")
  # formatScientificDigits
  expect_equal(formatScientificDigits(4)(pi), "3.142e+00")
  expect_equal(formatScientificDigits(10)(pi), "3.141593e+00")
  expect_equal(formatScientificDigits(3)(1.4556E11), "1.46e+11")
  # formatScientificMinimumDigits
  expect_equal(formatScientificMinimumDigits(5)(1.4556E11), "1.45560e+11")
  expect_equal(formatScientificMinimumDigits(15)(pi), "3.141592653589793e+00")
})
