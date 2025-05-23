#' @title ifelseProper
#'
#' @description ifelse replacement for properly returning all datatypes. Regular
#'  \link[base]{ifelse} is not capable of working with more complicated
#'  datatypes (like multi element vectors) for 'yes' and 'no' arguments
#'
#' @param logicValue variable or expression resulting in TRUE or FALSE,
#'  if missing or not logical then the function will return NULL.
#' @param ifTrue variable or expression to be returned when logicValue == TRUE
#' @param ifFalse variable or expression to be returned when logicValue == FALSE
#'
#' @returns depending on logicValue, ifTrue or ifFalse
#'
#' @note not vectorized
#'
#' @examples
#' ifelse(TRUE, 1, "B")
#' ifelse(TRUE, c(1,1), "B")
#' ifelse(FALSE, c(1,1), c("B","B"))
#' ifelseProper(TRUE, c(1,1), c("B","B"))
#' ifelseProper(FALSE, c(1,1), c("B","B"))
#'
#' @noRd
ifelseProper <- function(logicValue = NULL, ifTrue = NULL, ifFalse = NULL) {
  if (missing(logicValue)) {
    return(NULL)
  } else {
    if (!is.logical(logicValue)) {
      return(NULL)
    } else {
      if (logicValue) {
        return(ifTrue)
      } else {
        return(ifFalse)
      }
    }
  }
}

#' @title is.Class
#'
#' @description internal helper function to determine if object == whichClass or
#'  a descendant of a data type
#'
#' @param object a data object of some class
#' @param whichClass character string: class name to be tested
#'
#' @returns TRUE or FALSE
#'
#' @examples
#' is.Class(c("A", "C"), "character")
#' is.Class(c("A", "C"), "integer")
#' is.Class(1:4, "character")
#' is.Class(1:4, "integer")
#'
#' @export
is.Class <- function(object, whichClass) {
  return(whichClass %in% class(object))
}

#' @title strReplaceAll
#'
#' @description extended version of str_replace_all from the stringr package.
#'  More than one pattern and replacement can be replaced
#'
#' @note patterns (and their) replacements are handled sequentially, so one after
#'  another
#'
#' @param string one or more character vectors containing parts that need to be
#'  replaced
#' @param pattern one or more character vectors that need to be replaced
#' @param replacement one or more character vectors that are used as replacements
#'  This argument has to be either length one (used for all patterns) or must have
#'  the same length as the pattern argument
#'
#' @returns character vector(s)
#'
#' @examples
#' strReplaceAll(string = c("Hello", "Word"), pattern = "r", replacement = "rl")
#' strReplaceAll(string = c("Hello", "Word"), pattern = c("e","o"), replacement = "")
#' strReplaceAll(string = c("Hello", "Word"), pattern = c("e","o"), replacement = "")
#' strReplaceAll(string = c("Hello", "Word"), pattern = c("e","o"), replacement = c("a", "-"))
#'
#' @noRd
strReplaceAll <- function(string, pattern = NA, replacement = "") {
  if (identical(pattern, NA)) {
    return(string)
  }
  if (length(replacement) == 1) {
    replacement <- rep(replacement, length(pattern))
  } else {
    if (length(replacement) != length(pattern)) {
      stop("pattern & replacement arguments must be of same length")
    }
  }
  for (strCounter in 1:length(string)) {
    for (counter in 1:length(pattern)) {
      string[strCounter] <- stringr::str_replace_all(
        string[strCounter],
        pattern = pattern[counter],
        replacement = replacement[counter]
      )
    }
  }
  return(string)
}

# ---- Number formatting functions ----

#' @title formatDigitsWaiver
#'
#' @description Function to convert numerical vectors into strings. This specific one can be
#'  used in stead of the function factories below if needed when no options are needed
#'
#' @param v numeric vector to be converted to a character vector
#'
#' @return a character vector
#'
#' @examples
#' formatDigitsWaiver(1.234)
#' formatDigitsWaiver(1)
#' formatDigitsWaiver(1.234E01)
#' formatDigitsWaiver(1.234E-01)
#' formatDigitsWaiver(pi)
#'
#' @export
formatDigitsWaiver <- function(v) {
  return(as.character(v))
}

#' @title formatDigits
#'
#' @description Function factory to be used to specify the number of digits to be
#'  used in numbers
#'
#' @param digits integer value that specifies the number of digits to be used
#'  by the resulting function
#'
#' @returns a function that will take a numeric vector as an argument and
#'  returns a character vector of the numeric vector with the set number of
#'  digits (see ?scales::lebel_number for more info)
#'
#' @note this is more or less an example of a function to be used to specify
#'  axis-label formats in ggplot2.
#'
#' @examples
#' formatDigits(0)(pi)
#' formatDigits(1)(pi)
#' formatDigits(2)(pi)
#' formatDigits(5)(pi)
#' singleDigit <- formatDigits(2)
#' singleDigit(pi)
#'
#' @export
formatDigits <- function(digits) {
  force(digits)
  function(v) {
    return(scales::comma(v, accuracy = 10^(-digits)))
  }
}

#' @title formatMinimumDigits
#'
#' @description Function factory to be used to specify the minimum number of
#'  digits to be used in numbers. The function generates numbers as strings
#'
#' @param digits integer value that specifies the number of digits to be used
#'  by the resulting function
#'
#' @returns a function that will take a numeric vector as an argument and
#'  returns a character vector of the numeric vector with the set minimum number
#'  of digits (see ?formatC for more info)
#'
#' @examples
#' formatMinimumDigits(5)(pi)
#' formatMinimumDigits(5)(3.14)
#' formatMinimumDigits(5)(-0.1)
#'
#' @export
formatMinimumDigits <- function(digits) {
  force(digits)
  function(v) {
    return(formatC(v, format = "f", digits = digits))
  }
}

#' @title formatDigitsLargeNumbers
#'
#' @description Function factory to be used to specify the number of digits to be
#'  used in large numbers. The function generates numbers as strings w/o big
#'  marks (US/UK commas)
#'
#' @param digits integer value that specifies the number of digits to be used
#'  by the resulting function
#'
#' @returns a function that will take a numeric vector as an argument and
#'  returns a character vector of the numeric vector with the set number of
#'  digits (see ?scales::lebel_number for more info) but w/o big marks
#'
#' @examples
#' formatDigits(5)(5000)
#' formatDigitsLargeNumbers(5)(5000)
#' formatDigits(5)(50000000)
#' formatDigitsLargeNumbers(5)(50000000)
#'
#' @export
formatDigitsLargeNumbers <- function(digits) {
  force(digits)
  function(v) {
    return(scales::comma(v, accuracy = 10^(-digits), big.mark = ""))
  }
}

#' @title formatScientificDigits
#'
#' @description Function factory to be used to specify the number of digits to be
#'  used in numbers when using scientific notation
#'
#' @param digits integer value that specifies the number of digits to be used
#'  by the resulting function
#'
#' @returns a function that will take a numeric vector as an argument and
#'  returns a character vector of the numeric vector in scientific format with
#'  the set number of digits (see ?scales::lebel_scientific for more info)
#'
#' @examples
#' formatScientificDigits(4)(pi)
#' formatScientificDigits(10)(pi)
#' formatScientificDigits(3)(1.4556E11)
#'
#' @export
formatScientificDigits <- function(digits) {
  force(digits)
  function(v) {
    return(scales::scientific(v, digits = digits))
  }
}

#' @title formatScientificMinimumDigits
#'
#' @description Function factory to be used to specify the minimum number of digits
#'  to be used in numbers when using scientific notation
#'
#' @param digits integer value that specifies the number of digits to be used
#'  by the resulting function
#'
#' @returns a function that will take a numeric vector as an argument and
#'  returns a character vector of the numeric vector in scientific format with
#'  the set number of digits (see ?formatC for more info)
#'
#' @examples
#' formatScientificMinimumDigits(5)(1.4556E11)
#' formatScientificMinimumDigits(15)(pi)
#'
#' @export
formatScientificMinimumDigits <- function(digits) {
  force(digits)
  function(v) {
    return(formatC(v, format = "e", digits = digits))
  }
}
