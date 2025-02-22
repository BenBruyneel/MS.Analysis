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
ifelseProper <- function(logicValue = NULL, ifTrue = NULL, ifFalse = NULL){
  if (missing(logicValue)){
    return(NULL)
  } else {
    if (!is.logical(logicValue)){
      return(NULL)
    } else {
      if (logicValue){
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
#' @noRd
is.Class <- function(object, whichClass){
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
strReplaceAll <- function(string, pattern = NA, replacement = ""){
  if (identical(pattern, NA)){
    return(string)
  }
  if (length(replacement) == 1){
    replacement <- rep(replacement, length(pattern))
  } else {
    if (length(replacement) != length(pattern)){
      stop("pattern & replacement arguments must be of same length")
    }
  }
  for (strCounter in 1:length(string)){
    for (counter in 1:length(pattern)){
      string[strCounter] <- stringr::str_replace_all(string[strCounter],
                                                     pattern = pattern[counter],
                                                     replacement = replacement[counter])
    }
  }
  return(string)
}
