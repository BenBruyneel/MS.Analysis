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
