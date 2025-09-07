#' Wrapper for is(x, "ATO")
#' 
#' This function allows an easy check if the input is an \code{\link{ATO}} and
#' throw an error if that is not the case.
#' 
#' @param x the object to test
#' @param error Should the code error if x is not an ATO?
#' 
#' @return TRUE if x is an ATO, FALSE if it is not an ATO and error = FALSE.
#' Throws an error if x is not an ATO and error = TRUE.
#' 
#' @export
#' 
is_ato <- function(x, error = TRUE) {
  if (missing(x) || !is(x, "ATO")) {
    if (error) {
      stop("input is not an ATO object.",
           call. = FALSE)
    } else {
      return(FALSE)
    }
  }
  return(TRUE)
}
