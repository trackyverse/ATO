#' Substitute NA
#'
#'Substitute `NA` with a `value` e.g., `0`
#' @param x an `vector``
#' @param value the value to replace `NA`
#'
#' @return a vector that has `NA` replaced with a value
#'
#' @examples
#' x <- c(1:10, NA)
#'
#' ATO:::.sub_na(x, value = 11)
#'
#' @keywords internal

.sub_na <- function(x, value) {
  x[is.na(x)] <- value
  return(x)
}
