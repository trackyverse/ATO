#' Round value to a reasonable number of decimal places
#' 
#' @param value the value to be rounded
#' @param max the maximum number of decimal places allowed
#' 
#' @return the rounded value
#' 
#' @keywords internal
#' 
.dyn_round <- function(value, max = 10) {
  dec <- 2
  while(round(value, dec) == 0 & dec < max) {
    dec <- dec + 1
  }
  return(round(value, dec))
}

.data.table_exists <- function(error = TRUE) {
  if (!"data.table" %in% utils::installed.packages()) {
    if (error) {
      stop("This function requires package data.table to run.", call. = FALSE)
    } else {
      return(FALSE)
    }
  }
  return(TRUE)
}

.tibble_exists <- function(error = TRUE) {
  if (!"tibble" %in% utils::installed.packages()) {
    if (error) {
      stop("This function requires package tibble to run.", call. = FALSE)
    } else {
      return(FALSE)
    }
  }
  return(TRUE)
}
