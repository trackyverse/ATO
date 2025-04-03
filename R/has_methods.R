#' Generic to check if an ATO has data in a given slot
#' 
#' @param object an \code{\link{ATO}}
#' @param value a vector with the names of the requested slots
#' @param error Should the code execution stop if the requested slots are empty?
#' 
#' @return TRUE if the slots have data, FALSE if they're empty and
#'   error = FALSE. Throws an error if the slots are empty and error = TRUE.
#' 
#' @export
#' 
setGeneric("has", function(object, value, error = FALSE) standardGeneric("has"))
#' @rdname has
setMethod("has", c(object = "ATO"), function(object, value, error = FALSE) {
  is_ato(object) # not necessary, the method won't let it pass if it is not ATO.
  link <- !(value %in% c("det", "dep", "tag", "ani", "obs", "log"))
  if (any(link)) {
    aux <- sum(link)
    stop(paste0(value[link], collapse = "' "),
         ifelse(aux > 1,
                " are not recognized slot names",
                " is not a recognized slot name"),
         " for an ATO.", call. = FALSE)
  }
  check <- sapply(value, function(i) {
    nrow(slot(object, i)) == 0
  })
  if (any(check)) {
    aux <- sum(check)
    if (error) {
      stop("The following ",
           ifelse(aux > 1,
                  " slots are empty: ",
                  " slot is empty: "),
           paste0(value[check], collapse = "' "),
           call. = FALSE)
    } else {
      return(FALSE)
    }
  }
  return(TRUE)
})
