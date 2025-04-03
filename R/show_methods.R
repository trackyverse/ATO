#' Method to show an \code{\link{ATO}}
#' 
#' @param object an ATO
#' 
#' @return Nothing. Called for side-effects
#' 
#' @export
#' 
setMethod("show", "ATO", function(object) {
  cat(object@tbl)
  cat(" ATO object:\n")
  if (nrow(object@det) == 0) {
    cat("@det slot is empty.\n")
  } else {
    summary(object@det)
  }
  if (nrow(object@dep) == 0) {
    cat("@dep slot is empty.\n")
  } else {
    summary(object@dep)
  }
  if (nrow(object@tag) == 0) {
    cat("@tag slot is empty.\n")
  } else {
    summary(object@tag)
  }
  if (nrow(object@ani) == 0) {
    cat("@ani slot is empty.\n")
  } else {
    summary(object@ani)
  }
  if (nrow(object@obs) == 0) {
    cat("@obs slot is empty.\n")
  } else {
    summary(object@obs)
  }
  if (nrow(object@log) == 0) {
    cat("@log slot is empty.\n")
  } else {
    summary(object@log)
  }
})
