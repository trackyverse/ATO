setGeneric("add", function(x, value) standardGeneric("add"))

setMethod("add", c(x = "ATO", value = "ATO_det"), function(x, value) {
  x@det <- value
  validObject(x)
  if (nrow(x@tag) > 0) {
    x <- match_det_tag(x)
  }
  if (nrow(x@dep) > 0) {
    x <- match_det_dep(x)
  }
  return(x)
})

setMethod("add", c(x = "ATO", value = "ATO_dep"), function(x, value) {
  x@dep <- value
  validObject(x)
  if (nrow(x@det) > 0) {
    x <- match_det_dep(x)
  }
  return(x)
})

setMethod("add", c(x = "ATO", value = "ATO_tag"), function(x, value) {
  x@tag <- value
  validObject(x)
  return(x)
  if (nrow(x@det) > 0) {
    x <- match_det_tag(x)
  }
  return(x)
})