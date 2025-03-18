setGeneric("add_detections", function(x, value) standardGeneric("add_detections"))
setMethod("add_detections", "ATO", function(x, value) {
  x@detections <- check_is_dataframe(value)
  validObject(x)
  return(x)
})

setGeneric("add_deployments", function(x, value) standardGeneric("add_deployments"))
setMethod("add_deployments", "ATO", function(x, value) {
  x@deployments <- check_is_dataframe(value)
  validObject(x)
  return(x)
})

setGeneric("add_tags", function(x, value) standardGeneric("add_tags"))
setMethod("add_tags", "ATO", function(x, value) {
  x@tags <- check_is_dataframe(value)
  validObject(x)
  return(x)
})