setGeneric("get_detections", function(x) standardGeneric("detections"))
setMethod("get_detections", "ATO", function(x) x@detections)

setGeneric("get_deployments", function(x) standardGeneric("deployments"))
setMethod("get_deployments", "ATO", function(x) x@deployments)

setGeneric("get_tags", function(x) standardGeneric("tags"))
setMethod("get_tags", "ATO", function(x) x@tags)
