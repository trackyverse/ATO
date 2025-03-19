setGeneric("get_detections", function(x) standardGeneric("get_detections"))
setMethod("get_detections", "ATO", function(x) x@detections)

setGeneric("get_deployments", function(x) standardGeneric("get_deployments"))
setMethod("get_deployments", "ATO", function(x) x@deployments)

setGeneric("get_tags", function(x) standardGeneric("get_tags"))
setMethod("get_tags", "ATO", function(x) x@tags)
