setGeneric("get_det", function(x, receivers, transmitters) standardGeneric("get_det"))
setMethod("get_det", "ATO", function(x, receivers, transmitters) {
  if (missing("receivers")) {
    receiver_link <- rep(TRUE, nrow(x@detections))
  } else {
    receiver_link <- x@detections$receiver_serial %in% receivers
  }
  if (missing("transmitters")) {
    transmitter_link <- rep(TRUE, nrow(x@detections))
  } else {
    transmitter_link <- x@detections$transmitter %in% transmitters
  }
  output <- x@detections[receiver_link & transmitter_link, ]
  return(output)
})

setGeneric("get_det_tags", function(x, receivers) standardGeneric("get_det_tags"))
setMethod("get_det_tags", "ATO", function(x, receivers) {
  output <- get_det(x, receivers = receivers,
                    transmitters = x@tags$transmitter)
  return(output)
})

setGeneric("get_det_deps", function(x, receivers) standardGeneric("get_det_deps"))
setMethod("get_det_deps", "ATO", function(x, receivers) {
  output <- get_det(x, receivers = receivers,
                    transmitters = x@deployments$transmitter)
  return(output)
})

setGeneric("get_deployments", function(x) standardGeneric("get_deployments"))
setMethod("get_deployments", "ATO", function(x) x@deployments)

setGeneric("get_tags", function(x) standardGeneric("get_tags"))
setMethod("get_tags", "ATO", function(x) x@tags)
