setGeneric("get_det", function(x, receivers, transmitters) standardGeneric("get_det"))
setMethod("get_det", "ATO", function(x, receivers, transmitters) {
  if (missing("receivers")) {
    receiver_link <- rep(TRUE, nrow(x@det))
  } else {
    receiver_link <- x@det$receiver_serial %in% receivers
  }
  if (missing("transmitters")) {
    transmitter_link <- rep(TRUE, nrow(x@det))
  } else {
    transmitter_link <- x@det$transmitter %in% transmitters
  }
  output <- x@det[receiver_link & transmitter_link, ]
  return(output)
})

setGeneric("get_det_tags", function(x, receivers) standardGeneric("get_det_tags"))
setMethod("get_det_tags", "ATO", function(x, receivers) {
  output <- get_det(x, receivers = receivers,
                    transmitters = x@tag$transmitter)
  return(output)
})

setGeneric("get_det_deps", function(x, receivers) standardGeneric("get_det_deps"))
setMethod("get_det_deps", "ATO", function(x, receivers) {
  output <- get_det(x, receivers = receivers,
                    transmitters = x@dep$transmitter)
  return(output)
})

setGeneric("get_dep", function(x) standardGeneric("get_dep"))
setMethod("get_dep", "ATO", function(x) x@dep)

setGeneric("get_tag", function(x) standardGeneric("get_tag"))
setMethod("get_tag", "ATO", function(x) x@tag)
