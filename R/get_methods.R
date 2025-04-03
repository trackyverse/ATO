#' Generic to extract detections as a table
#' 
#' @param x an \code{\link{ATO}} object
#' @param receivers An optional vector of receiver serial numbers from which
#'   to extract detections.
#' @param transmitters An optional vector of transmitters for which to extract
#'   detections.
#' 
#' @return a table of detections
#' 
#' @export
#' 
setGeneric("get_det", function(x, receivers, transmitters) standardGeneric("get_det"))

#' @rdname get_det
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

#' A wrapper of \code{\link{get_det}} to extract detections for transmitters listed
#' in the tag slot
#' 
#' @inheritParams get_det
#' 
#' @return a table of detections
#' 
#' @export
#' 
setGeneric("get_det_tag", function(x, receivers) standardGeneric("get_det_tag"))

#' @rdname get_det_tag
setMethod("get_det_tag", "ATO", function(x, receivers) {
  output <- get_det(x, receivers = receivers,
                    transmitters = x@tag$transmitter)
  return(output)
})

#' A wrapper of \code{\link{get_det}} to extract detections for transmitters listed
#' in the dep slot
#' 
#' @inheritParams get_det
#' 
#' @return a table of detections
#' 
#' @export
#' 
setGeneric("get_det_dep", function(x, receivers) standardGeneric("get_det_dep"))

#' @rdname get_det_dep
setMethod("get_det_dep", "ATO", function(x, receivers) {
  output <- get_det(x, receivers = receivers,
                    transmitters = x@dep$transmitter)
  return(output)
})

#' Generic to extract the dep slot as a table
#' 
#' @param x an \code{\link{ATO}} object
#' 
#' @return The dep slot as a table
#' 
#' @export
#' 
setGeneric("get_dep", function(x) standardGeneric("get_dep"))

#' @rdname get_dep
setMethod("get_dep", "ATO", function(x) x@dep)

#' Generic to extract the tag slot as a table
#' 
#' @param x an \code{\link{ATO}} object
#' 
#' @return The tag slot as a table
#' 
#' @export
#' 
setGeneric("get_tag", function(x) standardGeneric("get_tag"))

#' @rdname get_tag
setMethod("get_tag", "ATO", function(x) x@tag)

#' Generic to extract the ani slot as a table
#' 
#' @param x an \code{\link{ATO}} object
#' 
#' @return The ani slot as a table
#' 
#' @export
#' 
setGeneric("get_ani", function(x) standardGeneric("get_ani"))

#' @rdname get_ani
setMethod("get_ani", "ATO", function(x) x@ani)

#' Generic to extract the obs slot as a table
#' 
#' @param x an \code{\link{ATO}} object
#' 
#' @return The obs slot as a table
#' 
#' @export
#' 
setGeneric("get_obs", function(x) standardGeneric("get_obs"))

#' @rdname get_obs
setMethod("get_obs", "ATO", function(x) x@obs)
