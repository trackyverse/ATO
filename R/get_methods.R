#' Generic to extract detections as a table
#'
#' @param x an \code{\link{ATO}} object
#' @param receivers An optional vector of receiver serial numbers from which
#'   to extract detections.
#' @param transmitters An optional vector of transmitters for which to extract
#'   detections.
#' @param type the type of detections to return. One of:
#'   'all' - returns both valid and invalid detections (default);
#'   'valid' - returns only valid detections;
#'   'invalid' - returns only invalid detections
#'
#' @return a table of detections
#'
#' @export
#'
setGeneric(
  "get_det",
  function(x, receivers, transmitters, type = c("all", "valid", "invalid"))
    standardGeneric("get_det")
)

#' @rdname get_det
setMethod(
  "get_det",
  "ATO",
  function(x, receivers, transmitters, type = c("all", "valid", "invalid")) {
    type <- match.arg(type)

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
    if (type == "all") {
      combined_link <- receiver_link & transmitter_link
    }
    if (type == "valid") {
      combined_link <- receiver_link & transmitter_link & x@det$valid
    }
    if (type == "invalid") {
      combined_link <- receiver_link & transmitter_link & !x@det$valid
    }
    output <- x@det[combined_link, ]
    return(output)
  }
)

#' A wrapper of \code{\link{get_det}} to extract detections for transmitters listed
#' in the tag slot
#'
#' @inheritParams get_det
#'
#' @return a table of detections
#'
#' @export
#'
setGeneric(
  "get_det_tag",
  function(x, receivers, type = c("all", "valid", "invalid"))
    standardGeneric("get_det_tag")
)

#' @rdname get_det_tag
setMethod(
  "get_det_tag",
  "ATO",
  function(x, receivers, type = c("all", "valid", "invalid")) {
    output <- get_det(
      x,
      receivers = receivers,
      transmitters = x@tag$transmitter,
      type = type
    )
    return(output)
  }
)

#' A wrapper of \code{\link{get_det}} to extract detections for transmitters listed
#' in the dep slot
#'
#' @inheritParams get_det
#'
#' @return a table of detections
#'
#' @export
#'
setGeneric(
  "get_det_dep",
  function(x, receivers, type = c("all", "valid", "invalid"))
    standardGeneric("get_det_dep")
)

#' @rdname get_det_dep
setMethod(
  "get_det_dep",
  "ATO",
  function(x, receivers, type = c("all", "valid", "invalid")) {
    output <- get_det(
      x,
      receivers = receivers,
      transmitters = x@dep$transmitter,
      type = type
    )
    return(output)
  }
)

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
#' @param type the type of observations to return. One of:
#'   'all' - returns both valid and invalid observations (default);
#'   'valid' - returns only valid observations;
#'   'invalid' - returns only invalid observations
#'
#' @return The obs slot as a table
#'
#' @export
#'
setGeneric(
  "get_obs",
  function(x, type = c("all", "valid", "invalid")) standardGeneric("get_obs")
)

#' @rdname get_obs
setMethod("get_obs", "ATO", function(x, type = c("all", "valid", "invalid")) {
  type <- match.arg(type)
  if (type == "all") {
    return(x@obs)
  }
  if (type == "valid") {
    return(x@obs[x@obs$valid, ])
  }
  if (type == "invalid") {
    return(x@obs[!x@obs$valid, ])
  }
})

#' Generic to extract the log slot as a table
#'
#' @param x an \code{\link{ATO}} object
#'
#' @return The log slot as a table
#'
#' @export
#'
setGeneric(
  "get_log",
  function(x, debug = FALSE) standardGeneric("get_log")
)

#' @rdname get_log
setMethod("get_log", "ATO", function(x, debug = FALSE) {
  if (debug) {
    return(x@log)
  } else {
    return(x@log[x@log$type != "debug", colnames(x@log) != "call"])
  }
})

#' `$` Method to extract ATO slots directly
#'
#' @param x an \code{\link{ATO}} object
#' @param name the name of the desired ATO slot
#'
#' @return The slot as defined in `name`, along with a warning to directly
#'  extract slots in the future.
#'
#' @seealso [Extract]
#'
#' @export
setMethod("$", "ATO", function(x, name) {
  left <- substitute(x)
  right <- substitute(name)
  pretty_subset <- function(subs) paste0('`', left, subs, right, '`')

  warning(
    paste(
      pretty_subset("$"),
      "converted to",
      pretty_subset("@"),
      "for convenience, please use",
      pretty_subset("@"),
      "in the future.\n  See `?slot` for more information."
    ),
    immediate. = TRUE
  )

  return(slot(x, name))
})
