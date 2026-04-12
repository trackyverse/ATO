#' Generic to extract detections as a table
#'
#' @param x an \code{\link{ATO}} object
#' @param receivers An optional vector of receiver serial numbers from which
#'   to extract detections.
#' @param transmitters An optional vector of transmitters for which to extract
#'   detections.
#' @param type the type of rows to return. One of:
#'   'all' - returns both valid and invalid rows (default);
#'   'valid' - returns only valid rows;
#'   'invalid' - returns only invalid rows
#'
#' @return a table of detections
#'
#' @examples
#' # extract all the detections from an ATO in table format
#' det <- get_det(example_ato)
#' summary(det)
#' 
#' # extract only detections from one or more specific receivers
#' sub_det <- get_det(example_ato, receivers = "132908")
#' summary(sub_det)
#' 
#' # or matching one or more specific transmitters
#' sub_det <- get_det(example_ato, transmitters = "R64K-4529")
#' summary(sub_det)
#' 
#' # or both!
#' sub_det <- get_det(example_ato,
#'                    receivers = "132908",
#'                    transmitters = "R64K-4529")
#' summary(sub_det)
#' 
#' # clean up
#' rm(det, sub_det)
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

#' A wrapper of \code{\link{get_det}} to extract detections for transmitters
#' listed in the tag slot
#'
#' @inheritParams get_det
#'
#' @return a table of detections
#'
#' @examples
#' # wrapper to extract all detections that match transmitters
#' # listed in the @tag slot
#' det <- get_det_tag(example_ato)
#' 
#' # extract only detections from one or more specific receivers
#' det <- get_det_tag(example_ato, receivers = "132908")
#' summary(det)
#' 
#' # clean up
#' rm(det)
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
#' @examples
#' # wrapper to extract all detections that match transmitters
#' # listed in the @dep slot (beacon tags)
#' det <- get_det_dep(example_ato)
#' summary(det)
#' # note: The example_ato does not have beacon detections,
#' # so this returns a table with 0 rows
#' 
#' # extract only detections from one or more specific receivers
#' det <- get_det_dep(example_ato, receivers = "132908")
#' summary(det)
#' # note: The example_ato does not have beacon detections,
#' # so this returns a table with 0 rows
#' 
#' # clean up
#' rm(det)
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
#' @inheritParams get_det
#'
#' @return The dep slot as a table
#'
#' @examples
#' # extract all the deployments from an ATO in table format
#' dep <- get_dep(example_ato)
#' summary(dep)
#' 
#' # clean up
#' rm(dep)
#' 
#' @export
#'
setGeneric(
  "get_dep",
  function(x, type = c("all", "valid", "invalid"))
    standardGeneric("get_dep")
)

#' @rdname get_dep
setMethod("get_dep", "ATO", 
          function(x, type = c("all", "valid", "invalid")) {
    type <- match.arg(type)

    if (type == "all") {
      return(x@dep)
    }
    if (type == "valid") {
      return(x@dep[x@dep$valid ,])
    }
    if (type == "invalid") {
      return(x@dep[!x@dep$valid ,])
    }
  }
)

#' Generic to extract the tag slot as a table
#'
#' @inheritParams get_det
#'
#' @return The tag slot as a table
#'
#' @examples
#' # extract all the tags from an ATO in table format
#' tag <- get_tag(example_ato)
#' summary(tag)
#' 
#' # clean up
#' rm(tag)
#' 
#' @export
#'
setGeneric(
  "get_tag",
  function(x, type = c("all", "valid", "invalid"))
    standardGeneric("get_tag")
)

#' @rdname get_tag
setMethod("get_tag", "ATO",
          function(x, type = c("all", "valid", "invalid")) {
    type <- match.arg(type)

    if (type == "all") {
      return(x@tag)
    }
    if (type == "valid") {
      return(x@tag[x@tag$valid ,])
    }
    if (type == "invalid") {
      return(x@tag[!x@tag$valid ,])
    }
  }
)

#' Generic to extract the ani slot as a table
#'
#' @inheritParams get_det
#'
#' @return The ani slot as a table
#'
#' @examples
#' # extract all the animals from an ATO in table format
#' ani <- get_ani(example_ato)
#' summary(ani)
#' 
#' # clean up
#' rm(ani)
#' 
#' @export
#'
setGeneric(
  "get_ani",
  function(x, type = c("all", "valid", "invalid"))
    standardGeneric("get_ani")
)

#' @rdname get_ani
setMethod("get_ani", "ATO",
          function(x, type = c("all", "valid", "invalid")) {
    type <- match.arg(type)

    if (type == "all") {
      return(x@ani)
    }
    if (type == "valid") {
      return(x@ani[x@ani$valid ,])
    }
    if (type == "invalid") {
      return(x@ani[!x@ani$valid ,])
    }
  }
)

#' Generic to extract the obs slot as a table
#'
#' @inheritParams get_det
#'
#' @return The obs slot as a table
#'
#' @examples
#' # extract all the observations from an ATO in table format
#' obs <- get_obs(example_ato)
#' summary(obs)
#' # note: The example ato object has no observations, so this
#' # returns 0 rows.
#' 
#' # clean up
#' rm(obs)
#' 
#' @export
#'
setGeneric(
  "get_obs",
  function(x, type = c("all", "valid", "invalid"))
    standardGeneric("get_obs")
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
#' @param debug should debug entries and the call column be displayed?
#'
#' @return The log slot as a table
#'
#' @examples
#' # extract all the log lines from an ATO in table format
#' log <- get_log(example_ato)
#' head(log)
#' 
#' # clean up
#' rm(log)
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
#' @examples
#' # access a slot using $ instead of @
#' dep <- example_ato$dep
#' summary(dep)
#' 
#' # clean up
#' rm(dep)
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
