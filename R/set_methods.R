#' Set an ATO slot
#' 
#' The family of set functions (set_ani, set_dep, set_det, set_tag, and set_obs)
#' brings already-formatted ATO data into an ATO object.
#' 
#' @param x an ATO
#' @param value The data to be included. Use one of the make functions to
#'  prepare it. See \code{\link{make_ani}}, \code{\link{make_dep}},
#' \code{\link{make_det}}, \code{\link{make_tag}}, and \code{\link{make_obs}}.
#' @param append Logical: Should the new data be appended to the
#'   data already present in the ATO?
#' @param silent Logical: Supress summary messages
#' 
#' @return The updated ATO
#' 
#' @examples
#' # split away parts of the example ATO for the example
#' ani <- get_ani(example_ato)
#' dep <- get_dep(example_ato)
#' 
#' # add them to the ATO object using the set methods
#' x <- set_ani(example_ato, ani)
#' x <- set_dep(x, dep)
#' 
#' # clean up
#' rm(ani, dep, x)
#' 
#' @seealso \code{\link{make_ani}}, \code{\link{make_dep}},
#' \code{\link{make_det}}, \code{\link{make_tag}}, \code{\link{make_obs}}
#' 
#' @name set
NULL

#' @rdname set
#' @export
setGeneric(
  "set_det",
  function(x, value, append = FALSE, silent = FALSE)
    standardGeneric("set_det"))

#' @rdname set
#' @export
setMethod(
  "set_det", 
  c(x = "ATO", value = "ATO_det"),
  function(x, value, append, silent) {
    x <- .set_worker(x, value, append, silent)
    return(x)
  }
)

#' @rdname set
#' @export
setGeneric(
  "set_dep",
  function(x, value, append = FALSE, silent = FALSE)
    standardGeneric("set_dep"))

#' @rdname set
#' @export
setMethod(
  "set_dep",
  c(x = "ATO", value = "ATO_dep"),
  function(x, value, append, silent) {
    x <- .set_worker(x, value, append, silent)
    return(x)
  }
)

#' @rdname set
#' @export
setGeneric(
  "set_tag",
  function(x, value, append = FALSE, silent = FALSE)
    standardGeneric("set_tag"))

#' @rdname set
#' @export
setMethod(
  "set_tag",
  c(x = "ATO", value = "ATO_tag"),
  function(x, value, append, silent) {
    x <- .set_worker(x, value, append, silent)
    return(x)
  }
)

#' @rdname set
#' @export
setGeneric(
  "set_ani",
  function(x, value, append = FALSE, silent = FALSE)
    standardGeneric("set_ani"))

#' @rdname set
#' @export
setMethod(
  "set_ani",
  c(x = "ATO", value = "ATO_ani"),
  function(x, value, append, silent) {
    x <- .set_worker(x, value, append, silent)
    return(x)
  }
)

#' @rdname set
#' @export
setGeneric(
  "set_obs",
  function(x, value, append = FALSE, silent = FALSE)
    standardGeneric("set_obs"))

#' @rdname set
#' @export
setMethod(
  "set_obs",
  c(x = "ATO", value = "ATO_obs"),
  function(x, value, append, silent) {
    x <- .set_worker(x, value, append, silent)
    return(x)
  }
)

#' @rdname set
#' @keywords internal
.set_worker <- function(x, value, append = FALSE, silent = FALSE) {
  target <- sub("ATO_", "", class(value)[1])

  # make sure everything is in the same time zone
  value <- .check_column_tzones(value, tzone(x))

  if (append) {
    check1 <- any(colnames(value) %in% colnames(slot(x, target)))
    check2 <- any(colnames(value) %in% colnames(slot(x, target)))
    if (check1 | check2) {
      stop("append is set to TRUE but column names of new detection data",
           " don't match the column names in @", target, ".", call. = FALSE)
    }
    
    to_set <- rbind(slot(x, target), value) # this can probably be sped up?
    class(to_set) <- class(slot(x, target))
    slot(x, target) <- to_set
    x <- log_event(x, "log", 
                   "Appended ", nrow(value),
                   " new detections to the @", target," slot.")
  } else {
    if (has(x, target)) {
      x <- log_event(x, "log", 
                     "Overwrote the @", target, " slot with ", nrow(value),
                     " detections.")
    } else {
      x <- log_event(x, "log", 
                     "Set a new @", target, " slot with ", nrow(value),
                     " detections.")
    }
    slot(x, target) <- value
  }
  
  validObject(x)
  if (getOption("ATO_match_immediate", default = TRUE)) {
    x <- match_update(x, silent = silent)
  }

  return(x)
}
