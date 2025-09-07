#' Generic add function
#' 
#' Brings already-formatted data into the ATO.
#' Adequated methods are picked based on the type of data
#' being incorporated.
#' 
#' @param x an ATO
#' @param value The data to be included.
#'   The output of one of the make_* functions.
#' @param append Logical: Should the new data be appended to the
#'   data already present in the ATO?
#' 
#' @return The updated ATO
#' 
#' @export
#' 
setGeneric("add", function(x, value, append = FALSE) standardGeneric("add"))

#' @rdname add
setMethod("add", c(x = "ATO", value = "ATO_det"), function(x, value, append) {
  if (append) {
    check1 <- any(colnames(value) %in% colnames(x@det))
    check2 <- any(colnames(value) %in% colnames(x@det))
    if (check1 | check2) {
      stop("append is set to TRUE but column names of new detection data",
           " doesn't match the column names in @det.", call. = FALSE)
    }
    to_add <- rbind(x@det, value)
    class(to_add) <- class(x@det)
    x@det <- to_add
    x <- log_event(x, "log", 
                   "Appended ", nrow(value),
                   " new detections to the @det slot.")
  } else {
    if (has(x, "det")) {
      x <- log_event(x, "log", 
                     "Overwrote the @det slot with ", nrow(value),
                     " detections.")
    } else {
      x <- log_event(x, "log", 
                     "Added a new @det slot with ", nrow(value),
                     " detections.")
    }
    x@det <- value
  }
  validObject(x)
  if (nrow(x@tag) > 0) {
    x <- match_det_tag(x)
  }
  if (nrow(x@dep) > 0) {
    x <- match_det_dep(x)
  }
  return(x)
})

#' @rdname add
setMethod("add", c(x = "ATO", value = "ATO_dep"), function(x, value, append) {
  if (append) {
    check1 <- any(colnames(value) %in% colnames(x@dep))
    check2 <- any(colnames(value) %in% colnames(x@dep))
    if (check1 | check2) {
      stop("append is set to TRUE but column names of new deployment data",
           " doesn't match the column names in @dep.", call. = FALSE)
    }
    to_add <- rbind(x@dep, value)
    class(to_add) <- class(x@dep)
    x@dep <- to_add
    x <- log_event(x, "log", 
                   "Appended ", nrow(value),
                   " new deployments to the @dep slot.")
  } else {
    if (has(x, "dep")) {
      x <- log_event(x, "log", 
                     "Overwrote the @dep slot with ", nrow(value),
                     " deployments.")
    } else {
      x <- log_event(x, "log", 
                     "Added a new @dep slot with ", nrow(value),
                     " deployments.")
    }
    x@dep <- value
  }
  validObject(x)
  if (nrow(x@det) > 0) {
    x <- match_det_dep(x)
  }
  return(x)
})

#' @rdname add
setMethod("add", c(x = "ATO", value = "ATO_tag"), function(x, value, append) {
  if (append) {
    check1 <- any(colnames(value) %in% colnames(x@tag))
    check2 <- any(colnames(value) %in% colnames(x@tag))
    if (check1 | check2) {
      stop("append is set to TRUE but column names of new tag data",
           " doesn't match the column names in @tag.", call. = FALSE)
    }
    to_add <- rbind(x@tag, value)
    class(to_add) <- class(x@tag)
    x@tag <- to_add
    x <- log_event(x, "log", 
                   "Appended ", nrow(value),
                   " new tags to the @tag slot.")
  } else {
    if (has(x, "tag")) {
      x <- log_event(x, "log", 
                     "Overwrote the @tag slot with ", nrow(value),
                     " tags.")
    } else {
      x <- log_event(x, "log", 
                     "Added a new @tag slot with ", nrow(value),
                     " tags.")
    }
    x@tag <- value
  }
  validObject(x)
  if (nrow(x@ani) > 0) {
    x <- match_tag_ani(x)
  }
  if (nrow(x@det) > 0) {
    x <- match_det_tag(x)
  }
  if (nrow(x@obs) > 0) {
    x <- match_obs_tag(x)
  }
  return(x)
})

#' @rdname add
setMethod("add", c(x = "ATO", value = "ATO_ani"), function(x, value, append) {
  if (append) {
    check1 <- any(colnames(value) %in% colnames(x@ani))
    check2 <- any(colnames(value) %in% colnames(x@ani))
    if (check1 | check2) {
      stop("append is set to TRUE but column names of new animal data",
           " doesn't match the column names in @ani.", call. = FALSE)
    }
    to_add <- rbind(x@ani, value)
    class(to_add) <- class(x@ani)
    x@ani <- to_add
    x <- log_event(x, "log", 
                   "Appended ", nrow(value),
                   " new animals to the @ani slot.")
  } else {
    if (has(x, "ani")) {
      x <- log_event(x, "log", 
                     "Overwrote the @ani slot with ", nrow(value),
                     " animals.")
    } else {
      x <- log_event(x, "log", 
                     "Added a new @ani slot with ", nrow(value),
                     " animals.")
    }
    x@ani <- value
  }
  validObject(x)
  if (nrow(x@tag) > 0) {
    x <- match_tag_ani(x)
  }
  if (nrow(x@obs) > 0) {
    x <- match_obs_ani(x)
  }
  return(x)
})

#' @rdname add
setMethod("add", c(x = "ATO", value = "ATO_obs"), function(x, value, append) {
  if (append) {
    check1 <- any(colnames(value) %in% colnames(x@obs))
    check2 <- any(colnames(value) %in% colnames(x@obs))
    if (check1 | check2) {
      stop("append is set to TRUE but column names of new observation data",
           " doesn't match the column names in @obs.", call. = FALSE)
    }
    to_add <- rbind(x@obs, value)
    class(to_add) <- class(x@obs)
    x@obs <- to_add
    x <- log_event(x, "log", 
                   "Appended ", nrow(value),
                   " new observations to the @obs slot.")
  } else {
    if (has(x, "obs")) {
      x <- log_event(x, "log", 
                     "Overwrote the @obs slot with ", nrow(value),
                     " observations.")
    } else {
      x <- log_event(x, "log", 
                     "Added a new @obs slot with ", nrow(value),
                     " observations.")
    }
    x@obs <- value
  }
  validObject(x)
  if (nrow(x@tag) > 0) {
    x <- match_obs_tag(x)
  }
  if (nrow(x@ani) > 0) {
    x <- match_obs_ani(x)
  }
  return(x)
})
