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
      stop("append is set to TRUE but column names of new detection data"
           " doesn't match the column names in @det.", call. = FALSE)
    }
    to_add <- rbind(x@det, value)
    class(to_add) <- class(x@det)
    x@det <- to_add
  } else {
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
      stop("append is set to TRUE but column names of new deployment data"
           " doesn't match the column names in @dep.", call. = FALSE)
    }
    to_add <- rbind(x@dep, value)
    class(to_add) <- class(x@dep)
    x@dep <- to_add
  } else {
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
      stop("append is set to TRUE but column names of new tag data"
           " doesn't match the column names in @tag.", call. = FALSE)
    }
    to_add <- rbind(x@tag, value)
    class(to_add) <- class(x@tag)
    x@tag <- to_add
  } else {
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
      stop("append is set to TRUE but column names of new animal data"
           " doesn't match the column names in @ani.", call. = FALSE)
    }
    to_add <- rbind(x@ani, value)
    class(to_add) <- class(x@ani)
    x@ani <- to_add
  } else {
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
      stop("append is set to TRUE but column names of new observation data"
           " doesn't match the column names in @obs.", call. = FALSE)
    }
    to_add <- rbind(x@obs, value)
    class(to_add) <- class(x@obs)
    x@obs <- to_add
  } else {
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
