#' Generic add function
#' 
#' Brings already-formatted data into the ATO.
#' Adequated methods are picked based on the type of data
#' being incorporated.
#' 
#' @param x an ATO
#' @param value The data to be included.
#'   The output of one of the make_* functions.
#' @param append Logical: Should the new data add to the
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
    original <- x@det[colnames(.ATO_det), ]
    to_add <- rbind(original, value)
    if (table_type(x) == "data.table") {
      to_add <- data.table::as.data.table(to_add)
    }
    if (table_type(x) == "tibble") {
      to_add <- tibble::as_tibble(to_add)
    }
    class(to_add) <- c("ATO_det", class(to_add))
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
    original <- x@dep[colnames(.ATO_dep), ]
    to_add <- rbind(original, value)
    if (table_type(x) == "data.table") {
      to_add <- data.table::as.data.table(to_add)
    }
    if (table_type(x) == "tibble") {
      to_add <- tibble::as_tibble(to_add)
    }
    class(to_add) <- c("ATO_dep", class(to_add))
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
    original <- x@tag[colnames(.ATO_tag), ]
    to_add <- rbind(original, value)
    if (table_type(x) == "data.table") {
      to_add <- data.table::as.data.table(to_add)
    }
    if (table_type(x) == "tibble") {
      to_add <- tibble::as_tibble(to_add)
    }
    class(to_add) <- c("ATO_tag", class(to_add))
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
    original <- x@ani[colnames(.ATO_ani), ]
    to_add <- rbind(original, value)
    if (table_type(x) == "data.table") {
      to_add <- data.table::as.data.table(to_add)
    }
    if (table_type(x) == "tibble") {
      to_add <- tibble::as_tibble(to_add)
    }
    class(to_add) <- c("ATO_ani", class(to_add))
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
    original <- x@obs[colnames(.ATO_obs), ]
    to_add <- rbind(original, value)
    if (table_type(x) == "data.table") {
      to_add <- data.table::as.data.table(to_add)
    }
    if (table_type(x) == "tibble") {
      to_add <- tibble::as_tibble(to_add)
    }
    class(to_add) <- c("ATO_obs", class(to_add))
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
