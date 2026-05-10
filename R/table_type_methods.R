#' Table type of ATO data
#' 
#' The tables within an ATO may be of one of three main groups:
#' 'data.frame', 'data.table', or 'tibble'. By default, the make functions
#' (e.g. \code{\link{make_ani}}) create data.frame tables, which in turn create
#' a data.frame ATO. This can be modified either by using the tbl argument
#' within the make functions, or globally by calling
#' \code{\link{ato_table_type_global}}.
#' 
#' table_type may be used on an ATO object to check the type of table within
#' its slots, or directly on a slot object. table_type may also be used to
#' change the type of tables of a slot object or of an ATO object.
#' 
#' @param x an \code{\link{ATO}}
#' @param expect An expected type of table. Optional.
#'   Errors if x is not of this type.
#' 
#' @return if expect is missing, returns the type of table used by the ATO.
#'   If expect is provided, either errors or returns nothing.
#' 
#' @export
#' 
setGeneric("table_type", function(x, expect) standardGeneric("table_type"))

#' @rdname table_type
setMethod("table_type", "ATO", function(x, expect) {
  tbl <- NULL
  recipient <- lapply(c("ani", "dep", "det", "obs", "tag"),
    function(i) {
      if (has(x, i)) {
        return(table_type(slot(x, i)))
      } else {
        return(NULL)
      }
    }
  )
  tbl <- unique(unlist(recipient))

  if (length(tbl) > 1) {
    stop(
      "More than one table type detected in this ATO. This ATO is corrupted.",
      " You can reset the ATO's table type with e.g.",
      " table_type(",
      substitute(x),
      ") <- 'data.frame'",
      call. = FALSE
    )
  }

  if (missing(expect)) {
    return(tbl)
  } else {
    if (length(tbl) == 0) {
      stop(
        "This ATO is still empty and therefore has no associated table type.",
        " See ?set_ani for ways to add data to this ATO.",
        call. = FALSE
      )
    }
    if (tbl != expect) {
      stop(
        "ATO (",
        tbl,
        ") is not of expected table type (",
        ").",
        call. = FALSE)
    }
  }
})

#' @rdname table_type
setMethod("table_type", "ATO_ani", function(x, expect) {
  tbl <- "data.frame"
  if (is(x, "data.table")) {
    tbl <- "data.table"
  }
  if (is(x, "tbl")) {
    tbl <- "tibble"
  }

  if (missing(expect)) {
    return(tbl)
  } else {
    if (tbl != expect) {
      stop(
        "@ani slot (",
        tbl,
        ") is not of expected table type (",
        expect,
        ").",
        call. = FALSE
      )
    }
  }
})

#' @rdname table_type
setMethod("table_type", "ATO_dep", function(x, expect) {
  tbl <- "data.frame"
  if (is(x, "data.table")) {
    tbl <- "data.table"
  }
  if (is(x, "tbl")) {
    tbl <- "tibble"
  }

  if (missing(expect)) {
    return(tbl)
  } else {
    if (tbl != expect) {
      stop(
        "@dep slot (",
        tbl,
        ") is not of expected table type (",
        expect,
        ").",
        call. = FALSE
      )
    }
  }
})

#' @rdname table_type
setMethod("table_type", "ATO_det", function(x, expect) {
  tbl <- "data.frame"
  if (is(x, "data.table")) {
    tbl <- "data.table"
  }
  if (is(x, "tbl")) {
    tbl <- "tibble"
  }

  if (missing(expect)) {
    return(tbl)
  } else {
    if (tbl != expect) {
      stop(
        "@det slot (",
        tbl,
        ") is not of expected table type (",
        expect,
        ").",
        call. = FALSE
      )
    }
  }
})

#' @rdname table_type
setMethod("table_type", "ATO_obs", function(x, expect) {
  tbl <- "data.frame"
  if (is(x, "data.table")) {
    tbl <- "data.table"
  }
  if (is(x, "tbl")) {
    tbl <- "tibble"
  }

  if (missing(expect)) {
    return(tbl)
  } else {
    if (tbl != expect) {
      stop(
        "@obs slot (",
        tbl,
        ") is not of expected table type (",
        expect,
        ").",
        call. = FALSE
      )
    }
  }
})

#' @rdname table_type
setMethod("table_type", "ATO_tag", function(x, expect) {
  tbl <- "data.frame"
  if (is(x, "data.table")) {
    tbl <- "data.table"
  }
  if (is(x, "tbl")) {
    tbl <- "tibble"
  }

  if (missing(expect)) {
    return(tbl)
  } else {
    if (tbl != expect) {
      stop(
        "@tag slot (",
        tbl,
        ") is not of expected table type (",
        expect,
        ").",
        call. = FALSE
      )
    }
  }
})

#' Assign a new table type to an ATO or an ATO slot.
#' 
#' @param x an \code{\link{ATO}}
#' @param value The new table type.
#' 
#' @return Acts directly on x
#' 
#' @export
#' 
setGeneric("table_type<-", 
           function(x, value = c("data.frame", "data.table", "tibble"))
             standardGeneric("table_type<-"))

#' @rdname table_type-set
setMethod("table_type<-", "ATO",
  function(x,
           value = c("data.frame", "data.table", "tibble")) {
    value <- match.arg(value)

  if (value == "data.frame") {
    ani <- as.data.frame(x@ani)
    dep <- as.data.frame(x@dep)
    det <- as.data.frame(x@det)
    obs <- as.data.frame(x@obs)
    tag <- as.data.frame(x@tag)
    log <- as.data.frame(x@log)
  }
  if (value == "data.table") {
    .data.table_exists()
    ani <- data.table::as.data.table(x@ani)
    dep <- data.table::as.data.table(x@dep)
    det <- data.table::as.data.table(x@det)
    obs <- data.table::as.data.table(x@obs)
    tag <- data.table::as.data.table(x@tag)
    log <- data.table::as.data.table(x@log)
  }
  if (value == "tibble") {
    .tibble_exists()
    ani <- tibble::as_tibble(x@ani)
    dep <- tibble::as_tibble(x@dep)
    det <- tibble::as_tibble(x@det)
    obs <- tibble::as_tibble(x@obs)
    tag <- tibble::as_tibble(x@tag)
    log <- tibble::as_tibble(x@log)
  }
  class(ani) <- c("ATO_ani", class(ani))
  class(dep) <- c("ATO_dep", class(dep))
  class(det) <- c("ATO_det", class(det))
  class(obs) <- c("ATO_obs", class(obs))
  class(tag) <- c("ATO_tag", class(tag))
  class(log) <- c("ATO_log", class(log))
  x@ani <- ani
  x@dep <- dep
  x@det <- det
  x@obs <- obs
  x@tag <- tag
  x@log <- log

  validObject(x)
  return(x)
})

#' @rdname table_type-set
setMethod("table_type<-", "ATO_ani",
  function(x,
           value = c("data.frame", "data.table", "tibble")) {
    value <- match.arg(value)

  if (value == "data.frame") {
    x <- as.data.frame(x)
  }
  if (value == "data.table") {
    .data.table_exists()
    x <- data.table::as.data.table(x)
  }
  if (value == "tibble") {
    .tibble_exists()
    x <- tibble::as_tibble(x)
  }
  class(x) <- c("ATO_ani", class(x))
  return(x)
})

#' @rdname table_type-set
setMethod("table_type<-", "ATO_dep",
  function(x,
           value = c("data.frame", "data.table", "tibble")) {
    value <- match.arg(value)

  if (value == "data.frame") {
    x <- as.data.frame(x)
  }
  if (value == "data.table") {
    .data.table_exists()
    x <- data.table::as.data.table(x)
  }
  if (value == "tibble") {
    .tibble_exists()
    x <- tibble::as_tibble(x)
  }
  class(x) <- c("ATO_dep", class(x))
  return(x)
})

#' @rdname table_type-set
setMethod("table_type<-", "ATO_det",
  function(x,
           value = c("data.frame", "data.table", "tibble")) {
    value <- match.arg(value)

  if (value == "data.frame") {
    x <- as.data.frame(x)
  }
  if (value == "data.table") {
    .data.table_exists()
    x <- data.table::as.data.table(x)
  }
  if (value == "tibble") {
    .tibble_exists()
    x <- tibble::as_tibble(x)
  }
  class(x) <- c("ATO_det", class(x))
  return(x)
})

#' @rdname table_type-set
setMethod("table_type<-", "ATO_obs",
  function(x,
           value = c("data.frame", "data.table", "tibble")) {
    value <- match.arg(value)

  if (value == "data.frame") {
    x <- as.data.frame(x)
  }
  if (value == "data.table") {
    .data.table_exists()
    x <- data.table::as.data.table(x)
  }
  if (value == "tibble") {
    .tibble_exists()
    x <- tibble::as_tibble(x)
  }
  class(x) <- c("ATO_obs", class(x))
  return(x)
})

#' @rdname table_type-set
setMethod("table_type<-", "ATO_tag",
  function(x,
           value = c("data.frame", "data.table", "tibble")) {
    value <- match.arg(value)

  if (value == "data.frame") {
    x <- as.data.frame(x)
  }
  if (value == "data.table") {
    .data.table_exists()
    x <- data.table::as.data.table(x)
  }
  if (value == "tibble") {
    .tibble_exists()
    x <- tibble::as_tibble(x)
  }
  class(x) <- c("ATO_tag", class(x))
  return(x)
})

#' Helper function to set the global type of ATO tables
#' 
#' @param type the desired type of ATO tables. One of data.frame (default),
#'   data.table (requires the package data.table installed), or
#'   tibble (requires the package tibble installed).
#' 
#' @return the current global table type if 'type' is missing.
#' 
#' @export
#' 
ato_table_type_global <- function(type = c("data.frame",
                                           "data.table",
                                           "tibble")) {
  if (missing(type)) {
    return(getOption("ATO_table_type", default = "data.frame"))
  } else {
    type <- match.arg(type)
    options(ATO_table_type = type)
    message("M: Newly created ATO objects will be of type ", type)
  }
}
