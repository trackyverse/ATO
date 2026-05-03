#' What is the table type of this ATO?
#' 
#' @param x an \code{\link{ATO}}
#' @param expect An expected type of table.
#'   Errors if the ATO is not of this type.
#' 
#' @return if expect is missing, returns the type of table used by the ATO.
#'   If expect is provided, either errors or returns nothing.
#' 
#' @export
#' 
setGeneric("table_type", function(x, expect) standardGeneric("table_type"))

#' @rdname table_type
setMethod("table_type", "ATO", function(x, expect) {
  if (missing(expect)) {
    as.character(x@tbl)
  } else {
    if (as.character(x@tbl) != expect) {
      stop("ATO is not of expected table type.", call. = FALSE)
    }
  }
})

#' Assign a new table type to the ATO
#' 
#' @param x an \code{\link{ATO}}
#' @param value The new table type to be used inside the ATO.
#' 
#' @return Nothing, acts directly on x
#' 
#' @export
#' 
setGeneric("table_type<-", function(x, value) standardGeneric("table_type<-"))

#' @rdname table_type-set
setMethod("table_type<-", "ATO", function(x, value) {
  new_format <- as.character(value)
  class(new_format) <- c("ATO_tbl", "character")
  x@tbl <- new_format
  if (value == "data.frame") {
    ani <- as.data.frame(x@ani)
    dep <- as.data.frame(x@dep)
    det <- as.data.frame(x@det)
    obs <- as.data.frame(x@obs)
    tag <- as.data.frame(x@tag)
    log <- as.data.frame(x@log)
  }
  if (value == "data.table") {
    ani <- data.table::as.data.table(x@ani)
    dep <- data.table::as.data.table(x@dep)
    det <- data.table::as.data.table(x@det)
    obs <- data.table::as.data.table(x@obs)
    tag <- data.table::as.data.table(x@tag)
    log <- data.table::as.data.table(x@log)
  }
  if (value == "tibble") {
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

#' Helper function to set the global type of ATO tables
#' 
#' @param type the desired type of ATO tables. One of data.frame (default),
#'   data.table (requires the package data.table installed), or
#'   tibble (requires the package tibble installed).
#' @param update_all If set to TRUE, this package will search the user's
#'   global environment for objects of class ATO and will update their
#'   table types (by calling \code{\link{table_type}} on them).
#' 
#' @return Nothing. called for side-effects
#' 
#' @export
#' 
ato_table_type_global <- function(type = c("data.frame",
                                           "data.table",
                                           "tibble"),
                                  update_all = FALSE) {
  type <- match.arg(type)
  options(ATO_table_type = type)

  .ATO_tbl <- type
  if (type == "data.frame") {
    .ATO_ani <- as.data.frame(.ATO_ani)
    .ATO_dep <- as.data.frame(.ATO_dep)
    .ATO_det <- as.data.frame(.ATO_det)
    .ATO_obs <- as.data.frame(.ATO_obs)
    .ATO_tag <- as.data.frame(.ATO_tag)
  }
  if (type == "data.table") {
    .ATO_ani <- data.table::as.data.table(.ATO_ani)
    .ATO_dep <- data.table::as.data.table(.ATO_dep)
    .ATO_det <- data.table::as.data.table(.ATO_det)
    .ATO_obs <- data.table::as.data.table(.ATO_obs)
    .ATO_tag <- data.table::as.data.table(.ATO_tag)
  }
  if (type == "tibble") {
    .ATO_ani <- tibble::as_tibble(.ATO_ani)
    .ATO_dep <- tibble::as_tibble(.ATO_dep)
    .ATO_det <- tibble::as_tibble(.ATO_det)
    .ATO_obs <- tibble::as_tibble(.ATO_obs)
    .ATO_tag <- tibble::as_tibble(.ATO_tag)
  }
  class(.ATO_ani) <- c("ATO_ani", class(.ATO_ani))
  class(.ATO_dep) <- c("ATO_dep", class(.ATO_dep))
  class(.ATO_det) <- c("ATO_det", class(.ATO_det))
  class(.ATO_obs) <- c("ATO_obs", class(.ATO_obs))
  class(.ATO_tag) <- c("ATO_tag", class(.ATO_tag))
  class(.ATO_tbl) <- c("ATO_tbl", "character")

  utils::assignInNamespace(".ATO_ani", .ATO_ani, ns = "ATO", pos = "package:ATO")
  utils::assignInNamespace(".ATO_dep", .ATO_dep, ns = "ATO", pos = "package:ATO")
  utils::assignInNamespace(".ATO_det", .ATO_det, ns = "ATO", pos = "package:ATO")
  utils::assignInNamespace(".ATO_obs", .ATO_obs, ns = "ATO", pos = "package:ATO")
  utils::assignInNamespace(".ATO_tag", .ATO_tag, ns = "ATO", pos = "package:ATO")
  utils::assignInNamespace(".ATO_tbl", .ATO_tbl, ns = "ATO", pos = "package:ATO")

  message("M: Newly created ATOs will have tables of type ",
          type)

  if (update_all) {
    objects <- ls(envir = .GlobalEnv)
    link <- sapply(objects, function(x) is(get(x), "ATO"))
    if (any(link)) {
      ATOs <- objects[link]
      for (x in ATOs) {
        eval(parse(text = paste0("table_type(", x, ") <- type")))
        eval(parse(text = paste0("assign('", x, "', ", x, 
                                 ", envir = .GlobalEnv)")))
      }
      message("M: Converted ", sum(link), " already existing objects",
              " of class ATO to table type ", type)
    }
  }
}
