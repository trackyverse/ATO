#' check generic
#' 
#' Checks a prospective ATO slot object to confirm that the
#' contents are as expected. Matches it with the respective prototype reference.
#' 
#' @param object The prospective ATO slot object
#' 
#' @return the checked ATO object
#' 
#' @keywords internal
#' 
#' @name check
.silly_function <- function(x) {
  return("this is a test")
}

#' @rdname check
setGeneric(
  "check",
  function(object, tz, tbl) {
    standardGeneric("check")
  }
)

#' @rdname check
setMethod(
  "check",
  "ATO_ani",
  function(object, tz, tbl) {
    # object class check
    if (!is.null(tbl)) {
      table_type(object, tbl)
    }
    # column class check
    .check_ato_table_cols(object, .ATO_ani)
    # check the timezones
    object <- .check_column_tzones(object, tz = tz)

    .check_dup_rows(object)

    return(object)
  }
)

#' @rdname check
setMethod(
  "check",
  "ATO_dep",
  function(object, tz, tbl) {
    # object class check
    if (!is.null(tbl)) {
      table_type(object, tbl)
    }
    # column names check
    .check_ato_table_cols(object, .ATO_dep)
    # check the timezones
    object <- .check_column_tzones(object, tz = tz)

    .check_dup_rows(object)

    return(object)
  }
)

#' @rdname check
setMethod(
  "check",
  "ATO_det",
  function(object, tz, tbl) {
    # object class check
    if (!is.null(tbl)) {
      table_type(object, tbl)
    }
    # column class check
    .check_ato_table_cols(object, .ATO_det)
    # check the timezones
    object <- .check_column_tzones(object, tz = tz)
    return(object)
  }
)

#' @rdname check
setMethod(
  "check",
  "ATO_obs",
  function(object, tz, tbl) {
    # object class check
    if (!is.null(tbl)) {
      table_type(object, tbl)
    }
    # column class check
    .check_ato_table_cols(object, .ATO_obs)
    # check the timezones
    object <- .check_column_tzones(object, tz = tz)

    # check only one terminal per tag
    if (any(!is.na(object$transmitter))) {
      aux <- object[!is.na(object$transmitter), ]
      aux <- split(aux, aux$transmitter)
      aux <- sapply(aux, function(x) {
        sum(x$terminal) > 1
      })
      if (any(aux)) {
        stop(
          "Transmitter", .s(sum(aux)),
          " ", .comma(names(aux)[aux]),
          " ", .has(sum(aux)),
          " more than one terminal observation.",
          " Transmitters and animals must be terminated only once.",
          call. = FALSE
        )
      }
    } 
    # check only one terminal per animal
    if (any(!is.na(object$animal))) {
      aux <- object[!is.na(object$animal), ]
      aux <- split(aux, aux$animal)
      aux <- sapply(aux, function(x) {
        sum(x$terminal) > 1
      })
      if (any(aux)) {
        stop(
          "Animal", .s(sum(aux)),
          " ", .comma(names(aux)[aux]),
          " ", .has(sum(aux)),
          " more than one terminal observation.",
          " Transmitters and animals must be terminated only once.",
          call. = FALSE
        )
      }
    } 
    return(object)
  }
)

#' @rdname check
setMethod(
  "check",
  "ATO_tag",
  function(object, tz, tbl) {
    # object class check
    if (!is.null(tbl)) {
      table_type(object, tbl)
    }
    # column class check
    .check_ato_table_cols(object, .ATO_tag)
    # check the timezones
    object <- .check_column_tzones(object, tz = tz)

    .check_dup_rows(object)

    # check tag-animal pairs
    if (any(!is.na(object$animal))) {
      tagani <- object[!is.na(object$animal), ]
      aux <- paste(tagani$transmitter, tagani$animal)
      check <- duplicated(aux)
      if (any(check)) {
        check <- unique(
          c(
            which(check),
            which(duplicated(aux, fromLast = TRUE))
          )
        )
        warning(
          "Transmitter", .s(length(unique(tagani$transmitter[check]))),
          " ", .comma(unique(tagani$transmitter[check])),
          " ", .was(length(unique(tagani$transmitter[check]))),
          " deployed to the same animal more than once!",
          " MOST ATO FUNCTIONS WILL CRASH!",
          call. = FALSE, immediate. = TRUE
        )
      }
    }
    return(object)
  }
)
