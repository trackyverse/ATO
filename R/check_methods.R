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
#' 
setGeneric(
  "check",
  function(object, tz, tbl) {
    standardGeneric("check")
  }
)

#' @method check ATO_ani
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

    # check that capture_datetime, if present, is before release_datetime
    
    # conceptual test
    # object <- data.frame(
    #   capture_datetime = as.POSIXct(
    #     c("2026-05-03 12:00:00",
    #       NA
    #     )
    #   ),
    #   release_datetime = as.POSIXct(
    #     c(
    #       "2026-05-02 12:00:00",
    #       "2026-05-02 12:00:00"
    #     )
    #   )
    # )
    check <- object$capture_datetime > object$release_datetime
    check <- .sub_na(check, FALSE)

    if (any(check)) {
      x <- which(check)
      stop(
        "capture_datetime for row", .s(length(x)),
        " ", .comma(x),
        " comes after release_datetime.",
        " Animals must be captured before they are released.",
        call. = FALSE)
    }

    return(object)
  }
)

#' @method check ATO_dep
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

#' @method check ATO_det
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

#' @method check ATO_obs
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

#' @method check ATO_tag
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
