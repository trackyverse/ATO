#' Check that data.table library is installed
#' 
#' @param error Stop if not installed?
#' 
#' @return TRUE if installed, FALSE if not installed and error = FALSE
#' 
#' @keywords internal
#' 
.check_data.table_exists <- function(error = TRUE) {
  if (!"data.table" %in% utils::installed.packages()) {
    if (error) {
      stop("You must install package data.table to run this function.",
           call. = FALSE)
    } else {
      return(FALSE)
    }
  }
  return(TRUE)
}

#' Check that tibble library is installed
#' 
#' @param error Stop if not installed?
#' 
#' @return TRUE if installed, FALSE if not installed and error = FALSE
#' 
#' @keywords internal
#' 
.check_tibble_exists <- function(error = TRUE) {
  if (!"tibble" %in% utils::installed.packages()) {
    if (error) {
      stop("This function requires package tibble to run.", call. = FALSE)
    } else {
      return(FALSE)
    }
  }
  return(TRUE)
}

#' Check that the table columns match the prototype
#' 
#' @param object A prospective ATO slot object
#' @param ref The prototype for the respective slot
#' 
#' @keywords internal
#' 
#' @return Nothing. Called to stop() if needed.
#' 
.check_ato_table_cols <- function(object, ref) {
  check <- colnames(ref) %in% colnames(object)
  if (any(!check)) {
    stop("The ", class(object)[1], 
         " is missing the following standard columns: ",
         paste0(colnames(ref)[!check], collapse = ", "),
         call. = FALSE)
  }
  # column class check
  ref_classes <- lapply(ref, class)
  col_classes <- lapply(object, class)
  items <- ""
  for (i in colnames(ref)) {
    class_check <- ref_classes[[i]] %in% col_classes[[i]]
    if (any(!class_check)) {
      items <- paste(items, "\n -", i, "is not",
                     paste(class(ref[, i])[!class_check],
                           collapse = ", "),
                     collapse = "")
    }
  }
  if (items != "") {
    stop("The following columns of the ", class(object)[1],
         " are not of the required class:",
         items, call. = FALSE)
  }
}

#' Check that all columns within a slot have the same timezone
#' 
#' How to use:
#' \code{object <- get_tag(example_ato)}
#' 
#' all in the same tz, works
#' \code{object <- .check_column_tzones(object)}
#' 
#' force fail by missing tz
#' \code{attributes(object$release_datetime)$tzone <- ""}
#' \code{object <- .check_column_tzones(object)}
#' 
#' force fail by differing tzs
#' \code{attributes(object$release_datetime)$tzone <- "UTC"}
#' \code{object <- .check_column_tzones(object)}
#' 
#' correct fail by adding tz argument
#' \code{object <- .check_column_tzones(object, "Europe/Copenhagen")}
#' 
#' confirm correction applied
#' \code{attributes(object$activation_datetime)$tzone}
#' \code{attributes(object$release_datetime)$tzone}
#' 
#' @param object A prospective ATO slot table
#' @param tz optional; a timezone to apply to all time columns
#' 
#' @keywords internal
#' 
.check_column_tzones <- function(object, tz) {
  link <- sapply(1:ncol(object), function(i) "POSIXt" %in% class(object[[i]]))
  if (any(link)) {
    time_cols <- colnames(object)[link]
    if (!missing(tz) && !is.null(tz)) { # empty ATOs have null tz
      for (i in time_cols) {
        attributes(object[[i]])$tzone <- tz
      }
    } else {
      tzones <- sapply(time_cols, function(i) {
        if (all(is.na(object[[i]]))) {
          # means this column was not initialized
          # so it just receivet its tzone from another
          return(NA)
        }
        if (is.null(attributes(object[[i]])$tzone)) {
          return("")
        } else {
          return(attributes(object[[i]])$tzone)
        }
      })
      # remove columns not initialized
      tzones <- tzones[!is.na(tzones)]
      # check if any tz is missing
      if (any(tzones == "")) {
        aux <- which(is.null(tzones) | tzones == "")
        stop("The POSIXt data in column", .s(length(aux)),
             " ", .comma(time_cols[aux]), " ", .is(length(aux)),
             " missing timezone information. Use argument tz to",
             " assign a timezone.", call. = FALSE)
      }
      # check if all columns have the same tz
      if (length(unique(tzones)) > 1) {
        stop("The POSIXt columns ", .comma(time_cols),
             " aren't all in the same timezone (", .comma(unique(tzones)),
             "). Use argument tz to convert all POSIXt columns to the",
             " same timezone.", call. = FALSE)
      }
    }
  }
  return(object)
}

#' Check that all slots inside the ATO are in the same timezone
#' 
#' Usage notes:
#' all in the same tz, returns the tz
#' \code{.check_ato_tzones(example_ato)}
#' 
#' force fail by different tz
#' \code{object <- example_ato}
#' \code{attributes(object@det$datetime)$tzone <- "UTC"}
#' \code{check_ato_tzones(object)}
#' 
#' @param x An ATO object
#' 
#' @keywords internal
#' 
.check_ato_tzones <- function(x) {
  slots <- c("ani", "dep", "det", "tag", "obs")
  
  tzones <- sapply(slots, function(s) {
    if (!has(x, s)) {
      return(NULL)
    } else {
      return(.check_slot_tzones(slot(x, s), s))
    }
  })
  tzones <- unlist(tzones)

  if (length(tzones) == 0) {
    return(NULL) # no time data yet
  }

  if (length(unique(tzones)) > 1) {
    stop("Not all slots are in the same timezone (see below).",
         " See ?tzone() for assistance correcting this.\n",
         paste0(paste0("@", names(tzones), ": ", tzones), collapse = "\n"),
         call. = FALSE)
  } else {
    return(unique(tzones))
  }
}

#' Check that all slots inside the ATO are in the same timezone
#' 
#' how to use:
#' all in the same tz, returns the tz
#' \code{.check_slot_tzones(tag(example_ato))}
#' 
#' force fail by different tz
#' \code{object <- example_ato}
#' \code{attributes(object@dep$deploy_datetime)$tzone <- "UTC"}
#' \code{check_slot_tzones(dep(object))}
#' 
#' @param x An ATO slot object
#' @param s the name of the slot, for messaging purposes
#' 
#' @keywords internal
#' 
.check_slot_tzones <- function(x, s) {
  link <- sapply(1:ncol(x), function(i) "POSIXt" %in% class(x[[i]]))
  if (any(link)) {
    time_cols <- colnames(x)[link]
    tzones <- sapply(time_cols, function(i) attributes(x[[i]])$tzone)
    # check if any tz is missing
    if (any(tzones == "")) {
      aux <- which(tzones == "")
      stop("@", s, " POSIXt data in column", .s(length(aux)),
           " ", .comma(time_cols[aux]), " ", .is(length(aux)),
           " missing timezone information. This should never happen...",
           " Did you use make_", s, "() to make this table?.", call. = FALSE)
    }
    # check if all columns have the same tz
    if (length(unique(tzones)) > 1) {
      stop("@", s, " POSIXt columns ", .comma(time_cols),
           " aren't all in the same timezone (", .comma(unique(tzones)),
           "). This should never happen...",
           " Did you use make_", s, "() to make this table?.", call. = FALSE)
    }
    return(unique(tzones))
  } else {
    return(NULL)
  }
}

#' Check the tbl argument
#' 
#' used by the make_ functions
#' 
#' @param tbl the tbl argument to be checked
#' 
#' @keywords internal
#' 
.check_tbl_argument <- function(tbl) {
  if (missing(tbl)) {
    tbl <- ato_table_type_global()
  } else {
    if (!tbl %in% c("data.frame", "data.table", "tibble")) {
      stop(
        "tbl was defined but value not recognized. Value must be one of",
        " 'data.frame', 'data.table', or 'tibble'. See ?table_type.",
        call. = FALSE
      )
    }
  }

  if (tbl == "data.table") {
    .check_data.table_exists()    
  }

  if (tbl == "tibble") {
    .check_tibble_exists()
  }
  return(tbl)
}

#' Check the matches between tag and dep transmitters
#' 
#' @param x the ATO to be checked
#' 
#' @return nothing. Stops if there's a fatal ambiguity.
#' 
#' @keywords internal
#' 
.check_tag_beacon <- function(x) {
  if (!is.null(x@det$tag_match) & !is.null(x@det$beacon_match)) {
    check <- !is.na(x@det$tag_match) & !is.na(x@det$beacon_match)
    if (any(check)) {
      # remove bad match
      x@det$dep_match <- NULL
      r <- which(check)
      stop("@det row", .s(length(r)), " ", .comma(r),
           " match", .es(length(r), TRUE), " both a row in @tag",
           " and a row in @dep. Fatal ambiguity.",
           " Can't assign detections correctly.",
           call. = FALSE)
    }
  }
}

#' Check for duplicated matches in the base version
#' 
#' This is called inside a for loop, where i is the
#' tag currently being matched to the detections
#' 
#' @param x the vector of a new subset of items to assign matches to
#' @param x_index the original index of the x values being evaluated
#' @param i the tag in y currently being matched against x
#' @param label_x the name of the slot on the left side of the match
#' @param label_y the name of the slot on the right side of the match
#' 
#' @return nothing, stops if a duplicate match is found.
#' 
#' @keywords internal
#' 
.check_dup_match_base <- function(x, x_index, i, label_x, label_y) {
  if (any(!is.na(x))) {
    dup_x_index <- x_index[!is.na(x)]
    y_rows <- sort(c(unique(x[!is.na(x)]), i))

    stop(
      "@", label_x,
      " row", .s(length(dup_x_index)),
      " ", .comma(dup_x_index),
      " match", .es(length(dup_x_index), TRUE),
      " @", label_y,
      " rows ", .comma(y_rows),
      ". Fatal ambiguity. Can't assign detections correctly.",
      call. = FALSE
    )
    
  }
}

#' Check for duplicated matches in the data.table version
#' 
#' data.table can run all the matches in one go and still show
#' duplicated matches
#' 
#' @param x the vector of the xid from the data.table::foverlaps output
#' @param y the vector of the yid from the data.table::foverlaps output
#' @param original_x the original row order of x, for error messages
#' @param original_y the original row order of x, for error messages
#' @param label_x the name of the slot on the left side of the match
#' @param label_y the name of the slot on the right side of the match
#' 
#' @return nothing, stops if duplicated match found
#' 
#' @keywords internal
#' 
.check_dup_match_datatable <- function(
  x,
  y,
  original_x,
  original_y,
  label_x,
  label_y
) {
  # x is the row indexes of the detections. each x should only
  # show up once, because each detection can only match once
  # to a specific tag row.

  # conceptual example
  # x <- c(1, 1, 1, 2, 3, 4)
  # y <- c(1, 2, 4, 1, 3, 1)
  # label_x <- "det"
  # label_y <- "tag"
  # original_x <- c(4, 3, 2, 1)
  # original_y <- c(1, 3, 2, 10)
  # i.e. the temporary detection row 1 matches temporary tag rows 1, 2, 4.
  # which trace back to detection 4 matching tags 1, 3, and 10 in the original
  # row orders.

  # find double matches on x (i.e. multiple tags match the same detection)
  check <- duplicated(x)
  if (any(check)) {
    # find the indexes of the troublesome detections
    x_duplicated <- unique(x[which(check)])
    # find the tags that those detections matched to
    y_duplicated <- unique(y[x %in% x_duplicated])
    stop(
      "@", label_x, " row",
      .s(length(x_duplicated)),
      " ", .comma(sort(original_x[x_duplicated])),
      " match", .es(length(x_duplicated), TRUE),
      " multiple @", label_y,
      " rows (" ,.comma(sort(original_y[y_duplicated])), ").",
      " Fatal ambiguity. Can't assign detections correctly.",
      call. = FALSE
    ) 
  }
}

.check_dup_rows <- function(object) {
  check <- duplicated(object)
  if (any(check)) {
    check <- unique(
      c(
        which(check),
        which(duplicated(object, fromLast = TRUE))
      )
    )
    warning(
      "Duplicated row", .s(length(check)), 
      " (", .comma(sort(check)),
      ") detected in ",
      sub("ATO_", "@", class(object)[1]), ".",
      " Expect issues downstream.",
      call. = FALSE, immediate. = TRUE
    )
  }
}
