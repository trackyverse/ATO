#' Check that the table type matches the current global option
#' 
#' @param object A prospective ATO slot object.
#' 
#' @keywords internal
#' 
#' @return Nothing. Called to stop() if needed.
#' 
.check_ato_table_type <- function(object) {
  ato_table_type <- getOption("ATO_table_type", default = "data.frame")
  if (is(object, "data.table") & !ato_table_type == "data.table") {
    stop("object is of type data.table but option ATO_table_type is set to ",
         ato_table_type, ".",
         " To change, run options(ATO_table_type = 'data.table').",
         call. = FALSE)
  }
  if (is(object, "tibble") & !ato_table_type == "tibble") {
    stop("object is of type tibble but option ATO_table_type is set to ",
         ato_table_type, ".",
         " To change, run options(ATO_table_type = 'tibble').",
         call. = FALSE)
  }
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
  items <- ""
  for (i in colnames(ref)) {
    target_col <- match(i, colnames(object))
    class_check <- class(ref[, i]) %in% class(object[, target_col])
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
#' @param object A prospective ATO slot table
#' @param tz optional; a timezone to apply to all time columns
#' 
#' @keywords internal
#' 
.check_column_tzones <- function(object, tz) {
  link <- sapply(1:ncol(object), function(i) "POSIXt" %in% class(object[[i]]))
  if (any(link)) {
    time_cols <- colnames(object)[link]
    if (!missing(tz)) {
      for (i in time_cols) {
        attributes(object[[i]])$tzone <- tz
      }
    } else {
      tzones <- sapply(time_cols, function(i) attributes(object[[i]])$tzone)
      # check if any tz is missing
      if (any(tzones == "")) {
        aux <- which(tzones == "")
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
# # How to use:
# object <- get_tag(example_ato)
# 
# # all in the same tz, works
# object <- .check_column_tzones(object)
# 
# # force fail by missing tz
# attributes(object$release_datetime)$tzone <- ""
# object <- .check_column_tzones(object)
# 
# # force fail by differing tzs
# attributes(object$release_datetime)$tzone <- "UTC"
# object <- .check_column_tzones(object)
# 
# # correct fail by adding tz argument
# object <- .check_column_tzones(object, "Europe/Copenhagen")
# 
# # confirm correction applied
# attributes(object$activation_datetime)$tzone
# attributes(object$release_datetime)$tzone


#' Check that all slots are in the same timezone
#' 
#' @param x An ATO object
#' 
#' @keywords internal
#' 
.check_slot_tzones <- function(x) {
  slots <- c("ani", "dep", "det", "tag", "obs")
  
  tzones <- sapply(slots, function(s) {
    if (!has(x, s)) {
      return(NULL)
    }

    x_s <- slot(x, s)

    link <- sapply(1:ncol(x_s), function(i) "POSIXt" %in% class(x_s[[i]]))
    if (any(link)) {
      time_cols <- colnames(x_s)[link]
      tzones <- sapply(time_cols, function(i) attributes(x_s[[i]])$tzone)
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
  })
  tzones <- unlist(tzones)

  if (length(tzones) == 0) {
    return(NULL) # no time data yet
  }

  if (length(unique(tzones)) > 1) {
    stop("Not all slots are in the same timezone (see below).",
         " See e.g. ?make_ani() for assistance using the tz argument.\n",
         paste0(paste0("@", names(tzones), ": ", tzones), collapse = "\n"),
         call. = FALSE)
  } else {
    return(unique(tzones))
  }
}
# # how to use:
# # all in the same tz, returns the tz
# .check_slot_tzones(example_ato)
# 
# # force fail by missing tz
# object <- example_ato
# attributes(object@det$datetime)$tzone <- "UTC"
# check_slot_tzones(object)
