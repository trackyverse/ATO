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
setGeneric("check", function(object, tz, tbl) standardGeneric("check"))

#' @rdname check
setMethod("check", "ATO_ani", function(object, tz, tbl) {
  # object class check
  if (!is.null(tbl)) {
    table_type(object, tbl)
  }
  # column class check
  .check_ato_table_cols(object, .ATO_ani)
  # check the timezones
  object <- .check_column_tzones(object, tz = tz)
  return(object)
})

#' @rdname check
setMethod("check", "ATO_dep", function(object, tz, tbl) {
  # object class check
  if (!is.null(tbl)) {
    table_type(object, tbl)
  }
  # column names check
  .check_ato_table_cols(object, .ATO_dep)
  # check the timezones
  object <- .check_column_tzones(object, tz = tz)
  return(object)
})

#' @rdname check
setMethod("check", "ATO_det", function(object, tz, tbl) {
  # object class check
  if (!is.null(tbl)) {
    table_type(object, tbl)
  }
  # column class check
  .check_ato_table_cols(object, .ATO_det)
  # check the timezones
  object <- .check_column_tzones(object, tz = tz)
  return(object)
})

#' @rdname check
setMethod("check", "ATO_obs", function(object, tz, tbl) {
  # object class check
  if (!is.null(tbl)) {
    table_type(object, tbl)
  }
  # column class check
  .check_ato_table_cols(object, .ATO_obs)
  # check the timezones
  object <- .check_column_tzones(object, tz = tz)
  return(object)
})

#' @rdname check
setMethod("check", "ATO_tag", function(object, tz, tbl) {
  # object class check
  if (!is.null(tbl)) {
    table_type(object, tbl)
  }
  # column class check
  .check_ato_table_cols(object, .ATO_tag)
  # check the timezones
  object <- .check_column_tzones(object, tz = tz)
  return(object)
})
