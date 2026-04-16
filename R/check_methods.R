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
setGeneric("check", function(object, tz) standardGeneric("check"))

#' @rdname check
setMethod("check", "ATO_det", function(object, tz) {
  # object class check
  .check_ato_table_type(object)
  # column class check
  .check_ato_table_cols(object, .ATO_det)
  # check the timezones
  object <- .check_column_tzones(object, tz = tz)
  return(object)
})

#' @rdname check
setMethod("check", "ATO_dep", function(object, tz) {
  # object class check
  .check_ato_table_type(object)
  # column names check
  .check_ato_table_cols(object, .ATO_dep)
  # check the timezones
  object <- .check_column_tzones(object, tz = tz)
  return(object)
})

#' @rdname check
setMethod("check", "ATO_tag", function(object, tz) {
  # object class check
  .check_ato_table_type(object)
  # column class check
  .check_ato_table_cols(object, .ATO_tag)
  # check the timezones
  object <- .check_column_tzones(object, tz = tz)
  return(object)
})

#' @rdname check
setMethod("check", "ATO_ani", function(object, tz) {
  # object class check
  .check_ato_table_type(object)
  # column class check
  .check_ato_table_cols(object, .ATO_ani)
  # check the timezones
  object <- .check_column_tzones(object, tz = tz)
  return(object)
})

#' @rdname check
setMethod("check", "ATO_obs", function(object, tz) {
  # object class check
  .check_ato_table_type(object)
  # column class check
  .check_ato_table_cols(object, .ATO_obs)
  # check the timezones
  object <- .check_column_tzones(object, tz = tz)
  return(object)
})

#' @rdname check
setMethod("check", "ATO_tbl", function(object) {
  if (!object %in% c("data.frame", "data.table", "tibble")) {
    stop("@tbl value is invalid. This should never happen. Contact dev.",
         call. = FALSE)
  }
})
