#' check generic
#' 
#' Checks a prospective ATO slot object to confirm that the
#' contents are as expected. Matches it with the respective prototype reference.
#' 
#' @param object The prospective ATO slot object
#' 
#' @return Nothing. Called to stop() if needed.
#' 
#' @keywords internal
#' 
setGeneric("check", function(object) standardGeneric("check"))

#' @rdname check
setMethod("check", "ATO_det", function(object) {
  # object class check
  check_ato_table_type(object)
  # column class check
  check_ato_table_cols(object, .ATO_det)
})

#' @rdname check
setMethod("check", "ATO_dep", function(object) {
  # object class check
  check_ato_table_type(object)
  # column names check
  check_ato_table_cols(object, .ATO_dep)
})

#' @rdname check
setMethod("check", "ATO_tag", function(object) {
  # object class check
  check_ato_table_type(object)
  # column class check
  check_ato_table_cols(object, .ATO_tag)
})

#' @rdname check
setMethod("check", "ATO_ani", function(object) {
  # object class check
  check_ato_table_type(object)
  # column class check
  check_ato_table_cols(object, .ATO_ani)
})

#' @rdname check
setMethod("check", "ATO_obs", function(object) {
  # object class check
  check_ato_table_type(object)
  # column class check
  check_ato_table_cols(object, .ATO_obs)
})

#' @rdname check
setMethod("check", "ATO_tbl", function(object) {
  if (!object %in% c("data.frame", "data.table", "tibble")) {
    stop("@tbl value is invalid. This should never happen. Contact dev.",
         call. = FALSE)
  }
})
