setGeneric("check", function(object) standardGeneric("check"))

setMethod("check", "ATO_det", function(object) {
  # object class check
  check_ato_table_type(object)
  # column class check
  check_ato_table_cols(object, .ATO_det)
})

setMethod("check", "ATO_dep", function(object) {
  # object class check
  check_ato_table_type(object)
  # column names check
  check_ato_table_cols(object, .ATO_dep)
})

setMethod("check", "ATO_tag", function(object) {
  # object class check
  check_ato_table_type(object)
  # column class check
  check_ato_table_cols(object, .ATO_tag)
})

setMethod("check", "ATO_ani", function(object) {
  # object class check
  check_ato_table_type(object)
  # column class check
  check_ato_table_cols(object, .ATO_ani)
})

setMethod("check", "ATO_obs", function(object) {
  # object class check
  check_ato_table_type(object)
  # column class check
  check_ato_table_cols(object, .ATO_obs)
})

setMethod("check", "ATO_tbl", function(object) {
  if (!object %in% c("data.frame", "data.table", "tibble")) {
    stop("@tbl value is invalid. This should never happen. Contact dev.",
         call. = FALSE)
  }
})
