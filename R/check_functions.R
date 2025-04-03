#' Check that the table type matches the current global option
#' 
#' @param object A prospective ATO slot object.
#' 
#' @keywords internal
#' 
#' @return Nothing. Called to stop() if needed.
#' 
check_ato_table_type <- function(object) {
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
check_ato_table_cols <- function(object, ref) {
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
