#' Generic to check if an ATO has data in a given slot
#' 
#' @param object an \code{\link{ATO}}
#' @param value a vector with the names of the requested slots
#' @param column a vector of column names to find within the requested slots
#' @param allow_NA Should NAs be allowed in the inspected columns?
#' @param error Should the code execution stop if the requested slots are empty?
#' 
#' @return TRUE if the slots have data, FALSE if they're empty and
#'   error = FALSE. Throws an error if the slots are empty and error = TRUE.
#' 
#' @examples
#' # check if ATO object has detections
#' has(example_ato, "det")
#' 
#' # check if ATO has observations
#' has(example_ato, "obs")
#' 
#' # alternatively, this would error
#' # has(example_ato, "obs", error = TRUE)
#' 
#' # check specific columns in slot
#' has(example_ato, "tag", c("release_datetime", "power_level"))
#' has(example_ato, "tag", c("test", "power_level"))
#' # has(example_ato, "tag", c("test", "power_level"), error = TRUE)
#' 
#' # check that columns don't have NAs
#' # has(example_ato, "tag",
#' #    c("release_datetime", "power_level"),
#' #    allow_NA = FALSE, error = TRUE)
#' 
#' @export
#' 
setGeneric("has",
  function(
    object,
    value,
    column,
    allow_NA = TRUE,
    error = FALSE)
  standardGeneric("has"))

#' @rdname has
setMethod("has", c(object = "ATO"),
  function(
    object,
    value,
    column,
    allow_NA = TRUE,
    error = FALSE) {
  is_ato(object) # not necessary, the method won't let it pass if it is not ATO.
  
  link <- !(value %in% c("det", "dep", "tag", "ani", "obs", "log"))
  if (any(link)) {
    aux <- sum(link)
    stop(.comma(value[link]), " do", .es(aux),
         " not match ATO slot names.", call. = FALSE)
  }
  check <- sapply(value, function(i) {
    nrow(slot(object, i)) == 0
  })
  if (any(check)) {
    aux <- sum(check)
    if (error) {
      stop("The following slot",
           .s(aux),
           " ",
           .is(aux),
           " empty: ",
           .comma(value[check]),
           call. = FALSE)
    } else {
      return(FALSE)
    }
  }
  if (!missing(column)) {
    for (i in value) {
      check <- !(column %in% colnames(slot(object, i)))
      if (any(check)) {
        if (error) {
          stop("Slot ", i,
               " does not contain column",
               .s(sum(check)),
               " ",
               .comma(column[check]),
               ".",
               call. = FALSE)
        } else {
          return(FALSE)
        }
      }
      if (!allow_NA) {
        if (table_type(object) == "data.table") {
          check <- apply(slot(object, i)[, column, with = FALSE],
                         2, function(i) any(is.na(i)))
        } else {
          check <- apply(slot(object, i)[, column],
                         2, function(i) any(is.na(i)))
        }
        if (any(check)) {
          if (error) {
            stop("NAs found in column",
                 .s(sum(check)),
                 " ",
                 .comma(names(check)[check]),
                 " of slot ",
                 i,
                 ".",
                 call. = FALSE)
          } else {
            return(FALSE)
          }
        }
      }
    }
  }
  return(TRUE)
})
