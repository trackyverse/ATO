#' What is the timezone of this ATO object?
#' 
#' You may use tzone to check and display the timezone of the datetime data
#' of an ATO object or of any ATO slot object.
#' 
#' @param x an \code{\link{ATO}} or ATO slot object.
#' 
#' @return Returns the timezone of the ATO object.
#' 
#' @examples
#' # check the timezone of the example ATO
#' tzone(example_ato)
#' 
#' @export
#' 
setGeneric("tzone", function(x) standardGeneric("tzone"))

#' @rdname tzone
setMethod("tzone", "ATO", function(x) {
  .check_ato_tzones(x)
})

#' @rdname tzone
setMethod("tzone", "ATO_ani", function(x) {
  .check_slot_tzones(x)
})

#' @rdname tzone
setMethod("tzone", "ATO_dep", function(x) {
  .check_slot_tzones(x)
})

#' @rdname tzone
setMethod("tzone", "ATO_det", function(x) {
  .check_slot_tzones(x)
})

#' @rdname tzone
setMethod("tzone", "ATO_obs", function(x) {
  .check_slot_tzones(x)
})

#' @rdname tzone
setMethod("tzone", "ATO_tag", function(x) {
  .check_slot_tzones(x)
})

#' Modify the time zone of an ATO object
#' 
#' @param x an \code{\link{ATO}} or ATO slot object.
#' @param value the desired time zone
#' 
#' @return Nothing, acts directly on x
#' 
#' @examples
#' # check the timezone of the example ATO
#' tzone(example_ato)
#' summary(get_det(example_ato))
#' 
#' # change the tz to UTC
#' tzone(example_ato) <- "UTC"
#' tzone(example_ato)
#' summary(get_det(example_ato))
#' 
#' # clean_up
#' tzone(example_ato) <- "Europe/Copenhagen"
#' 
#' @export
#' 
setGeneric("tzone<-", function(x, value) standardGeneric("tzone<-"))

#' @rdname tzone-set
setMethod("tzone<-", "ATO", function(x, value) {
  slots <- c("ani", "dep", "det", "obs", "tag")
  
  for (s in slots) {
    if (!has(x, s)) {
      next()
    }
    x_s <- slot(x, s)

    link <- sapply(1:ncol(x_s), function(i) "POSIXt" %in% class(x_s[[i]]))
    if (any(link)) {
      time_cols <- colnames(x_s)[link]
      for (i in time_cols) {
        attributes(slot(x, s)[[i]])$tzone <- value
      }
    }
  }

  return(x)
})

#' @rdname tzone-set
setMethod("tzone<-", "ATO_ani", function(x, value) {
  x <- .check_column_tzones(x, tz = value)
  return(x)
})

#' @rdname tzone-set
setMethod("tzone<-", "ATO_det", function(x, value) {
  x <- .check_column_tzones(x, tz = value)
  return(x)
})

#' @rdname tzone-set
setMethod("tzone<-", "ATO_dep", function(x, value) {
  x <- .check_column_tzones(x, tz = value)
  return(x)
})

#' @rdname tzone-set
setMethod("tzone<-", "ATO_obs", function(x, value) {
  x <- .check_column_tzones(x, tz = value)
  return(x)
})

#' @rdname tzone-set
setMethod("tzone<-", "ATO_tag", function(x, value) {
  x <- .check_column_tzones(x, tz = value)
  return(x)
})
