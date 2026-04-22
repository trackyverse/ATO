#' What is the timezone of this ATO?
#' 
#' @param x an \code{\link{ATO}}
#' 
#' @return Returns the timezone of the ATO.
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
  .check_slot_tzones(x)
})

#' Modify the time zone of the ATO
#' 
#' @param x an \code{\link{ATO}}
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
  slots <- c("ani", "dep", "det", "tag", "obs")
  
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
