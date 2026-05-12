#' Helper function to turn debug mode on or off
#' 
#' @param value logical. Should matching be done on the fly?
#' 
#' @return the current value for ATO_match_immediate if no new value is provided
#' 
#' @examples
#' # Use without arguments to verify the current state, or
#' # with TRUE of FALSE to activate/deactivate debug mode
#' ato_debug()
#' 
#' @export
#' 
ato_debug <- function(value = c(TRUE, FALSE)) {
  if (missing(value)) {
    return(getOption("ATO_debug", default = FALSE))
  } else {
    if (length(value) != 1) {
      stop ("value must of length 1.", call. = FALSE)
    }
    if (!is.logical(value)) {
      stop ("value must be logical.", call. = FALSE)
    }
    options(ATO_debug = value)
    if (value) {
      message("M: ATO debug mode activated.")
    } else {
      message("M: ATO debug mode deactiviated.")
    }
  }
}

#' Helper function send a message at the start of each function
#' 
#' This function is intended for developers. If you want to benefit
#' from the same debug properties as the ATO functions, include a call
#' to this function at the head of your own.
#' 
#' @return nothing, called for side effects
#' 
#' @examples
#' # include this at the top of your function. e.g.
#' x <- function(arg) {
#'   ato_debug_header()
#'   return(arg * 2)
#' }
#' 
#' # and then use ato_debug to get a message when this function starts
#' old_ato_debug <- ato_debug()
#' ato_debug(TRUE)
#' x(arg = 2)
#' ato_debug(old_ato_debug)
#' 
#' @export
#' 
ato_debug_header <- function() {
  fun_call <- sys.calls()[[sys.nframe() - 1]]
  fun <- deparse(fun_call[[1]])
  if (ato_debug()) {
    message(
      "Debug: Starting ",
      fun,
      " (",
      Sys.time(),
      ")"
    )
  }
}
