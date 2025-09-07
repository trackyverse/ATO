#' Log a new event
#'
#' Logs useful information as the analysis progresses.
#'
#' @param ato the ATO object to which to log the entry.
#' @param type One of several event flags:
#'  \itemize{
#'    \item 'log' saves to the log without displaying.
#'    \item 'message' (or 'msg') displays a message on screen.
#'    \item 'warning' displays a warning on screen.
#'    \item 'debug' flags the entry as debug (not shown by default).
#'    \item 'comment' user comment entry.
#'  }
#' @param ... The text fragments that compose the event message.
#' @param blame Not used currently
#' 
#' @return The updated ATO.
#'
#' @export
#'
log_event <- function(ato, type = c("log", "message", "msg", "warning", "debug",
                      "comment"), ..., blame) {
  is_ato(ato)

  type <- match.arg(type)
  if (type == "msg") type <- "message"

  fun_call <- sys.calls()[[sys.nframe()-1]]
  fun <- deparse(fun_call[[1]])
  fun_call <- deparse(fun_call)
  pkg <- findFunction(fun)
  if (length(pkg) == 0) {
    pkg <- "not loaded (likely :: call)"
  } else {
    pkg <- unlist(lapply(pkg, attributes))["name"]
    pkg <- sub("package:", "", pkg)
  }

  event_text <- paste0(...)

  new_line <- data.frame(
    datetime = Sys.time(),
    type = type,
    pkg = pkg,
    fun = fun,
    call = fun_call,
    log = event_text
  )
  updated_log <- rbind(ato@log, new_line)
  rownames(updated_log) <- NULL
  class(updated_log) <- class(ato@log)
  ato@log <- updated_log

  if (type == "warning") {
    warning(event_text, immediate. = TRUE, call. = FALSE)    
  }
  if (type == "message") {
    message(event_text)
  }
  flush.console() # for buffered consoles.

  return(ato)
}

#' Log a new comment
#'
#' Logs a user-written comment into the log.
#'
#' @param ato the ATO object to which to log the comment
#' @param ... The text fragments that compose the comment.
#' 
#' @return the updated ATO.
#'
#' @export
#'
log_comment <- function(ato, ...) {
  ato <- log_event(ato, type = "comment", ...)
  return(ato)
}
