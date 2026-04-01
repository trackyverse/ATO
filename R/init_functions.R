#' Initiate an \code{\link{ATO}}
#'
#' This is a wrapper around the usual way of creating S4 objects through new().
#' The main purpose of this function is to ensure the ATO is created with the
#' right table type. See \code{\link{table_type}} for more details.
#'
#' @param det an object of class ATO_det. See \code{\link{make_det}}.
#' @param dep an object of class ATO_dep. See \code{\link{make_dep}}.
#' @param tag an object of class ATO_tag. See \code{\link{make_tag}}.
#' @param ani an object of class ATO_ani. See \code{\link{make_ani}}.
#' @param obs an object of class ATO_obs. See \code{\link{make_obs}}.
#'
#' @return an \code{\link{ATO}}
#'
#' @export
#'
init_ato <- function(det, dep, tag, ani, obs, silent = FALSE) {
  ato <- new(
    "ATO",
    det = .ATO_det,
    dep = .ATO_dep,
    tag = .ATO_tag,
    ani = .ATO_ani,
    obs = .ATO_obs,
    log = .ATO_log,
    tbl = .ATO_tbl,
    pkg = list()
  )

  old_match_immediate <- getOption("ATO_match_immediate", default = TRUE)
  on.exit(options(ATO_match_immediate = old_match_immediate))
  options(ATO_match_immediate = FALSE)
  
  if (!missing(det)) {
    ato <- add(ato, det, silent = silent)
  }
  if (!missing(dep)) {
    ato <- add(ato, dep, silent = silent)
  }
  if (!missing(tag)) {
    ato <- add(ato, tag, silent = silent)
  }
  if (!missing(ani)) {
    ato <- add(ato, ani, silent = silent)
  }
  if (!missing(obs)) {
    ato <- add(ato, obs, silent = silent)
  }
  
  if (old_match_immediate) {
    ato <- match_update(ato, silent = silent)
  }
  
  return(ato)
}
