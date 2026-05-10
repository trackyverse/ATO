#' Initiate an \code{\link{ATO}}
#'
#' This is a wrapper around the usual way of creating S4 objects through new().
#' The main purpose of this function is to ensure the ATO is created with the
#' right table type. See \code{\link{table_type}} for more details.
#'
#' @param ani an object of class ATO_ani. See \code{\link{make_ani}}.
#' @param dep an object of class ATO_dep. See \code{\link{make_dep}}.
#' @param det an object of class ATO_det. See \code{\link{make_det}}.
#' @param obs an object of class ATO_obs. See \code{\link{make_obs}}.
#' @param tag an object of class ATO_tag. See \code{\link{make_tag}}.
#' @param silent Supresses summary messages
#'
#' @examples
#' # split apart the example ATO
#' ani <- get_ani(example_ato)
#' dep <- get_dep(example_ato)
#' det <- get_det(example_ato)
#' tag <- get_tag(example_ato)
#' 
#' # and now use the parts to build a new ato
#' x <- init_ato(ani = ani,
#'               dep = dep,
#'               det = det,
#'               tag = tag)
#' 
#' # clean up
#' rm(ani, dep, det, tag, x)
#' 
#' @return an \code{\link{ATO}}
#'
#' @export
#'
init_ato <- function(ani, dep, det, obs, tag, silent = FALSE) {
  ato <- new(
    "ATO",
    ani = .ATO_ani,
    dep = .ATO_dep,
    det = .ATO_det,
    obs = .ATO_obs,
    tag = .ATO_tag,
    log = .ATO_log,
    pkg = list()
  )

  table_type(ato) <- getOption("ATO_table_type", default = "data.frame")

  old_match_immediate <- getOption("ATO_match_immediate", default = TRUE)
  on.exit(options(ATO_match_immediate = old_match_immediate))
  options(ATO_match_immediate = FALSE)
  
  if (!missing(ani)) {
    ato <- set_ani(ato, ani, silent = silent)
  }
  if (!missing(dep)) {
    ato <- set_dep(ato, dep, silent = silent)
  }
  if (!missing(det)) {
    ato <- set_det(ato, det, silent = silent)
  }
  if (!missing(obs)) {
    ato <- set_obs(ato, obs, silent = silent)
  }
  if (!missing(tag)) {
    ato <- set_tag(ato, tag, silent = silent)
  }
  
  if (old_match_immediate) {
    ato <- match_update(ato, silent = silent)
  }
  
  return(ato)
}
