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
init_ato <- function(det, dep, tag, ani, obs) {
  ato <- new("ATO",
             det = ATO:::.ATO_det,
             dep = ATO:::.ATO_dep,
             tag = ATO:::.ATO_tag,
             ani = ATO:::.ATO_ani,
             obs = ATO:::.ATO_obs,
             log = ATO:::.ATO_log,
             tbl = ATO:::.ATO_tbl,
             pkg = list())

  if (!missing(det)) {
    ato <- add(ato, det)
  }
  if (!missing(dep)) {
    ato <- add(ato, dep)
  }
  if (!missing(tag)) {
    ato <- add(ato, tag)
  }
  if (!missing(ani)) {
    ato <- add(ato, ani)
  }
  if (!missing(obs)) {
    ato <- add(ato, obs)
  }
  return(ato)
}
