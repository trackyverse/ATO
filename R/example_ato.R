#' Example ATO object
#'
#' The ATO package comes with a small example ATO object to allow for quick
#' exploration of the object's structure
#'
#' @format An S4 object with 8 slots:
#' \describe{
#'   \item{ani}{Information about tagged animals. See \code{\link{make_ani}}}
#'   \item{dep}{Information about deployments. See \code{\link{make_dep}}}
#'   \item{det}{Information about detections. See \code{\link{make_det}}}
#'   \item{tag}{Information about tags. See \code{\link{make_tag}}}
#'   \item{obs}{Information about observations. See \code{\link{make_obs}}}
#'   \item{log}{Log of actions. See \code{\link{event}}}
#'   \item{tbl}{ATO table type. See \code{\link{table_type}}}
#'   \item{pkg}{Reserved for other packages of the trackyverse}
#' }
#' @source The data in the example is the same as provided by the package actel,
#'  and was collected by the author
#'
#' @keywords internal
#'
#' @name example_ato
NULL
