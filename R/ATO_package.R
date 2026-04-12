#' ATO: Animal Tracking Object Class
#'
#' The ATO is the core object format of the trackyverse (see trackyverse.org). 
#' This package is intented to be very light-weight, providing only the 
#' essentials to build and perform basic manipulations of the ATO S4 class.
# ' The ATO is made with intercompatibility in mind, supporting tables of
#' either data.frame, data.table, or tibble format.
#'
#'
#' ATO is an S4-class object with several slots. Use the make functions listed
#' below to prepare each of the slots, and then \code{\link{init_ato}} to
#' create the ato object.
#' 
#' # @ani
#' 
#' The @ani(mal) slot is intended to store information about animals that are
#' relevant for the study. The ATO package includes the function 
#' \code{\link{make_ani}} to assist users in formatting this information
#' correctly.
#' 
#' # @dep
#' 
#' The @dep(loyments) slot contains information about the deployment of
#' stationary infrastructure, such as receivers, ping-tags, and transceivers.
#' Check out the cuntion \code{\link{make_dep}} for more details.
#' 
#' # @det
#' 
#' The @det(ections) slot contains all the detections recorded by stationary
#' receivers. See \code{\link{make_det}}.
#' 
#' # @tag
#' 
#' The @tag slot contains information on all the (non-stationary) transmitters
#' deployed in the study. These tags are then associated with the animals
#' listed in the @ani slot. See \code{\link{make_tag}}.
#' 
#' # @obs
#' 
#' The @obs(ervations) slot contains information on direct or indirect
#' observations that were not recorded by stationary receivers. e.g. manual
#' tracking, or binocular spotting. See  \code{\link{make_obs}}
#' 
#' # @log
#' 
#' The @log slot contains a history of the actions performed on the ato
#' object. Packages within the trackyverse know how to interact with this
#' slot automatically to add information. The user may also use the function
#' \code{\link{log_event}} to manually add additional log lines.
#' 
#' # @tbl
#' 
#' This internal slot indicates the type of tables contained within the
#' ato object. ATO supports three types of tables: data.frame, data.table,
#' and tibble. Do not edit this slot manually. See \code{\link{table_type}}.
#' 
#' ## @pkg
#' 
#' The @pkg slot is intended to allow expandability without compromising
#' structure. Packages within the trackyverse that create new outputs may
#' store them inside this slot.
#'
#'
#' @importFrom methods is new slot show validObject findFunction
#' @importFrom utils flush.console
#' @import stats
#'
#' @keywords internal
#'
#' @docType package
#' @name ATO
"_PACKAGE"