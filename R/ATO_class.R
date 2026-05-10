#' S4 class: ATO_ani
#'
#' An S4 class for the \code{\link{ATO}} animals (@ani).
#' Does not contain internal slots. It is a table of
#' either data.frame, data.table, or tibble format.
#'
#' The prototype @ani slot
#' contains the standard columns of the @ani slot.
#' Other columns are allowed, but these are necessary,
#' and must be of the designated types.
#'
#' The ani slot contains information on the animals tagged.
#'
#' Can be of type data.frame, data.table, or tibble,
#' depending on the @tbl slot.
#' See \code{\link{table_type}} for more details on
#' \code{\link{ATO}} table types.
#'
#' @format See \code{\link{make_ani}}.
#'
#' @keywords classes
#'
#' @seealso \code{\link{make_ani}}
#'
#' @name ATO_ani
#'
setClass("ATO_ani")

#' @rdname ATO_ani
#' @name .ATO_ani
#' @export
.ATO_ani <- data.frame(
  animal = NA_character_,
  release_location = NA_character_,
  release_datetime = as.POSIXct(NA_real_, tz = "UTC"),
  release_lat = NA_real_,
  release_lon = NA_real_,
  capture_location = NA_character_,
  capture_datetime = as.POSIXct(NA_real_, tz = "UTC"),
  capture_lat = NA_real_,
  capture_lon = NA_real_,
  valid = NA
)[-1, ]
class(.ATO_ani) <- c("ATO_ani", "data.frame")

#' S4 class: ATO_dep
#'
#' An S4 class for the \code{\link{ATO}} deployments (@dep).
#' Does not contain internal slots. It is a table of
#' either data.frame, data.table, or tibble format.
#'
#' The prototype @dep slot
#' contains the standard columns of the @dep slot.
#' Other columns are allowed, but these are necessary,
#' and must be of the designated types.
#'
#' Deployments include both the deployments of receivers (where the receiver
#' information is filled but the transmitter information isn't); transceivers
#' (where both the receiver and the transmitter information are filled in;
#' i.e. a receiver with a beacon tag); and reference tags (where there is no
#' receiver information, but there is transmitter information).
#'
#' Can be of type data.frame, data.table, or tibble,
#' depending on the @tbl slot.
#' See \code{\link{table_type}} for more details on
#' \code{\link{ATO}} table types.
#'
#' @format See \code{\link{make_dep}}.
#'
#' @keywords classes
#'
#' @seealso \code{\link{make_dep}}
#'
#' @name ATO_dep
#'
setClass("ATO_dep")

#' @rdname ATO_dep
#' @name .ATO_dep
#' @export
.ATO_dep <- data.frame(
  deploy_datetime = as.POSIXct(NA_real_, tz = "UTC"),
  recover_datetime = as.POSIXct(NA_real_, tz = "UTC"),
  deploy_location = NA_character_,
  deploy_lat = NA_real_,
  deploy_lon = NA_real_,
  deploy_z = NA_real_,
  recover_lat = NA_real_,
  recover_lon = NA_real_,
  receiver_manufacturer = NA_character_,
  receiver_model = NA_character_,
  receiver_serial = NA_character_,
  receiver_codeset = NA_character_,
  transmitter = NA_character_,
  transmitter_manufacturer = NA_character_,
  transmitter_ping_rate = NA_real_,
  transmitter_model = NA_character_,
  transmitter_serial = NA_character_,
  valid = NA
)[-1, ]
class(.ATO_dep) <- c("ATO_dep", "data.frame")

#' S4 class: ATO_det
#'
#' An S4 class for the \code{\link{ATO}} detections (@det).
#' Does not contain internal slots. It is a table of
#' either data.frame, data.table, or tibble format.
#'
#' The prototype @det slot contains the standard columns
#' of the @det slot. Other columns are allowed, but these
#' are necessary, and must be of the designated types.
#'
#' Can be of type data.frame, data.table, or tibble,
#' depending on the @tbl slot.
#' See \code{\link{table_type}} for more details on
#' \code{\link{ATO}} table types.
#'
#' @format See \code{\link{make_det}}.
#'
#' @keywords classes
#'
#' @seealso \code{\link{make_det}}
#'
#' @name ATO_det
#'
setClass("ATO_det")

#' @rdname ATO_det
#' @name .ATO_det
#' @export
.ATO_det <- data.frame(
  datetime = as.POSIXct(NA_real_, tz = "UTC"),
  frac_second = NA_real_,
  receiver_serial = NA_character_,
  transmitter = NA_character_,
  sensor_value = NA_real_,
  valid = NA
)[-1, ]
class(.ATO_det) <- c("ATO_det", "data.frame")

#' S4 class: ATO_obs
#'
#' An S4 class for the \code{\link{ATO}} observations (@obs).
#' Does not contain internal slots. It is a table of
#' either data.frame, data.table, or tibble format.
#'
#' The prototype @obs slot
#' contains the standard columns of the @obs slot.
#' Other columns are allowed, but these are necessary,
#' and must be of the designated types.
#'
#' The observations slot contains information about locations where an animal
#' was seen or a tag was heard. E.g. fin observations, or detections through
#' manual tracking.
#'
#' Can be of type data.frame, data.table, or tibble,
#' depending on the @tbl slot.
#' See \code{\link{table_type}} for more details on
#' \code{\link{ATO}} table types.
#'
#' @format See \code{\link{make_obs}}.
#'
#' @keywords classes
#'
#' @seealso \code{\link{make_obs}}
#'
#' @name ATO_obs
#'
setClass("ATO_obs")

#' @rdname ATO_obs
#' @name .ATO_obs
#' @export
.ATO_obs <- data.frame(
  datetime = as.POSIXct(NA_real_, tz = "UTC"),
  animal = NA_character_,
  transmitter = NA_character_,
  terminal = NA, # logical
  type = NA_character_,
  location = NA_character_,
  lat = NA_real_,
  lon = NA_real_,
  valid = NA
)[-1, ]
class(.ATO_obs) <- c("ATO_obs", "data.frame")

#' S4 class: ATO_tag
#'
#' An S4 class for the \code{\link{ATO}} tags (@tag).
#' Does not contain internal slots. It is a table of
#' either data.frame, data.table, or tibble format.
#'
#' The prototype @tag slot
#' contains the standard columns of the @tag slot.
#' Other columns are allowed, but these are necessary,
#' and must be of the designated types.
#'
#' The tag slot contains information on the different transmitters being
#' tracked. Tags with multiple transmitter codes are coded as multiple rows
#' associated with a single serial number and a single animal.
#'
#' Can be of type data.frame, data.table, or tibble,
#' depending on the @tbl slot.
#' See \code{\link{table_type}} for more details on
#' \code{\link{ATO}} table types.
#'
#' @format See \code{\link{make_tag}}.
#'
#' @keywords classes
#'
#' @seealso \code{\link{make_tag}}
#'
#' @name ATO_tag
#'
setClass("ATO_tag")

#' @rdname ATO_tag
#' @name .ATO_tag
#' @export
.ATO_tag <- data.frame(
  manufacturer = NA_character_,
  model = NA_character_,
  power_level = NA_real_,
  ping_rate = NA_real_, # seconds
  ping_variation = NA_real_, # 0 if fixed ping rate
  serial = NA_character_,
  transmitter = NA_character_,
  activation_datetime = as.POSIXct(NA_real_, tz = "UTC"),
  battery_life = NA_real_, # days
  sensor_type = NA_character_,
  sensor_unit = NA_character_,
  animal = NA_character_,
  valid = NA
)[-1, ]
class(.ATO_tag) <- c("ATO_tag", "data.frame")

#' S4 class: ATO_log
#'
#' An S4 class for the \code{\link{ATO}} log (@log).
#' Does not contain internal slots. It is a table of
#' either data.frame, data.table, or tibble format.
#'
#' The prototype @log slot
#' contains the standard columns of the @log slot.
#' Other columns are not allowed.
#'
#' Contains summary information of the actions performed throughout
#' the life of the ATO.
#'
#' Can be of type data.frame, data.table, or tibble,
#' depending on the @tbl slot.
#' See \code{\link{table_type}} for more details on
#' \code{\link{ATO}} table types.
#'
#' @format A data frame with 0 rows and 4 variables:
#' \describe{
#'   \item{datetime}{date and time of the log entry.}
#'   \item{type}{type of log entry.}
#'   \item{pkg}{name of the package that hosts the function that triggered the log entry.}
#'   \item{fun}{name of the function that triggered the log entry.}
#'   \item{call}{full function call, for debugging.}
#'   \item{log}{log entry.}
#' }
#'
#' @keywords classes
#'
#' @name ATO_log
#'
setClass("ATO_log")

#' @rdname ATO_log
#' @name .ATO_log
#' @export
.ATO_log <- data.frame(
  datetime = as.POSIXct(NA_real_, tz = "UTC"),
  type = NA_character_,
  pkg = NA_character_,
  fun = NA_character_,
  call = NA_character_,
  log = NA_character_
)[-1, ]
class(.ATO_log) <- c("ATO_log", "data.frame")

#' S4 class: ATO
#'
#' The Animal Tracking Object (ATO) class contains slots that allow
#' researchers to package their study data in a standard format, to be used
#' by the R packages that collectively make the trackyverse.
#'
#' @slot ani The animals slot. See \code{\link{ATO_ani}}.
#' @slot dep The deployments slot. See \code{\link{ATO_dep}}.
#' @slot det The detections slot. See \code{\link{ATO_det}}.
#' @slot obs The observations slot. See \code{\link{ATO_obs}}.
#' @slot tag The tags slot. See \code{\link{ATO_tag}}.
#' @slot log The log slot. See \code{\link{ATO_log}}.
#' @slot pkg A list slot reserved for package outputs.
#'
#' @export
#' 
setClass(
  "ATO",
  slots = c(
    ani = "ATO_ani",
    dep = "ATO_dep",
    det = "ATO_det",
    obs = "ATO_obs",
    tag = "ATO_tag",
    log = "ATO_log",
    pkg = "list"
  ),
  prototype = list(
    ani = .ATO_ani,
    dep = .ATO_dep,
    det = .ATO_det,
    obs = .ATO_obs,
    tag = .ATO_tag,
    log = .ATO_log,
    pkg = list()
  )
)

#' Validity checks for objects of class ATO
#'
#' @keywords internal
#'
#' @name ATO validity
#'
setValidity("ATO", function(object) {
  slots <- c("ani", "dep", "det", "obs", "tag")
  for (i in slots) {
    check(slot(object, i), tz = tzone(object), tbl = table_type(object))
  }
  return(TRUE)
})
