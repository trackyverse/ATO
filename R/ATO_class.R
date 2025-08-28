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
#' @format A data frame with 0 rows and 5 variables:
#' \describe{
#'   \item{datetime}{date and time, posixct format.}
#'   \item{frac_second}{fractional second, numeric.}
#'   \item{receiver_serial}{receiver serial number, integer.}
#'   \item{transmitter}{transmitter code, character.}
#'   \item{sensor_value}{reported sensor value, numeric.}
#' }
#'
#' @keywords classes
#'
#' @seealso make_det
#'
#' @name ATO_det
#'
setClass("ATO_det")

#' @rdname ATO_det
#' @name .ATO_det
#' @export
.ATO_det <- data.frame(
  datetime = as.POSIXct(NA_real_),
  frac_second = NA_real_,
  receiver_serial = NA_integer_,
  transmitter = NA_character_,
  sensor_value = NA_real_,
  valid = NA
)[-1, ]
class(.ATO_det) <- c("ATO_det", "data.frame")

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
#' @format A data frame with 0 rows and 16 variables:
#' \describe{
#'   \item{receiver_model}{Model of the receiver, character.}
#'   \item{receiver_serial}{Receiver serial number, integer.}
#'   \item{receiver_codeset}{Codeset of the receiver, character.}
#'   \item{deploy_location}{Name of the location where the receiver was deployed, character.}
#'   \item{deploy_datetime}{date and time of the deployment, posixct.}
#'   \item{deploy_lat}{latitude of the deployment. Preferably in WGS84, numeric.}
#'   \item{deploy_lon}{longitude of the deployment. Preferably in WGS84, numeric.}
#'   \item{deploy_z}{depth of the deployment, as measured from the reference surface of the water body, numeric.}
#'   \item{recover_datetime}{date and time of the recovery, posixct.}
#'   \item{recover_lat}{latitude of the recovery point. Preferably in WGS84, numeric.}
#'   \item{recover_lon}{longitude of the recovery point. Preferably in WGS84, numeric.}
#'   \item{transmitter}{Transmitter code for a beacon/reference tag, character.}
#'   \item{transmitter_manufacturer}{Manufacturer of the transmitter, character.}
#'   \item{transmitter_ping_rate}{Expected ping rate of the transmitter, numeric. In seconds.}
#'   \item{transmitter_model}{Model of the transmitter, character.}
#'   \item{transmitter_serial}{Serial number of the transmitter, integer.}
#' }
#'
#' @keywords classes
#'
#' @seealso make_dep
#'
#' @name ATO_dep
#'
setClass("ATO_dep")

#' @rdname ATO_dep
#' @name .ATO_dep
#' @export
.ATO_dep <- data.frame(
  receiver_model = NA_character_,
  receiver_serial = NA_integer_,
  receiver_codeset = NA_character_,
  deploy_location = NA_character_,
  deploy_datetime = as.POSIXct(NA_real_),
  deploy_lat = NA_real_,
  deploy_lon = NA_real_,
  deploy_z = NA_real_,
  recover_datetime = as.POSIXct(NA_real_),
  recover_lat = NA_real_,
  recover_lon = NA_real_,
  transmitter = NA_character_,
  transmitter_manufacturer = NA_character_,
  transmitter_ping_rate = NA_real_,
  transmitter_model = NA_character_,
  transmitter_serial = NA_integer_
)[-1, ]
class(.ATO_dep) <- c("ATO_dep", "data.frame")

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
#' The tag slot contains information on the different transmitters being tracked.
#' Tags with multiple transmitter codes are coded as multiple rows associated
#' with a single serial number and a single animal.
#'
#' Can be of type data.frame, data.table, or tibble,
#' depending on the @tbl slot.
#' See \code{\link{table_type}} for more details on
#' \code{\link{ATO}} table types.
#'
#' @format A data frame with 0 rows and 12 variables:
#' \describe{
#'   \item{manufacturer}{Manufacturer of the transmitter, character.}
#'   \item{model}{Model of the transmitter, character.}
#'   \item{power_level}{Power level of the transmitter, real.}
#'   \item{ping_rate}{Expected ping rate of the transmitter, numeric. In seconds.}
#'   \item{ping_variation}{Range of the variation added between pings, to reduce tag collisions, numeric. In seconds}
#'   \item{serial}{Serial number of the tag, integer.}
#'   \item{transmitter}{Transmitter code, character.}
#'   \item{activation_datetime}{date and time of the tag activation, posixct.}
#'   \item{battery_life}{expected battery duration of the tag, numeric.}
#'   \item{sensor_type}{Type of sensor data associated with the transmitter, character.}
#'   \item{sensor_unit}{Unit of the data associated with the transmitter, character.}
#'   \item{animal}{Name of the animal that received the tag, character.}
#' }
#'
#' @keywords classes
#'
#' @seealso make_tag
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
  serial = NA_integer_,
  transmitter = NA_character_,
  activation_datetime = as.POSIXct(NA_real_),
  battery_life = NA_real_, # days
  sensor_type = NA_character_,
  sensor_unit = NA_character_,
  animal = NA_character_
)[-1, ]
class(.ATO_tag) <- c("ATO_tag", "data.frame")

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
#' @format A data frame with 0 rows and 9 variables:
#' \describe{
#'   \item{animal}{Name of the animal that received the tag, character.}
#'   \item{capture_location}{Name of the location where the animal was captured, character.}
#'   \item{capture_datetime}{date and time of the capture, posixct.}
#'   \item{capture_lat}{latitude of the capture. Preferably in WGS84, numeric.}
#'   \item{capture_lon}{longitude of the capture. Preferably in WGS84, numeric.}
#'   \item{release_location}{Name of the location where the animal was released, character.}
#'   \item{release_datetime}{date and time of the release, posixct.}
#'   \item{release_lat}{latitude of the release. Preferably in WGS84, numeric.}
#'   \item{release_lon}{longitude of the release. Preferably in WGS84, numeric.}
#' }
#'
#' @keywords classes
#'
#' @seealso make_ani
#'
#' @name ATO_ani
#'
setClass("ATO_ani")

#' @rdname ATO_ani
#' @name .ATO_ani
#' @export
.ATO_ani <- data.frame(
  animal = NA_character_,
  capture_location = NA_character_,
  capture_datetime = as.POSIXct(NA_real_),
  capture_lat = NA_real_,
  capture_lon = NA_real_,
  release_location = NA_character_,
  release_datetime = as.POSIXct(NA_real_),
  release_lat = NA_real_,
  release_lon = NA_real_
)[-1, ]
class(.ATO_ani) <- c("ATO_ani", "data.frame")

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
#' @format A data frame with 0 rows and 8 variables:
#' \describe{
#'   \item{animal}{Name of the animal that was observed, character.}
#'   \item{transmitter}{Transmitter code that was observed, character.}
#'   \item{type}{Type of observation (e.g. directly seen, manual tracking), character.}
#'   \item{terminal}{Was the animal permanently captured at the moment of observation? logical.}
#'   \item{location}{Name of the place where the observation occurred, character.}
#'   \item{datetime}{date and time of the observation, posixct format.}
#'   \item{lat}{latitude of the observation. Preferably in WGS84, numeric.}
#'   \item{lon}{longitude of the observation. Preferably in WGS84, numeric.}
#' }
#'
#' @keywords classes
#'
#' @seealso make_obs
#'
#' @name ATO_obs
#'
setClass("ATO_obs")

#' @rdname ATO_obs
#' @name .ATO_obs
#' @export
.ATO_obs <- data.frame(
  animal = NA_character_,
  transmitter = NA_character_,
  type = NA_character_,
  terminal = NA, # logical
  location = NA_character_,
  datetime = as.POSIXct(NA_real_),
  lat = NA_real_,
  lon = NA_real_,
  valid = NA
)[-1, ]
class(.ATO_obs) <- c("ATO_obs", "data.frame")

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
#'   \item{datetime}{date and time of the log entry, posixct format.}
#'   \item{package}{name of the package that hosts the function that made the log entry, character.}
#'   \item{call}{function call that made the log entry, character.}
#'   \item{log}{log entry, character.}
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
  datetime = as.POSIXct(NA_real_),
  package = NA_character_,
  call = NA_character_,
  log = NA_character_
)[-1, ]
class(.ATO_log) <- c("ATO_log", "data.frame")

#' S4 class: ATO_tbl
#'
#' An S4 class for the \code{\link{ATO}} table format (@tbl).
#' Does not contain internal slots. It is a single value.
#' One of: "data.frame", "data.table", or "tibble"
#'
#' The prototype @tbl slot contains the default value for the @log slot.
#'
#' @keywords classes
#'
#' @name ATO_tbl
#'
setClass("ATO_tbl")

#' @rdname ATO_tbl
#' @name .ATO_tbl
#' @export
.ATO_tbl <- "data.frame"
class(.ATO_tbl) <- c("ATO_tbl", "character")

#' S4 class: ATO
#'
#' The Animal Tracking Object (ATO) class contains slots that allow
#' researchers to package their study data in a standard format, to be used
#' by the R packages that collectively make the trackyverse.
#'
#' @slot det The detections slot. See \code{\link{ATO_det}}.
#' @slot dep The deployments slot. See \code{\link{ATO_dep}}.
#' @slot tag The tags slot. See \code{\link{ATO_tag}}.
#' @slot ani The animals slot. See \code{\link{ATO_ani}}.
#' @slot obs The observations slot. See \code{\link{ATO_obs}}.
#' @slot log The log slot. See \code{\link{ATO_log}}.
#' @slot tbl The table style slot. See \code{\link{ATO_tbl}}.
#' @slot pkg A list slot reserved for package outputs.
#'
#' @export
#'
setClass(
  "ATO",
  slots = c(
    det = "ATO_det",
    dep = "ATO_dep",
    tag = "ATO_tag",
    ani = "ATO_ani",
    obs = "ATO_obs",
    log = "ATO_log",
    tbl = "ATO_tbl",
    pkg = "list"
  ),
  prototype = list(
    det = .ATO_det,
    dep = .ATO_dep,
    tag = .ATO_tag,
    ani = .ATO_ani,
    obs = .ATO_obs,
    log = .ATO_log,
    tbl = .ATO_tbl,
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
  check(object@det)
  check(object@dep)
  check(object@tag)
  check(object@ani)
  check(object@obs)
  check(object@tbl)

  return(TRUE)
})
