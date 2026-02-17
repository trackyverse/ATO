#' Make an ATO detections object
#' 
#' Formats the input data into the ATO format and appends the ATO_det class.
#' 
#' @param datetime date and time, posixct format. Mandatory.
#' @param frac_second fractional second, numeric. Optional.
#' @param receiver_serial Mandatory. receiver serial number, integer. Mandatory.
#' @param transmitter Mandatory. transmitter code, character. Mandatory.
#' @param sensor_value reported sensor value, numeric. Optional.
#' @param tz the timezone of the datetime data. Mandatory.
#' @param ... Non-standard columns to be added to the table. Optional.
#' 
#' @return an ATO_det object, ready to be used by \code{\link{add}} or
#'   \code{\link{init_ato}}.
#' 
#' @export
#' 
#' @seealso ATO_det
#' 
make_det <- function(datetime,
                     frac_second = NA_real_,
                     receiver_serial,
                     transmitter,
                     sensor_value = NA_real_,
                     tz,
                     ...) {
  ato_table_type <- getOption("ATO_table_type", default = "data.frame")
  # detections objects can be very big.
  # to avoid spending a long time loading everything
  # before checking the quality of the data, we can make
  # a fast mock output, test it, then compile the real thing.
  if (ato_table_type == "data.frame") {
    mock <- data.frame(datetime = datetime[1],
                       frac_second = frac_second[1],
                       receiver_serial = receiver_serial[1],
                       transmitter = transmitter[1],
                       sensor_value = sensor_value[1],
                       valid = TRUE)
  }
  if (ato_table_type == "data.table") {
    .data.table_exists()
    mock <- data.table::data.table(datetime = datetime[1],
                                  frac_second = frac_second[1],
                                  receiver_serial = receiver_serial[1],
                                  transmitter = transmitter[1],
                                  sensor_value = sensor_value[1],
                                  valid = TRUE)
  }
  if (ato_table_type == "tibble") {
    .tibble_exists()
    mock <- tibble::tibble(datetime = datetime[1],
                           frac_second = frac_second[1],
                           receiver_serial = receiver_serial[1],
                           transmitter = transmitter[1],
                           sensor_value = sensor_value[1],
                           valid = TRUE)
  }
  class(mock) <- c("ATO_det", class(mock))
  attributes(mock$datetime)$tzone <- tz
  check(mock)

  # now check for potential issues with fractional seconds
  # more info here: https://github.com/trackyverse/ATO/issues/18
  if (length(frac_second) == 1 & is.na(frac_second)) {
    frac_from_dt <- as.numeric(datetime) %% 1
    if (!all(frac_from_dt == 0)) { # same speed as all(!frac_from_dt)
      # This is situation 1, pick solution 1A
      warning("datetime contains millisecond information. Because frac_second",
              " was not provided, fractional seconds will be stripped from the",
              " datetime and stored in the frac_second column instead.", 
              " To avoid this warning, strip fractional seconds from the",
              " timestamps and optionally add them through the frac_second",
              " argument.", call. = FALSE, immediate. = TRUE)
      datetime <- datetime - frac_from_dt
      frac_second <- frac_from_dt
    }
  } else {
    frac_from_dt <- as.numeric(datetime) %% 1
    if (!all(frac_from_dt == 0)) {
      if (all(frac_second == frac_from_dt)) {
        # This is situation 2.1, pick solution 1B
        warning("datetime contains millisecond information. This information",
                " matches the values provided through frac_second. Discarding",
                " millisecond information in datetime. To avoid this warning,",
                " strip fractional seconds from the timestamps.",
                call. = FALSE, immediate. = TRUE)
        datetime <- datetime - frac_from_dt
      } else {
        # This is situation 2.2, pick solution 2.2B (error out)
        stop("datetime contains millisecond information that does not match",
             " the values provided through frac_second. Please resolve this",
             " conflict manually before building the det slot.",
             call. = FALSE)
      }
    }
  }

  # now the real thing, which should run smoothly.
  if (ato_table_type == "data.frame") {
    output <- data.frame(datetime = datetime,
                         frac_second = frac_second,
                         receiver_serial = receiver_serial,
                         transmitter = transmitter,
                         sensor_value = sensor_value,
                         valid = TRUE,
                         ...)
  }
  if (ato_table_type == "data.table") {
    .data.table_exists()
    output <- data.table::data.table(datetime = datetime,
                                    frac_second = frac_second,
                                    receiver_serial = receiver_serial,
                                    transmitter = transmitter,
                                    sensor_value = sensor_value,
                                    valid = TRUE,
                                    ...)
  }
  if (ato_table_type == "tibble") {
    .tibble_exists()
    output <- tibble::tibble(datetime = datetime,
                             frac_second = frac_second,
                             receiver_serial = receiver_serial,
                             transmitter = transmitter,
                             sensor_value = sensor_value,
                             valid = TRUE,
                             ...)
  }
  class(output) <- c("ATO_det", class(output))
  attributes(output$datetime)$tzone <- tz
  return(output)
}

#' Make an ATO deployments object
#' 
#' Formats the input data into the ATO format and appends the ATO_dep class.
#' 
#' @param receiver_model Model of the receiver, character. Optional.
#' @param receiver_serial Receiver serial number, integer. Mandatory.
#' @param receiver_codeset Codeset of the receiver, character. Optional.
#' @param deploy_location Name of the location where the receiver was deployed,
#'   character. Mandatory.
#' @param deploy_datetime date and time of the deployment, posixct. Mandatory.
#' @param deploy_lat latitude of the deployment. Preferably in WGS84, numeric.
#'   Optional.
#' @param deploy_lon longitude of the deployment. Preferably in WGS84, numeric.
#'   Optional.
#' @param deploy_z depth of the deployment, as measured from the reference
#'   surface of the water body, numeric. Optional.
#' @param recover_datetime date and time of the recovery, posixct. Mandatory.
#' @param recover_lat latitude of the recovery point. Preferably in WGS84,
#'   numeric. Optional.
#' @param recover_lon longitude of the recovery point. Preferably in WGS84,
#'   numeric. Optional.
#' @param transmitter Transmitter code for a beacon/reference tag, character.
#'   Optional.
#' @param transmitter_manufacturer Manufacturer of the transmitter, character.
#'   Optional.
#' @param transmitter_ping_rate Expected ping rate of the transmitter, numeric.
#'   In seconds. Required if transmitter is provided.
#' @param transmitter_model Model of the transmitter, character. Optional.
#' @param transmitter_serial Serial number of the transmitter, integer.
#'   Required if transmitter is provided.
#' @param tz the timezone of the datetime data. Mandatory.
#' @param ... Non-standard columns to be added to the table.
#' 
#' @return an ATO_dep object, ready to be used by \code{\link{add}} or
#'   \code{\link{init_ato}}.
#' 
#' @export
#' 
#' @seealso ATO_dep
#' 
make_dep <- function(receiver_model = NA_character_,
                     receiver_serial,
                     receiver_codeset = NA_character_,
                     deploy_location,
                     deploy_datetime,
                     deploy_lat = NA_real_,
                     deploy_lon = NA_real_,
                     deploy_z = NA_real_,
                     recover_datetime,
                     recover_lat = NA_real_,
                     recover_lon = NA_real_,
                     transmitter = NA_character_,
                     transmitter_manufacturer = NA_character_,
                     transmitter_ping_rate = NA_real_,
                     transmitter_model = NA_character_,
                     transmitter_serial = NA_character_,
                     tz,
                     ...) {
  ato_table_type <- getOption("ATO_table_type", default = "data.frame")
  if (missing(tz)) {
    stop("Please use 'tz' to define the study area timezone.", call. = FALSE)
  }

  # if receivers have transmitters, then their ping rate must be provided
  if (any(!is.na(transmitter) & is.na(transmitter_ping_rate))) {
    stop("transmitter provided but transmitter_ping_rate missing.",
         call. = FALSE)
  }

  output <- data.frame(receiver_model = receiver_model,
                       receiver_serial = receiver_serial,
                       receiver_codeset = receiver_codeset,
                       deploy_location = deploy_location,
                       deploy_datetime = deploy_datetime,
                       deploy_lat = deploy_lat,
                       deploy_lon = deploy_lon,
                       deploy_z = deploy_z,
                       recover_datetime = recover_datetime,
                       recover_lat = recover_lat,
                       recover_lon = recover_lon,
                       transmitter = transmitter,
                       transmitter_manufacturer = transmitter_manufacturer,
                       transmitter_ping_rate = transmitter_ping_rate,
                       transmitter_model = transmitter_model,
                       transmitter_serial = transmitter_serial,
                       ...)
  if (ato_table_type == "data.table") {
    .data.table_exists()
    output <- data.table::as.data.table(output)
  }
  if (ato_table_type == "tibble") {
    .tibble_exists()
    output <- tibble::as_tibble(output)
  }
  class(output) <- c("ATO_dep", class(output))
  attributes(output$deploy_datetime)$tzone <- tz
  attributes(output$recover_datetime)$tzone <- tz
  check(output)
  return(output)
}

#' Make an ATO tags object
#' 
#' Formats the input data into the ATO format and appends the ATO_tag class.
#' 
#' @param manufacturer Manufacturer of the transmitter, character. Optional.
#' @param model Model of the transmitter, character. Optional.
#' @param power_level Power level of the transmitter, real. Optional.
#' @param ping_rate Expected ping rate of the transmitter, numeric. In seconds. Optional.
#' @param ping_variation Range of the variation added between pings,
#'   to reduce tag collisions, numeric. In seconds. Optional.
#' @param serial Serial number of the tag, integer. Optional.
#' @param transmitter Transmitter code, character. Mandatory.
#' @param activation_datetime date and time of the tag activation, posixct. Optional.
#' @param battery_life expected battery duration of the tag, numeric. Optional.
#' @param sensor_type Type of sensor data associated with the transmitter,
#'   character. Optional.
#' @param sensor_unit Unit of the data associated with the transmitter,
#'   character. Optional.
#' @param animal Name of the animal that received the tag, character. Optional.
#' @param tz the timezone of the datetime data. Mandatory.
#' @param ... Non-standard columns to be added to the table.
#' 
#' @return an ATO_tag object, ready to be used by \code{\link{add}} or
#'   \code{\link{init_ato}}.
#' 
#' @export
#' 
#' @seealso ATO_tag
#' 
make_tag <- function(manufacturer = NA_character_,
                     model = NA_character_,
                     power_level = NA_real_,
                     ping_rate = NA_real_,
                     ping_variation = NA_real_,
                     serial = NA_character_,
                     transmitter,
                     activation_datetime = as.POSIXct(NA_real_),
                     battery_life = NA_real_,
                     sensor_type = NA_character_,
                     sensor_unit = NA_character_,
                     animal = NA_character_,
                     tz,
                     ...) {
  ato_table_type <- getOption("ATO_table_type", default = "data.frame")
  if (missing(tz)) {
    stop("Please use 'tz' to define the study area timezone.", call. = FALSE)
  }
  output <- data.frame(manufacturer = manufacturer,
                       model = model,
                       power_level = power_level,
                       ping_rate = ping_rate,
                       ping_variation = ping_variation,
                       serial = serial,
                       transmitter = transmitter,
                       activation_datetime = activation_datetime,
                       battery_life = battery_life,
                       sensor_type = sensor_type,
                       sensor_unit = sensor_unit,
                       animal = animal,
                       ...)
  if (ato_table_type == "data.table") {
    .data.table_exists()
    output <- data.table::as.data.table(output)
  }
  if (ato_table_type == "tibble") {
    .tibble_exists()
    output <- tibble::as_tibble(output)
  }
  class(output) <- c("ATO_tag", class(output))
  check(output)
  return(output)
}

#' Make an ATO animals object
#' 
#' Formats the input data into the ATO format and appends the ATO_ani class.
#' 
#' @param animal Name of the animal that received the tag, character. Mandatory.
#' @param capture_location Name of the location where the animal was captured,
#'   character. Optional.
#' @param capture_datetime date and time of the capture, posixct. Optional.
#' @param capture_lat latitude of the capture. Preferably in WGS84, numeric.
#'   Optional.
#' @param capture_lon longitude of the capture. Preferably in WGS84, numeric.
#'   Optional.
#' @param release_location Name of the location where the animal was released,
#'   character. Mandatory.
#' @param release_datetime date and time of the release, posixct. Mandatory.
#' @param release_lat latitude of the release. Preferably in WGS84, numeric.
#'   Optional.
#' @param release_lon longitude of the release. Preferably in WGS84, numeric.
#'   Optional.
#' @param tz the timezone of the datetime data.
#' @param ... Non-standard columns to be added to the table.
#' 
#' @return an ATO_ani object, ready to be used by \code{\link{add}} or
#'   \code{\link{init_ato}}.
#' 
#' @export
#' 
#' @seealso ATO_ani
#' 
make_ani <- function(animal,
                     capture_location = NA_character_,
                     capture_datetime = as.POSIXct(NA_real_),
                     capture_lat = NA_real_,
                     capture_lon = NA_real_,
                     release_location,
                     release_datetime,
                     release_lat = NA_real_,
                     release_lon = NA_real_,
                     tz,
                     ...) {
  ato_table_type <- getOption("ATO_table_type", default = "data.frame")
  if (missing(tz)) {
    stop("Please use 'tz' to define the study area timezone.", call. = FALSE)
  }
  output <- data.frame(animal = animal,
                       capture_location = capture_location,
                       capture_datetime = capture_datetime,
                       capture_lat = capture_lat,
                       capture_lon = capture_lon,
                       release_location = release_location,
                       release_datetime = release_datetime,
                       release_lat = release_lat,
                       release_lon = release_lon,
                       ...)
  if (ato_table_type == "data.table") {
    .data.table_exists()
    output <- data.table::as.data.table(output)
  }
  if (ato_table_type == "tibble") {
    .tibble_exists()
    output <- tibble::as_tibble(output)
  }
  class(output) <- c("ATO_ani", class(output))
  attributes(output$capture_datetime)$tzone <- tz
  attributes(output$release_datetime)$tzone <- tz
  check(output)
  return(output)
}

#' Make an ATO observations object
#' 
#' Formats the input data into the ATO format and appends the ATO_obs class.
#' 
#' @param animal Name of the animal that was observed, character. Each
#'   observation must have an animal or transmitter code (or both).
#' @param transmitter Transmitter code that was observed, character. Each
#'   observation must have an animal or transmitter code (or both).
#' @param type Type of observation (e.g. directly seen, manual tracking),
#'   character. Optional.
#' @param terminal Was the animal permanently captured at the moment of
#'   observation? logical. Mandatory.
#' @param location Name of the place where the observation occurred, character.
#'   Mandatory.
#' @param datetime date and time of the observation, posixct format.
#'   Mandatory.
#' @param lat latitude of the observation. Preferably in WGS84, numeric.
#'   Optional.
#' @param lon longitude of the observation. Preferably in WGS84, numeric.
#'   Optional.
#' @param tz the timezone of the datetime data. Mandatory.
#' @param ... Non-standard columns to be added to the table.
#' 
#' @return an ATO_obs object, ready to be used by \code{\link{add}} or
#'   \code{\link{init_ato}}.
#' 
#' @export
#' 
#' @seealso ATO_obs
#' 
make_obs <- function(animal = NA_character_,
                     transmitter = NA_character_,
                     type = NA_character_,
                     terminal,
                     location,
                     datetime,
                     lat = NA_real_,
                     lon = NA_real_,
                     tz,
                     ...) {
  ato_table_type <- getOption("ATO_table_type", default = "data.frame")
  if (missing(tz)) {
    stop("Please use 'tz' to define the study area timezone.", call. = FALSE)
  }

  if (any(is.na(animal) & is.na(transmitter))) {
    stop("Each observation must be associated to either an animal or a",
         " transmitter, or both", call. = FALSE)
  }
  if (ato_table_type == "data.frame") {
    output <- data.frame(animal = animal,
                         transmitter = transmitter,
                         type = type,
                         terminal = terminal,
                         location = location,
                         datetime = datetime,
                         lat = lat,
                         lon = lon,
                         ...)
  }
  if (ato_table_type == "data.table") {
    .data.table_exists()
    output <- data.table::data.table(animal = animal,
                                     transmitter = transmitter,
                                     type = type,
                                     terminal = terminal,
                                     location = location,
                                     datetime = datetime,
                                     lat = lat,
                                     lon = lon,
                                     ...)
  }
  if (ato_table_type == "tibble") {
    .tibble_exists()
    output <- tibble::tibble(animal = animal,
                             transmitter = transmitter,
                             type = type,
                             terminal = terminal,
                             location = location,
                             datetime = datetime,
                             lat = lat,
                             lon = lon,
                             ...)
  }
  class(output) <- c("ATO_obs", class(output))
  attributes(output$datetime)$tzone <- tz
  check(output)
  return(output)
}
