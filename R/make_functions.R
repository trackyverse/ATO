#' Make an ATO animals object
#'
#' Formats the input data into the ATO format and appends the ATO_ani class.
#'
#' @param animal Name of the animal being tracked (character). Required.
#' @param release_datetime date and time of the release (POSIXct). Required.
#' @param release_location Name of the location where the animal was released
#'   character).
#' @param release_lat Latitude of the release. Preferably in WGS84 (numeric).
#' @param release_lon Longitude of the release. Preferably in WGS84 (numeric).
#' @param capture_location Name of the location where the animal was captured
#'   (character).
#' @param capture_datetime Date and time of the capture (POSIXct).
#' @param capture_lat Latitude of the capture. Preferably in WGS84 (numeric).
#' @param capture_lon Longitude of the capture. Preferably in WGS84 (numeric).
#' @param tz the timezone of the datetime data.
#' @param tbl The type of table to be generated. One of "data.frame",
#'   "data.table", or "tibble". If omitted, the output of
#'   \code{\link{ato_table_type_global}} is used.
#' @param ... Non-standard columns to be added to the table.
#'
#' @return an ATO_ani object, ready to be used by one of the \code{\link{set}}
#'  functions or \code{\link{init_ato}}.
#'
#' @export
#' 
#' @examples
#' # only two columns are required to start an ani table.
#' # Note that posix objects created on-the-fly do not necessarily
#' # have an associated timezone - try running the code below 
#' # without the tz argument and it will fail.
#' x <- make_ani(animal = letters[1:10],
#'               release_datetime = rep(Sys.time(), 10),
#'               tz = "America/Halifax")
#' head(x)
#' 
#' # additionally, you can change the type of table being created by using 
#' # the tbl argument - this requires having either data.table or tibble
#' # installed on your machine.
#'
#' @seealso \code{\link{ATO_ani}}
#'
make_ani <- function(
  animal,
  release_datetime,
  release_location = NA_character_,
  release_lat = NA_real_,
  release_lon = NA_real_,
  capture_datetime = as.POSIXct(NA_real_),
  capture_location = NA_character_,
  capture_lat = NA_real_,
  capture_lon = NA_real_,
  tz,
  tbl,
  ...
) {
  mandatory_cols <- c(
    "animal",
    "release_datetime"
  )
  check <- sapply(mandatory_cols, function(i) {
    any(is.na(get(i)))
  })
  if (any(check)) {
    stop(
      "Missing data detected in ",
      .comma(mandatory_cols[check]),
      ". All animals must have ",
      .comma(mandatory_cols[check]),
      " information. ",
      ifelse(
        check[2],
        paste0(
          " If you are unsure what the exact release times were,",
          " use an approximate value."
        ),
        ""
      ),
      call. = FALSE
    )
  }

  tbl <- .check_tbl_argument(tbl)

  # add a timezone to capture datetime if argument wasn't used
  if (length(capture_datetime) == 1 && is.na(capture_datetime)) {
    attributes(capture_datetime)$tzone <- attributes(release_datetime)$tzone
  }

  output <- data.frame(
    animal = animal,
    release_location = release_location,
    release_datetime = release_datetime,
    release_lat = release_lat,
    release_lon = release_lon,
    capture_location = capture_location,
    capture_datetime = capture_datetime,
    capture_lat = capture_lat,
    capture_lon = capture_lon,
    valid = TRUE,
    ...
  )

  if (tbl == "data.table") {
    data.table::setDT(output)
  }
  if (tbl == "tibble") {
    output <- tibble::as_tibble(output)
  }
  
  class(output) <- c("ATO_ani", class(output))
  
  output <- check(output, tz = tz, tbl = tbl)
  return(output)
}

#' Make an ATO deployments object
#'
#' Formats the input data into the ATO format and appends the ATO_dep class.
#'
#' @param deploy_datetime date and time of the deployment (POSIXct). Required.
#' @param recover_datetime date and time of the recovery (POSIXct). Required.
#' @param deploy_location Name of the location where the receiver was deployed
#'   (character).
#' @param deploy_lat latitude of the deployment. Preferably in WGS84 (numeric).
#' @param deploy_lon longitude of the deployment. Preferably in WGS84 (numeric).
#' @param deploy_z depth of the deployment, as measured from the reference
#'   surface of the water body (numeric).
#' @param recover_lat latitude of the recovery point. Preferably in WGS84
#'   (numeric).
#' @param recover_lon longitude of the recovery point. Preferably in WGS84
#'   (numeric).
#' @param receiver_manufacturer Maker of the receiver (character).
#' @param receiver_model Model of the receiver (character).
#' @param receiver_serial Receiver serial number (character).
#' @param receiver_codeset Codeset of the receiver (character).
#' @param transmitter Transmitter code for a beacon/reference tag (character).
#' @param transmitter_manufacturer Manufacturer of the transmitter (character).
#' @param transmitter_model Model of the transmitter (character).
#' @param transmitter_serial Serial number of the transmitter (integer).
#' @param transmitter_ping_rate Expected ping rate of the transmitter,
#'   in seconds (numeric).
#' @param tz the timezone of the datetime data.
#' @param tbl The type of table to be generated. One of "data.frame",
#'   "data.table", or "tibble". If omitted, the output of
#'   \code{\link{ato_table_type_global}} is used.
#' @param ... Non-standard columns to be added to the table.
#'
#' @return an ATO_dep object, ready to be used by one of the \code{\link{set}}
#'  functions or \code{\link{init_ato}}.
#'
#' @examples
#' # only two columns are required to start a dep table.
#' # Note that posix objects created on-the-fly do not necessarily
#' # have an associated timezone - try running the code below 
#' # without the tz argument and it will fail.
#' x <- make_dep(deploy_datetime = rep(Sys.time(), 10),
#'               recover_datetime = rep(Sys.time(), 10),
#'               tz = "America/Halifax")
#' head(x)
#' 
#' # additionally, you can change the type of table being created by using 
#' # the tbl argument - this requires having either data.table or tibble
#' # installed on your machine.
#' 
#' # while you can create deployments with only deployment and recovery times,
#' # they will likely not be very useful unless you also include a receiver
#' # serial number or an associated transmitter code.
#' 
#' @export
#'
#' @seealso \code{\link{ATO_dep}}
#'
make_dep <- function(
  deploy_datetime,
  recover_datetime,
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
  transmitter_model = NA_character_,
  transmitter_serial = NA_character_,
  transmitter_ping_rate = NA_real_,
  tz,
  tbl,
  ...
) {
  mandatory_cols <- c(
    "deploy_datetime",
    "recover_datetime"
  )
  check <- sapply(mandatory_cols, function(i) {
    any(is.na(get(i)))
  })
  if (any(check)) {
    stop(
      "Missing data detected in ",
      .comma(mandatory_cols[check]),
      ". All deployments must have ",
      .comma(mandatory_cols[check]),
      " information. If you are unsure what the exact deployment and recovery",
      " times were, use an approximate value.",
      ifelse(
        check[2],
        paste0(
          " For rolling receiver deployments, use the time of",
          " last download as the recovery time."),
        ""),
      call. = FALSE
    )
  }

  tbl <- .check_tbl_argument(tbl)

  output <- data.frame(
    deploy_datetime = deploy_datetime,
    recover_datetime = recover_datetime,
    deploy_location = deploy_location,
    deploy_lat = deploy_lat,
    deploy_lon = deploy_lon,
    deploy_z = deploy_z,
    recover_lat = recover_lat,
    recover_lon = recover_lon,
    receiver_manufacturer = receiver_manufacturer,
    receiver_model = receiver_model,
    receiver_serial = receiver_serial,
    receiver_codeset = receiver_codeset,
    transmitter = transmitter,
    transmitter_manufacturer = transmitter_manufacturer,
    transmitter_model = transmitter_model,
    transmitter_serial = transmitter_serial,
    transmitter_ping_rate = transmitter_ping_rate,
    valid = TRUE,
    ...
  )
  if (tbl == "data.table") {
    data.table::setDT(output)
  }
  if (tbl == "tibble") {
    output <- tibble::as_tibble(output)
  }
  class(output) <- c("ATO_dep", class(output))
  output <- check(output, tz = tz, tbl = tbl)
  return(output)
}

#' Make an ATO detections object
#'
#' Formats the input data into the ATO format and appends the ATO_det class.
#'
#' @param datetime date and time of the detection (POSIXct). Required.
#' @param receiver_serial Receiver serial (character). Required.
#' @param transmitter Transmitter code (character). Required.
#' @param frac_second fractional second of the detection (numeric).
#' @param sensor_value reported sensor value (numeric).
#' @param tz the timezone of the datetime data.
#' @param tbl The type of table to be generated. One of "data.frame",
#'   "data.table", or "tibble". If omitted, the output of
#'   \code{\link{ato_table_type_global}} is used.
#' @param ... Non-standard columns to be added to the table.
#'
#' @return an ATO_det object, ready to be used by one of the \code{\link{set}}
#'  functions or \code{\link{init_ato}}.
#'
#' @examples
#' # only three columns are required to start a det table.
#' # Note that posix objects created on-the-fly do not necessarily
#' # have an associated timezone - try running the code below 
#' # without the tz argument and it will fail.
#' x <- make_det(datetime = rep(round(Sys.time()), 10),
#'               receiver_serial = letters[1:10],
#'               transmitter = paste0(LETTERS[1:10], "-", 1:10),
#'               tz = "America/Halifax")
#' head(x)
#' 
#' # additionally, you can change the type of table being created by using 
#' # the tbl argument - this requires having either data.table or tibble
#' # installed on your machine.
#' 
#' # Note that make_det will complain if the datetime argument contains
#' # milliseconds. Try removing the round() call above to see it
#' 
#' @export
#'
#' @seealso \code{\link{ATO_det}}
#'
make_det <- function(
  datetime,
  receiver_serial,
  transmitter,
  frac_second = NA_real_,
  sensor_value = NA_real_,
  tz,
  tbl,
  ...
) {
  mandatory_cols <- c(
    "datetime",
    "receiver_serial",
    "transmitter"
  )
  check <- sapply(mandatory_cols, function(i) {
    any(is.na(get(i)))
  })
  if (any(check)) {
    stop(
      "Missing data detected in ",
      .comma(mandatory_cols[check]),
      ". All detections must have ",
      .comma(mandatory_cols[check]),
      " information. ",
      call. = FALSE
    )
  }

  tbl <- .check_tbl_argument(tbl)

  # detections objects can be very big.
  # to avoid spending a long time loading everything
  # before checking the quality of the data, we can make
  # a fast mock output, test it, then compile the real thing.
  mock <- data.frame(
    datetime = datetime[1],
    frac_second = frac_second[1],
    receiver_serial = receiver_serial[1],
    transmitter = transmitter[1],
    sensor_value = sensor_value[1],
    valid = TRUE
  )
  if (tbl == "data.table") {
    data.table::setDT(mock)
  }
  if (tbl == "tibble") {
    mock <- tibble::as.tibble(mock)
  }
  class(mock) <- c("ATO_det", class(mock))
  mock <- check(mock, tz = tz, tbl = tbl)

  # now check for potential issues with fractional seconds
  # more info here: https://github.com/trackyverse/ATO/issues/18
  if (length(frac_second) == 1 && is.na(frac_second)) {
    frac_from_dt <- as.numeric(datetime) %% 1
    if (!all(frac_from_dt == 0)) {
      # same speed as all(!frac_from_dt)
      # This is situation 1, pick solution 1A
      warning(
        "datetime contains millisecond information. Because frac_second",
        " was not provided, fractional seconds will be stripped from the",
        " datetime and stored in the frac_second column instead.",
        " To avoid this warning, strip fractional seconds from the",
        " timestamps and optionally add them through the frac_second",
        " argument.",
        call. = FALSE,
        immediate. = TRUE
      )
      datetime <- datetime - frac_from_dt
      frac_second <- frac_from_dt
    }
  } else {
    frac_from_dt <- as.numeric(datetime) %% 1
    if (!all(frac_from_dt == 0)) {
      if (all(frac_second == frac_from_dt)) {
        # This is situation 2.1, pick solution 1B
        warning(
          "datetime contains millisecond information. This information",
          " matches the values provided through frac_second. Discarding",
          " millisecond information in datetime. To avoid this warning,",
          " strip fractional seconds from the timestamps.",
          call. = FALSE,
          immediate. = TRUE
        )
        datetime <- datetime - frac_from_dt
      } else {
        # This is situation 2.2, pick solution 2.2B (error out)
        stop(
          "datetime contains millisecond information that does not match",
          " the values provided through frac_second. Please resolve this",
          " conflict manually before building the det slot.",
          call. = FALSE
        )
      }
    }
  }

  # now the real thing, which should run smoothly.
  output <- data.frame(
    datetime = datetime,
    frac_second = frac_second,
    receiver_serial = receiver_serial,
    transmitter = transmitter,
    sensor_value = sensor_value,
    valid = TRUE,
    ...
  )
  if (tbl == "data.table") {
    data.table::setDT(output)
  }
  if (tbl == "tibble") {
    output <- tibble::as.tibble(output)
  }
  class(output) <- c("ATO_det", class(output))
  # run only the timezone checks here to save computing time
  output <- .check_column_tzones(output, tz = tz)
  return(output)
}

#' Make an ATO observations object
#'
#' Formats the input data into the ATO format and appends the ATO_obs class.
#'
#' @param datetime date and time of the observation (POSIXct). Required.
#' @param animal Name of the animal that was observed (character). Each
#'   observation must have animal or transmitter information (or both).
#' @param transmitter Transmitter code that was observed (character). Each
#'   observation must have animal or transmitter information (or both).
#' @param terminal Was the animal/transmitter permanently captured at the
#'   moment of observation (logical)? Required.
#' @param location Name of the place where the observation occurred (character).
#' @param type Type of observation (e.g. directly seen, manual tracking)
#'   (character).
#' @param lat latitude of the observation. Preferably in WGS84 (numeric).
#' @param lon longitude of the observation. Preferably in WGS84 (numeric).
#' @param tz the timezone of the datetime data.
#' @param tbl The type of table to be generated. One of "data.frame",
#'   "data.table", or "tibble". If omitted, the output of
#'   \code{\link{ato_table_type_global}} is used.
#' @param ... Non-standard columns to be added to the table.
#'
#' @return an ATO_obs object, ready to be used by one of the \code{\link{set}}
#'  functions or \code{\link{init_ato}}.
#'
#' @export
#'
#' @examples
#' # To start an obs table, you need datetime, either the name
#' # of the animal or the code of the transmitter observed, and to
#' # specify if that observation is terminal (i.e. there will be no
#' # more detections/observations for this animal or transmitter).
#' # Note that posix objects created on-the-fly do not necessarily
#' # have an associated timezone - try running the code below 
#' # without the tz argument and it will fail.
#' x <- make_obs(datetime = rep(Sys.time(), 10),
#'               transmitter = c(paste0(LETTERS[1:5], "-", 1:5), rep(NA, 5)),
#'               animal = c(rep(NA, 5), letters[6:10]),
#'               terminal = rep(FALSE, 10),
#'               tz = "America/Halifax")
#' head(x)
#' 
#' # additionally, you can change the type of table being created by using 
#' # the tbl argument - this requires having either data.table or tibble
#' # installed on your machine.
#' 
#' @seealso \code{\link{ATO_obs}}
#'
make_obs <- function(
  datetime,
  animal = NA_character_,
  transmitter = NA_character_,
  terminal,
  location = NA_character_,
  type = NA_character_,
  lat = NA_real_,
  lon = NA_real_,
  tz,
  tbl,
  ...
) {
  mandatory_cols <- c(
    "datetime",
    "terminal"
  )
  check <- sapply(mandatory_cols, function(i) {
    any(is.na(get(i)))
  })
  if (any(check)) {
    stop(
      "Missing data detected in ",
      .comma(mandatory_cols[check]),
      ". All observations must have ",
      .comma(mandatory_cols[check]),
      " information. ",
      call. = FALSE
    )
  }

  if (any(is.na(animal) & is.na(transmitter))) {
    stop(
      "Each observation must be associated to either an animal or a",
      " transmitter, or both",
      call. = FALSE
    )
  }

  tbl <- .check_tbl_argument(tbl)

  output <- data.frame(
    animal = animal,
    transmitter = transmitter,
    type = type,
    terminal = terminal,
    location = location,
    datetime = datetime,
    lat = lat,
    lon = lon,
    valid = TRUE,
    ...
  )
  if (tbl == "data.table") {
    .data.table_exists()
    data.table::setDT(output)
  }
  if (tbl == "tibble") {
    .tibble_exists()
    output <- tibble::as_tibble(output)
  }
  class(output) <- c("ATO_obs", class(output))
  output <- check(output, tz = tz, tbl = tbl)
  return(output)
}

#' Make an ATO tags object
#'
#' Formats the input data into the ATO format and appends the ATO_tag class.
#'
#' @param transmitter Transmitter code (character). Required.
#' @param manufacturer Manufacturer of the transmitter (character).
#' @param model Model of the transmitter (character).
#' @param serial Serial number of the tag (integer).
#' @param power_level Power level of the transmitter (real).
#' @param ping_rate,ping_variation Expected average ping rate of the
#'  transmitter and respective variation around the average, in seconds
#'  (numeric). E.g. if a tag's ping interval may vary between
#'  60 and 120s, then ping_rate = 90, and ping_variation = 30.
#' @param activation_datetime Date and time of the tag activation (POSIXct).
#' @param battery_life Expected battery duration of the tag, in days (numeric).
#' @param sensor_type Type of sensor data associated with the transmitter
#'   (character).
#' @param sensor_unit Unit of the data associated with the transmitter
#'   (character).
#' @param animal Name of the animal that received the tag (character).
#' @param tz the timezone of the datetime data.
#' @param tbl The type of table to be generated. One of "data.frame",
#'   "data.table", or "tibble". If omitted, the output of
#'   \code{\link{ato_table_type_global}} is used.
#' @param ... Non-standard columns to be added to the table.
#'
#' @return an ATO_tag object, ready to be used by one of the \code{\link{set}}
#'  functions or \code{\link{init_ato}}.
#'
#' @examples
#' # All you need to make a tag table is the respective transmitter
#' # code.
#' x <- make_tag(transmitter = LETTERS[1:5])
#' head(x)
#' 
#' # additionally, you can change the type of table being created by using 
#' # the tbl argument - this requires having either data.table or tibble
#' # installed on your machine.
#' 
#' @export
#'
#' @seealso \code{\link{ATO_tag}}
#'
make_tag <- function(
  transmitter,
  manufacturer = NA_character_,
  model = NA_character_,
  serial = NA_character_,
  power_level = NA_real_,
  ping_rate = NA_real_,
  ping_variation = NA_real_,
  activation_datetime = as.POSIXct(NA_real_),
  battery_life = NA_real_,
  sensor_type = NA_character_,
  sensor_unit = NA_character_,
  animal = NA_character_,
  tz,
  tbl,
  ...
) {
  if (any(is.na(transmitter))) {
    stop(
      "Missing data detected in transmitter.",
      " All tags must have transmitter information.",
      call. = FALSE
    )
  }

  tbl <- .check_tbl_argument(tbl)

  output <- data.frame(
    transmitter = transmitter,
    manufacturer = manufacturer,
    model = model,
    serial = serial,
    power_level = power_level,
    ping_rate = ping_rate,
    ping_variation = ping_variation,
    activation_datetime = activation_datetime,
    battery_life = battery_life,
    sensor_type = sensor_type,
    sensor_unit = sensor_unit,
    animal = animal,
    valid = TRUE,
    ...
  )
  if (tbl == "data.table") {
    data.table::setDT(output)
  }
  if (tbl == "tibble") {
    output <- tibble::as_tibble(output)
  }
  class(output) <- c("ATO_tag", class(output))
  output <- check(output, tz = tz, tbl = tbl)
  return(output)
}
