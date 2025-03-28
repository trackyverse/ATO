make_det <- function(datetime = as.POSIXct(NA_real_),
                     frac_second = NA_real_,
                     receiver_serial = NA_integer_,
                     transmitter = NA_character_,
                     sensor_value = NA_real_,
                     tz,
                     ...) {
  ato_table_type <- getOption("ATO_table_type", default = "data.frame")
  if (missing(tz)) {
    stop("Please use 'tz' to define the study area timezone.", call. = FALSE)
  }
  # detections objects can be very big.
  # to avoid spending a long time loading everything
  # before checking the quality of the data, we can make
  # a fast mock output, test it, then compile the real thing.
  mock <- data.frame(datetime = datetime[1],
                     frac_second = frac_second[1],
                     receiver_serial = receiver_serial[1],
                     transmitter = transmitter[1],
                     sensor_value = sensor_value[1],
                     ...)
  if (ato_table_type == "data.table") {
    mock <- data.table::as.data.table(mock)
  }
  if (ato_table_type == "tibble") {
    mock <- tibble::as_tibble(mock)
  }
  class(mock) <- c("ATO_det", class(mock))
  attributes(mock$datetime)$tzone <- tz
  check(mock)
  # now the real thing, which should run smoothly.
  output <- data.frame(datetime = datetime,
                       frac_second = frac_second,
                       receiver_serial = receiver_serial,
                       transmitter = transmitter,
                       sensor_value = sensor_value,
                       ...)
  if (ato_table_type == "data.table") {
    output <- data.table::as.data.table(output)
  }
  if (ato_table_type == "tibble") {
    output <- tibble::as_tibble(output)
  }
  class(output) <- c("ATO_det", class(output))
  attributes(output$datetime)$tzone <- tz
  return(output)
}

make_dep <- function(receiver_model = NA_character_,
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
                     transmitter_serial = NA_integer_,
                     tz,
                     ...) {
  ato_table_type <- getOption("ATO_table_type", default = "data.frame")
  if (missing(tz)) {
    stop("Please use 'tz' to define the study area timezone.", call. = FALSE)
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
    output <- data.table::as.data.table(output)
  }
  if (ato_table_type == "tibble") {
    output <- tibble::as_tibble(output)
  }
  class(output) <- c("ATO_dep", class(output))
  attributes(output$deploy_datetime)$tzone <- tz
  attributes(output$recover_datetime)$tzone <- tz
  check(output)
  return(output)
}

make_tag <- function(manufacturer = NA_character_,
                     model = NA_character_,
                     power_level = NA_real_,
                     ping_rate = NA_real_,
                     ping_variation = NA_real_,
                     serial = NA_integer_,
                     transmitter = NA_character_,
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
    output <- data.table::as.data.table(output)
  }
  if (ato_table_type == "tibble") {
    output <- tibble::as_tibble(output)
  }
  class(output) <- c("ATO_tag", class(output))
  check(output)
  return(output)
}

make_ani <- function(animal = NA_character_,
                     capture_location = NA_character_,
                     capture_datetime = as.POSIXct(NA_real_),
                     capture_lat = NA_real_,
                     capture_lon = NA_real_,
                     release_location = NA_character_,
                     release_datetime = as.POSIXct(NA_real_),
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
    output <- data.table::as.data.table(output)
  }
  if (ato_table_type == "tibble") {
    output <- tibble::as_tibble(output)
  }
  class(output) <- c("ATO_ani", class(output))
  attributes(output$capture_datetime)$tzone <- tz
  attributes(output$release_datetime)$tzone <- tz
  check(output)
  return(output)
}

make_obs <- function(animal = NA_character_,
                     transmitter = NA_character_,
                     type = NA_character_,
                     terminal = NA, # logical
                     location = NA_character_,
                     datetime = as.POSIXct(NA_real_),
                     lat = NA_real_,
                     lon = NA_real_,
                     tz,
                     ...) {
  ato_table_type <- getOption("ATO_table_type", default = "data.frame")
  if (missing(tz)) {
    stop("Please use 'tz' to define the study area timezone.", call. = FALSE)
  }
  output <- data.frame(animal = animal,
                       transmitter = transmitter,
                       type = type,
                       terminal = terminal,
                       location = location,
                       datetime = datetime,
                       lat = lat,
                       lon = lon,
                       ...)
  if (ato_table_type == "data.table") {
    output <- data.table::as.data.table(output)
  }
  if (ato_table_type == "tibble") {
    output <- tibble::as_tibble(output)
  }
  class(output) <- c("ATO_obs", class(output))
  attributes(output$datetime)$tzone <- tz
  check(output)
  return(output)
}
