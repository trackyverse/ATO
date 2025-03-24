setClass("ATO_det")
setClass("ATO_dep")
setClass("ATO_tag")

.ATO_det <- data.frame(datetime = as.POSIXct(NA_real_),
                              frac_second = NA_real_,
                              receiver_serial = NA_integer_,
                              transmitter = NA_character_,
                              sensor_value = NA_real_)[-1, ]
class(.ATO_det) <- c("ATO_det", "data.frame")

.ATO_dep <- data.frame(receiver_model = NA_character_,
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
                       transmitter_ping_rate = NA_real_,
                       transmitter_model = NA_character_,
                       transmitter_serial = NA_integer_)[-1,]
class(.ATO_dep) <- c("ATO_dep", "data.frame")

.ATO_tag <- data.frame(manufacturer = NA_character_,
                       model = NA_character_,
                       power_level = NA_real_,
                       ping_rate = NA_real_, # seconds
                       ping_variation = NA_real_, # 0 if fixed ping rate
                       serial = NA_integer_,
                       transmitter = NA_character_,
                       activation_datetime = as.POSIXct(NA_real_),
                       battery_life = NA_integer_, # days
                       sensor_type = NA_character_,
                       sensor_unit = NA_character_, # thoughts on sensor info needs?
                       animal = NA_character_, # let scientists nickname their animals :)
                       capture_location = NA_character_,
                       capture_datetime = as.POSIXct(NA_real_),
                       capture_lat = NA_real_,
                       capture_lon = NA_real_,
                       release_location = NA_character_,
                       release_datetime = as.POSIXct(NA_real_),
                       release_lat = NA_real_,
                       release_lon = NA_real_)[-1,]
class(.ATO_tag) <- c("ATO_tag", "data.frame")

setClass("ATO",
         slots = c(det = "ATO_det",
                   dep = "ATO_dep",
                   tag = "ATO_tag"),
         prototype = list(det = .ATO_det,
                          dep = .ATO_dep,
                          tag = .ATO_tag)
         )

setValidity("ATO", function (object) {
  check(object@det)
  check(object@dep)
  check(object@tag)

  # if (nrow(object@detections) > 0 & nrow(object@deployments) > 0) {
  #   check_dets_deps(object)
  # }
  
  return(TRUE)
})
