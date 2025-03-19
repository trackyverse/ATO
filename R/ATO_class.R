setClass("ATO_detections")
setClass("ATO_deployments")
setClass("ATO_tags")

.ATO_detections <- data.frame(datetime = as.POSIXct(NA_real_),
                              frac_second = NA_real_,
                              receiver_serial = NA_integer_,
                              transmitter = NA_character_,
                              sensor_value = NA_real_)[-1, ]
class(.ATO_detections) <- c("data.frame", "ATO_detections")

.ATO_deployments <- data.frame(receiver_model = NA_character_,
                               receiver_serial = NA_integer_,
                               receiver_codeset = NA_character_,
                               deploy_location = NA_character_,
                               deploy_datetime = as.POSIXct(NA_real_),
                               deploy_lat = NA_real_,
                               deploy_lon = NA_real_,
                               recover_datetime = as.POSIXct(NA_real_),
                               recover_lat = NA_real_,
                               recover_lon = NA_real_,
                               transmitter = NA_character_,
                               transmitter_model = NA_character_,
                               transmitter_serial = NA_integer_)[-1,]
class(.ATO_deployments) <- c("data.frame", "ATO_deployments")

.ATO_tags <- data.frame(manufacturer = NA_character_,
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
class(.ATO_tags) <- c("data.frame", "ATO_tags")

setClass("ATO",
         slots = c(detections = "data.frame",
                   deployments = "data.frame",
                   tags = "data.frame"),
         prototype = list(
           detections = .ATO_detections,
           deployments = .ATO_deployments,
           tags = .ATO_tags)
         )

setValidity("ATO", function (object) {
  check_detections(object)
  check_deployments(object)
  check_tags(object)

  # if (nrow(object@detections) > 0 & nrow(object@deployments) > 0) {
  #   check_dets_deps(object)
  # }
  
  return(TRUE)
})
