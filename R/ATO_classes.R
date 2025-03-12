#' ATO structure
#' 
setClass("ATO",
         slots = c(detections = "Detections",
                   deployments = "Deployments",
                   tags = "Tags"))

setClass("Detections",
         slots = c(datetime = "POSIXct",
                   frac_second = "numeric",
                   receiver_serial = "integer",
                   transmitter = "character",
                   sensor_value = "numeric"
                   ))

setClass("Deployments",
         slots = c(receiver_model = "character",
                   receiver_serial = "integer",
                   receiver_codeset = "character",
                   deploy_location = "POSIXct",
                   deploy_datetime = "POSIXct",
                   deploy_lat = "numeric",
                   deploy_lon = "numeric",
                   recover_datetime = "POSIXct",
                   recover_lat = "numeric",
                   recover_lon = "numeric",
                   transmitter = "character",
                   transmitter_model = "character",
                   transmitter_serial = "integer",
                   comments = "character"
                   ))

setClass("Tags",
         slots = c(manufacturer = "character",
                   model = "character",
                   power_level = "character",
                   ping_rate = "integer", # seconds
                   ping_variation = "integer", # 0 if fixed ping rate
                   serial = "integer",
                   transmitter = "character",
                   activation_datetime = "POSIXct",
                   battery_life = "numeric", # days
                   sensor_type = "character",
                   sensor_unit = "character", # thoughts on sensor info needs?
                   animal = "character", # let scientists nickname their animals :)
                   # species = "character", # maybe leave this for an "animals" object? together with weights, lenghts, and anything else.
                   capture_location = "character",
                   capture_datetime = "POSIXct",
                   capture_lat = "numeric",
                   capture_lon = "numeric",
                   release_location = "character",
                   release_datetime = "POSIXct",
                   release_lat = "numeric",
                   release_lon = "numeric",
                   comments = "character"
                   ))

# if an animal is recaptured, that would be a new line in the
# tags? for the same tag but with an older capture date?
# then if the animal was re-released, just fill the release info
# again? thoughts?
