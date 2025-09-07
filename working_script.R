# THIS IS A SANDBOX FILE
# DO NOT KEEP PRECIOUS CODE IN HERE

# quick test of changing table types
ato1 <- init_ato()
ato1@tbl
class(ato1@det)

ato_table_type_global("data.table")
ato2 <- init_ato()
ato2@tbl
class(ato2@det)

table_type(ato1) <- "data.frame"
# ---

# creating a basic ATO
my_ATO <- new("ATO")
my_ATO

dets <- make_det(datetime = as.POSIXct(actel:::example.detections$Timestamp),
                 frac_second = NA_real_,
                 receiver_serial = actel:::example.detections$Receiver,
                 transmitter = with(actel:::example.detections, paste0(CodeSpace, "-", Signal)),
                 sensor_value = actel:::example.detections$Sensor.Value,
                 tz = "UTC")
class(dets)

my_ATO <- add(my_ATO, dets)
my_ATO
get_log(my_ATO)

head(actel:::example.deployments)
head(actel:::example.spatial)
deps <- make_dep(receiver_model = NA_character_,
                 receiver_serial = as.integer(actel:::example.deployments$Receiver),
                 receiver_codeset = NA_character_,
                 deploy_location = actel:::example.deployments$Station.name,
                 deploy_datetime = actel:::example.deployments$Start,
                 deploy_lat = actel:::example.spatial$Latitude[-18],
                 deploy_lon = actel:::example.spatial$Longitude[-18],
                 recover_datetime = actel:::example.deployments$Stop,
                 recover_lat = actel:::example.spatial$Latitude[-18],
                 recover_lon = actel:::example.spatial$Longitude[-18],
                 transmitter = NA_character_,
                 transmitter_model = NA_character_,
                 transmitter_serial = NA_integer_,
                 tz = "UTC")

my_ATO <- add(my_ATO, deps)
my_ATO

tags <- make_tag(manufacturer = "Thelma",
                 model = NA_character_,
                 power_level = NA_real_,
                 ping_rate = 60, # seconds
                 ping_variation = 30, # 0 if fixed ping rate
                 serial = actel:::example.biometrics$Serial.nr,
                 transmitter = paste0("R64K-", actel:::example.biometrics$Signal),
                 activation_datetime = as.POSIXct(NA_real_),
                 battery_life = NA_real_, # days
                 sensor_type = NA_character_,
                 sensor_unit = NA_character_,
                 animal = as.character(1:nrow(actel:::example.biometrics)), 
                 capture_location = as.character(actel:::example.biometrics$Release.site),
                 capture_datetime = actel:::example.biometrics$Release.date,
                 capture_lat = actel:::example.spatial$Latitude[18],
                 capture_lon = actel:::example.spatial$Longitude[18],
                 release_location = as.character(actel:::example.biometrics$Release.site),
                 release_datetime = actel:::example.biometrics$Release.date,
                 release_lat = actel:::example.spatial$Latitude[18],
                 release_lon = actel:::example.spatial$Longitude[18],
                 tz = "UTC")

my_ATO <- add(my_ATO, tags)
my_ATO







  # if (has(object, "det") {
  #   message("M: No detections to filter.")
  # } else {
  #   if (is.null(object@det$ping_dev)) {
  #     stop("Ping deviation has not been calculated yet.",
  #          call. = FALSE)
  #   }
  #   if (any(!object@det$tag_match)) {
  #     stop("The dataset contains off-target detections.",
  #          " Please discard those first.", call. = FALSE)
  #   }
  #   by_receiver <- split(object@det,
  #                        object@det$receiver)
  #   # receiver <- by_receiver[[1]]
  #   recipient1 <- lapply(by_receiver, function(receiver) {
  #     # cat(receiver$receiver_serial[1], "\n")
  #     by_tag <- split(receiver,
  #                     receiver$transmitter)
  #     tag <- by_tag[[1]]
  #     recipient2 <- lapply(by_tag, function(tag) {
  #       # cat(" - ", tag$transmitter[1], "\n")
  #       tag <- tag[order(tag$datetime), ]
  #       to_keep <- rep(FALSE, nrow(tag))
  #       # simple hack to remove the NA at the start
  #       tag$ping_dev[1] <- Inf
  #       for (i in bands) {
  #         new_keep <- tag$ping_dev >= (i - grace) &
  #                     tag$ping_dev <= (i + grace)
  #         to_keep <- to_keep | new_keep
  #       }
  #       # keep also the detection that leads to the first
  #       # valid ping deviance, since the deviance relates to
  #       # the distance between i and i-1.
  #       aux <- rle(to_keep)
  #       to_flip <- cumsum(aux$lengths)[!aux$values]
  #       # the last detection can be picked up to flip.
  #       # we don't want that.
  #       if (tail(to_flip, 1) == nrow(tag)) {
  #         to_flip <- to_flip[-length(to_flip)]
  #       }
  #       to_keep[to_flip] <- TRUE
  #       # remove ping deviation values for the
  #       # salvaged detections so they don't cause
  #       # confusion going forward
  #       tag$ping_dev[to_flip] <- NA
  #       return(tag[to_keep, ])
  #     })
  #     output <- data.table::rbindlist(recipient2)
  #     return(output)
  #   })
  #   new_dets <- data.table::rbindlist(recipient1)

  #   aux1 <- nrow(new_dets)
  #   aux2 <- nrow(object@det)

  #   if (table_type(object) == "data.frame") {
  #     output <- as.data.frame(new_dets)
  #   }
  #   if (table_type(object) == "data.table") {
  #     output <- data.table::as.data.table(new_dets)
  #   }
  #   if (table_type(object) == "tibble") {
  #     output <- tibble::as.tibble(new_dets)
  #   }
  #   class(output) <- c("ATO_det", class(output))
  #   object@det <- output

  #   message("M: Filtered ", aux1,
  #           " detections (",
  #           .dyn_round(aux1 / aux2 * 100),
  #           "%) matching set thresholds.")
  # }
  # return(object)