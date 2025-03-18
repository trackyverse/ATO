# THIS IS A SANDBOX FILE
# DO NOT KEEP PRECIOUS CODE IN HERE

my_ATO <- new("ATO")
validObject(my_ATO)

dets <- data.frame(datetime = as.POSIXct(actel:::example.detections$Timestamp),
                   frac_second = NA_real_,
                   receiver_serial = actel:::example.detections$Receiver,
                   transmitter = with(actel:::example.detections, paste0(CodeSpace, "-", Signal)),
                   sensor_value = actel:::example.detections$Sensor.Value)

check_detections(as.data.frame(dets))

my_ATO <- add_detections(my_ATO, dets)
validObject(my_ATO)

head(actel:::example.deployments)
head(actel:::example.spatial)
deps <- data.frame(receiver_model = NA_character_,
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
                   transmitter_serial = NA_integer_)

my_ATO <- add_deployments(my_ATO, deps)

tags <- data.frame(manufacturer = "Thelma",
                   model = NA_character_,
                   power_level = NA_real_,
                   ping_rate = 60, # seconds
                   ping_variation = 30, # 0 if fixed ping rate
                   serial = actel:::example.biometrics$Serial.nr,
                   transmitter = paste0("R64K-", actel:::example.biometrics$Signal),
                   activation_datetime = as.POSIXct(NA_real_),
                   battery_life = NA_integer_, # days
                   sensor_type = NA_character_,
                   sensor_unit = NA_character_,
                   animal = as.character(1:nrow(actel:::example.biometrics)), 
                   capture_location = actel:::example.biometrics$Release.site,
                   capture_datetime = actel:::example.biometrics$Release.date,
                   capture_lat = actel:::example.spatial$Latitude[18],
                   capture_lon = actel:::example.spatial$Longitude[18],
                   release_location = actel:::example.biometrics$Release.site,
                   release_datetime = actel:::example.biometrics$Release.date,
                   release_lat = actel:::example.spatial$Latitude[18],
                   release_lon = actel:::example.spatial$Longitude[18])
