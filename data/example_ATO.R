# library("actel")
# library("ATO")

# # @ani
# head(actel:::example.biometrics)
# tail(actel:::example.spatial)
# ani <- make_ani(animal = as.character(actel:::example.biometrics$Signal),
#                 capture_location = as.character(actel:::example.biometrics$Release.site),
#                 capture_datetime = actel:::example.biometrics$Release.date,
#                 capture_lat = tail(actel:::example.spatial$Latitude, 1),
#                 capture_lon = tail(actel:::example.spatial$Longitude, 1),
#                 release_location = as.character(actel:::example.biometrics$Release.site),
#                 release_datetime = actel:::example.biometrics$Release.date,
#                 release_lat = tail(actel:::example.spatial$Latitude, 1),
#                 release_lon = tail(actel:::example.spatial$Longitude, 1),
#                 tz = "Europe/Copenhagen",
#                 group = actel:::example.biometrics$Group,
#                 length_mm = actel:::example.biometrics$Total.Length.mm,
#                 mass_g = actel:::example.biometrics$Mass.g)
# summary(ani)
# head(ani)

# # @dep
# head(actel:::example.deployments)
# head(actel:::example.spatial)
# link <- match(actel:::example.deployments$Station.name,
#               actel:::example.spatial$Station.name)
# dep <- make_dep(receiver_model = "VR2",
#                 receiver_serial = actel:::example.deployments$Receiver,
#                 receiver_codeset = "69kHz",
#                 deploy_location = actel:::example.deployments$Station.name,
#                 deploy_datetime = actel:::example.deployments$Start,
#                 deploy_lat = actel:::example.spatial$Latitude[link],
#                 deploy_lon = actel:::example.spatial$Longitude[link],
#                 deploy_z = NA_real_,
#                 recover_datetime = actel:::example.deployments$Stop,
#                 recover_lat = actel:::example.spatial$Latitude[link],
#                 recover_lon = actel:::example.spatial$Longitude[link],
#                 transmitter = NA_character_,
#                 transmitter_manufacturer = NA_character_,
#                 transmitter_ping_rate = NA_real_,
#                 transmitter_model = NA_character_,
#                 transmitter_serial = NA_character_,
#                 tz = "Europe/Copenhagen",
#                 array = actel:::example.spatial$Array[link],
#                 section = actel:::example.spatial$Section[link])
# summary(dep)
# head(dep)

# # @det
# head(actel:::example.detections)
# det <- make_det(datetime = as.POSIXct(actel:::example.detections$Timestamp),
#                 frac_second = NA_real_,
#                 receiver_serial = as.character(actel:::example.detections$Receiver),
#                 transmitter = paste0(actel:::example.detections$CodeSpace,
#                                      "-", actel:::example.detections$Signal),
#                 sensor_value = actel:::example.detections$Sensor.Value,
#                 tz = "Europe/Copenhagen")
# summary(det)
# head(det)

# # @tag
# head(actel:::example.biometrics)
# head(actel:::example.detections)
# link <- match(actel:::example.biometrics$Signal,
#               actel:::example.detections$Signal)
# tag <- make_tag(manufacturer = "Thelma",
#                 model = "7.3 mm tag",
#                 power_level = NA_real_,
#                 ping_rate = NA_real_,
#                 ping_variation = NA_real_,
#                 serial = as.character(actel:::example.biometrics$Serial.nr),
#                 transmitter = paste0("R69K-", actel:::example.biometrics$Signal),
#                 activation_datetime = as.POSIXct(actel:::example.biometrics$Release.date,
#                                                  tz = "Europe/Copenhagen") - 3600,
#                 battery_life = 150,
#                 sensor_type = "Temperature",
#                 sensor_unit = "C",
#                 animal = as.character(actel:::example.biometrics$Signal),
#                 tz = "Europe/Copenhagen",
#                 mass_air_g = 1.9,
#                 mass_water_g = 1.2)
# summary(tag)

# example_ato <- init_ato(ani = ani,
#                 dep = dep,
#                 det = det,
#                 tag = tag)

# example_ato

# usethis::use_data(example_ato, overwrite = TRUE)
