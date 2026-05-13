skip_on_cran()
oldtz <- Sys.getenv('TZ', unset = NA)
Sys.setenv(TZ = 'UTC')
# 
tests.home <- getwd()
setwd(tempdir())

# TESTS START HERE ----
# ---------------------

test_that("make_ani works when given good data", {
  x <- as.data.frame(ani(example_ato))
  y <- make_ani(
    animal = x$animal,
    capture_location = x$capture_location,
    capture_datetime = x$capture_datetime,
    capture_lat = x$capture_lat,
    capture_lon = x$capture_lon,
    release_location = x$release_location,
    release_datetime = x$release_datetime,
    release_lat = x$release_lat,
    release_lon = x$release_lon,
    tz = tzone(example_ato),
    group = x$group,
    length_mm = x$length_mm,
    mass_g = x$mass_g)

	expect_equal(ani(example_ato)[, 1:13], y)
})

test_that("make_dep works when given good data", {
  x <- as.data.frame(dep(example_ato))
  y <- make_dep(
    receiver_model = x$receiver_model,
    receiver_serial = x$receiver_serial,
    receiver_codeset = x$receiver_codeset,
    deploy_location = x$deploy_location,
    deploy_datetime = x$deploy_datetime,
    deploy_lat = x$deploy_lat,
    deploy_lon = x$deploy_lon,
    deploy_z = x$deploy_z,
    recover_datetime = x$recover_datetime,
    recover_lat = x$recover_lat,
    recover_lon = x$recover_lon,
    transmitter = x$transmitter,
    transmitter_manufacturer = x$transmitter_manufacturer,
    transmitter_ping_rate = x$transmitter_ping_rate,
    transmitter_model = x$transmitter_model,
    transmitter_serial = x$transmitter_serial,
    tz = tzone(example_ato),
    array = x$array,
    section = x$section
  )
  expect_equal(dep(example_ato)[, 1:20], y)
})

test_that("make_det works when given good data", {
  x <- as.data.frame(det(example_ato))
  y <- make_det(
    datetime = x$datetime,
    frac_second = x$frac_second,
    receiver_serial = x$receiver_serial,
    transmitter = x$transmitter,
    sensor_value = x$sensor_value,
    tz = tzone(example_ato),
  )
  expect_equal(det(example_ato)[, 1:6], y)
})

test_that("make_obs works when given good data", {
  y <- make_obs(
    datetime = rep(Sys.time(), 10),
    transmitter = LETTERS[1:10],
    terminal = rep(FALSE, 10),
    tz = tzone(example_ato)
  )
  expect_equal(class(y)[1], "ATO_obs")
})

test_that("make_tag works when given good data", {
  x <- as.data.frame(tag(example_ato))
  y <- make_tag(
    manufacturer = x$manufacturer,
    model = x$model,
    serial = x$serial,
    transmitter = x$transmitter,
    activation_datetime = x$activation_datetime,
    battery_life = x$battery_life,
    sensor_type = x$sensor_type,
    sensor_unit = x$sensor_unit,
    animal = x$animal,
    tz = tzone(example_ato),
    mass_air_g = x$mass_air_g,
    mass_water_g = x$mass_water_g
  )

  expect_equal(tag(example_ato)[, 1:15], y)
})

# TESTS END HERE ------
# ---------------------

setwd(tests.home)

if (is.na(oldtz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = oldtz)

rm(list = ls())
