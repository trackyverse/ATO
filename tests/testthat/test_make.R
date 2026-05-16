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
    mass_g = x$mass_g
  )
	expect_equal(y, ani(example_ato)[, 1:13])
  expect_equal(class(y), c("ATO_ani", "data.frame"))
})

test_that("make_ani complains when bad NAs found", {
  expect_error(
    make_ani(
      animal = "1",
      release_datetime = NA),
    paste0(
      "Missing data detected in release_datetime.",
      " All animals must have release_datetime information.",
      " If you are unsure what the exact release times were,",
      " use an approximate value."
    ),
    fixed = TRUE)

  expect_error(
    make_ani(
      animal = NA,
      release_datetime = Sys.time()
    ),
    paste0(
      "Missing data detected in animal.",
      " All animals must have animal information."
    ),
    fixed = TRUE
  )
})

test_that("make_ani transfers tz to capture_datetime if needed", {
  x <- as.data.frame(ani(example_ato))
  y <- make_ani(
    animal = x$animal,
    release_datetime = x$release_datetime
  )
  expect_equal(attributes(y$capture_datetime)$tzone, "Europe/Copenhagen")
})

test_that("make_ani can make data.tables and tibbles", {
  x <- as.data.frame(ani(example_ato))
  if ("data.table" %in% utils::installed.packages()) {
    y <- make_ani(
      animal = x$animal,
      release_datetime = x$release_datetime,
      tbl = "data.table"
    )
    expect_equal(class(y), c("ATO_ani", "data.table", "data.frame"))
  } else {
    expect_error(
      make_ani(
        animal = x$animal,
        release_datetime = x$release_datetime,
        tbl = "data.table"
      ),
      "You must install package data.table to run this function.",
      fixed = TRUE
    )
  }
  if ("tibble" %in% utils::installed.packages()) {
    y <- make_ani(
      animal = x$animal,
      release_datetime = x$release_datetime,
      tbl = "tibble"
    )
    expect_equal(class(y), c("ATO_ani", "tbl_df", "tbl", "data.frame"))
  } else {
    expect_error(
      make_ani(
        animal = x$animal,
        release_datetime = x$release_datetime,
        tbl = "tibble"
      ),
      "You must install package tibble to run this function.",
      fixed = TRUE
    )
  }
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
  expect_equal(class(y), c("ATO_dep", "data.frame"))
})

test_that("make_dep complains when bad NAs found", {
  expect_error(
    make_dep(
      deploy_datetime = Sys.time(),
      recover_datetime = NA),
    paste0(
      "Missing data detected in recover_datetime.",
      " All deployments must have recover_datetime information.",
      " If you are unsure what the exact deployment and recovery times were,",
      " use an approximate value.",
      " For rolling receiver deployments, use the time of",
      " last download as the recovery time."
    ),
    fixed = TRUE)

  expect_error(
    make_dep(
      deploy_datetime = NA,
      recover_datetime = Sys.time()),
    paste0(
      "Missing data detected in deploy_datetime.",
      " All deployments must have deploy_datetime information.",
      " If you are unsure what the exact deployment and recovery times were,",
      " use an approximate value."
    ),
    fixed = TRUE
  )
})

test_that("make_dep can make data.tables and tibbles", {
  x <- as.data.frame(dep(example_ato))
  if ("data.table" %in% utils::installed.packages()) {
    y <- make_dep(
      deploy_datetime = x$deploy_datetime,
      recover_datetime = x$recover_datetime,
      tbl = "data.table"
    )
    expect_equal(class(y), c("ATO_dep", "data.table", "data.frame"))
  } else {
    expect_error(
      make_dep(
        deploy_datetime = x$deploy_datetime,
        recover_datetime = x$recover_datetime,
        tbl = "data.table"
      ),
      "You must install package data.table to run this function.",
      fixed = TRUE
    )
  }
  if ("tibble" %in% utils::installed.packages()) {
    y <- make_dep(
      deploy_datetime = x$deploy_datetime,
      recover_datetime = x$recover_datetime,
      tbl = "tibble"
    )
    expect_equal(class(y), c("ATO_dep", "tbl_df", "tbl", "data.frame"))
  } else {
    expect_error(
      make_dep(
        deploy_datetime = x$deploy_datetime,
        recover_datetime = x$recover_datetime,
        tbl = "tibble"
      ),
      "You must install package tibble to run this function.",
      fixed = TRUE
    )
  }
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
  expect_equal(class(y), c("ATO_det", "data.frame"))
})

test_that("make_det complains when bad NAs found", {
  expect_error(
    make_det(
      datetime = round(Sys.time()),
      receiver_serial = NA,
      transmitter = NA
    ),
    paste0(
      "Missing data detected in receiver_serial and transmitter.",
      " All detections must have receiver_serial and transmitter information."
    ),
    fixed = TRUE
  )
})

test_that("make_det stops if datetime data has no timezone", {
  expect_error(
    y <- make_det(
      datetime = Sys.time(),
      receiver_serial = "1",
      transmitter = "a"
    ),
    paste0(
      "The POSIXt data in column datetime is missing timezone information.",
      " Use argument tz to assign a timezone."
    ),
    fixed = TRUE
  )
})

test_that("make_det complains about milliseconds in datetime", {
  x <- Sys.time()
  expect_warning(
    y <- make_det(
      datetime = x,
      receiver_serial = "1",
      transmitter = "a",
      tz = "America/Halifax"
    ),
    paste0(
      "datetime contains millisecond information.",
      " Because frac_second was not provided"
    ),
    fixed = TRUE
  )
  expect_equal(as.numeric(y$datetime) - round(as.numeric(y$datetime)), 0)
  expect_equal(y$frac_second, as.numeric(x) - floor(as.numeric(x)))

  x <- Sys.time()
  expect_warning(
    y <- make_det(
      datetime = x,
      frac_second = as.numeric(x) - floor(as.numeric(x)),
      receiver_serial = "1",
      transmitter = "a",
      tz = "America/Halifax"
    ),
    paste0(
      "datetime contains millisecond information.",
      " This information matches the values provided"
    ),
    fixed = TRUE
  )
  expect_equal(as.numeric(y$datetime) - round(as.numeric(y$datetime)), 0)
  expect_equal(y$frac_second, as.numeric(x) - floor(as.numeric(x)))

  expect_error(
    y <- make_det(
      datetime = Sys.time(),
      frac_second = 0.5,
      receiver_serial = "1",
      transmitter = "a",
      tz = "America/Halifax"
    ),
    paste0(
      "datetime contains millisecond information that does not match the",
      " values provided through frac_second."
    ),
    fixed = TRUE
  )
})

test_that("make_det can make data.tables and tibbles", {
  x <- as.data.frame(det(example_ato))
  if ("data.table" %in% utils::installed.packages()) {
    y <- make_det(
      datetime = x$datetime,
      receiver_serial = x$receiver_serial,
      transmitter = x$transmitter,
      tbl = "data.table"
    )
    expect_equal(class(y), c("ATO_det", "data.table", "data.frame"))
  } else {
    expect_error(
      make_det(
        datetime = x$datetime,
        receiver_serial = x$receiver_serial,
        transmitter = x$transmitter,
        tbl = "data.table"
      ),
      "You must install package data.table to run this function.",
      fixed = TRUE
    )
  }
  if ("tibble" %in% utils::installed.packages()) {
    y <- make_det(
      datetime = x$datetime,
      receiver_serial = x$receiver_serial,
      transmitter = x$transmitter,
      tbl = "tibble"
    )
    expect_equal(class(y), c("ATO_det", "tbl_df", "tbl", "data.frame"))
  } else {
    expect_error(
      make_det(
        datetime = x$datetime,
        receiver_serial = x$receiver_serial,
        transmitter = x$transmitter,
        tbl = "tibble"
      ),
      "You must install package tibble to run this function.",
      fixed = TRUE
    )
  }
})

test_that("make_obs works when given good data", {
  y <- make_obs(
    datetime = rep(Sys.time(), 10),
    transmitter = LETTERS[1:10],
    terminal = rep(FALSE, 10),
    tz = tzone(example_ato)
  )
  expect_equal(class(y), c("ATO_obs", "data.frame"))
})

test_that("make_obs complains when bad NAs found", {
  expect_error(
    make_obs(
      datetime = round(Sys.time()),
      terminal = NA,
    ),
    paste0(
      "Missing data detected in terminal.",
      " All observations must have terminal information."
    ),
    fixed = TRUE
  )

  expect_error(
    make_obs(
      datetime = round(Sys.time()),
      terminal = TRUE
    ),
    paste0(
      "Each observation must be associated to either",
      " an animal or a transmitter, or both."
    ),
    fixed = TRUE
  )

  y <- make_obs(
    datetime = round(Sys.time()),
    terminal = TRUE,
    animal = "1",
    tz = "America/Halifax"
  )
  expect_equal(y$transmitter, NA_character_)
})

test_that("make_obs can make data.tables and tibbles", {
  if ("data.table" %in% utils::installed.packages()) {
    y <- make_obs(
      datetime = rep(Sys.time(), 10),
      transmitter = LETTERS[1:10],
      terminal = rep(FALSE, 10),
      tz = tzone(example_ato),
      tbl = "data.table"
    )
    expect_equal(class(y), c("ATO_obs", "data.table", "data.frame"))
  } else {
    expect_error(
      make_obs(
        datetime = rep(Sys.time(), 10),
        transmitter = LETTERS[1:10],
        terminal = rep(FALSE, 10),
        tz = tzone(example_ato),
        tbl = "data.table"
      ),
      "You must install package data.table to run this function.",
      fixed = TRUE
    )
  }
  if ("tibble" %in% utils::installed.packages()) {
    y <- make_obs(
      datetime = rep(Sys.time(), 10),
      transmitter = LETTERS[1:10],
      terminal = rep(FALSE, 10),
      tz = tzone(example_ato),
      tbl = "tibble"
    )
    expect_equal(class(y), c("ATO_obs", "tbl_df", "tbl", "data.frame"))
  } else {
    expect_error(
      make_obs(
        datetime = rep(Sys.time(), 10),
        transmitter = LETTERS[1:10],
        terminal = rep(FALSE, 10),
        tz = tzone(example_ato),
        tbl = "tibble"
      ),
      "You must install package tibble to run this function.",
      fixed = TRUE
    )
  }
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
  expect_equal(class(y), c("ATO_tag", "data.frame"))
})

test_that("make_tag complains when bad NAs found", {
  expect_error(
    make_tag(
      transmitter = NA,
    ),
    paste0(
      "Missing data detected in transmitter.",
      " All tags must have transmitter information."
    ),
    fixed = TRUE
  )
})

test_that("make_tag can make data.tables and tibbles", {
  x <- as.data.frame(tag(example_ato))
  if ("data.table" %in% utils::installed.packages()) {
    y <- make_tag(
      transmitter = LETTERS[1:10],
      tbl = "data.table"
    )
    expect_equal(class(y), c("ATO_tag", "data.table", "data.frame"))
  } else {
    expect_error(
      make_tag(
        transmitter = LETTERS[1:10],
        tbl = "data.table"
      ),
      "You must install package data.table to run this function.",
      fixed = TRUE
    )
  }
  if ("tibble" %in% utils::installed.packages()) {
    y <- make_tag(
      transmitter = LETTERS[1:10],
      tbl = "tibble"
    )
    expect_equal(class(y), c("ATO_tag", "tbl_df", "tbl", "data.frame"))
  } else {
    expect_error(
      make_tag(
        transmitter = LETTERS[1:10],
        tbl = "tibble"
      ),
      "You must install package tibble to run this function.",
      fixed = TRUE
    )
  }
})

# TESTS END HERE ------
# ---------------------

setwd(tests.home)

if (is.na(oldtz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = oldtz)

rm(list = ls())
