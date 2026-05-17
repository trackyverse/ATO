skip_on_cran()
oldtz <- Sys.getenv('TZ', unset = NA)
Sys.setenv(TZ = 'UTC')

tests.home <- getwd()
setwd(tempdir())

# # TESTS START HERE ----
# # ---------------------

test_that(".match_obs_ani returns expected values", {
  ob <- make_obs(
    datetime = Sys.time(),
    animal = ani(example_ato)$animal[2],
    terminal = TRUE,
    tz = tzone(example_ato)
  )
  an <- make_ani(
    animal = ani(example_ato)$animal[1:5],
    release_datetime = ani(example_ato)$release_datetime[1:5]
  )

  x <- init_ato(
    ani = an,
    obs = ob
  )

  expect_equal(obs(x)$ani_match, 2)
  expect_equal(ani(x)$n_obs, c(0, 1, 0, 0, 0))
  expect_equal(ani(x)$terminal_datetime[1], as.POSIXct(NA_real_, tz = tzone(x)))
  expect_equal(ani(x)$terminal_datetime[2], obs(x)$datetime[1])
})

test_that(".match_obs_ani messages about stray animals", {
  ob <- make_obs(
    datetime = Sys.time(),
    animal = "someone",
    terminal = TRUE,
    tz = tzone(example_ato)
  )
  an <- make_ani(
    animal = ani(example_ato)$animal[1:5],
    release_datetime = ani(example_ato)$release_datetime[1:5]
  )

  expect_message(
    x <- init_ato(
      ani = an,
      obs = ob
    ),
    "M: 1 off-target valid observation (from 1 stray animal) found",
    fixed = TRUE
  )

  expect_equal(obs(x)$ani_match, NA_integer_)
  expect_equal(ani(x)$n_obs, c(0, 0, 0, 0, 0))
  expect_equal(ani(x)$terminal_datetime[1], as.POSIXct(NA_real_, tz = tzone(x)))
  expect_equal(ani(x)$terminal_datetime[2], as.POSIXct(NA_real_, tz = tzone(x)))
})

test_that(".match_ani_tag returns expected values", {
  an <- make_ani(
    animal = ani(example_ato)$animal[1:5],
    release_datetime = ani(example_ato)$release_datetime[1:5]
  )
  ta <- make_tag(
    transmitter = tag(example_ato)$transmitter[1:5],
    animal = ani(example_ato)$animal[1:5]
  )

  x <- init_ato(
    ani = an,
    tag = ta
  )

  expect_equal(ani(x)$n_tag, c(1, 1, 1, 1, 1))
  expect_true(all(!is.na(tag(x)$release_datetime)))
  expect_equal(tag(x)$release_datetime, ani(x)$release_datetime)
  expect_equal(tag(x)$ani_match, 1:5)
})

test_that(".match_ani_tag returns expected values", {
  an <- make_ani(
    animal = ani(example_ato)$animal[2:6],
    release_datetime = ani(example_ato)$release_datetime[1:5]
  )
  ta <- make_tag(
    transmitter = tag(example_ato)$transmitter[1:5],
    animal = ani(example_ato)$animal[1:5]
  )

  expect_message(
    x <- init_ato(
      ani = an,
      tag = ta
    ),
    "M: 1 off-target valid tag (from 1 stray animal) found",
    fixed = TRUE
  )

  expect_equal(ani(x)$n_tag, c(1, 1, 1, 1, 0))
  expect_equal(tag(x)$ani_match, c(NA_integer_, 1:4))
  expect_equal(
    tag(x)$release_datetime[2:5],
    ani(x)$release_datetime[1:4]
  )
  expect_equal(
    tag(x)$release_datetime[1],
    as.POSIXct(NA_real_, tz = tzone(x))
  )
})

test_that(".match_ani_tag can handle two transmitters per animal", {
  an <- make_ani(
    animal = ani(example_ato)$animal[1:4],
    release_datetime = ani(example_ato)$release_datetime[1:4]
  )
  ta <- make_tag(
    transmitter = tag(example_ato)$transmitter[1:4],
    animal = ani(example_ato)$animal[c(1, 1, 2, 2)]
  )

  expect_message(
    x <- init_ato(
      ani = an,
      tag = ta
    ),
    "M: 2 valid animals have no valid tags.",
    fixed = TRUE
  )

  expect_equal(ani(x)$n_tag, c(2, 2, 0, 0))
  expect_equal(tag(x)$ani_match, c(1, 1, 2, 2))
  expect_equal(
    tag(x)$release_datetime,
    ani(x)$release_datetime[c(1, 1, 2, 2)]
  )
})

test_that(".match_ani_tag can handle transmitter redeployment", {
  an <- make_ani(
    animal = ani(example_ato)$animal[1:4],
    release_datetime = ani(example_ato)$release_datetime[1:4]
  )
  an$release_datetime[4] <- an$release_datetime[4] + 2 * 24 * 3600

  ta <- make_tag(
    transmitter = tag(example_ato)$transmitter[c(1:3, 1)],
    animal = ani(example_ato)$animal[2:5]
  )

  expect_error(
    x <- init_ato(
      ani = an,
      tag = ta
    ),
    "Duplicated transmitters found in @tag but no @ani match.",
    fixed = TRUE
  )

  an <- make_ani(
    animal = ani(example_ato)$animal[1:4],
    release_datetime = ani(example_ato)$release_datetime[1:4]
  )
  an$release_datetime[4] <- an$release_datetime[4] + 2 * 24 * 3600

  ta <- make_tag(
    transmitter = tag(example_ato)$transmitter[c(1:3, 1)],
    animal = ani(example_ato)$animal[1:4]
  )

  expect_error(
    x <- init_ato(
      ani = an,
      tag = ta
    ),
    "Duplicated transmitters found in @tag but ato has no @obs",
    fixed = TRUE
  )

  ob <- make_obs(
    datetime = ani(example_ato)$release_datetime[1] + 3 * 24 * 3600,
    animal = ani(example_ato)$animal[1],
    terminal = TRUE
  )

  expect_error(
    x <- init_ato(
      ani = an,
      obs = ob,
      tag = ta
    ),
    "Transmitter R64K-4450 was used in animals 4450 and 4453 but these",
    fixed = TRUE
  )

  ob <- make_obs(
    datetime = ani(example_ato)$release_datetime[1] + 24 * 3600,
    transmitter = tag(example_ato)$transmitter[1],
    terminal = TRUE
  )

  expect_error(
    x <- init_ato(
      ani = an,
      obs = ob,
      tag = ta
    ),
    "Transmitter R64K-4450 was used in animals 4450 and 4453 but animal 4450",
    fixed = TRUE
  )

  ob <- make_obs(
    datetime = ani(example_ato)$release_datetime[1] + 24 * 3600,
    animal = ani(example_ato)$animal[1],
    terminal = TRUE
  )

  x <- init_ato(
    ani = an,
    obs = ob,
    tag = ta
  )

  expect_equal(ani(x)$n_tag, c(1, 1, 1, 1))
  expect_equal(tag(x)$ani_match, c(1, 2, 3, 4))
  expect_equal(obs(x)$ani_match, 1)
})

test_that(".match_obs_tag returns expected values", {
  an <- make_ani(
    animal = ani(example_ato)$animal[2:6],
    release_datetime = ani(example_ato)$release_datetime[1:5]
  )
  ta <- make_tag(
    transmitter = tag(example_ato)$transmitter[1:5],
    animal = ani(example_ato)$animal[1:5]
  )

  expect_message(
    x <- init_ato(
      ani = an,
      tag = ta
    ),
    "M: 1 off-target valid tag (from 1 stray animal) found",
    fixed = TRUE
  )

  expect_equal(ani(x)$n_tag, c(1, 1, 1, 1, 0))
  expect_equal(tag(x)$ani_match, c(NA_integer_, 1:4))
  expect_equal(
    tag(x)$release_datetime[2:5],
    ani(x)$release_datetime[1:4]
  )
  expect_equal(
    tag(x)$release_datetime[1],
    as.POSIXct(NA_real_, tz = tzone(x))
  )
  an <- make_ani(
    animal = ani(example_ato)$animal[1:4],
    release_datetime = ani(example_ato)$release_datetime[1:4]
  )
  an$release_datetime[4] <- an$release_datetime[4] + 2 * 24 * 3600

  ta <- make_tag(
    transmitter = tag(example_ato)$transmitter[c(1:3, 1)],
    animal = ani(example_ato)$animal[1:4]
  )

  ob <- make_obs(
    datetime = c(
      ani(example_ato)$release_datetime[1] + 3 * 24 * 3600,
      ani(example_ato)$release_datetime[1] + 24 * 3600
    ),
    transmitter = c(
      tag(example_ato)$transmitter[1],
      NA_character_
    ),
    animal = c(
      NA_character_,
      ani(example_ato)$animal[1]
    ),
    terminal = c(
      FALSE,
      TRUE
    )
  )

  x <- init_ato(
    obs = ob,
    ani = an,
    tag = ta
  )

  ani(x)
  tag(x)

  ob <- make_obs(
    datetime = c(
      ani(example_ato)$release_datetime[1] + 3 * 24 * 3600,
      ani(example_ato)$release_datetime[1] + 24 * 3600
    ),
    transmitter = c(
      tag(example_ato)$transmitter[1],
      NA_character_
    ),
    animal = c(
      NA_character_,
      ani(example_ato)$animal[1]
    ),
    terminal = c(
      TRUE,
      TRUE
    )
  )

  ato_match_immediate(T)
  x <- init_ato(
    obs = ob,
    ani = an,
    tag = ta
  )
  tag(x)
  obs(x)
})

# # TESTS END HERE ------
# # ---------------------

setwd(tests.home)

if (is.na(oldtz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = oldtz)

rm(list = ls())
