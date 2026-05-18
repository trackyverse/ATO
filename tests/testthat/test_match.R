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

test_that(".match_ani_tag messages about stray animals", {
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

test_that(".match_ani_tag warns tag activation time > animal release time", {
  an <- make_ani(
    animal = ani(example_ato)$animal[1],
    release_datetime = ani(example_ato)$release_datetime[1]
  )
  ta <- make_tag(
    transmitter = tag(example_ato)$transmitter[1],
    animal = ani(example_ato)$animal[1],
    activation_datetime = ani(example_ato)$release_datetime[1] + 3600
  )
  expect_warning(
    x <- init_ato(
      ani = an,
      tag = ta
    ),
    "4450 comes after the release time of the respective animal(s)",
    fixed = TRUE
  )
})

test_that(".match_ani_tag warns tag activation time > animal terminal time", {
  an <- make_ani(
    animal = ani(example_ato)$animal[1],
    release_datetime = ani(example_ato)$release_datetime[1]
  )
  ob <- make_obs(
    datetime = ani(example_ato)$release_datetime[1] + 1800,
    animal = ani(example_ato)$animal[1],
    terminal = TRUE
  )
  ta <- make_tag(
    transmitter = tag(example_ato)$transmitter[1],
    animal = ani(example_ato)$animal[1],
    activation_datetime = ani(example_ato)$release_datetime[1] + 3600
  )
  expect_warning(
    x <- init_ato(
      ani = an,
      tag = ta,
      obs = ob
    ),
    "4450 comes after the terminal time of the respective animal(s)",
    fixed = TRUE
  )
  expect_warning(
    x <- init_ato(
      ani = an,
      tag = ta,
      obs = ob
    ),
    "4450 comes after the terminal time of the respective transmitter(s)",
    fixed = TRUE
  )
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

  expect_equal(ani(x)$n_tag, c(1, 1, 1, 1))
  expect_equal(tag(x)$ani_match, 1:4)
  expect_equal(tag(x)$n_obs, c(0, 0, 0, 1))
  expect_equal(ani(x)$n_obs, c(1, 0, 0, 0))
  expect_equal(
    tag(x)$release_datetime,
    ani(x)$release_datetime
  )
  expect_equal(
    tag(x)$terminal_datetime[1],
    ani(example_ato)$release_datetime[1] + 24 * 3600
  )
  expect_equal(
    tag(x)$terminal_datetime[4],
    as.POSIXct(NA_real_, tz = tzone(x))
  )
})

test_that(".match_obs_tag works without ani", {
  ta <- make_tag(
    transmitter = tag(example_ato)$transmitter[1:4],
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
    tag = ta
  )

  expect_equal(ani(x), NULL)
  expect_equal(obs(x)$tag_match, c(1, NA))
  expect_equal(obs(x)$ani_match, NULL)
  expect_equal(tag(x)$n_obs, c(1, 0, 0, 0))

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

  x <- init_ato(
    obs = ob,
    tag = ta
  )

  expect_equal(
    tag(x)$terminal_datetime[1],
    ani(example_ato)$release_datetime[1] + 3 * 24 * 3600
  )
})

test_that(".match_obs_tag manages tag reuse", {
  # tag is reused, there's a terminal in the middle
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
      TRUE,
      TRUE
    )
  )

  x <- init_ato(
    obs = ob,
    ani = an,
    tag = ta
  )

  expect_equal(ani(x)$n_tag, c(1, 1, 1, 1))
  expect_equal(tag(x)$ani_match, 1:4)
  expect_equal(tag(x)$n_obs, c(0, 0, 0, 1))
  expect_equal(ani(x)$n_obs, c(1, 0, 0, 0))
  expect_equal(
    tag(x)$release_datetime,
    ani(x)$release_datetime
  )
  expect_equal(
    tag(x)$terminal_datetime[1],
    ani(example_ato)$release_datetime[1] + 24 * 3600
  )
  expect_equal(
    tag(x)$terminal_datetime[4],
    ani(example_ato)$release_datetime[1] + 3 * 24 * 3600
  )

  # but issues error if the first animal isn't terminated
  ob <- make_obs(
    datetime = c(
      ani(example_ato)$release_datetime[1] + 3 * 24 * 3600,
      ani(example_ato)$release_datetime[1] + 24 * 3600
    ),
    transmitter = c(
      tag(example_ato)$transmitter[2],
      NA_character_
    ),
    animal = c(
      NA_character_,
      ani(example_ato)$animal[2]
    ),
    terminal = c(
      TRUE,
      TRUE
    )
  )

  expect_error(
    init_ato(
      obs = ob,
      ani = an,
      tag = ta
    ),
    "Transmitter R64K-4450 was used in animals 4450 and 4453 but animal 4450",
    fixed = TRUE
  )
})

test_that(".match_obs_tag manages terminal in both transmitter and animal", {
  an <- make_ani(
    animal = ani(example_ato)$animal[1:4],
    release_datetime = ani(example_ato)$release_datetime[1:4]
  )
  an$release_datetime[4] <- an$release_datetime[4] + 2 * 24 * 3600

  ta <- make_tag(
    transmitter = tag(example_ato)$transmitter[c(1:3, 1)],
    animal = ani(example_ato)$animal[1:4]
  )

  # both tag and animal have terminal obs.
  # tag terminal comes after animal terminal, so
  # the terminal obs for the tag is considered a stray.
  ob <- make_obs(
    datetime = c(
      ani(example_ato)$release_datetime[1] + 3 * 24 * 3600,
      ani(example_ato)$release_datetime[1] + 24 * 3600,
      ani(example_ato)$release_datetime[1] + 24 * 3600
    ),
    transmitter = c(
      tag(example_ato)$transmitter[2],
      NA_character_,
      NA_character_
    ),
    animal = c(
      NA_character_,
      ani(example_ato)$animal[1],
      ani(example_ato)$animal[2]
    ),
    terminal = c(
      TRUE,
      TRUE,
      TRUE
    )
  )

  x <- init_ato(
    obs = ob,
    ani = an,
    tag = ta
  )

  expect_equal(
    tag(x)$terminal_datetime[2],
    ani(example_ato)$release_datetime[1] + 24 * 3600
  )

  # both tag and animal have terminal obs.
  # tag terminal comes before animal terminal, so
  # the terminal obs for the tag wins over the animal.
  ob <- make_obs(
    datetime = c(
      ani(example_ato)$release_datetime[1] + 2 * 24 * 3600,
      ani(example_ato)$release_datetime[1] + 24 * 3600,
      ani(example_ato)$release_datetime[1] + 3 * 24 * 3600
    ),
    transmitter = c(
      tag(example_ato)$transmitter[2],
      NA_character_,
      NA_character_
    ),
    animal = c(
      NA_character_,
      ani(example_ato)$animal[1],
      ani(example_ato)$animal[2]
    ),
    terminal = c(
      TRUE,
      TRUE,
      TRUE
    )
  )

  x <- init_ato(
    obs = ob,
    ani = an,
    tag = ta
  )

  expect_equal(
    tag(x)$terminal_datetime[2],
    ani(example_ato)$release_datetime[1] + 2 * 24 * 3600
  )
})

test_that(".match_obs_tag picks up activation datetime", {
  an <- make_ani(
    animal = ani(example_ato)$animal[1],
    release_datetime = ani(example_ato)$release_datetime[1]
  )
  ta <- make_tag(
    transmitter = tag(example_ato)$transmitter[1:2],
    animal = c(ani(example_ato)$animal[1], NA),
    activation_datetime = rep(ani(example_ato)$release_datetime[1] - 3600, 2)
  )

  ob <- make_obs(
    datetime = c(
      ani(example_ato)$release_datetime[1] + 3 * 24 * 3600,
      ani(example_ato)$release_datetime[2] - 4000
    ),
    transmitter = tag(example_ato)$transmitter[1:2],
    terminal = c(
      FALSE,
      FALSE
    )
  )

  expect_message(
    x <- init_ato(
      obs = ob,
      ani = an,
      tag = ta
    ),
    "1 off-target valid observation (from 1 stray tag) found in the dataset.",
    fixed = TRUE
  )
})

test_that(".match_obs_tag stops in case of ambiguity", {
  an <- make_ani(
    animal = ani(example_ato)$animal[1],
    release_datetime = ani(example_ato)$release_datetime[1]
  )
  ta <- make_tag(
    transmitter = tag(example_ato)$transmitter[1:2],
    animal = rep(ani(example_ato)$animal[1], 2),
    activation_datetime = rep(ani(example_ato)$release_datetime[1] - 3600, 2)
  )
  ta <- rbind(ta, ta[2, ])

  ob <- make_obs(
    datetime = c(
      ani(example_ato)$release_datetime[1] + 3 * 24 * 3600,
      ani(example_ato)$release_datetime[2] + 4000
    ),
    transmitter = tag(example_ato)$transmitter[1:2],
    terminal = c(
      FALSE,
      FALSE
    )
  )

  expect_error(
    expect_warning(
      init_ato(
        obs = ob,
        ani = an,
        tag = ta
      ),
      "Duplicated rows (2 and 3) detected in @tag",
      fixed = TRUE
    ),
    "@obs row 1 matches @tag rows 2 and 3. Fatal ambiguity.",
    fixed = TRUE
  )
})

test_that(".match_det_tag_base returns expected values", {
  old_force_base <- getOption("ATO_force_base", default = FALSE)
  on.exit(options(ATO_force_base = old_force_base))
  options(ATO_force_base = TRUE)

  ta <- make_tag(
    transmitter = tag(example_ato)$transmitter[1:5],
    animal = ani(example_ato)$animal[1:5],
    tz = tzone(example_ato)
  )

  expect_message(
    x <- init_ato(
      tag = ta,
      det = det(example_ato)
    ),
    "M: 13618 valid detections (from 51 transmitters) do not match",
    fixed = TRUE
  )

  check <- table(example_ato@det$transmitter)[x@tag$transmitter]
  check[is.na(check)] <- 0
  expect_equal(x@tag$n_det, as.vector(check))

  check <- table(x@det$transmitter, x@det$tag_match)
  expect_equal(x@tag$n_det[2], check[1, 1])
  expect_equal(x@tag$n_det[4], check[2, 2])
  expect_equal(x@tag$n_det[5], check[3, 3])
})

test_that(".match_det_tag_base handles activation time and battery life", {
  old_force_base <- getOption("ATO_force_base", default = FALSE)
  on.exit(options(ATO_force_base = old_force_base))
  options(ATO_force_base = TRUE)

  ta <- make_tag(
    transmitter = tag(example_ato)$transmitter[1:5],
    activation_datetime = c(
      as.POSIXct(NA),
      ani(example_ato)$release_datetime[1]-1,
      NA, NA, NA
    ),
    battery_life = c(NA, 20, NA, NA, NA),
    animal = ani(example_ato)$animal[1:5],
    tz = tzone(example_ato)
  )

  expect_message(
    x <- init_ato(
      tag = ta,
      det = det(example_ato)
    ),
    "M: 13831 valid detections (from 52 transmitters) do not match",
    fixed = TRUE
  )

  check <- table(example_ato@det$transmitter)[x@tag$transmitter]
  check[is.na(check)] <- 0
  expect_equal(x@tag$n_det[-2], as.vector(check)[-2])
  expect_equal(x@tag$n_det[2], 336)

  check <- table(x@det$transmitter, x@det$tag_match)
  expect_equal(x@tag$n_det[2], 336)
  expect_equal(x@tag$n_det[4], check[2, 2])
  expect_equal(x@tag$n_det[5], check[3, 3])
})

test_that(".match_det_tag_base handles animal links", {
  old_force_base <- getOption("ATO_force_base", default = FALSE)
  on.exit(options(ATO_force_base = old_force_base))
  options(ATO_force_base = TRUE)

  ta <- make_tag(
    transmitter = tag(example_ato)$transmitter[1:5],
    activation_datetime = c(
      as.POSIXct(NA),
      ani(example_ato)$release_datetime[1]-1,
      NA, NA, NA
    ),
    battery_life = c(NA, 20, NA, NA, NA),
    animal = ani(example_ato)$animal[1:5],
    tz = tzone(example_ato)
  )

  an <- make_ani(
    animal = ani(example_ato)$animal[1:5],
    release_datetime = ani(example_ato)$release_datetime[1:5]
  )

  x <- init_ato(
    tag = ta,
    det = det(example_ato),
    ani = an
  )

  expect_equal(x@ani$n_det, x@tag$n_det)

  an <- make_ani(
    animal = ani(example_ato)$animal[1],
    release_datetime = ani(example_ato)$release_datetime[1]
  )

  ta <- make_tag(
    transmitter = tag(example_ato)$transmitter[1:5],
    animal = ani(example_ato)$animal[1]
  )

  x <- init_ato(
    tag = ta,
    det = det(example_ato),
    ani = an
  )

  expect_equal(x@ani$n_det, sum(x@tag$n_det))

  an <- make_ani(
    animal = ani(example_ato)$animal[1],
    release_datetime = ani(example_ato)$release_datetime[1]
  )

  ta <- make_tag(
    transmitter = tag(example_ato)$transmitter[1:5],
    animal = ani(example_ato)$animal[1]
  )

  ob <- make_obs(
    datetime = ani(example_ato)$release_datetime[1] + 20 * 24 * 3600,
    animal = ani(example_ato)$animal[1],
    terminal = TRUE
  )

  expect_message(
    x <- init_ato(
      tag = ta,
      det = det(example_ato),
      ani = an,
      obs = ob
    ),
    "M: 13846 valid detections (from 53 transmitters) do not match",
    fixed = TRUE
  )

  expect_equal(x@ani$n_det, 698)

  ob <- make_obs(
    datetime = ani(example_ato)$release_datetime[1] + 10 * 24 * 3600,
    transmitter = tag(example_ato)$transmitter[5],
    terminal = TRUE
  )

  expect_message(
    x <- init_ato(
      tag = ta,
      det = det(example_ato),
      ani = an,
      obs = ob
    ),
    "M: 13764 valid detections (from 52 transmitters) do not match",
    fixed = TRUE
  )

  expect_equal(x@ani$n_det, 780)
})

test_that(".match_det_tag_data.table returns expected values", {
  old_force_base <- getOption("ATO_force_base", default = FALSE)
  on.exit(options(ATO_force_base = old_force_base))
  options(ATO_force_base = FALSE)

  ta <- make_tag(
    transmitter = tag(example_ato)$transmitter[1:5],
    animal = ani(example_ato)$animal[1:5],
    tz = tzone(example_ato)
  )

  expect_message(
    x <- init_ato(
      tag = ta,
      det = det(example_ato)
    ),
    "M: 13618 valid detections (from 51 transmitters) do not match",
    fixed = TRUE
  )

  check <- table(example_ato@det$transmitter)[x@tag$transmitter]
  check[is.na(check)] <- 0
  expect_equal(x@tag$n_det, as.vector(check))

  check <- table(x@det$transmitter, x@det$tag_match)
  expect_equal(x@tag$n_det[2], check[1, 1])
  expect_equal(x@tag$n_det[4], check[2, 2])
  expect_equal(x@tag$n_det[5], check[3, 3])
})

test_that(".match_det_tag_data.table handles activation time and battery life", {
  old_force_base <- getOption("ATO_force_base", default = FALSE)
  on.exit(options(ATO_force_base = old_force_base))
  options(ATO_force_base = FALSE)

  ta <- make_tag(
    transmitter = tag(example_ato)$transmitter[1:5],
    activation_datetime = c(
      as.POSIXct(NA),
      ani(example_ato)$release_datetime[1]-1,
      NA, NA, NA
    ),
    battery_life = c(NA, 20, NA, NA, NA),
    animal = ani(example_ato)$animal[1:5],
    tz = tzone(example_ato)
  )

  expect_message(
    x <- init_ato(
      tag = ta,
      det = det(example_ato)
    ),
    "M: 13831 valid detections (from 52 transmitters) do not match",
    fixed = TRUE
  )

  check <- table(example_ato@det$transmitter)[x@tag$transmitter]
  check[is.na(check)] <- 0
  expect_equal(x@tag$n_det[-2], as.vector(check)[-2])
  expect_equal(x@tag$n_det[2], 336)

  check <- table(x@det$transmitter, x@det$tag_match)
  expect_equal(x@tag$n_det[2], 336)
  expect_equal(x@tag$n_det[4], check[2, 2])
  expect_equal(x@tag$n_det[5], check[3, 3])
})

test_that(".match_det_tag_data.table handles animal links", {
  old_force_base <- getOption("ATO_force_base", default = FALSE)
  on.exit(options(ATO_force_base = old_force_base))
  options(ATO_force_base = FALSE)

  ta <- make_tag(
    transmitter = tag(example_ato)$transmitter[1:5],
    activation_datetime = c(
      as.POSIXct(NA),
      ani(example_ato)$release_datetime[1]-1,
      NA, NA, NA
    ),
    battery_life = c(NA, 20, NA, NA, NA),
    animal = ani(example_ato)$animal[1:5],
    tz = tzone(example_ato)
  )

  an <- make_ani(
    animal = ani(example_ato)$animal[1:5],
    release_datetime = ani(example_ato)$release_datetime[1:5]
  )

  x <- init_ato(
    tag = ta,
    det = det(example_ato),
    ani = an
  )

  expect_equal(x@ani$n_det, x@tag$n_det)

  an <- make_ani(
    animal = ani(example_ato)$animal[1],
    release_datetime = ani(example_ato)$release_datetime[1]
  )

  ta <- make_tag(
    transmitter = tag(example_ato)$transmitter[1:5],
    animal = ani(example_ato)$animal[1]
  )

  x <- init_ato(
    tag = ta,
    det = det(example_ato),
    ani = an
  )

  expect_equal(x@ani$n_det, sum(x@tag$n_det))

  an <- make_ani(
    animal = ani(example_ato)$animal[1],
    release_datetime = ani(example_ato)$release_datetime[1]
  )

  ta <- make_tag(
    transmitter = tag(example_ato)$transmitter[1:5],
    animal = ani(example_ato)$animal[1]
  )

  ob <- make_obs(
    datetime = ani(example_ato)$release_datetime[1] + 20 * 24 * 3600,
    animal = ani(example_ato)$animal[1],
    terminal = TRUE
  )

  expect_message(
    x <- init_ato(
      tag = ta,
      det = det(example_ato),
      ani = an,
      obs = ob
    ),
    "M: 13846 valid detections (from 53 transmitters) do not match",
    fixed = TRUE
  )

  expect_equal(x@ani$n_det, 698)

  ob <- make_obs(
    datetime = ani(example_ato)$release_datetime[1] + 10 * 24 * 3600,
    transmitter = tag(example_ato)$transmitter[5],
    terminal = TRUE
  )

  expect_message(
    x <- init_ato(
      tag = ta,
      det = det(example_ato),
      ani = an,
      obs = ob
    ),
    "M: 13764 valid detections (from 52 transmitters) do not match",
    fixed = TRUE
  )

  expect_equal(x@ani$n_det, 780)
})

# # TESTS END HERE ------
# # ---------------------

setwd(tests.home)

if (is.na(oldtz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = oldtz)

rm(list = ls())
