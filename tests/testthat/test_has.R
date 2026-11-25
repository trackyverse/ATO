start_test()
# # TESTS START HERE ----
# # ---------------------

test_that("has() method and function errors when value does not 
meet the desired values", {
  expect_error(
    has(
      object = example_ato,
      value = "test",
      column = c("test", "power_level")
    ),
    "test does not match ATO slot names",
    fixed = TRUE
  )

  expect_error(
    has(
      object = example_ato,
      value = c("test", "don't"),
      column = c("test", "power_level")
    ),
    "test and don't do not match ATO slot names",
    fixed = TRUE
  )
})


test_that("has() errors when there is an empty slot that it is looking 
for when error = TRUE", {
  expect_error(
    has(
      object = init_ato(),
      value = "obs",
      error = TRUE
    ),
    fixed = TRUE,
    "The following slot is empty: obs"
  )
  expect_error(
    has(
      object = init_ato(),
      value = c("obs", "tag"),
      error = TRUE
    ),
    fixed = TRUE,
    "The following slots are empty: obs and tag"
  )
})

test_that("has() errors when there is an empty slot that it is looking 
for when error = FALSE it reutrns a false object", {
  expect_false(
    has(
      object = init_ato(),
      value = "obs",
    )
  )
  expect_false(
    has(
      object = init_ato(),
      value = c("obs", "tag"),
    )
  )
})


test_that("has() errors when missing a column when error = TRUE", {
  test_ato <- example_ato
  test_ato@tag$transmitter <- NULL

  expect_error(
    has(test_ato, value = "tag", column = "transmitter", error = TRUE),
    "Slot tag does not contain column transmitter.",
    fixed = TRUE
  )
  test_ato@tag$serial <- NULL

  expect_error(
    has(
      test_ato,
      value = "tag",
      column = c("transmitter", "serial"),
      error = TRUE
    ),
    "Slot tag does not contain columns transmitter and serial.",
    fixed = TRUE
  )
})

test_that("has() errors when missing a column when error = FALSE", {
  test_ato <- example_ato
  test_ato@tag$transmitter <- NULL

  expect_false(
    has(test_ato, value = "tag", column = "transmitter")
  )
  test_ato@tag$serial <- NULL

  expect_false(
    has(
      test_ato,
      value = "tag",
      column = c("transmitter", "serial")
    )
  )
})


test_that("has() errors when NAs are introduced and not ALLOWED when error = TRUE", {
  test_ato_na <- example_ato
  test_ato_na@tag$transmitter <- NA

  expect_error(
    has(
      test_ato_na,
      value = "tag",
      column = "transmitter",
      allow_NA = FALSE,
      error = TRUE
    ),
    "NAs found in column transmitter of slot tag.",
    fixed = TRUE
  )
  test_ato_na@tag$serial <- NA

  expect_error(
    has(
      test_ato_na,
      value = "tag",
      column = c("transmitter", "serial"),
      allow_NA = FALSE,
      error = TRUE
    ),
    "NAs found in columns transmitter and serial of slot tag",
    fixed = TRUE
  )
})


test_that("has() errors when NAs are introduced and not ALLOWED when error = FALSE", {
  test_ato_na <- example_ato
  test_ato_na@tag$transmitter <- NA

  expect_false(
    has(
      test_ato_na,
      value = "tag",
      column = "transmitter",
      allow_NA = FALSE,
      error = FALSE
    )
  )
  test_ato_na@tag$serial <- NA

  expect_false(
    has(
      test_ato_na,
      value = "tag",
      column = c("transmitter", "serial"),
      allow_NA = FALSE,
      error = FALSE
    )
  )
})


test_that("has() behaves properly for data.table ATO objects when allowed_NA = FALSE and  
error = TRUE", {
  ato_table_type_global("data.table")

  ani_df <- as.data.frame(ani(example_ato))
  dep_df <- as.data.frame(dep(example_ato))

  if ("data.table" %in% utils::installed.packages()) {
    ani_dt <- make_ani(
      animal = ani_df$animal,
      release_datetime = ani_df$release_datetime,
      tbl = "data.table"
    )
  }

  if ("data.table" %in% utils::installed.packages()) {
    dep_dt <- make_dep(
      deploy_datetime = dep_df$deploy_datetime,
      recover_datetime = dep_df$recover_datetime,
      tbl = "data.table"
    ) |>
      suppressWarnings()
  }

  test_ato_dt <- init_ato(ani = ani_dt, dep = dep_dt) |>
    suppressWarnings()

  test_ato_dt@ani$animal <- NA
  expect_error(
    has(
      test_ato_dt,
      value = "ani",
      column = c("animal"),
      allow_NA = FALSE,
      error = TRUE
    ),
    "NAs found in column animal of slot ani.",
    fixed = TRUE
  )

  test_ato_dt@ani$release_datetime <- NA
  expect_error(
    has(
      test_ato_dt,
      value = "ani",
      column = c("animal", "release_datetime"),
      allow_NA = FALSE,
      error = TRUE
    ),
    "NAs found in columns animal and release_datetime of slot ani.",
    fixed = TRUE
  )
})
test_that("has() behaves properly for data.table ATO objects when allowed_NA = FALSE and  
error = FALSE", {
  ani_df <- as.data.frame(ani(example_ato))
  dep_df <- as.data.frame(dep(example_ato))

  if ("data.table" %in% utils::installed.packages()) {
    ani_dt <- make_ani(
      animal = ani_df$animal,
      release_datetime = ani_df$release_datetime,
      tbl = "data.table"
    )
  }

  if ("data.table" %in% utils::installed.packages()) {
    dep_dt <- make_dep(
      deploy_datetime = dep_df$deploy_datetime,
      recover_datetime = dep_df$recover_datetime,
      tbl = "data.table",
    ) |>
      suppressWarnings()
  }

  test_ato_dt <- init_ato(ani = ani_dt, dep = dep_dt) |>
    suppressWarnings()
  test_ato_dt@ani$animal <- NA

  expect_false(
    has(
      test_ato_dt,
      value = "ani",
      column = c("animal"),
      allow_NA = FALSE,
      error = FALSE
    )
  )

  test_ato_dt@ani$release_datetime <- NA
  expect_false(
    has(
      test_ato_dt,
      value = "ani",
      column = c("animal", "release_datetime"),
      allow_NA = FALSE,
      error = FALSE
    )
  )
})


# # TESTS END HERE ------
# # ---------------------

end_test()
