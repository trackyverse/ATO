# # TESTS START HERE ----
# # ---------------------

test_that("Check if has method and function errors when value does not 
meet the desired values", {
  expect_error(
    has(
      object = example_ato,
      value = "test",
      column = c("test", "power_level")
    ),
    "test do not match ATO slot names",
    fixed = TRUE
  )

  expect_error(
    has(
      object = example_ato,
      value = c("test", "don't"),
      column = c("test", "power_level")
    ),
    "test and don't does not match ATO slot names",
    fixed = TRUE
  )
})


test_that("Check if has errors when there is an empty slot that it is looking 
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

test_that("Check if has errors when there is an empty slot that it is looking 
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


test_that("Check if errors when missing a column when error = TRUE", {
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

test_that("Check if errors when missing a column when error = FALSE", {
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

# # TESTS END HERE ------
# # ---------------------

setwd(tests.home)

if (is.na(oldtz)) {
  Sys.unsetenv("TZ")
} else {
  Sys.setenv(TZ = oldtz)
}

rm(list = ls())
apply()
