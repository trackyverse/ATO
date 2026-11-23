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


has(
  object = init_ato(),
  value = "obs",
  column = c("datetime")
)


# # TESTS END HERE ------
# # ---------------------

setwd(tests.home)

if (is.na(oldtz)) {
  Sys.unsetenv("TZ")
} else {
  Sys.setenv(TZ = oldtz)
}

rm(list = ls())
