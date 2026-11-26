test_that("ato_debug() errors when value is given more than 1 option", {
  expect_error(
    ato_debug(value = c(TRUE, TRUE)),
    fixed = TRUE,
    "value must of length 1."
  )
})
test_that("ato_debug() errors when value is not logical", {
  expect_error(
    ato_debug(value = "TRUE"),
    fixed = TRUE,
    "value must be logical."
  )
})

test_that("ato_debug() prints correct message when TRUE", {
  expect_message(
    ato_debug(value = TRUE),
    "M: ATO debug mode activated."
  )
})
test_that("ato_debug() prints correct message when TRUE", {
  expect_message(
    ato_debug(value = FALSE),
    "M: ATO debug mode deactiviated."
  )
})
