start_test()

# # TESTS START HERE ----
# # ---------------------

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
# ----- end: ato_debug() tests -------
# ----- start: ato_debug_header() tests -----
test_that("ato_debug_header() prints correct info when debug is turned on", {
  ato_debug(TRUE)
  x <- function(arg = 1) {
    ato_debug_header()
    return(arg * 2)
  }

  expect_message(
    x(),
    paste0("Debug: Starting x (", round(Sys.time()), ")"),
    fixed = TRUE
  )
  expect_equal(suppressMessages(x()), 2)
})

test_that("ato_debug_header() prints correct info when debug is turned of", {
  ato_debug(FALSE)
  x <- function(arg = 1) {
    ato_debug_header()
    return(arg * 2)
  }
  expect_no_message(x())
})
# ----- end: ato_debug_header()
# # TESTS END HERE ------
# # ---------------------

end_test()
