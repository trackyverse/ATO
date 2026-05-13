# skip_on_cran()
# oldtz <- Sys.getenv('TZ', unset = NA)
# Sys.setenv(TZ = 'UTC')

# tests.home <- getwd()
# setwd(tempdir())

# # TESTS START HERE ----
# # ---------------------

# test_that("what's being tested?", {
# 	expect_equal(a, b)
# 	expect_message(code, "warning message", fixed = TRUE)
# 	expect_warning(code, "error message", fixed = TRUE)
# 	expect_error(code, "error message", fixed = TRUE)
# })

# # TESTS END HERE ------
# # ---------------------

# setwd(tests.home)

# if (is.na(oldtz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = oldtz)

# rm(list = ls())
