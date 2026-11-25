
start_test <- function() {
  # ----- reset ato table type globally to data.frame ----- 
  ato_table_type_global("data.frame")

  # ----- set timezone -----
  oldtz <- Sys.getenv('TZ', unset = NA)
  Sys.setenv(TEST_OLD_TZ = oldtz)
  Sys.setenv(TZ = 'UTC')
  # ----- set directory ---
  tests.home <- getwd()
  Sys.setenv(TEST_HOME_WD = tests.home)
  setwd(tempdir())
}

end_test <- function() {
  # reset to data.frame so the rest of the tests pass
  ato_table_type_global("data.frame")

  # reset timezone
  oldtz <- Sys.getenv('TEST_OLD_TZ')
  if (is.na(oldtz)) {
    Sys.unsetenv("TZ")
  } else {
    Sys.setenv(TZ = oldtz)
  }
  Sys.unsetenv("TEST_OLD_TZ")

  # go back home
  tests.home <- Sys.getenv('TEST_HOME_WD')
  setwd(tests.home)
  Sys.unsetenv("TEST_HOME_WD")
}
