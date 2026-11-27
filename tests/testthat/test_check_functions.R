start_test()

# TESTS START HERE ----
# ---------------------
# ----- start test .check_data.table_exist() -------
test_that(".check_data.table_exists() errors when data.table isn't installed", {
  testthat::local_mocked_bindings(
    installed.packages = function(...) {
      matrix("base", 1, 1, dimnames = list("base", "Package"))
    },
    .package = "utils"
  )
  expect_error(
    .check_data.table_exists(),
    "You must install package data.table to run this function.",
    fixed = TRUE
  )
  expect_false(
    .check_data.table_exists(FALSE)
  )
})

test_that(".check_data.table_exists() returns TRUE when data.table is installed", {
  expect_true(
    .check_data.table_exists()
  )
})
# ----- end test .check_data.table_exixts() -------
# ----- start test .check_tibble_exixts() -------
test_that(".check_tibble_exists() errors when tibble isn't installed", {
  testthat::local_mocked_bindings(
    installed.packages = function(...) {
      matrix("base", 1, 1, dimnames = list("base", "Package"))
    },
    .package = "utils"
  )
  expect_error(
    .check_tibble_exists(),
    "You must install package tibble to run this function.",
    fixed = TRUE
  )
  expect_false(
    .check_tibble_exists(FALSE)
  )
})

test_that(".check_tibble_exists() returns TRUE when tibble is installed", {
  expect_true(
    .check_tibble_exists()
  )
})
# ----- end test .check_tibble_exixts() -------
# ----- start test .check_ato_table_cols() -------
test_that(".check_ato_table_cols() errors when columns in object don't meet reference", {
  ani <- example_ato@ani
  ref <- colnames(ani)

  ani$n_det <- NULL

  expect_error(
    .check_ato_table_cols(object = ani, ref = ani(example_ato)),
    "The ATO_ani is missing the following standard columns: n_det",
    fixed = TRUE
  )
})

test_that(".check_ato_table_cols() errors when columns in object don't meet expected class", {
  ani <- example_ato@ani
  ref <- colnames(ani)

  ani$n_det <- "1"

  expect_error(
    .check_ato_table_cols(object = ani, ref = ani(example_ato)),
    "The following columns of the ATO_ani are not of the required class: \n - n_det is not numeric",
    fixed = TRUE
  )
})
# ----- end test .check_ato_table_cols() -------
# ----- start test .check_column_tzones() -------
test_that(".check_column_tzones() errors when two columns have differing tz when tz arg isn't supplied", {
  x <- data.frame(
    start_time = as.POSIXct("2026-11-26 22:45:00", tz = "UTC"),
    end_time = as.POSIXct("2026-11-26 22:45:00", tz = "Europe/Copenhagen")
  )

  expect_error(
    .check_column_tzones(object = x),
    "The POSIXt columns start_time and end_time aren't all in the same timezone (UTC and Europe/Copenhagen). Use argument tz to convert all POSIXt columns to the same timezone.",
    fixed = TRUE
  )
})
# ----- end test .check_column_tzones() -------
# ----- start test .check_ato_tzones() -------
test_that(".check_ato_tzones() errors when two ATO slots have differing tz", {
  # attr(example_ato@ani$release_datetime, "tzone") <- "America/New_York"
  attr(example_ato@dep$deploy_datetime, "tzone") <- "American/Chicago"
  attr(example_ato@dep$recover_datetime, "tzone") <- "American/Chicago"
  # attr(example_ato@dep$, "tzone") <- "American/Chicago"

  expect_error(
    .check_ato_tzones(example_ato),
    "Not all slots are in the same timezone (see below). See ?tzone() for assistance correcting this.\n@ani: Europe/Copenhagen\n@dep: American/Chicago\n@det: Europe/Copenhagen\n@tag: Europe/Copenhagen",
    fixed = TRUE
  )
})
# ----- end test .check_ato_tzones() -------
# ----- start test .check_slot_tzones() -------
test_that(".check_slot_tzones() errors when two columns ATO slots has differing tz", {
  attr(example_ato@ani$release_datetime, "tzone") <- "American/Chicago"

  expect_error(
    .check_slot_tzones(x = example_ato@ani, s = "ani"),
    "@ani POSIXt columns release_datetime and capture_datetime aren't all in the same timezone (American/Chicago and Europe/Copenhagen). This should never happen... Did you use make_ani() to make this table?.",
    fixed = TRUE
  )
})
test_that(".check_slot_tzones() errors when ATO slots has tz of ''", {
  attr(example_ato@ani$release_datetime, "tzone") <- ""

  expect_error(
    .check_slot_tzones(x = example_ato@ani, s = "ani"),
    "@ani POSIXt data in column release_datetime is missing timezone information. This should never happen... Did you use make_ani() to make this table?.",
    fixed = TRUE
  )
})

test_that(".check_slot_tzones() returns null when slot doesn't have any posixct values ", {
  example_ato@ani$release_datetime <- as.character(
    example_ato@ani$release_datetime
  )
  example_ato@ani$capture_datetime <- as.character(
    example_ato@ani$capture_datetime
  )

  expect_null(
    .check_slot_tzones(x = example_ato@ani, s = "ani")
  )
})
# ----- end test .check_slot_tzones() -------
# ----- start test .check_tbl_argument() -------
test_that(".check_tbl_argument() errors when tbl isn't data.frame, data.table, or tibble", {
  expect_error(
    .check_tbl_argument(tbl = "t"),
    "tbl was defined but value not recognized. Value must be one of 'data.frame', 'data.table', or 'tibble'. See ?table_type.",
    fixed = TRUE
  )
})
# ----- end test .check_tbl_argument() -------
# ----- start test .check_tag_beacon() -------
test_that(".check_tag_beacon() errors when tag_match or beacon_match is not NA", {
  example_ato@det$beacon_match <- 1

  expect_error(
    .check_tag_beacon(example_ato),
    "@det rows 1, 2, 3, 4, 5, and 14539 others match both a row in @tag and a row in @dep. Fatal ambiguity. Can't assign detections correctly.",
    fixed = TRUE
  )
})
# ----- end test .check_tag_beacon() -------
# ----- start test .check_dup_match_base() -------
test_that(".check_dup_match_base() if it errors properly", {
  transmitter <- "R64K-4549"
  first_time <- min(example_ato@det$datetime) + 1
  last_time <- max(example_ato@det$datetime) + 1

  link <- example_ato@det$transmitter == transmitter &
    example_ato@det$datetime >= first_time &
    example_ato@det$datetime <= last_time

  expect_error(
    .check_dup_match_base(
      x = example_ato@det$tag_match[link],
      x_index = which(link),
      i = transmitter,
      label_x = "det",
      label_y = "tag"
    ),
    "@det rows 4732, 4734, 4735, 4736, 4737, and 91 others match @tag rows 60 and R64K-4549. Fatal ambiguity. Can't assign detections correctly.",
    fixed = TRUE
  )
})
# ----- end test .check_dup_match_base() -------
# ----- start test .check_dup_match_datatable() -------
test_that("..check_dup_match_datatable() errors properly", {
  x <- c(1, 1, 1, 2, 3, 4)
  y <- c(1, 2, 4, 1, 3, 1)
  label_x <- "det"
  label_y <- "tag"
  original_x <- c(4, 3, 2, 1)
  original_y <- c(1, 3, 2, 10)
  # i.e. the temporary detection row 1 matches temporary tag rows 1, 2, 4.
  # which trace back to detection 4 matching tags 1, 3, and 10 in the original
  # row orders.

  expect_error(
    .check_dup_match_datatable(
      x = x,
      y = y,
      original_x = original_x,
      original_y = original_y,
      label_x = label_x,
      label_y = label_y
    ),
    "@det row 4 matches multiple @tag rows (1, 3, and 10). Fatal ambiguity. Can't assign detections correctly.",
    fixed = TRUE
  )
})

# TESTS END HERE ------
# ---------------------

end_test()
