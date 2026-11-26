test_that("global table types are set", {
  initial_table_type <- options()$ATO_table_type

  # ATO object is created as data.frame
  ato_table_type_global("data.frame") |>
    expect_message("M: Newly created ATOs will have tables of type data.frame")

  ato_df <- init_ato()

  expect_type(ato_df, "S4")
  expect_s4_class(ato_df, "ATO")

  expect_true(ato_df@tbl == "data.frame")
  expect_s3_class(ato_df@tbl, c("ATO_tbl", "character"), exact = TRUE)

  expect_s3_class(ato_df@det, c("ATO_det", "data.frame"), exact = TRUE)

  # ATO object is created as data.table
  ato_table_type_global("data.table") |>
    expect_message("M: Newly created ATOs will have tables of type data.table")

  ato_dt <- init_ato()

  expect_type(ato_dt, "S4")
  expect_s4_class(ato_dt, "ATO")

  expect_true(ato_dt@tbl == "data.table")
  expect_s3_class(ato_dt@tbl, c("ATO_tbl", "character"), exact = TRUE)

  expect_s3_class(
    ato_dt@det,
    c("ATO_det", "data.table", "data.frame"),
    exact = TRUE
  )

  # ATO object is created as tibble
  ato_table_type_global("tibble") |>
    expect_message("M: Newly created ATOs will have tables of type tibble")
  ato_tbl <- init_ato()

  expect_type(ato_tbl, "S4")
  expect_s4_class(ato_tbl, "ATO")

  expect_true(ato_tbl@tbl == "tibble")
  expect_s3_class(ato_tbl@tbl, c("ATO_tbl", "character"), exact = TRUE)

  expect_s3_class(
    ato_tbl@det,
    c("ATO_det", "tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )
  # Reset current option
  options("ATO_table_type" = initial_table_type)
})


test_that("throws error if not data.frame, data.table, or tibble", {
  ato_table_type_global("tso") |>
    expect_error(
      "\'arg\' should be one of \"data.frame\", \"data.table\", \"tibble\""
    )
})
