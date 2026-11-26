start_test()

# # TESTS START HERE ----
# # --------------------
# built by MOB
test_that("global table types are set", {
  initial_table_type <- options()$ATO_table_type

  # ATO object is created as data.frame
  ato_table_type_global("data.frame") |>
    expect_message("M: Newly created ATO objects will be of type data.frame")

  ato_df <- init_ato()

  expect_type(ato_df, "S4")
  expect_s4_class(ato_df, "ATO")

  expect_s3_class(ato_df@det, c("ATO_det", "data.frame"), exact = TRUE)

  # ATO object is created as data.table
  ato_table_type_global("data.table") |>
    expect_message("M: Newly created ATO objects will be of type data.table")

  ato_dt <- init_ato()

  expect_type(ato_dt, "S4")
  expect_s4_class(ato_dt, "ATO")

  expect_s3_class(
    ato_dt@det,
    c("ATO_det", "data.table", "data.frame"),
    exact = TRUE
  )

  # ATO object is created as tibble
  ato_table_type_global("tibble") |>
    expect_message("M: Newly created ATO objects will be of type tibble")
  ato_tbl <- init_ato()

  expect_type(ato_tbl, "S4")
  expect_s4_class(ato_tbl, "ATO")

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
      "\'arg\' should be one of .data.frame., .data.table., .tibble."
    )
})

# # TESTS END HERE ------
# # ---------------------

end_test()
