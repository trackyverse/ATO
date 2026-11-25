start_test()

# # TESTS START HERE ----
# # ---------------------

test_that("check() errors when capture_datetime > release_date for data.frame", {
  example_ani <- ani(example_ato)

  # make capture date greater than release date
  example_ani$capture_datetime <- as.POSIXct(
    c("2026-05-03 12:00:00"),
    "UTC"
  )

  example_ani$release_datetime <- as.POSIXct(
    "2026-05-02 12:00:00",
    "UTC"
  )

  expect_error(
    check(object = example_ani, tbl = 'data.frame'),
    "capture_datetime for rows 1, 2, 3, 4, 5, and 55 others comes after release_datetime. Animals must be captured before they are released.",
    fixed = TRUE
  )
})

test_that("check() errors when capture_datetime > release_date for data.trable", {
  ato_table_type_global("data.table")

  ani_df <- as.data.frame(ani(example_ato))

  if ("data.table" %in% utils::installed.packages()) {
    ani_dt <- make_ani(
      animal = ani_df$animal,
      release_datetime = ani_df$release_datetime,
      capture_datetime = ani_df$capture_datetime,
      tbl = "data.table"
    )
  }

  example_ato_dt <- init_ato(ani = ani_dt)
  # make capture date greater than release date
  example_ani_dt <- ani(example_ato_dt)

  example_ani_dt$capture_datetime <- as.POSIXct(
    c("2026-05-03 12:00:00"),
    "UTC"
  )

  example_ani_dt$release_datetime <- as.POSIXct(
    "2026-05-02 12:00:00",
    "UTC"
  )

  # test for data.frame
  expect_error(
    check(object = example_ani_dt, tbl = "data.table"),
    "capture_datetime for rows 1, 2, 3, 4, 5, and 55 others comes after release_datetime. Animals must be captured before they are released.",
    fixed = TRUE
  )
})
test_that("check() errors when capture_datetime > release_date for tibble", {
  ato_table_type_global("tibble")

  ani_df <- as.data.frame(ani(example_ato))

  if ("tibble" %in% utils::installed.packages()) {
    ani_tb <- make_ani(
      animal = ani_df$animal,
      release_datetime = ani_df$release_datetime,
      capture_datetime = ani_df$capture_datetime,
      tbl = "tibble"
    )
  }

  example_ato_tb <- init_ato(ani = ani_tb)
  # make capture date greater than release date
  example_ani_tb <- ani(example_ato_tb)

  example_ani_tb$capture_datetime <- as.POSIXct(
    c("2026-05-03 12:00:00"),
    "UTC"
  )

  example_ani_tb$release_datetime <- as.POSIXct(
    "2026-05-02 12:00:00",
    "UTC"
  )

  # test for data.frame
  expect_error(
    check(object = example_ani_tb, tbl = "tibble"),
    "capture_datetime for rows 1, 2, 3, 4, 5, and 55 others comes after release_datetime. Animals must be captured before they are released.",
    fixed = TRUE
  )
})
# # TESTS END HERE ------
# # ---------------------

end_test()
