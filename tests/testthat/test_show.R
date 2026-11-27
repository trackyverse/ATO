start_test()

# # TESTS START HERE ----
# # ---------------------
test_that("show() displays the table type as data.frame", {
  expect_output(show(example_ato), "data.frame ATO object:")
})

test_that("show() displays @ani slot", {
  expect_output(show(example_ato), "@ani:")
})

test_that("show() displays empty @ani slot", {
  expect_output(show(init_ato()), "@ani slot is empty.\n")
})

test_that("show() displays empty @obs slot", {
  test_obs <- make_obs(
    datetime = rep(Sys.time(), 10),
    transmitter = c(paste0(LETTERS[1:5], "-", 1:5), rep(NA, 5)),
    animal = c(rep(NA, 5), letters[6:10]),
    terminal = rep(FALSE, 10),
    tz = "America/Halifax"
  )

  obs_ato <- init_ato(
    obs = test_obs
  )
  expect_output(show(obs_ato), "@obs:")
})

test_that("show() displays table type as data.table", {
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

  example_ato_dt <- init_ato(ani = ani_dt) |>
    suppressWarnings()
  expect_output(show(example_ato_dt), "data.table ATO object:")
})

test_that("show() displays table type as tibble", {
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

  example_ato_tb <- init_ato(ani = ani_tb) |>
    suppressWarnings()
  expect_output(show(example_ato_tb), "tibble ATO object:")
})

# # TESTS END HERE ------
# # ---------------------

end_test()
