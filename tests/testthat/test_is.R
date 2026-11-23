
test_that("does is_ato() error when given a non-ato object", {
  x <- data.frame(
    y = seq(1, 10, 1), 
    z = rep(letters[1:2], 5)
  )

  expect_error(
    is_ato(x), 
    "input is not an ATO object.",
    fixed = TRUE
  )
})
test_that("does is_ato() not error when error = FALSE when given a non-ato object", {
  x <- data.frame(
    y = seq(1, 10, 1), 
    z = rep(letters[1:2], 5)
  )

  expect_false( is_ato(x, error = FALSE))
  expect_no_error(is_ato(x, error = FALSE))
})

test_that("does is_ato() does not error when given an ATO object", {
  ob <- make_obs(
    datetime = Sys.time(),
    animal = ani(example_ato)$animal[2],
    terminal = TRUE,
    tz = tzone(example_ato)
  )
  an <- make_ani(
    animal = ani(example_ato)$animal[1:5],
    release_datetime = ani(example_ato)$release_datetime[1:5]
  )

  x <- init_ato(
    ani = an,
    obs = ob
  )
  
  expect_true(is_ato(x))
})