start_test()

# # TESTS START HERE ----
# # ---------------------

test_that("summary() displays the correct info in @ani slot", {
  an <- make_ani(
    animal = ani(example_ato)$animal[2:6],
    release_datetime = ani(example_ato)$release_datetime[1:5]
  )
  ta <- make_tag(
    transmitter = tag(example_ato)$transmitter[1:5],
    animal = ani(example_ato)$animal[1:5]
  )

  x <- init_ato(
    ani = an,
    tag = ta
  )

  test_ani <- get_ani(x)
  # expect_output(summary(test_ani), " - 1 without associated tags")
})


test_that("summary() displays the correct info in @dep slot", {
  depl <- get_dep(example_ato)
})
test_that("summary() displays the correct info in @det slot", {
  dets <- get_det(example_ato)
})
# # TESTS END HERE ------
# # ---------------------

end_test()
