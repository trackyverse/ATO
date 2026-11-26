# start_test()

# # TESTS START HERE ----
# # ---------------------

test_that("summary() displays the correct info in @ani slot", {
  # BH: These could be moved to test_summary.R as summary() is called
  # here and produces this information.
  expect_output(summary(get_ani(example_ato)), " - 60 animals in total")
  expect_output(summary(get_ani(example_ato)), " - All with associated tags")
  expect_output(summary(get_ani(example_ato)), " - 6 with no detection")
})

# # TESTS END HERE ------
# # ---------------------

end_test()
