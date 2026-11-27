start_test()

# TESTS START HERE ----
# ---------------------

test_that(".sub_na() returns the proper value", {
  x <- c(1:10, NA)
  expect_equal(.sub_na(x, value = 11), c(1:11))
})

# TESTS END HERE ------
# ---------------------

end_test()
