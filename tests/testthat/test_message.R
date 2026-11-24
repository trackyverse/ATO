# # TESTS START HERE ----
# # ---------------------

# ----- test .comma() ------
test_that("check that .comma() returns the object when length is 1", {
  out <- .comma(LETTERS[1])
  expect_equal(out, "A")
  expect_equal(length(out), 1)
})

test_that("check that .comma() returns the object when length is = 2", {
  out <- .comma(LETTERS[1:2])
  expect_equal(out, c("A and B"))
  expect_equal(length(out), 1)
})

test_that("check that .comma() returns the object when length is > 2", {
  out <- .comma(LETTERS[1:3])
  expect_equal(out, c("A, B, and C"))
  expect_equal(length(out), 1)
})

test_that("check that .comma() returns the object when length is > 2", {
  out <- .comma(LETTERS[1:3], max_i = 2)
  expect_equal(out, c("A, B, and 1 other"))
  expect_equal(length(out), 1)
})

test_that("check that .comma() returns the object when length is > 2", {
  out <- .comma(LETTERS[1:4], max_i = 2)
  expect_equal(out, c("A, B, and 2 others"))
  expect_equal(length(out), 1)
})

test_that("check that .comma() returns the object when length is > 2", {
  out <- .comma(LETTERS[1:4], max_i = 1)
  expect_equal(out, c("A and 3 others"))
  expect_equal(length(out), 1)
})

test_that("check that .comma() returns the object when length is > 2", {
  out <- .comma(LETTERS[1:2], max_i = 1)
  expect_equal(out, c("A and 1 other"))
  expect_equal(length(out), 1)
})

test_that("check that .comma() returns the object when length is > 2", {
  out <- .comma(LETTERS[1], max_i = 1)
  expect_equal(out, c("A"))
  expect_equal(length(out), 1)
})
# ------ end of test for .comma -----
# ------ start of next message_function test -----

# # TESTS END HERE ------
# # ---------------------

setwd(tests.home)

if (is.na(oldtz)) {
  Sys.unsetenv("TZ")
} else {
  Sys.setenv(TZ = oldtz)
}

rm(list = ls())
