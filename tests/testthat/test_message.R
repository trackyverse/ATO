start_test()
# # TESTS START HERE ----
# # ---------------------

# ----- test .message_stray_dets() ------
test_that(".message_stray_dets() output is as expected when tag_match is NULL", {
  ato_msg_stray <- example_ato
  ato_msg_stray@det$tag_match <- NULL

  # check messgage when tag_match is null
  expect_message(
    .message_stray_dets(x = ato_msg_stray),
    "M: 14544 valid detections \\(from 54 transmitters\\) do not match transmitters listed in @dep, or fall outside their active times, but @tag not yet matched."
  )

  # check val
  val <- .message_stray_dets(x = ato_msg_stray)
  expect_equal(val, 14544)
})
# ------ end of test for .message_stray_dets -----
# ----- start to test  .dyn_round() ------

test_that('.dyn_round rounds properly', {
  val <- .dyn_round(1.323)
  expect_equal(val, 1.32)
})

test_that('.dyn_round rounds properly when 0 and max = 10', {
  val <- .dyn_round(0)
  expect_equal(val, "<0.0000000001")
})

test_that('.dyn_round rounds properly when 0 and max = 1', {
  val <- .dyn_round(0, max = 1)
  expect_equal(val, "<0.1")
})

# ------ end of test for .dyn_round() -----
# ----- test .comma() ------
test_that(".comma() returns the object when length is 1", {
  out <- .comma(LETTERS[1])
  expect_equal(out, "A")
  expect_equal(length(out), 1)
})

test_that(".comma() returns the object when length is = 2", {
  out <- .comma(LETTERS[1:2])
  expect_equal(out, c("A and B"))
  expect_equal(length(out), 1)
})

test_that(".comma() returns the object when length is > 2", {
  out <- .comma(LETTERS[1:3])
  expect_equal(out, c("A, B, and C"))
  expect_equal(length(out), 1)
})

test_that(".comma() returns the object when length is > 2", {
  out <- .comma(LETTERS[1:3], max_i = 2)
  expect_equal(out, c("A, B, and 1 other"))
  expect_equal(length(out), 1)
})

test_that(".comma() returns the object when length is > 2", {
  out <- .comma(LETTERS[1:4], max_i = 2)
  expect_equal(out, c("A, B, and 2 others"))
  expect_equal(length(out), 1)
})

test_that(".comma() returns the object when length is > 2", {
  out <- .comma(LETTERS[1:4], max_i = 1)
  expect_equal(out, c("A and 3 others"))
  expect_equal(length(out), 1)
})

test_that(".comma() returns the object when length is > 2", {
  out <- .comma(LETTERS[1:2], max_i = 1)
  expect_equal(out, c("A and 1 other"))
  expect_equal(length(out), 1)
})

test_that(".comma() returns the object when length is > 2", {
  out <- .comma(LETTERS[1], max_i = 1)
  expect_equal(out, c("A"))
  expect_equal(length(out), 1)
})
# ------ end of test for .comma -----
# ------ start of next .has() test -----
test_that(".has() returns has when n = 1", {
  expect_equal(.has(1), "has")
})
test_that(".has() returns have when n = 2", {
  expect_equal(.has(2), "have")
})
# ------ end of test for .has -----
# ------ start of next .was() test -----
test_that(".was() returns was when n = 1", {
  expect_equal(.was(1), "was")
})
test_that(".was() returns were when n = 2", {
  expect_equal(.was(2), "were")
})

# ------ end of test for .was -----
# ------ start of next .is() test -----
test_that(".is() returns is when n = 1", {
  expect_equal(.is(1), "is")
})
test_that("is.() returns are when n = 2", {
  expect_equal(.is(2), "are")
})

# ------ end of test for .is -----
# ------ start of next .s() test -----
test_that(".s() returns '' when reverse = FALSE when n = 1", {
  expect_equal(.s(1), "")
})
test_that(".s() returns s when reverse = FALSE when n = 2", {
  expect_equal(.s(2), "s")
})
test_that(".s() returns is '' when reverse = TRUE when n = 1", {
  expect_equal(.s(1, reverse = TRUE), "s")
})
test_that(".s() returns is '' when resverse = TRUE when n = 1", {
  expect_equal(.s(2, reverse = TRUE), "")
})
# ------ end of test for .s -----
# ------ start of next .es() test -----
test_that(".es() returns '' when reverse = FALSE when n = 1", {
  expect_equal(.es(1), "")
})
test_that(".es() returns es when reverse = FALSE when n = 2", {
  expect_equal(.es(2), "es")
})
test_that(".es() returns is 'es' when reverse = TRUE when n = 1", {
  expect_equal(.es(1, reverse = TRUE), "es")
})
test_that(".es() returns is '' when resverse = TRUE when n = 1", {
  expect_equal(.es(2, reverse = TRUE), "")
})
# ------ end of test for .es -----
# ------ start of next .y() test -----
test_that(".y() returns 'y' when n = 1", {
  expect_equal(.y(1), "y")
})
test_that(".y() returns 'ies' when n = 2", {
  expect_equal(.y(2), "ies")
})

# # TESTS END HERE ------
# # ---------------------

end_test()
