skip_on_cran()
oldtz <- Sys.getenv('TZ', unset = NA)
Sys.setenv(TZ = 'UTC')

tests.home <- getwd()
setwd(tempdir())

# # TESTS START HERE ----
# # ---------------------

test_that("init_ato makes an ATO identical to example_ato", {
  # x <- init_ato(
  #   ani = ani(example_ato),
  #   dep = dep(example_ato),
  #   det = det(example_ato),
  #   tag = tag(example_ato)
  # )
  # a <- table(det(example_ato)$receiver_serial)[dep(x)$receiver_serial]
  # a[1] <- 0
  # expect_equal(
  #   as.vector(a),
  #   as.vector(dep(x)$n_det)
  # )

  # expect_equal(ani(x), ani(example_ato))
  # expect_equal(dep(x), dep(example_ato))
  # expect_equal(det(x), det(example_ato))
  # expect_equal(tag(x), tag(example_ato))
})


test_that("init_ato handles observations", {
  # o <- make_obs(
  #   datetime = Sys.time(),
  #   animal = ani(example_ato)$animal[2],
  #   terminal = TRUE,
  #   tz = tzone(example_ato)
  # )

  # x <- init_ato(
  #   ani = ani(example_ato),
  #   dep = dep(example_ato),
  #   det = det(example_ato),
  #   obs = o,
  #   tag = tag(example_ato)
  # )
  # ani(x)
  # ani(example_ato)
  # expect_equal(obs(x)[, 1:9], o)
})

# # TESTS END HERE ------
# # ---------------------

setwd(tests.home)

if (is.na(oldtz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = oldtz)

rm(list = ls())
