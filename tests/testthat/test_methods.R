context("methods")

test_that("basic run", {
  testthat::skip_on_cran()

  ## summary
  expect_is(summary(run_MC_TL_TUN(
    s = 3.5e12,
    E = 1.45,
    rho = 0.015,
    r_c = 0.5,
    times = 100:500,
    method = "seq"
  )), "data.frame")

  expect_is(summary(run_MC_TL_TUN(
    s = 3.5e12,
    E = 1.45,
    rho = 0.015,
    r_c = 0.5,
    times = 100:110,
    clusters = 2,
    method = "seq"
  )), "data.frame")


  ## check object dimension 2 (one cluster)
  expect_is(summary(run_MC_TL_TUN(
    s = 3.5e12,
    E = 1.45,
    rho = 0.015,
    r_c = 0.5,
    times = 100:110,
    clusters = 1,
    method = "seq"
  )), "data.frame")

})

