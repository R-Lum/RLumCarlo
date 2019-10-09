context("run_MC_TL_TUN")

test_that("basic run", {
  testthat::skip_on_cran()

  ## run function with basic setting
  results <- run_MC_TL_TUN(
    s = 3.5e12,
    E = 1.45,
    rho = 0.015,
    r_c = 0.85,
    times = 200:500
  )

  ## check output
  expect_s3_class(results, class = "RLumCarlo_Model_Output")
  expect_length(results, 2)

})
