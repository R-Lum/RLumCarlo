context("run_MC_CW_IRSL_TUN")

test_that("basic run", {
  testthat::skip_on_cran()
  
  ##break the function on purpose
  expect_error(run_MC_CW_IRSL_TUN(method = "error"), "Allowed keywords for 'method' are either 'par' or 'seq'!")
  expect_error(run_MC_CW_IRSL_TUN(output = "error"), "Allowed keywords for 'output' are either 'signal' or 'remaining_e'!")
  
  ## run function with basic setting
  ## sequential and multicore
  results_seq <- expect_silent(run_MC_CW_IRSL_TUN(
    A = 1e-3,
    rho = 1e-7,
    times = 0:100,
    clusters = 1,
    N_e = 2,
    r_c = 1,
    delta.r = 1e-3,
    method = "seq"
  ))
  
  results_par <- expect_silent(run_MC_CW_IRSL_TUN(
    A = 1e-3,
    rho = 1e-7,
    times = 0:100,
    clusters = 1,
    N_e = 2,
    r_c = 1,
    delta.r = 1e-3,
    method = "par"
  ))
  
  ## check output
  expect_s3_class(results_par, class = "RLumCarlo_Model_Output")
  expect_length(results_par, 2)
  expect_s3_class(results_seq, class = "RLumCarlo_Model_Output")
  expect_length(results_seq, 2)
  
})