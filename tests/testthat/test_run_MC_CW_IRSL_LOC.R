context("run_MC_CW_IRSL_LOC")

test_that("basic run", {
  testthat::skip_on_cran()
  
  ##break the function on purpose
  expect_error(run_MC_CW_IRSL_LOC(method = "error"), "Allowed keywords for 'method' are either 'par' or 'seq'!")
  expect_error(run_MC_CW_IRSL_LOC(output = "error"), "Allowed keywords for 'output' are either 'signal' or 'remaining_e'!")
  
  ## run function with basic setting
  ## sequential and multicore
  results_seq <- expect_silent(run_MC_CW_IRSL_LOC(
    A = 1e-3,
    times = 0:100,
    clusters = 1e1,
    n_filled = 1,
    r = 1e-7,
    method = "seq"
  ))
  
  results_par <- expect_silent(run_MC_CW_IRSL_LOC(
    A = 1e-3,
    times = 0:100,
    clusters = 1e1,
    n_filled = 1,
    r = 1e-7,
    method = "par"
  ))
  
  ## check output
  expect_s3_class(results_par, class = "RLumCarlo_Model_Output")
  expect_length(results_par, 2)
  expect_s3_class(results_seq, class = "RLumCarlo_Model_Output")
  expect_length(results_seq, 2)
  
})
