test_that("test internal functions", {
  testthat::skip_on_cran()
  local_edition(3)

  ## crash the function
  ## times not long enough
  ## we skip this test on macOS, R 3.6 though, there is somewhat a problem
  ## related to C++ and testthat
  i <- utils::sessionInfo()
  if (!(grepl("darwin", i$platform) & as.numeric(i$R.version$major) < 4)) {
    expect_error(run_MC_CW_OSL_DELOC(
      A = 0.12,
      R = 0.1,
      times = 1,
      clusters = 10,
      method = "seq"),
      regexp = "task 1 failed - \"\\[RLumCarlo Internal Error\\] The length of times cannot be smaller than 2!\"")

    ## times non-equidistant
    expect_error(run_MC_CW_OSL_DELOC(
      A = 0.12,
      R = 0.1,
      times = c(0,1,10,2,1),
      clusters = 10,
      method = "seq"),
      regexp = "task 1 failed - \"\\[RLumCarlo Internal Error\\] Non-equidistant elements in times are not supported!\"")
  }

})

