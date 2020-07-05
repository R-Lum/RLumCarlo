context("benchmark models")

test_that("basic run", {
  testthat::skip_on_cran()

  set.seed(48)
  ## complex test after wrong calculation results
  ## RLumCarlo
  TL110 <- RLumCarlo::run_MC_TL_DELOC(
    s = 5e12, E = 0.97, R = 5e-10, times = seq(20,400,2),
    N_e = output$`conc. level 1 (TL)`[1,2]/1e5, method = "seq")

  TL230 <- RLumCarlo::run_MC_TL_DELOC(
    s = 5e14, E = 1.55, R = 5e-10, times = seq(20,400,2),
    N_e = output$`conc. level 2 (TL)`[1,2]/1e5, method = "seq")

  TL325 <- RLumCarlo::run_MC_TL_DELOC(
    s = 5e13, E = 1.7, R = 5e-10, times = seq(20,400,2),
    N_e = output$`conc. level 3 (TL)`[1,2]/1e5, method = "seq")

  ## combine
  object <- c(TL110, TL230, TL325)

  ##test result
  expect_equal(round(sum(object$signal), digits = 0),10141)


})
