context("utils")

test_that("basic run", {
  testthat::skip_on_cran()

  ## .registerClusters
  cl <- expect_silent(.registerClusters(method = "par", cores = NULL, FALSE))
  parallel::stopCluster(cl)


})

