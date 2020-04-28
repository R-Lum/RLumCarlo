context("utils")

test_that("basic run", {
  testthat::skip_on_cran()

  ## .registerClusters ... cores and verbose
  cl <- expect_type(.registerClusters(method = "par", cores = NULL, verbose = TRUE), "list")
  parallel::stopCluster(cl)


})

