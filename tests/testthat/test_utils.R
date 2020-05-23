context("utils")

test_that("basic run", {
  testthat::skip_on_cran()

  ## .registerClusters ... cores and verbose
  cl <- expect_type(.registerClusters(method = "par", cores = NULL, verbose = TRUE), "list")
  parallel::stopCluster(cl)

  ## .distribute_electrons
  expect_is(
    .distribute_electrons(clusters = create_ClusterSystem(n = 10), N_system = 100),
    'data.frame'
  )

})

