context("create_ClusterSystem")

test_that("basic run", {
  testthat::skip_on_cran()

  ##create a simple cluster system and plot
  expect_s3_class(create_ClusterSystem(plot = TRUE), class = "RLumCarlo_ClusterSystem")


})

