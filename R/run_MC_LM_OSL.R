#' Run Monte-Carlo simulation for LM-OSL
#'
#' @param A \code{\link{numeric}}
#' @param rho \code{\link{numeric}}
#' @param times \code{\link{vector}} (with default)
#' @param clusters \code{\link{numeric}} (with default):
#' @param r \code{\link{numeric}} (with default):
#' @param delta.r \code{\link{numeric}} (with default):
#' @param N_e \code{\link{numeric}} (with default):
#' @param method \code{\link{character}} (with default):
#' @param output \code{\link{character}} (with default):
#' @param \dots further arguments
#'
#' @return This function returns a list.
#'
#' @section Function version: 0.0.1 [2017-01-27]
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany)
#'
#' @references
#' Pagonis 2017
#' @export
run_MC_LM_OSL <- function(
  A,
  rho,
  times,
  clusters = 10,
  r = NULL,
  delta.r = 0.1,
  N_e = 200,
  method = "par",
  output = "signal",
  ...){

  if(is.null(r)) r <- seq(from = 0, to = 2, by = 0.1)

  ## register backend ----
  cores <- detectCores()
  if(cores == 1) method <- "seq"

  if(method != "par"){

    cl <- parallel::makeCluster(1)
    doParallel::registerDoParallel(cl)
    on.exit(stopCluster(cl))

  } else {

    cl <- parallel::makeCluster(cores-1)
    doParallel::registerDoParallel(cl)
    on.exit(stopCluster(cl))
  }
  ## -----

  temp <- foreach(c = 1:clusters,
                  .packages = 'RLumCarlo',
                  .combine = 'comb_array',
                  .multicombine = TRUE) %dopar% {

      results <- RLumCarlo:::MC_C_LM_OSL(times = times,
                  N_e = N_e,
                  r = r,
                  rho = rho,
                  A = A)

      return(results[[output]])

  }  # end c-loop

  return(list(signal = temp,
              time = times))
}
