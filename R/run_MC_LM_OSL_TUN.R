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
#' @section Function version: 0.1.0
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany)
#'
#' @references
#' Pagonis 2017
#'
#' @examples
#'
#'\dontrun{
#'
#'##TODO: Primary example, should be verified
#'run_MC_LM_OSL_TUN(A = 10000, rho = 0.0001, times = 1:100, clusters = 10, r = NULL,
#'  delta.r = 0.1,
#'  N_e = 200, method = "par", output = "signal") %>%
#'  plot_RLumCarlo(norm = T)
#'
#'}
#'
#' @md
#' @export
run_MC_LM_OSL_TUN <- function(
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
    ##ensures that we do not have any particular problems
    registerDoSEQ()
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

      results <- MC_C_LM_OSL_TUN(times = times,
                  N_e = N_e,
                  r = r,
                  rho = rho,
                  A = A)

      return(results[[output]])

  }  # end c-loop

  ## return model output
  .return_ModelOutput(signal = temp, time = times)
}
