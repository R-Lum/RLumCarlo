#' Run Monte-Carlo simulation for CW-IRSL
#'
#' @param A \code{\link{numeric}}
#' @param rho \code{\link{numeric}}
#' @param times \code{\link{vector}} (with default)
#' @param clusters \code{\link{numeric}} (with default):
#' @param r \code{\link{numeric}} (with default)
#' @param N_e \code{\link{numeric}} (with default):
#' @param method \code{\link{character}} (with default):
#' @param output \code{\link{character}} (with default):
#' @param \dots further arguments
#'
#' @return This function returns a list.
#'
#' @section Function version: 0.2.0
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany), Sebastian Kreutzer, IRAMAT-CRP2A, Université Bordeaux Montaigne (France)
#'
#' @references
#' Pagonis 2017
#'
#' @examples
#' \dontrun{
#'
#' ##============================================================================##
#' ## Example 1: Simulate CW-IRSL measurement
#' ##============================================================================##
#'
#' run_MC_CW_IRSL(A = 0.12, rho = 0.003, times = 0:1000) %>%
#'   calc_RLumCarlo() %>%
#'     plot_RLumCarlo(norm = T, legend = T)
#'}
#' @md
#' @export
run_MC_CW_IRSL <- function(
  A,
  rho,
  times,
  clusters = 10,
  r = NULL,
  N_e = 200,
  method = "seq",
  output = "signal",
  ...){


  ## register backend ----
  cores <- parallel::detectCores()
  if(cores == 1) method <- "seq"

  if(method != "par"){

    cl <- parallel::makeCluster(1)
    doParallel::registerDoParallel(cl)

    ##ensures that we do not have any particular problems
    registerDoSEQ()
    on.exit(parallel::stopCluster(cl))

  } else {
    cl <- parallel::makeCluster(cores-1)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))
  }

  ## -----

 if(is.null(r)) r <- seq(from = 0, to = 2, by = 0.1)

  temp <- foreach(
    c = 1:clusters,
    .packages = 'RLumCarlo',
    .combine = 'comb_array',
    .multicombine = TRUE) %dopar% {

    results <- MC_C_CW_IRSL(
        times = times,
        N_e = N_e,
        r = r,
        rho = rho,
        A = A
    )

   return(results[[output]])

  }  # end c-loop

  ## return model output
  .return_ModelOutput(signal = temp, time = times)

}


