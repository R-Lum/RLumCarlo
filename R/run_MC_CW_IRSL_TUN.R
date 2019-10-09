#' @title Run Monte-Carlo simulation for CW-IRSL
#'
#' @description Runs a Monte-Carlo (MC) simulation of constant wave infrared stimulated luminesence (CW-IRSL) using the model.
#'
#' @details
#'
#' ####equation here please####
#'
#'
#' @param A [numeric] (**required**): The transition probability (cm^3/s).
#'
#' @param rho [numeric] (**required**): The calculated dimesionless Charge density (normally written Rho').
#'
#' @param times [numeric] (*with default*): The number of MC runs.
#'
#' @param clusters [numeric] (*with default*): The number of clusters.
#'
#' @param r [numeric] (*with default*): The retrapping ratio.
#'
#' @param N_e [numeric] (*with default*): The number of electrons
#'
#' @param method [character] (*with default*):
#'
#' @param output [character] (*with default*):
#'
#' @param \dots further arguments
#'
#' @return This function returns a list.
#'
#' @section Function version: 0.2.0
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany), Sebastian Kreutzer, IRAMAT-CRP2A, Université Bordeaux Montaigne (France)
#'
#' @references
#'
#' Pagonis, V. and Kulp, C. (2017) ‘Monte Carlo simulations of tunneling phenomena and nearest neighbor hopping mechanism in feldspars’, Journal of Luminescence. Elsevier, 181, pp. 114–120. doi: 10.1016/j.jlumin.2016.09.014.
#'
#' @examples
#' \dontrun{
#'
#' ##============================================================================##
#' ## Example 1: Simulate CW-IRSL measurement
#' ##============================================================================##
#'
#' run_MC_CW_IRS_TUNL(A = 0.12, rho = 0.003, times = 0:1000) %>%
#'     plot_RLumCarlo(norm = T, legend = T)
#'}
#' @md
#' @export
run_MC_CW_IRSL_TUN <- function(
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

    results <- MC_C_CW_IRSL_TUN(
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


