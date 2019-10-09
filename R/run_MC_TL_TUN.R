#' @title Run Monte-Carlo simulation for TL
#'
#' @description
#'
#' @details
#'
#'  ADD EQUATION
#'
#' @param s [list] (**required**): Escape frequency of the trap (s^-1).
#'
#' @param E [numeric] (**required**): Thermal activation energy of the trap (eV).
#'
#' @param rho [numeric] (**required**): The calculated dimesionless Charge density (normally written Rho').
#'
#' @param r_c [numeric] (*with default*):
#'
#' @param times [vector] (*with default*): The number of MC runs.
#'
#' @param clusters  [numeric] (*with default*): The number of clusters.
#'
#' @param N_e [numeric] (*with default*): The number of electrons
#'
#' @param delta.r [numeric] (*with default*):
#'
#' @param method [character] (*with default*):
#'
#' @param output [character] (*with default*):
#'
#' @param \dots further arguments
#'
#' @return This function returns an \code{\link{array}} with dimension length(times) x length(r) x clusters
#'
#' @section Function version: 0.1.0
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany)
#'
#' @references
#'
#' Pagonis, V. and Kulp, C. (2017) ‘Monte Carlo simulations of tunneling phenomena and nearest neighbor hopping mechanism in feldspars’, Journal of Luminescence. Elsevier, 181, pp. 114–120. doi: 10.1016/j.jlumin.2016.09.014.
#'
#' @examples
#' \dontrun{
#' ##============================================================================##
#' ## Example 1: Simulate TL measurement
#' ##============================================================================##
#'
#' times <- seq(200, 500) # time = temperature
#'
#' run_MC_TL_TUN(s = 3.5e12,
#'           E = 1.45,
#'           rho = 0.015,
#'           r_c = 0.85,
#'           times = times) %>%
#'     plot_RLumCarlo(legend = T)
#'}
#' @md
#' @export
run_MC_TL_TUN <- function(
  s,
  E,
  rho,
  r_c,
  times,
  clusters = 10,
  N_e = 200,
  delta.r = 0.1,
  method = "par",
  output = "signal",
  ...){

  r <- seq(r_c, 2, delta.r)

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

  if(is.null(r)) r <- seq(from = 0, to = 2, by = 0.1)

  temp <- foreach(c = 1:clusters,
                  .packages = 'RLumCarlo',
                  .combine = 'comb_array',
                  .multicombine = TRUE) %dopar% {

    results <- MC_C_TL_TUN(times = times,
            N_e = N_e,
            r = r,
            rho = rho,
            E = E,
            s = s)

    return(results[[output]])

  }  # end c-loop

  ## standard return
  .return_ModelOutput(signal = temp, time = times)
}
