#' @title Run Monte-Carlo Simulation for TL using Tunnelling Transition
#'
#' @description Runs a Monte-Carlo (MC) simulation of thermo-luminesence (TL) using the tunneling (TUN) model.
#' Tunneling transitions refers to the direct movement of electrons from a trap directly to the recombination centre.
#'
#' @details
#'
#' \deqn{
#' I_{TUN}(t) = -dn/dt = A * (n^2 / (r + n))
#' }
#'
#'where in the function `N` := `N_e` := `rho` := `rho'` := `r_c` := `rho'_c`
#' @param s [list] (**required**): Escape frequency of the trap (s^-1).
#'
#' @param E [numeric] (**required**): Thermal activation energy of the trap (eV).
#'
#' @param rho [numeric] (**required**): The calculated dimesionless Charge density.
#'
#' @param r_c [numeric] (*with default*): The dimensionless minimal critical radius.
#'
#' @param times [vector] (*with default*): The number of MC runs.
#'
#' @param clusters  [numeric] (*with default*): The number of clusters.
#'
#' @param N_e [numeric] (*with default*): The number of electrons
#'
#' @param delta.r [numeric] (*with default*): The approriate distance interval along the r axis (dimensionless).
#'
#' @param method [character] (*with default*): sequential `'seq'` or parallel processing `'par'`
#'
#' @param output [character] (*with default*): output is either the `'signal'` (the default) or `'remaining_e'` (the remaining
#' charges, electrons, in the trap)
#'
#' @param \dots further arguments
#'
#' @return This function returns an \code{\link{array}} with dimension length(times) x length(r) x clusters
#'
#' @section Function version: 0.1.0
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany), Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, Université Bordeaux Montaigne (France)
#'
#' @references
#' Pagonis, V. and Kulp, C., 2017. Monte Carlo simulations of tunneling phenomena and nearest neighbor hopping mechanism in feldspars. Journal of Luminescence 181, 114–120. \doi{10.1016/j.jlumin.2016.09.014}
#'
#' Pagonis, V., Friedrich, J., Discher, M., Müller-Kirschbaum, A., Schlosser, V., Kreutzer, S., Chen, R. and Schmidt, C., 2019. Excited state luminescence signals from a random distribution of defects: A new Monte Carlo simulation approach for feldspar. Journal of Luminescence 207, 266–272. \doi{10.1016/j.jlumin.2018.11.024}
#'
#' **Further reading**
#' Aitken, M.J., 1985. Thermoluminescence dating. 276-280. \doi{10.1002/gea.3340020110}
#'
#' @examples
#' \dontrun{
#' ##============================================================================##
#' ## Example 1: Simulate TL measurement
#' ##============================================================================##
#' run_MC_TL_TUN(s = 3.5e12,
#'           E = 1.45,
#'           rho = 0.015,
#'           r_c = 0.85,
#'           times = 200:500) %>%
#'     plot_RLumCarlo(legend = T)
#'}
#'
#'context("run_MC_TL_TUN")
#'
#'
#'
#'
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

 # Integrity checks ----------------------------------------------------------------------------
  if(!method %in% c("par", "seq"))
    stop("[run_MC_TL_TUN()] Allowed keywords for 'method' are either 'par' or 'seq'!", call. = FALSE)

  if(!output %in% c("signal", "remaining_e"))
    stop("[run_MC_TL_TUN()] Allowed keywords for 'output' are either 'signal' or 'remaining_e'!", call. = FALSE)

# Register multi-core backend -----------------------------------------------------------------
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


# Setting parameters --------------------------------------------------------------------------
  r <- seq(r_c, 2, delta.r)


# Run model -----------------------------------------------------------------------------------
  temp <- foreach(
    c = 1:clusters,
    .packages = 'RLumCarlo',
    .combine = 'comb_array',
    .multicombine = TRUE
  ) %dopar% {
    results <- MC_C_TL_TUN(
      times = times,
      N_e = N_e,
      r = r,
      rho = rho,
      E = E,
      s = s
    )
   return(results[[output]])

  }

# Return --------------------------------------------------------------------------------------
.return_ModelOutput(signal = temp, time = times)
}
