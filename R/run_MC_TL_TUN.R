#' @title Run Monte-Carlo Simulation for TL for tunnelling transition
#'
#' @description Runs a Monte-Carlo (MC) simulation of thermo-luminesence (TL) using the tunneling (TUN) model. Tunneling refers to the direct movement of electrons from a trap directly to the recombination center
#'
#' @details
#' 
#' \deqn{
#' p(t) = s * e ^ (-E / k_{B} * T) * e ^ (-r' / \rho' ^ 1 / 3)
#' }
#' \deqn{
#' I_{TUN}(t) = 3 * n * p(t) *  r' ^ 2 * e ^ (-r' ^ 3) 
#' }
#' 
#' Where in the function: \cr `p(t)` := `The experimental stimulation mode` \cr `e`:= `Exponentional function` \cr \eqn{k_{B}} := `Boltzmann constant` \cr `T` := `Temperature` \cr `r'` := `r` \cr \eqn{\rho'} := `rho` \cr `t` := `Time` \cr `n` := `The Instantaneous number of electrons`
#'
#' @param E [numeric] (**required**): Thermal activation energy of the trap (eV).
#'
#' @param s [list] (**required**): The frequency factor of the trap (s^-1).
#'
#' @param rho [numeric] (**required**): The density of recombination centers (defined as rho' in Huntley 2006) (unitless).
#'
#' @param r_c [numeric] (*with default*): Distance parameter (radius of tunneling) (unitless).
#'
#' @param times [vector] (*with default*): The sequence of temperature steps within the simulation (s).
#' 
#' @param clusters  [numeric] (*with default*): The number of MC runs (unitless).
#'
#' @param N_e [numeric] (*with default*): The total number of electron traps available (unitless).
#'
#' @param delta.r [numeric] (*with default*): The  increments of r_c (unitless).
#' 
#' @param r [numeric] (*with default*): The radius of tunneling (unitless).
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
#' Huntley, D.J., 2006. An explanation of the power-law decay of luminescence. Journal of Physics: Condensed Matter, 18(4), 1359.\doi{10.1088/0953-8984/18/4/020}
#' 
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
#' @md
#' @export
run_MC_TL_TUN <- function(
  s,
  E,
  rho,
  r_c = 0,
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
  r <- seq(abs(r_c), 2, abs(delta.r))


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
