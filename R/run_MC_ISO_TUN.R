#' @title Run Monte-Carlo Simulation for ISO-TL for tunneling transition
#'
#' @description Runs a Monte-Carlo (MC) simulation of isothermally stimulated luminesence (ISO-TL or ITL) using the tunneling (TUN) model. Tunneling refers to the direct movement of electrons from a trap directly to the recombination center.
#'
#' @details
#'
#' \deqn{
#' p(t) = s * e ^ (-E / kB * T) * e ^ (-r / rho ^ 1 / 3)
#' }
#' \deqn{
#' I_{TUN}(t) = 3 * n * p(t) *  r ^ 2 * e ^ (-r ^ 3)
#' }
#'
#' Where in the function `n` := `n_filled` := `t`:= `times`
#'
#' @param E [numeric] (**required**): Thermal activation energy of the trap (eV).
#'
#' @param s [numeric] (**required**): Frequency factor of the trap (s^-1).
#'
#' @param T [numeric] (**required**): Temperature (degrees C).
#'
#' @param rho [numeric] (**required**): The density of recombination centers (defined as rho' in Huntley 2006) (unitless).
#'
#' @param times [numeric] (*with default*): The sequence of temperature steps within the simulation (s).
#'
#' @param clusters [numeric] (*with default*): The number of MC runs (unitless).
#'
#' @param N_e [numeric] (*width default*): The total number of electron traps available (unitless).
#'
#' @param r_c [numeric] (*with default*): The radius of tunneling (dimensionless)
#'
#' @param delta.r [numeric] (*with default*):
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
#' @return This function returns a list.
#'
#' @section Function version: 0.1.0
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany), Sebastian Kreutzer, IRAMAT-CRP2A,
#' UMR 5060, CNRS - Univerité Bordeaux Montaigne (France)
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
#' ## Example 1: Simulate isothermal measurement
#' ##============================================================================##
#' run_MC_ISO_TUN(
#'  E = 1.2,
#'  s = 1e10,
#'  T = 200,
#'  rho = 0.007,
#'  times = 0:5000) %>%
#'   plot_RLumCarlo(legend = TRUE)
#'}
#' @md
#' @export
run_MC_ISO_TUN <- function(
  E,
  s,
  T = 200,
  rho,
  times,
  clusters = 10,
  r_c = 0,
  delta.r = 0.1,
  N_e = 200L,
  method = "par",
  output = "signal",
  ...){

# Integrity checks ----------------------------------------------------------------------------
  if(!method %in% c("par", "seq"))
    stop("[run_MC_ISO_TUN()] Allowed keywords for 'method' are either 'par' or 'seq'!", call. = FALSE)

  if(!output %in% c("signal", "remaining_e"))
    stop("[run_MC_ISO_TUN()] Allowed keywords for 'output' are either 'signal' or 'remaining_e'!", call. = FALSE)

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
r <- seq(abs(r_c[1]), 2, abs(delta.r[1]))

# Run model -----------------------------------------------------------------------------------
  temp <- foreach(c = 1:clusters,
                  .packages = 'RLumCarlo',
                  .combine = 'comb_array',
                  .multicombine = TRUE) %dopar% {

    results <- MC_C_ISO_TUN(times = times,
             N_e = N_e,
             r = r,
             rho = rho,
             E = E,
             s = s,
             T = T
             )

    return(results[[output]])
  }

# Return --------------------------------------------------------------------------------------
.return_ModelOutput(signal = temp, time = times)
}
