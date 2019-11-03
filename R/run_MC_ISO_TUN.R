#' @title Monte-Carlo Simulation for Isothermal-TL for Tunneling Transition
#'
#' @description Runs a Monte-Carlo (MC) simulation of isothermally stimulated luminesence
#' (ISO-TL or ITL) using the tunneling (TUN) model. Tunneling refers to the direct transition
#' of electrons from an excited state directly into the recombination center without
#' involving the conduction band.
#'
#' @details
#'
#' ** Model description **
#'
#' \deqn{
#' p(t) = s * e ^ (-E / k_{B} * T) * e^{(-r' / \rho'^{1 / 3})}
#' }
#'
#' \deqn{
#' I_{TUN}(t) = 3 * n * p(t) *  (r')^{2 * e^{(-r'^3)}}
#' }
#'
#' Where in the function: \cr
#' `p(t)` := The experimental stimulation mode \cr
#' \eqn{k_{B}} := Boltzmann constant \cr
#' `r` := `r` \cr
#' \eqn{\rho} := `rho` \cr
#' `t` := `Time` \cr
#' `n` := The Instantaneous number of electrons \cr
#' `n` := `n_filled` \cr
#' `t`:= `times`
#'
#' @param E [numeric] (**required**): Thermal activation energy of the trap (eV).
#'
#' @param s [numeric] (**required**): Frequency factor of the trap (s^-1).
#'
#' @param T [numeric] (*with default*): Constant stimulation temperature (degrees C).
#'
#' @param rho [numeric] (**required**): The density of recombination centres
#' (defined as rho' in Huntley 2006) (unitless).
#'
#' @param times [numeric] (*with default*): The sequence of temperature steps within the simulation (s).
#'
#' @param clusters [numeric] (*with default*): The number of MC runs (unitless).
#'
#' @param N_e [numeric] (*width default*): The total number of electron traps available (unitless).
#'
#' @param r_c [numeric] (*with default*): The radius of tunneling (dimensionless)
#'
#' @param delta.r [numeric] (*with default*): Fractional change of the dimensionless distance
#' of nearest recombination centres (r', which is preset at 2)
#'
#' @param method [character] (*with default*): sequential `'seq'` or parallel processing `'par'`
#'
#' @param output [character] (*with default*): output is either the `'signal'` (the default) or
#' `'remaining_e'` (the remaining charges, electrons, in the trap)
#'
#' @param \dots further arguments
#'
#' @return This function returns an object of class `RLumCarlo_Model_Output` which
#' is a [list] consisting of an [array] with dimension length(times) x length(r) x clusters
#' and a [numeric] time vector.
#'
#' @section Function version: 0.1.0
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany),
#' Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS - Université Bordeaux Montaigne (France)
#'
#' @references
#' Pagonis, V. and Kulp, C., 2017. Monte Carlo simulations of tunneling phenomena
#' and nearest neighbor hopping mechanism in feldspars.
#' Journal of Luminescence 181, 114–120. \doi{10.1016/j.jlumin.2016.09.014}
#'
#' **Further reading**
#' Aitken, M.J., 1985. Thermoluminescence dating. Academic Press.
#'
#' Huntley, D.J., 2006. An explanation of the power-law decay of luminescence.
#' Journal of Physics: Condensed Matter, 18(4), 1359.\doi{10.1088/0953-8984/18/4/020}
#'
#' Pagonis, V., Friedrich, J., Discher, M., Müller-Kirschbaum, A., Schlosser, V., Kreutzer, S.,
#' Chen, R. and Schmidt, C., 2019. Excited state luminescence signals from a random
#' distribution of defects: A new Monte Carlo simulation approach for feldspar.
#' Journal of Luminescence 207, 266–272. \doi{10.1016/j.jlumin.2018.11.024}
#'
#' @examples
#' run_MC_ISO_TUN(
#'  E = .8,
#'  s = 1e16,
#'  T = 50,
#'  rho = 1e-4,
#'  times = 0:100,
#'  clusters = 10,
#'  N_e = 2,
#'  r_c = 1e-4,
#'  delta.r = 0.5,
#'  method = "seq") %>%
#'  plot_RLumCarlo(legend = TRUE)
#'
#' @keywords models data
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
    stop("[run_MC_ISO_TUN()] Allowed keywords for 'method' are either 'par' or 'seq'!",
         call. = FALSE)

  if(!output %in% c("signal", "remaining_e"))
    stop("[run_MC_ISO_TUN()] Allowed keywords for 'output' are either 'signal' or 'remaining_e'!",
         call. = FALSE)

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
