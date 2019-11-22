#' @title Monte-Carlo Simulation for Isothermal-TL (tunneling transitions)
#'
#' @description Runs a Monte-Carlo (MC) simulation of isothermally stimulated luminesence
#' (ISO-TL or ITL) using the tunneling (TUN) model. Tunneling refers to quantum mechanical
#' tunneling processes from the excited state of the trapped charge,
#' into a state of the recombination center.
#'
#' @details
#'
#' **The model**
#'
#' \deqn{
#' I_{TUN}(r',t) = -dn/dt = (s * exp(-E/(k_{B}*T_{ISO}))) * exp(-(\rho')^(-1/3) * r') * n (r',t)
#' }
#'
#' Where in the function: \cr
#' E := thermal activation energy (eV) \cr
#' s := the effective frequency factor for the tunneling process (1/s) \cr
#' \eqn{T_{ISO}} := The temperature of the isothermal experiment (degrees C)\cr
#' \eqn{k_{B}} := Boltzmann constant \cr
#' `r` := the unitless tunneling radius \cr
#' \eqn{\rho} := `rho` the unitless density of recombination centres \cr
#' `t` := Time (s) \cr
#' `n` := The Instantaneous number of electrons \cr
#'
#' @param E [numeric] (**required**): Thermal activation energy of the trap (eV).
#'
#' @param s [numeric] (**required**): The effective frequency factor for the tunneling process (1/s).
#'
#' @param T [numeric] (*with default*): Constant stimulation temperature (degrees C).
#'
#' @param rho [numeric] (**required**): The dimensionless density of recombination centres
#' (defined as \eqn{\rho}' in Huntley 2006) (unitless).
#'
#' @param times [numeric] (*with default*): The sequence of temperature steps within the simulation (s).
#'
#' @param clusters [numeric] (*with default*): The number of MC runs (unitless).
#'
#' @param N_e [numeric] (*width default*): The total number of electron traps available (unitless).
#'
#' @param r_c [numeric] (*with default*): Critical distance (>0) that must be provided if the
#' sample has 1 been thermally and/or optically pretreated. This parameter expresses the fact
#' that electron-hole pairs within a critical radius `r_c` have already been recombined.
#'
#' @param delta.r [numeric] (*with default*): Fractional change of the dimensionless distance
#' of nearest recombination centres (r')
#'
#' @param method [character] (*with default*): Sequential `'seq'` or parallel `'par'`processing. In
#' the parallel mode the function tries to run the simulation on multiple CPU cores (if available) with
#' a positive effect on the computation time.
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
#' Jain, M., Guralnik, B., Andersen, M.T., 2012. Stimulated luminescence emission from
#' localized recombination in randomly distributed defects.
#' J. Phys.: Condens. Matter 24, 385402. \doi{10.1088/0953-8984/24/38/385402}
#'
#' Pagonis, V., Friedrich, J., Discher, M., Müller-Kirschbaum, A., Schlosser, V., Kreutzer, S.,
#' Chen, R. and Schmidt, C., 2019. Excited state luminescence signals from a random
#' distribution of defects: A new Monte Carlo simulation approach for feldspar.
#' Journal of Luminescence 207, 266–272. \doi{10.1016/j.jlumin.2018.11.024}
#'
#' @examples
#' ## short example
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
#' \dontrun{
#' ## long (meaningful) example
#' run_MC_ISO_TUN(
#'  E = .8,
#'  s = 1e16,
#'  T = 50,
#'  rho = 1e-4,
#'  times = 0:100,
#'  clusters = 1000,
#'  N_e = 200,
#'  r_c = 1e-4,
#'  delta.r = 0.5,
#'  method = "par") %>%
#'  plot_RLumCarlo(legend = TRUE)
#' }
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
