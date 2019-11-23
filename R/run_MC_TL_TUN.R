#' @title Run Monte-Carlo Simulation for TL (tunneling transitions)
#'
#' @description Runs a Monte-Carlo (MC) simulation of thermoluminesence (TL) caused by
#' tunnelling (TUN) transitions.  Tunneling refers to quantum mechanical
#' tunneling processes from the excited state of the trapped charge,
#' into a recombination center.
#'
#' @details
#'
#' **The model**
#'
#' \deqn{
#' I_{TUN}(r',t) = -dn/dt = (s * exp(-E/(k_{B} * T))) * exp(-(\rho')^{-1/3} * r') * n(r',t)
#' }
#'
#' Where in the function: \cr
#' s := frequency for the tunneling process (s^-1) \cr
#' E := thermal activation energy (eV) \cr
#' \eqn{k_{B}} := Boltzmann constant \cr
#' T := temperature \cr
#' r' := the unitless tunneling radius \cr
#' \eqn{\rho}' := `rho'`, the unitless density of recombination centres (see Huntley (2006)) \cr
#' t := time (s) \cr
#' n := the instantaneous number of electrons
#'
#' @param E [numeric] (**required**): Thermal activation energy of the trap (eV)
#'
#' @param s [list] (**required**): The frequency factor for the tunneling process (s^-1)
#'
#' @param rho [numeric] (**required**): The dimensionless density of recombination centers
#' (defined as \eqn{\rho}' in Huntley 2006)
#'
#' @param r_c [numeric] (*with default*): Critical distance (>0) that is to be used if
#' the sample has been thermally and/or optically pretreated. This parameter expresses the fact
#' that electron-hole pairs within a critical radius `r_c` have already recombined.
#'
#' @param times [vector] (*wih default*): The sequence of time steps within the simulation (s)
#'
#' @param clusters  [numeric] (*with default*): The number of MC runs (unitless)
#'
#' @param N_e [numeric] (*with default*): The total number of electron traps available (unitless)
#'
#' @param delta.r [numeric] (*with default*): The increments of `r_c` (unitless)
#'
#' @param method [character] (*with default*): Sequential `'seq'` or parallel `'par'`processing. In
#' the parallel mode the function tries to run the simulation on multiple CPU cores (if available) with
#' a positive effect on the computation time.
#'
#' @param output [character] (*with default*): output is either the `'signal'` (the default)
#' or `'remaining_e'` (the remaining charges/electrons in the trap)
#'
#' @param \dots further arguments
#'
#' @return This function returns an object of class `RLumCarlo_Model_Output` which
#' is a [list] consisting of an [array] with dimension length(times) x length(r) x clusters
#' and a [numeric] time vector.
#'
#' @section Function version: 0.1.0
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany), Sebastian Kreutzer,
#' IRAMAT-CRP2A, UMR 5060, Université Bordeaux Montaigne (France)
#'
#' @references
#' Huntley, D.J., 2006. An explanation of the power-law decay of luminescence.
#' Journal of Physics: Condensed Matter, 18(4), 1359. \doi{10.1088/0953-8984/18/4/020}
#'
#' Pagonis, V. and Kulp, C., 2017. Monte Carlo simulations of tunneling
#' phenomena and nearest neighbor hopping mechanism in feldspars.
#' Journal of Luminescence 181, 114–120. \doi{10.1016/j.jlumin.2016.09.014}
#'
#' Pagonis, V., Friedrich, J., Discher, M., Müller-Kirschbaum, A., Schlosser, V., Kreutzer, S.,
#' Chen, R. and Schmidt, C., 2019. Excited state luminescence signals from a random
#' distribution of defects: A new Monte Carlo simulation approach for feldspar.
#' Journal of Luminescence 207, 266–272. \doi{10.1016/j.jlumin.2018.11.024}
#'
#' **Further reading**
#'
#' Aitken, M.J., 1985. Thermoluminescence dating. Academic Press.
#'
#' Jain, M., Guralnik, B., Andersen, M.T., 2012. Stimulated luminescence emission from
#' localized recombination in randomly distributed defects.
#' J. Phys.: Condens. Matter 24, 385402. \doi{10.1088/0953-8984/24/38/385402}
#'
#' @examples
#' ## the short example
#' run_MC_TL_TUN(
#'  s = 1e12,
#'  E = 0.9,
#'  rho = 1,
#'  r_c = 1,
#'  times = 80:120,
#'  clusters = 2,
#'  method = 'seq',
#'  delta.r = 1e-1) %>%
#' plot_RLumCarlo()
#'
#' \dontrun{
#' ## the long (meaningful example)
#' results <- run_MC_TL_TUN(
#'  s = 1e12,
#'  E = 0.9,
#'  rho = 0.01,
#'  r_c = 0.1,
#'  times = 80:220,
#'  clusters = 100,
#'  method = 'par',
#'  delta.r = 1e-1)
#'
#' ## plot
#' plot_RLumCarlo(results)
#' }
#'
#' @keywords models data
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
