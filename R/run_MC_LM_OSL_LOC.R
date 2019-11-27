#' @title Run Monte-Carlo Simulation for LM-OSL (localized transitions)
#'
#' @description Runs a Monte-Carlo (MC) simulation of linearly modulated optically stimulated
#' luminesence (LM-OSL) using the generalized one trap (GOT) model. Localized transitions refer to
#' transitions which do not involve the conduction or valence band. These transitions take place
#' between the ground state and an excited state of the trapped charge, and also involve
#' a an excited state of the recombination center.
#'
#' @details
#'
#' **The model**
#'
#' \deqn{
#' I_{LOC}(t) = -dn/dt = (A * t/P) * (n^2 / (r + n))
#' }
#'
#' Where in the function: \cr
#'  A := optical excitation rate from the trap to the conduction band (1/s)\cr
#'  P := total excitation time (s) \cr
#'  t := time (s) \cr
#'  n := `n_filled`, the instantaneous number of electrons \cr
#'  r := the retrapping ratio for localized transitions \cr
#'  P := the total stimulation period (s)
#'
#' @param A [numeric] (**required**): The optical excitation rate from trap to conduction band (s^-1)
#'
#' @param times [numeric] (*with default*): The sequence of time steps within the simulation (s)
#'
#' @param clusters [numeric] (*with default*): The number of created clusters for the MC runs
#'
#' @param n_filled [integer] (*with default*): The number of filled electron traps at the
#' beginning of the simulation (unitless)
#'
#' @param r [numeric] (*with default*): The retrapping ratio for localized transitions
#'
#' @param method [character] (*with default*): Sequential `'seq'` or parallel `'par'`processing. In
#' the parallel mode the function tries to run the simulation on multiple CPU cores (if available) with
#' a positive effect on the computation time.
#'
#' @param output [character] (*with default*): output is either the `'signal'` (the default)
#' or `'remaining_e'` (the remaining charges, electrons, in the trap)
#'
#' @param \dots further arguments
#'
#' @return This function returns an object of class `RLumCarlo_Model_Output` which
#' is a [list] consisting of an [array] with dimension length(times) x clusters
#' and a [numeric] time vector.
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS - Université Bordeaux Montaigne (France)
#'
#' @references
#' Pagonis, V., Friedrich, J., Discher, M., Müller-Kirschbaum, A., Schlosser, V.,
#' Kreutzer, S., Chen, R. and Schmidt, C., 2019. Excited state luminescence signals
#' from a random distribution of defects: A new Monte Carlo simulation approach for feldspar.
#' Journal of Luminescence 207, 266–272. \doi{10.1016/j.jlumin.2018.11.024}
#'
#' @examples
#' ## short example
#' run_MC_LM_OSL_LOC(
#'  A = 1,
#'  s = 1e8,
#'  E = 0.5,
#'  times = 0:40,
#'  clusters = 10,
#'  n_filled = 10,
#'  r = 1e-7,
#'  method = "seq",
#'  output = "signal") %>%
#' plot_RLumCarlo(legend = TRUE)
#'
#' \dontrun{
#' ## the long (meaningful) example
#' results <- run_MC_LM_OSL_LOC(
#'  A = 1,
#'  s = 1e8,
#'  E = 0.6,
#'  times = 0:100,
#'  clusters = 100,
#'  n_filled = 100,
#'  r = 1e-7,
#'  method = "par",
#'  output = "signal")
#'
#' ## plot
#' plot_RLumCarlo(results, legend = TRUE)
#' }
#'
#' @keywords models data
#' @encoding UTF-8
#' @md
#' @export
run_MC_LM_OSL_LOC <- function(
  A,
  times,
  clusters = 10,
  n_filled = 100,
  r,
  method = "par",
  output = "signal",
  ...){

# Integrity checks ----------------------------------------------------------------------------
  if(!method %in% c("par", "seq"))
    stop("[run_MC_LM_OSL_LOC()] Allowed keywords for 'method' are either 'par' or 'seq'!", call. = FALSE)

  if(!output %in% c("signal", "remaining_e"))
    stop("[run_MC_LM_OSL_LOC()] Allowed keywords for 'output' are either 'signal' or 'remaining_e'!", call. = FALSE)

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

# Run model -----------------------------------------------------------------------------------
  temp <- foreach(c = 1:clusters,
                  .packages = 'RLumCarlo',
                  .combine = 'comb_array',
                  .multicombine = TRUE) %dopar% {

    results <- MC_C_LM_OSL_LOC(
      times = times,
      n_filled = n_filled,
      r = r,
      A
      )

    return(results[[output]])

  }  # end c-loop

# Return --------------------------------------------------------------------------------------
  .return_ModelOutput(signal = temp, time = times)
}
