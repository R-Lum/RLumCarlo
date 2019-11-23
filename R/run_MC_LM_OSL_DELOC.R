#' @title Run Monte-Carlo Simulation for LM-OSL (delocalized transitions)
#'
#' @description Runs a Monte-Carlo (MC) simulation of linearly modulated optically stimulated
#' luminesence (LM-OSL) using the one trap one recombination center (OTOR) model.
#' Delocalized refers to involvement of the conduction band.
#'
#' @details
#'
#' **The model**
#'
#' \deqn{
#' I_{DELOC}(t) = -dn/dt = A * (n^2 / (N*R + n(1-R)))
#' }
#'
#' Where in the function: \cr
#'  t := time (s) \cr
#'  A := the optical excitation rate from trap to conduction band \cr
#'  n := `n_filled`, the tnstantaneous number of electrons \cr
#'  R :=  the retrapping ratio for delocalized transitions \cr
#'  N := `N_e`, the total number of electron traps available (unitless) \cr
#'  P := total stimulation time"
#'
#' @param A [numeric] (**required**): The optical excitation rate from trap to conduction band (s^-1)
#'
#' @param times [numeric] (*with default*): The sequence of time steps within the simulation (s)
#'
#' @param clusters [numeric] (*with default*): The number of MC runs (unitless)
#'
#' @param N_e [integer] (*with default*): The total number of electron traps available (unitless)
#'
#' @param n_filled [integer] (*with default*): The number of filled electron traps at the beginning
#' of the simulation (unitless)
#'
#' @param R [numeric] (*with default*): The retrapping ratio for delocalized transitions
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
#' is a [list] consisting of an [array] with dimension length(times) x clusters
#' and a [numeric] time vector.
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS - Université Bordeaux Montaigne (France)
#'
#' @references
#' Pagonis, V., Friedrich, J., Discher, M., Müller-Kirschbaum, A., Schlosser, V., Kreutzer, S.,
#' Chen, R. and Schmidt, C., 2019. Excited state luminescence signals from a random distribution of
#' defects: A new Monte Carlo simulation approach for feldspar.
#' Journal of Luminescence 207, 266–272. \doi{10.1016/j.jlumin.2018.11.024}
#'
#' **Further reading**
#'
#' Chen, R., McKeever, S.W.S., 1997. Theory of Thermoluminescence and Related Phenomena.
#' WORLD SCIENTIFIC. \doi{10.1142/2781}
#'
#' @examples
#' run_MC_LM_OSL_DELOC(
#'  A = 0.12,
#'  R = 0.1,
#'  times = 0:50,
#'  method = "seq",
#'  clusters = 10) %>%
#' plot_RLumCarlo(legend = TRUE)
#'
#' @keywords models data
#' @md
#' @export
run_MC_LM_OSL_DELOC <- function(
  A,
  times,
  clusters = 10,
  N_e = 200,
  n_filled = N_e,
  R,
  method = "par",
  output = "signal",
  ...){

# Integrity checks ----------------------------------------------------------------------------
  if(!method %in% c("par", "seq"))
    stop("[run_MC_LM_OSL_DELOC()] Allowed keywords for 'method' are either 'par' or 'seq'!", call. = FALSE)

  if(!output %in% c("signal", "remaining_e"))
    stop("[run_MC_LM_OSL_DELOC()] Allowed keywords for 'output' are either 'signal' or 'remaining_e'!", call. = FALSE)

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

    results <- MC_C_LM_OSL_DELOC(
      times = times,
      N_e = N_e,
      n_filled = n_filled,
      R = R,
      A = A)

    return(results[[output]])

  }  # end c-loop

# Return --------------------------------------------------------------------------------------
.return_ModelOutput(signal = temp, time = times)
}

