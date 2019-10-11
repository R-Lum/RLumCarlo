#' @title Run Monte-Carlo simulation for CW-IRSL for localized transition
#'
#' @description Runs a Monte-Carlo (MC) simulation of constant wave infrared stimulated luminesence (CW-IRSL) using the generalized one trap (GOT) model. Localized refers to an excited state that is shared by the electron and the recombination center, so that the conduction band is not involved in the recombination process.
#'
#' @details
#'
#' \deqn{
#' I_{LOC}(t) = -dn/dt = A * (n^2 / (r + n))
#' }
#'
#' Where in the function: \cr
#'  t := Time \cr
#'  n := `n_filled``
#'
#' @param A [numeric] (**required**): The optical excitation rate from trap to the excited state (s^-1).
#'
#' @param times [numeric] (*with default*): The sequence of time steps within the simulation (s).
#'
#' @param clusters [numeric] (*with default*): The number of MC runs (unitless).
#'
#' @param n_filled [integer] (*with default*): The number of filled electron traps at the beginning of the simulation (unitless).
#'
#' @param r [numeric] (*with default*): The localized retrapping ratio (unitless).
#'
#' @param method [character] (*with default*): sequential `'seq'` or parallel `'par'`processing 
#'
#' @param output [character] (*with default*): output is either the `'signal'` (the default) or `'remaining_e'` (the remaining
#' charges, electrons, in the trap)
#'
#' @param \dots further arguments
#'
#' @return This function returns an [array] with dimension length(times) x length(r) x clusters
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS - Université Bordeaux Montaigne (France)
#'
#' @references
#'
#' Pagonis, V., Friedrich, J., Discher, M., Müller-Kirschbaum, A., Schlosser, V., Kreutzer, S., Chen, R. and Schmidt, C., 2019. Excited state luminescence signals from a random distribution of defects: A new Monte Carlo simulation approach for feldspar. Journal of Luminescence 207, 266–272. \doi{10.1016/j.jlumin.2018.11.024}
#'
#' Reuven, C. and S. Mckeever, 1997. Theory of thermoluminescence and related phenomena.
#'
#' @examples
#' ##============================================================================##
#' ## Example 1: Single Plot for Monte-Carlo (MC) simulations for localized CW_IRSL
#' ##============================================================================##
#' \dontrun{
#' run_MC_CW_IRSL_LOC(
#'  A = 0.12,
#'  times = 0:100,
#'  clusters = 50,
#'  n_filled = 1,
#'  r = 1e-7,
#'  method = "seq",
#'  output = "signal"
#' ) %>%
#'  #Plot results of the MC simulation
#' plot_RLumCarlo(legend = T)
#'
#' }
#'
#'
#' @md
#' @export
run_MC_CW_IRSL_LOC <- function(
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
    stop("[run_MC_CW_IRSL()] Allowed keywords for 'method' are either 'par' or 'seq'!", call. = FALSE)

  if(!output %in% c("signal", "remaining_e"))
    stop("[run_MC_CW_IRSL()] Allowed keywords for 'output' are either 'signal' or 'remaining_e'!", call. = FALSE)

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

    results <- MC_C_CW_IRSL_LOC(
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

