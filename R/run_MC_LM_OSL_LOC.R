#' @title Run Monte-Carlo simulation for LM-OSL for localized transition
#'
#' @description Runs a Monte-Carlo (MC) simulation of linearly modulated optically stimulated luminesence (LM-OSL) using the generalized one trap (GOT) model. Localized refers to excitation of an electron before it recombines, but without the involvement of the conduction band.
#'
#' @details
#'
#' \deqn{
#' I_{LOC}(t) = -dn/dt = p(t) * (n^2 / (r + n))
#' }
#'
#' Where in the function: \cr
#' `t` := `Time` \cr
#' `p(t)` := The experimental stimulation mode \cr
#' `n` := The Instantaneous number of electrons
#'
#' @param A [numeric] (**required**): The optical excitation rate from trap to conduction band (s^-1).
#'
#' @param times [numeric] (*with default*): The sequence of temperature steps within the simulation (s).
#'
#' @param clusters [numeric] (*with default*): The number of MC runs (unitless).
#'
#' @param n_filled [integer] (*with default*): The number of filled electron traps at the beginning of the simulation (unitless).
#'
#' @param r [numeric] (*with default*): The localized retrapping ratio (unitless)
#'
#' @param method [character] (*with default*): sequential `'seq'` or parallel processing `'par'`
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
#' @examples
#' ##============================================================================##
#' ## Example 1: Simulate LM-OSL
#' ##============================================================================##
#' \dontrun{
#' run_MC_LM_OSL_LOC(
#'  A = 0.12,
#'  r = 1,
#'  times = 0:100) %>%
#'    plot_RLumCarlo(legend = T)
#'
#' }
#'
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
