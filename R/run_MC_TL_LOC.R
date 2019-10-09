#' @title Run Monte-Carlo simulation for TL for localised transition
#'
#' @description Runs a Monte-Carlo (MC) simulation of thermo-luminesence (TL) using the generalized one trap (GOT) model. Localized refers to excitation of an electron before it recombines, but without the involvement of the conduction band.
#'
#' @details
#'
#' \deqn{
#' I_{LOC}(t) = -dn/dt = p(t) * (n^2 / (r + n))
#' }
#'
#'where in the function `n` :=`n_filled`
#' @param s [numeric] (**required**): Escape frequency of the trap (s^-1).
#'
#' @param E [numeric] (**required**): Thermal activation energy of the trap (eV).
#'
#' @param times [numeric] (*with default*): The specified time within the simulation with the same syntax as the function seq().
#'
#' @param clusters [numeric] (*with default*): The number of MC runs.
#'
#' @param n_filled [integer] (*with default*): The number of electron traps that are filled at the beginning of the simulation.
#'
#' @param r [numeric] (*with default*): The retrapping ratio.
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
#' ## Example 1: Simulate TL
#' ##============================================================================##
#' 
#' 
#' \dontrun{
#' run_MC_TL_LOC(
#'  s = 3.5e12,
#'  E = 1.45,
#'  r = 1,
#'  times = 100:450) %>%
#'    plot_RLumCarlo(legend = T)
#'
#' }
#'
#'
#'
#' @md
#' @export
run_MC_TL_LOC <- function(
  s,
  E,
  times,
  clusters = 10,
  n_filled = 100,
  r,
  method = "par",
  output = "signal",
  ...){

# Integrity checks ----------------------------------------------------------------------------
  if(!method %in% c("par", "seq"))
    stop("[run_MC_TL_LOC()] Allowed keywords for 'method' are either 'par' or 'seq'!", call. = FALSE)

  if(!output %in% c("signal", "remaining_e"))
    stop("[run_MC_TL_LOC()] Allowed keywords for 'output' are either 'signal' or 'remaining_e'!", call. = FALSE)

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

    results <- MC_C_TL_LOC(
      times = times,
      n_filled = n_filled,
      r = r,
      E = E,
      s = s)

    return(results[[output]])

  }  # end c-loop

# Return --------------------------------------------------------------------------------------
  .return_ModelOutput(signal = temp, time = times)
}
