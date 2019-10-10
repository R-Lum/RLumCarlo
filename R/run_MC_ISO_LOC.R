#' @title Run Monte-Carlo simulation for ISO-TL for localized transition
#'
#' @description Runs a Monte-Carlo (MC) simulation of isothermally stimulated luminesence (ISO-TL or ITL) using the genralized one trap (GOT) model. Localized refers to excitation of an electron before it recombines, but without the involvement of the conduction band.
#'
#' @details
#'
#' \deqn{
#' ISO I_{LOC}(t) = -dn/dt = (s * e^-E/kT_ITL/ISO) * (n^2 / (r + n)))
#' }
#'
#'Where in the function `n` := `n_filled` := `N` := `N_e`
#'
#' @param E [numeric] (**required**): Thermal activation energy of the trap (eV).
#'
#' @param s [numeric] (**required**): The frequency factor of the trap (s^-1).
#'
#' @param T [numeric] (*with default*): Temperature (degrees C).
#'
#' @param times [numeric] (*with default*): The sequence of temperature steps within the simulation (s).
#'
#' @param clusters [numeric] (*with default*): The number of MC runs (unitless).
#'
#' @param n_filled [integer] (*with default*): The number of filled electron traps at the beginning of the simulation (unitless).
#'
#' @param r [numeric] (*with default*): the retrapping ratio (unitless). 
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
#' @section Function version: 0.0.1
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS - Université Bordeaux Montaigne (France)
#'
#' @references
#'
#' Pagonis, V., Friedrich, J., Discher, M., Müller-Kirschbaum, A., Schlosser, V., Kreutzer, S., Chen, R. and Schmidt, C., 2019. Excited state luminescence signals from a random distribution of defects: A new Monte Carlo simulation approach for feldspar. Journal of Luminescence 207, 266–272. \doi{10.1016/j.jlumin.2018.11.024}
#'
#' @examples
#' ##============================================================================##
#' ## Example 1: Simulate ITL
#' ##============================================================================##
#'
#' 
#'  
#'   
#'     \dontrun{
#' run_MC_ISO_LOC(
#'  s = 3.5e12,
#'  E = 1.45,
#'  T = 200,
#'  r = 1,
#'  times = 0:10000) %>%
#'    plot_RLumCarlo(legend = T)
#'
#' }
#' 
#' 
#'
#' @md
#' @export
run_MC_ISO_LOC <- function(
  s,
  E,
  T = 20,
  times,
  clusters = 10,
  n_filled = 100,
  r,
  method = "par",
  output = "signal",
  ...){

# Integrity checks ----------------------------------------------------------------------------
  if(!method %in% c("par", "seq"))
    stop("[run_MC_ISO_LOC()] Allowed keywords for 'method' are either 'par' or 'seq'!", call. = FALSE)

  if(!output %in% c("signal", "remaining_e"))
    stop("[run_MC_ISO_LOC()] Allowed keywords for 'output' are either 'signal' or 'remaining_e'!", call. = FALSE)

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

    results <- MC_C_ISO_LOC(
      times = times,
      n_filled = n_filled,
      r = r,
      E = E,
      s = s,
      T = T
      )

    return(results[[output]])

  }  # end c-loop

# Return --------------------------------------------------------------------------------------
.return_ModelOutput(signal = temp, time = times)
}
