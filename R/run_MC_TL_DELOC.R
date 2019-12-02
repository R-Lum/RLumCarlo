#' @title Run Monte-Carlo Simulation for TL (delocalized transitions)
#'
#' @description Runs a Monte-Carlo (MC) simulation of thermoluminesence (TL)
#' using the one trap one recombination center (OTOR) model.
#' Delocalized refers to involvement of the conduction band. The heating rate in this function
#' is assumed to be 1 K/s.
#'
#' @details
#'
#' **The model**
#'
#' \deqn{
#' I_{DELOC}(t) = -dn/dt = (s*exp(-E/(k_{B}*T))) * (n^2 / (N * R + n(1-R))))
#' }
#'
#' Where in the function: \cr
#'  E := the thermal activation enery (eV) \cr
#'  s := the frequency factor in (s^-1) \cr
#'  t := time (s) \cr
#' \eqn{k_{B}} := Boltzmann constant (8.617 x 10^-5 eV K^-1)\cr
#'  T := temperature (°C) \cr
#'  n := `n_filled`, the instantaneous number of electrons \cr
#'  N := `N_e`, the total number of electron traps available (unitless) \cr
#'
#' @param E [numeric] (**required**): Thermal activation energy of the trap (eV)
#'
#' @param s [numeric] (**required**): The frequency factor of the trap (s^-1)
#'
#' @param times [numeric] (*with default*): The sequence of temperature steps within the simulation (s).
#' The heating rate in this function is assumed to be 1 K/s.
#'
#' @param clusters [numeric] (*with default*): The number of created clusters for the MC runs
#'
#' @param N_e [integer] (*with default*): The total number of electron traps available (unitless)
#'
#' @param n_filled [integer] (*with default*): The number of filled electron traps at the beginning
#' of the simulation (unitless)
#'
#' @param R [numeric] (*with default*): Retrapping ratio for delocalized transitions
#'
#' @param method [character] (*with default*): Sequential `'seq'` or parallel `'par'`processing. In
#' the parallel mode the function tries to run the simulation on multiple CPU cores (if available) with
#' a positive effect on the computation time.
#'
#' @param output [character] (*with default*): output is either the `'signal'` (the default) or
#' `'remaining_e'` (the remaining charges/electrons in the trap)
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
#'
#' Pagonis, V., Friedrich, J., Discher, M., Müller-Kirschbaum, A., Schlosser, V., Kreutzer, S.,
#' Chen, R. and Schmidt, C., 2019. Excited state luminescence signals from a random distribution
#' of defects: A new Monte Carlo simulation approach for feldspar.
#' Journal of Luminescence 207, 266–272. \doi{10.1016/j.jlumin.2018.11.024}
#'
#' **Further reading**
#'
#' Chen, R., McKeever, S.W.S., 1997. Theory of Thermoluminescence and Related Phenomena.
#' WORLD SCIENTIFIC. \doi{10.1142/2781}
#'
#' @examples
#' ## the short example
#' run_MC_TL_DELOC(
#'  s = 3.5e12,
#'  E = 1.45,
#'  R = 0.1,
#'  method = 'seq',
#'  clusters = 100,
#'  times = 150:350) %>%
#' plot_RLumCarlo(legend = TRUE)
#'
#' \dontrun{
#' ## the long (meaningful) example
#' # define your parameters
#' times <- seq(100, 450, 1)
#' s <- rep(3.5e12, 4)
#' E <- rep(1.45, 4)
#' R <- c(0.7e-6, 1e-6, 0.01, 0.1)
#' clusters <- 300
#' N_e <- c(400, 500, 700, 400)
#' n_filled <- c(400, 500, 300, 70)
#' method <- "par"
#' output <- "signal"
#' col <- c(1, 2, 3, 4) # different colours for the individual curves
#' plot_uncertainty <- c(TRUE, TRUE, TRUE, TRUE)  # do you want to see the uncertainty?
#' add_TF <- c(FALSE, rep(TRUE, (length(R) - 1)))
#'
#' # loop to plot different curves into one plot
#' for (u in 1:length(R)){
#'  results <- run_MC_TL_DELOC(
#'   times=times,
#'   s = s[u],
#'   E = E[u],
#'   clusters = clusters,
#'   N_e = N_e[u],
#'   n_filled = n_filled[u],
#'   R = R[u],
#'   method = method,
#'   output = output)
#'
#' plot_RLumCarlo(
#'  results,
#'  add = add_TF[u],
#'  legend = FALSE,
#'  col=col[u],
#'  main = " your plot",
#'  ylim=c(0,20))
#' }
#' #add your legend with your parameters
#' legend("topright",
#'   ncol = 5,
#'   cex = 0.55,
#'   bty = "n",
#'   title = "parameters",
#'   legend = c(
#'    paste0("E = ", E),
#'    paste0("s = ", s),
#'    paste0("n_filled = ", n_filled),
#'    paste0("N_e = ", N_e), paste0("R = ", R)),
#'    text.col = col)
#' }
#'
#' @keywords models data
#' @md
#' @encoding UTF-8
#' @export
run_MC_TL_DELOC <- function(
  s,
  E,
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
    stop("[run_MC_TL_DELOC()] Allowed keywords for 'method' are either 'par' or 'seq'!", call. = FALSE)

  if(!output %in% c("signal", "remaining_e"))
    stop("[run_MC_TL_DELOC()] Allowed keywords for 'output' are either 'signal' or 'remaining_e'!", call. = FALSE)

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

    results <- MC_C_TL_DELOC(
      times = times,
      N_e = N_e,
      n_filled = n_filled,
      R = R,
      E = E,
      s = s)

    return(results[[output]])

  }  # end c-loop

# Return --------------------------------------------------------------------------------------
.return_ModelOutput(signal = temp, time = times)
}

