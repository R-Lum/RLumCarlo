#' @title Run Monte-Carlo simulation for TL for delocalized transition
#'
#' @description Runs a Monte-Carlo (MC) simulation of thermo-luminesence (TL) using the one trap one recombination center (OTOR) model. Delocalized refers to involvement of the conduction band.
#'
#' @details
#'
#' \deqn{
#' I_{DELOC}(t) = -dn/dt = (s * e^-E/k_{b} * T) * (n^2 / (N * R + n(1-R))))
#' }
#'
#' Where in the function: \cr
#' `t` := `Time` \cr
#' `e`:= Exponentional function \cr
#' \eqn{k_{B}} := Boltzmann constant \cr
#' `T`= Temperature \cr
#' `n` :=  The Instantaneous number of electrons \cr
#' `N` := `N_e`
#'
#' @param E [numeric] (**required**): Thermal activation energy of the trap (eV).
#'
#' @param s [numeric] (**required**): The frequency factor of the trap (s^-1).
#'
#' @param times [numeric] (*with default*): The sequence of temperature steps within the simulation (s).
#'
#' @param clusters [numeric] (*with default*): The number of MC runs (unitless).
#'
#' @param N_e [integer] (*with default*): The total number of electron traps available (unitless).
#'
#' @param n_filled [integer] (*with default*): The number of filled electron traps at the beginning of the simulation (unitless).
#'
#' @param R [numeric] (*with default*): The delocalized retrapping ratio (unitless).
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
#' Reuven, C. and S. Mckeever, 1997. Theory of thermoluminescence and related phenomena.
#'
#' @examples
#' ##============================================================================##
#' ## Example 1: Single Plot for Monte-Carlo (MC) simulations for delocalized TL
#' ##============================================================================##
#' \dontrun{
#' run_MC_TL_DELOC(
#'  s = 3.5e12,
#'  E = 1.45,
#'  R = 1,
#'  times = 100:450) %>%
#'  #Plot results of the MC simulation
#'    plot_RLumCarlo(legend = T)
#'
#' }
#'
#' #' @examples
#' ##============================================================================##
#' ## Example 2: Plot multiple TL stimulation TL curves in R with varying params
#' ##============================================================================##
#'
#'
#'
#'       \dontrun{
#' # define your parameters
#'times=seq(100,450,1)
#'s=rep(3.5e12,4)
#'E=rep(1.45,4)
#'R<-c(0.7e-6,1e-6,0.01,0.1)
#'clusters=1000
#'N_e =c(400, 500, 700, 400)
#'n_filled =c(400, 500, 300, 70)
#'method="par"
#'output ="signal"
#'col=c(1,2,3,4) # different colours for the individual curves
#'plot_uncertainty <- c(TRUE,TRUE,TRUE,TRUE)  # do you want to see the uncertainty?
#'add_TF <- c(FALSE,rep(TRUE, (length(R)-1)))
# loop to plot different curves into one plot
#'for (u in 1:length(R)){
#'  results <-run_MC_TL_DELOC(times=times, s=s[u],E=E[u], clusters =clusters, N_e = N_e[u],
#'                            n_filled = n_filled[u], R=R[u], method = method, output = output)
#'  plot_RLumCarlo(results,add=add_TF[u],legend = FALSE, col=col[u], main=" your plot", ylim=c(0,20))
#'}
# add your legend with your parameters
#'legend("topright",ncol=5,cex=0.55,title = "parameters" ,legend=c(paste0("E = ", E),
#'                                                                 paste0("s = ", s),
#'                                                                 paste0("n_filled = ", n_filled),
#'                                                                 paste0("N_e = ", N_e),
#'                                                                 paste0("R = ", R)),  text.col=col)


#' }
#'
#' @md
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

