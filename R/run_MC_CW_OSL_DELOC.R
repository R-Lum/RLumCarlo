#' @title Run Monte-Carlo simulation for CW-OSL for delocalized transition
#'
#' @description Runs a Monte-Carlo (MC) simulation of constant wave optically stimulated luminesence (CW-OSL) using the one trap one recombination center (OTOR) model. Delocalized refers to involvement of the conduction band.
#'
#' @details
#'
#' \deqn{
#' I_{DELOC}(t) = -dn/dt = p(t) * (n^2 / (N*R + n(1-R)))
#' }
#' 
#' Where in the function: \cr `t` := `Time` \cr `p(t)` := `The experimental stimulation mode` \cr `n` := `The Instantaneous number of electrons` \cr `N` = `N_e`
#'
#' @param A [numeric] (**required**): The optical excitation rate from trap to conduction band (s^-1).
#'
#' @param times [numeric] (*with default*): The sequence of temperature steps within the simulation (s)
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
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS - Université Bordeaux Montaigne (France)
#'
#' @references
#' Pagonis, V., Friedrich, J., Discher, M., Müller-Kirschbaum, A., Schlosser, V., Kreutzer, S., Chen, R. and Schmidt, C., 2019. Excited state luminescence signals from a random distribution of defects: A new Monte Carlo simulation approach for feldspar. Journal of Luminescence 207, 266–272. \doi{10.1016/j.jlumin.2018.11.024}
#'
#' Reuven, C. and S. Mckeever, 1997. Theory of thermoluminescence and related phenomena.
#'
#' @examples
#' ##============================================================================##
#' ## Example 1: Simulate CW-OSL
#' ##============================================================================##
#' \dontrun{
#' run_MC_CW_OSL_DELOC(
#'  A = 0.12,
#'  R = 1,
#'  times = 0:100) %>%
#'    plot_RLumCarlo(legend = T)
#'
#' }
#'
#'#' @examples
#' ##============================================================================##
#' ## Example 2: Simulate CW-OSL DELOC with several parameter changes
#' ##============================================================================##
#' \dontrun{
#'
#'# define your parameters
#'A=c(0.1,0.3,0.5,1)
#'times=seq(0,60,1)
#'s=1e12
#'E=1
#'R<-c(1e-7,1e-6,0.01,0.1) # sequence of different R values
#'clusters=1000 # number of Monte Carlo simulations
#'N_e =c(200, 500, 700, 400) # number of free electrons
#'n_filled =c(200, 500, 100, 70) # number of filled traps
#'method="par"
#'output ="signal"
#'col=c(1,2,3,4) # ifferent colours for the individual curves
#'plot_uncertainty <- c(T,F,T,F)  # do you want to see the uncertainty?
#'add_TF <- c(F,rep(T, (length(R)-1)))
# loop to plot different curves into one plot
#'for (u in 1:length(R)){
#'  results <-run_MC_CW_OSL_DELOC(A=A[u], times, clusters =clusters, N_e = N_e[u],
#'                                 n_filled = n_filled[u], R=R[u], method = method, output = output)
#'  plot_RLumCarlo(results,add=add_TF[u],legend = F, col=col[u], main=" your plot")
#'}
# add your legend with your parameters
#'legend("topright",ncol=4,cex=0.55,title = "parameters" ,legend=c(paste0("A = ", A),
#'                                                                 paste0("n_filled = ", n_filled),
#'                                                                 paste0("N_e = ", N_e),
#'                                                                 paste0("R = ", R)),  text.col=col)
#'
#' }
#'
#'
#' @md
#' @export
run_MC_CW_OSL_DELOC <- function(
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
    stop("[run_MC_CW_OSL_DELOC()] Allowed keywords for 'method' are either 'par' or 'seq'!", call. = FALSE)

  if(!output %in% c("signal", "remaining_e"))
    stop("[run_MC_CW_OSL_DELOC()] Allowed keywords for 'output' are either 'signal' or 'remaining_e'!", call. = FALSE)

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

    results <- MC_C_CW_OSL_DELOC(
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

