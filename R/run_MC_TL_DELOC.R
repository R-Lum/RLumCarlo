#' @title Run Monte-Carlo simulation for TL for delocalized transition
#'
#' @description Runs a Monte-Carlo (MC) simulation of thermo-luminesence (TL) using the one trap one recombination center (OTOR) model. Delocalized refers to involvement of the conduction band.
#'
#' @details
#'
#' \deqn{
#' I_{DELOC}(t) = -dn/dt = p(t) * (n^2 / (NR + n(1-R)))
#' }
#'  
#' where in the function `N` := `N_e`:= `n` :=`n_filled`
#' @param s [numeric] (**required**): Escape frequency of the trap (s^-1).
#'
#' @param E [numeric] (**required**): Thermal activation energy of the trap (eV).
#'
#' @param times [numeric] (*with default*): the number of MC runs.
#'
#' @param clusters [numeric] (*with default*): the number of clusters.
#'
#' @param N_e [integer] (*with default*): The number of electrons.
#'
#' @param n_filled [integer] (*with default*): The number of electron traps that are filled at the beginning of the simulation.
#'
#' @param R [numeric] (*with default*): The retrapping ratio.
#'
#' @param method [character] (*with default*): ##TODO
#'
#' @param output [character] (*with default*): ##TODO
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
#' ## Example 1: Simulate TL
#' ##============================================================================##
#' \dontrun{
#' run_MC_TL_DELOC(
#'  s = 3.5e12,
#'  E = 1.45,
#'  R = 1,
#'  times = 100:450) %>%
#'    plot_RLumCarlo(legend = T)
#'
#' }
#' 
#' #' @examples
#' ##============================================================================##
#' ## Example 2: Plot multiple TL stimulation TL curves in R with varying params
#' ##============================================================================##
#' \dontrun{
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

  ## register backend ----
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
  ## -----

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

  ## return model output
  .return_ModelOutput(signal = temp, time = times)
}

