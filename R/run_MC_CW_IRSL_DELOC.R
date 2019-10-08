#' @title Run Monte-Carlo simulation for CW-IRSL for delocalized transition
#'
#' @description Runs a Monte-Carlo (MC) simulation of constant wave infrared stimulated luminesence (CW-IRSL) using the one trap one recombination center (OTOR) model.
#'
#' @details
#'
#' \deqn{
#' I_{DELOC}(t) = -dn/dt = p(t) * (n^2 / (NR + n(1-R)))
#' }
#'
#' @param A [numeric] (**required**): The transition probability (cm^3/s).
#'
#' @param times [numeric] (*with default*): The number of MC runs.
#'
#' @param clusters [numeric] (*with default*): The number of clusters.
#'
#' @param N_e [integer] (*with default*): The number of electrons.
#'
#' @param n_filled [integer] (*with default*): The number of electron traps that are filled at the beginning of the simulation.
#'
#' @param R [numeric] (*with default*): The retrapping ratio.
#'
#' @param method [character] (*with default*):
#'
#' @param output [character] (*with default*):
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
#' Pagonis, V. and Kulp, C. (2017) ‘Monte Carlo simulations of tunneling phenomena and nearest neighbor hopping mechanism in feldspars’, Journal of Luminescence. Elsevier, 181, pp. 114–120. doi: 10.1016/j.jlumin.2016.09.014.
#'
#' @examples
#' ##============================================================================##
#' ## Example 1: Simulate CW-IRSL
#' ##============================================================================##
#' \dontrun{
#' run_MC_CW_IRSL_DELOC(
#'  A = 0.12,
#'  R = 1,
#'  times = 0:100) %>%
#'    plot_RLumCarlo(legend = T)
#'
#' }
#'
#'#' @examples
#' ##============================================================================##
#' ## Example 2: Simulate CW-IRSL DELOC with several parameter changes
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
#'col=c(1,2,3,4) # ifferent colours for the individual curves 
#'plot_uncertainty <- c(T,F,T,F)  # do you want to see the uncertainty?
#'add_TF <- c(F,rep(T, (length(R)-1)))
# loop to plot different curves into one plot
#'for (u in 1:length(R)){
#'  results <-run_MC_CW_IRSL_DELOC(A=A[u], times, clusters =clusters, N_e = N_e[u],
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
run_MC_CW_IRSL_DELOC <- function(
  A,
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

    results <- MC_C_CW_IRSL_DELOC(
      times = times,
      N_e = N_e,
      n_filled = n_filled,
      R = R,
      A = A)

    return(results[[output]])

  }  # end c-loop

  ## return model output
  .return_ModelOutput(signal = temp, time = times)
}

