#' @title Run Monte-Carlo simulation for CW-IRSL for GOT model
#'
#' @description Runs a Monte Carlo (MC) simulation of constant wave _ (CW-IRSL) using the generalized one trap (GOT) model.
#'
#' @details
#'
#' \deqn{
#' I_{DELOC}(t) = -dn/dt = p(t) * (n^2 / (NR + n(1-R)))
#' }
#'
#' @param A [numeric] (**required**): The transition probability in cm^3/s
#'
#' @param times [numeric] (with default): The number of MC runs
#'
#' @param clusters [numeric] (with default): The number of clusters.
#'
#' @param N_e [integer] (with default): The number of electrons.
#'
#' @param n_filled [integer] (with default): The number of electron traps that are filled at the beginning of the simulation.
#'
#' @param R [numeric] (with default): The retrapping ratio
#'
#' @param method [character] (with default):
#'
#' @param output [character] (with default):
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
#' ##TODO
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

