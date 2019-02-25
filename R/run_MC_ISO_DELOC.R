#' @title Run Monte-Carlo simulation for ISO for GOT model
#'
#' @description ##TODO
#'
#' @details
#'
#' \deqn{
#' I_{DELOC}(t) = -dn/dt = p(t) * (n^2 / (NR + n(1-R)))
#' }
#'
#' @param s [numeric] (*required*)
#'
#' @param E [numeric] (*required*)
#'
#' @param T [numeric] (*with default*)
#'
#' @param times [numeric] (with default)
#'
#' @param clusters [numeric] (with default):
#'
#' @param N_e [integer] (with default)
#'
#' @param n_filled [integer] (with default)
#'
#' @param R [numeric] (with default):
#'
#' @param method [character] (with default):
#'
#' @param output [character] (with default):
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
#' ##TODO
#'
#' @examples
#' ##============================================================================##
#' ## Example 1: Simulate ITL
#' ##============================================================================##
#' \dontrun{
#' run_MC_ISO_DELOC(
#'  s = 3.5e12,
#'  E = 1.45,
#'  T = 200,
#'  R = 1,
#'  times = 0:10000) %>%
#'    calc_RLumCarlo() %>%
#'    plot_RLumCarlo(legend = T)
#'
#' }
#'
#' @md
#' @export
run_MC_ISO_DELOC <- function(
  s,
  E,
  T = 20,
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

    results <- MC_C_ISO_DELOC(
      times = times,
      N_e = N_e,
      n_filled = n_filled,
      R = R,
      E = E,
      T = T,
      s = s)

    return(results[[output]])

  }  # end c-loop

  return(list(signal = temp,
              time = times))
}

