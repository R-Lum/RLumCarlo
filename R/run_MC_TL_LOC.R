#' @title Run Monte-Carlo simulation for TL for localised transition
#'
#' @description ##TODO
#'
#' @details
#'
#' \deqn{
#' I_{LOC}(t) = -dn/dt = p(t) * (n^2 / (r + n))
#' }
#'
#' @param s [numeric] (*required*)
#'
#' @param E [numeric] (*required*)
#'
#' @param times [numeric] (with default)
#'
#' @param clusters [numeric] (with default):
#'
#' @param n_filled [integer] (with default)
#'
#' @param r [numeric] (with default):
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
#' ## Example 1: Simulate TL
#' ##============================================================================##
#' \dontrun{
#' run_MC_TL_LOC(
#'  s = 3.5e12,
#'  E = 1.45,
#'  r = 1,
#'  times = 100:450) %>%
#'    calc_RLumCarlo() %>%
#'    plot_RLumCarlo(legend = T)
#'
#' }
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

    results <- MC_C_TL_LOC(
      times = times,
      n_filled = n_filled,
      r = r,
      E = E,
      s = s)

    return(results[[output]])

  }  # end c-loop

  return(list(signal = temp,
              time = times))
}

