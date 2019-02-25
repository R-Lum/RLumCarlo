#' Run Monte-Carlo simulation for isothermal measurements
#'
#' @param E [numeric] (**required**)
#'
#' @param s [numeric] (**required**)
#'
#' @param T [numeric] (**required**)
#'
#' @param rho [numeric] (**required**)
#'
#' @param times [numeric] (*with default*)
#'
#' @param clusters [numeric] (*with default*):
#'
#' @param r [numeric] (*with default*)
#'
#' @param N_e [numeric] (*with default*)
#'
#' @param method [character] (*with default*)
#'
#' @param output [character] (*with default*)
#'
#' @param \dots further arguments
#'
#' @return This function returns a list.
#'
#' @section Function version: 0.1.0
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany), Sebastian Kreutzer, IRAMAT-CRP2A,
#' UMR 5060, CNRS - Univerité Bordeaux Montaigne (France)
#'
#' @references
#' Pagonis 2017
#'
#' @examples
#' \dontrun{
#' ##============================================================================##
#' ## Example 1: Simulate isothermal measurement
#' ##============================================================================##
#'
#' times <- seq(0, 5000)
#' run_MC_ISO(
#'  E = 1.2,
#'  s = 1e10,
#'  T = 200,
#'  rho = 0.007,
#'  times = times) %>%
#'   calc_RLumCarlo() %>%
#'   plot_RLumCarlo(legend = T)
#'}
#' @md
#' @export
run_MC_ISO <- function(
  E,
  s,
  T = 200,
  rho,
  times,
  clusters = 10,
  r = NULL,
  N_e = 200,
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

  if(is.null(r)) r <- seq(from = 0, to = 2, by = 0.1)

  temp <- foreach(c = 1:clusters,
                  .packages = 'RLumCarlo',
                  .combine = 'comb_array',
                  .multicombine = TRUE) %dopar% {

    results <- MC_C_ISO(times = times,
             N_e = N_e,
             r = r,
             rho = rho,
             E = E,
             s = s,
             T = T
             )

    return(results[[output]])

  }  # end c-loop

  return(list(signal = temp,
              time = times))

}
