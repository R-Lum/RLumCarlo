#' Run Monte-Carlo simulation for TL
#'
#' @param s \code{\link{list}}
#' @param E \code{\link{numeric}}
#' @param rho \code{\link{numeric}}
#' @param r_c \code{\link{numeric}} (with default)
#' @param times \code{\link{vector}} (with default)
#' @param clusters \code{\link{numeric}} (with default):
#' @param N_e \code{\link{numeric}} (with default):
#' @param delta.r \code{\link{numeric}} (with default):
#' @param method \code{\link{character}} (with default):
#' @param output \code{\link{character}} (with default):
#' @param \dots further arguments
#'
#' @return This function returns an \code{\link{array}} with dimension length(times) x length(r) x clusters
#'
#' @section Function version: 0.0.1 [2017-01-27]
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany)
#'
#' @references
#' Pagonis 2017
#'
#' @examples
#' \dontrun{
#' ##============================================================================##
#' ## Example 1: Simulate TL measurement
#' ##============================================================================##
#'
#' times <- seq(200, 500) # time = temperature
#'
#' run_MC_TL(s = 3.5e12,
#'           E = 1.45,
#'           rho = 0.015,
#'           r_c = 0.85,
#'           times = times) %>%
#'   calc_RLumCarlo() %>%
#'     plot_RLumCarlo(legend = T)
#'}
#' @export
run_MC_TL <- function(
  s,
  E,
  rho,
  r_c,
  times,
  clusters = 10,
  N_e = 200,
  delta.r = 0.1,
  method = "par",
  output = "signal",
  ...){

  r <- seq(r_c, 2, delta.r)

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

    results <- MC_C_TL(times = times,
            N_e = N_e,
            r = r,
            rho = rho,
            E = E,
            s = s)

    return(results[[output]])

  }  # end c-loop

  return(list(signal = temp,
              time = times))
}
