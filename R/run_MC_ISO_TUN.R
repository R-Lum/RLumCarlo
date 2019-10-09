#' @title Run Monte-Carlo simulation for isothermal measurements for tunneling transition
#'
#' @description Runs a Monte-Carlo (MC) simulation of isothermally stimulated luminesence (ISO-TL or ITL) using the tunneling (TUN) model. Tunneling transitions refers to the direct movement of electrons from a trap directly to the recombination center (*without passing go or collecting $200*).
#'
#' @details
#'
#' \deqn{
#' I_{TUN}(t) = -dn/dt = A * (n^2 / (r + n))
#' }
#'
#' Where in the function `n` := `n_filled` := `N` := `N_e` := `rho` := `\rho'` := `r_c` := `\rho'_c`
#' @param s [numeric] (**required**): Escape frequency of the trap (s^-1).
#'
#' @param E [numeric] (**required**): Thermal activation energy of the trap (eV).
#'
#' @param T [numeric] (**required**): Temperature (deg. C).
#'
#' @param rho [numeric] (**required**): The calculated dimesionless Charge density (normally written Rho').
#'
#' @param times [numeric] (*with default*): The number of MC runs.
#'
#' @param clusters [numeric] (*with default*): The number of clusters.
#'
#' @param r [numeric] (*with default*): The retrapping ratio.
#'
#' @param N_e [numeric] (*with default*): The number of electrons. 
#'
#' @param method [character] (*with default*): ##TODO
#'
#' @param output [character] (*with default*): ##TODO
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
#' 
#' Pagonis, V. and Kulp, C., 2017. Monte Carlo simulations of tunneling phenomena and nearest neighbor hopping mechanism in feldspars. Journal of Luminescence 181, 114–120. \doi{10.1016/j.jlumin.2016.09.014}
#' 
#' Pagonis, V., Friedrich, J., Discher, M., Müller-Kirschbaum, A., Schlosser, V., Kreutzer, S., Chen, R. and Schmidt, C., 2019. Excited state luminescence signals from a random distribution of defects: A new Monte Carlo simulation approach for feldspar. Journal of Luminescence 207, 266–272. \doi{10.1016/j.jlumin.2018.11.024}
#' 
#' for a discussion of tunneling see:
#' Aitken, M.J., 1985. Thermoluminescence dating. 276-280. \doi{10.1002/gea.3340020110}
#'
#' @examples
#' \dontrun{
#' ##============================================================================##
#' ## Example 1: Simulate isothermal measurement
#' ##============================================================================##
#'
#' times <- seq(0, 5000)
#' run_MC_ISO_TUN(
#'  E = 1.2,
#'  s = 1e10,
#'  T = 200,
#'  rho = 0.007,
#'  times = times) %>%
#'   plot_RLumCarlo(legend = T)
#'}
#' @md
#' @export
run_MC_ISO_TUN <- function(
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
  cores <- parallel::detectCores()
  if(cores == 1) method <- "seq"

  if(method != "par"){
    cl <- parallel::makeCluster(1)
    doParallel::registerDoParallel(cl)
    ##ensures that we do not have any particular problems
    registerDoSEQ()
    on.exit(parallel::stopCluster(cl))

  } else {

    cl <- parallel::makeCluster(cores-1)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))
  }
  ## -----

  if(is.null(r)) r <- seq(from = 0, to = 2, by = 0.1)

  temp <- foreach(c = 1:clusters,
                  .packages = 'RLumCarlo',
                  .combine = 'comb_array',
                  .multicombine = TRUE) %dopar% {

    results <- MC_C_ISO_TUN(times = times,
             N_e = N_e,
             r = r,
             rho = rho,
             E = E,
             s = s,
             T = T
             )

    return(results[[output]])

  }  # end c-loop

  ## return model output
  .return_ModelOutput(signal = temp, time = times)

}
