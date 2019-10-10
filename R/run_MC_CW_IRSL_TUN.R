#' @title Run Monte-Carlo simulation for CW-IRSL
#'
#' @description Runs a Monte-Carlo (MC) simulation of constant wave infrared stimulated luminesence (CW-IRSL) using the model. Tunneling refers to the direct movement of electrons from a trap directly to the recombination center.
#'
#' @details
#' \deqn{
#' p(t) = A * e^{(-r' / \rho^{(-1 / 3)})}
#' }
#'
#' \deqn{
#' I_{TUN}(t) = 3 * n * p(t) *  (r')^2 * e^{(-r'^3)}
#' }
#'
#'Where in the function: \cr
#' `p(t)` := The experimental stimulation mode \cr
#' `e`:= Exponentional function \cr
#'  r' := `r` \cr
#'  \eqn{\rho}' := `rho` \cr
#'  `t` := `Time` \cr
#'  `n` := The Instantaneous number of electrons
#'
#' @param A [numeric] (**required**): The optical excitation rate from ground state of trap to excited state of trap (s^-1).
#'
#' @param rho [numeric] (**required**): The density of recombination centers (defined as rho' in Huntley 2006) (unitless).
#'
#' @param times [numeric] (*with default*): The sequence of temperature steps within the simulation (s).
#'
#' @param clusters [numeric] (*with default*): The number of MC runs (unitless).
#'
#' @param N_e [numeric] (*width default*): The total number of electron traps available (unitless).
#'
#' @param r_c [numeric] (*with default*): The retrapping ratio.
#'
#' @param delta.r [numeric] (*with default*):
#'
#' @param r [numeric] (*with default*): The radius of tunneling (unitless).
#'
#' @param method [character] (*with default*): sequential `'seq'` or parallel processing `'par'`
#'
#' @param output [character] (*with default*): output is either the `'signal'` (the default) or `'remaining_e'` (the remaining
#' charges, electrons, in the trap)
#'
#' @param \dots further arguments
#'
#' @return This function returns a list.
#'
#' @section Function version: 0.2.0
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany), Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, Université Bordeaux Montaigne (France)
#'
#' @references
#' Huntley, D.J., 2006. An explanation of the power-law decay of luminescence. Journal of Physics: Condensed Matter, 18(4), 1359.\doi{10.1088/0953-8984/18/4/020}
#'
#' Pagonis, V., Friedrich, J., Discher, M., Müller-Kirschbaum, A., Schlosser, V., Kreutzer, S., Chen, R. and Schmidt, C., 2019. Excited state luminescence signals from a random distribution of defects: A new Monte Carlo simulation approach for feldspar. Journal of Luminescence 207, 266–272. \doi{10.1016/j.jlumin.2018.11.024}
#'
#' Reuven, C. and S. Mckeever, 1997. Theory of thermoluminescence and related phenomena.
#'
#' @examples
#' \dontrun{
#'
#' ##============================================================================##
#' ## Example 1: Simulate CW-IRSL measurement
#' ##============================================================================##
#'
#' run_MC_CW_IRS_TUNL(A = 0.12, rho = 0.003, times = 0:1000) %>%
#'     plot_RLumCarlo(norm = T, legend = T)
#'}
#' @md
#' @export
run_MC_CW_IRSL_TUN <- function(
  A,
  rho,
  times,
  clusters = 10,
  r_c = 0,
  delta.r = 0.1,
  N_e = 200,
  method = "seq",
  output = "signal",
  ...){

# Integrity checks ----------------------------------------------------------------------------
  if(!method %in% c("par", "seq"))
    stop("[run_MC_CW_IRSL_TUN()] Allowed keywords for 'method' are either 'par' or 'seq'!", call. = FALSE)

  if(!output %in% c("signal", "remaining_e"))
    stop("[run_MC_CW_IRSL_TUN()] Allowed keywords for 'output' are either 'signal' or 'remaining_e'!", call. = FALSE)

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


# Setting parameters --------------------------------------------------------------------------
r <- seq(abs(r_c[1]), 2, abs(delta.r[1]))

# Run model -----------------------------------------------------------------------------------
temp <- foreach(
  c = 1:clusters,
  .packages = 'RLumCarlo',
  .combine = 'comb_array',
  .multicombine = TRUE
) %dopar% {
  results <- MC_C_CW_IRSL_TUN(
    times = times,
    N_e = N_e,
    r = r,
    rho = rho,
    A = A
  )

  return(results[[output]])

}  # end c-loop

# Return --------------------------------------------------------------------------------------
.return_ModelOutput(signal = temp, time = times)
}


