#' @title Run Monte-Carlo Simulation for ISO-TL (delocalized transitions)
#'
#' @description Runs a Monte-Carlo (MC) simulation of isothermally stimulated luminescence
#' (ISO-TL or ITL) using the one trap one recombination centre (OTOR) model.
#' Delocalised refers to involvement of the conduction band.
#'
#' @details
#'
#'  **The model**
#'
#' \deqn{
#' I_{DELOC}(t) = -dn/dt = (s * exp(-E/(k_{B} * T_{ISO}))) * (n^2 / (N*R + n(1-R)))
#' }
#'
#' Where in the function: \cr
#' t := time \cr
#' \eqn{k_{B}} := Boltzmann constant (8.617 x 10^-5 eV K^-1) \cr
#' \eqn{T_{ISO}} = temperature of the isothermal experiment (°C) \cr
#'  n := `n_filled`, the number of filled electron traps at the beginning of the simulation\cr
#'  E := the trap depth (eV) \cr
#'  s := the frequency factor in (s^-1) \cr
#'  N := `N_e`, the total number of electron traps available (dimensionless) \cr
#'  R := the retrapping ratio for delocalized transitions
#'
#' @param E [numeric] (**required**): Thermal activation energy of the trap (eV)
#'
#' @param s [numeric] (**required**): The frequency factor of the trap (s^-1)
#'
#' @param T [numeric] (*with default*): Constant stimulation temperature (°C)
#'
#' @param times [numeric] (*with default*): The sequence of time steps within the simulation (s)
#'
#' @param clusters [numeric] (*with default*): The number of created clusters for the MC runs. The input can be the output of [create_ClusterSystem]. In that case `n_filled` indicate absolute numbers of a system.
#'
#' @param N_e [integer] (*with default*): The total number of electron traps available (dimensionless). Can be a vector of `length(clusters)`, shorter values are recycled.
#'
#' @param n_filled [integer] (*with default*): The number of filled electron traps at the
#' beginning of the simulation (dimensionless). Can be a vector of `length(clusters)`, shorter values are recycled.
#'
#' @param R [numeric] (**required**): The delocalized retrapping ratio (dimensionless)
#'
#' @param method [character] (*with default*): Sequential `'seq'` or parallel `'par'`processing. In
#' the parallel mode the function tries to run the simulation on multiple CPU cores (if available) with
#' a positive effect on the computation time.
#'
#' @param output [character] (*with default*): Output is either the `'signal'` (the default) or
#' `'remaining_e'` (the remaining charges, electrons, in the trap)
#'
#' @param \dots further arguments, such as `cores` to control the number of used CPU cores or `verbose` to silence the terminal
#'
#' @return This function returns an object of class `RLumCarlo_Model_Output` which
#' is a [list] consisting of an [array] with dimension length(times) x clusters
#' and a [numeric] time vector.
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @references
#'
#' Pagonis, V., Friedrich, J., Discher, M., Müller-Kirschbaum, A., Schlosser, V.,
#' Kreutzer, S., Chen, R. and Schmidt, C., 2019. Excited state luminescence signals
#' from a random distribution of defects: A new Monte Carlo simulation approach for feldspar.
#' Journal of Luminescence 207, 266–272. \doi{10.1016/j.jlumin.2018.11.024}
#'
#' **Further reading**
#'
#' Chen, R., McKeever, S.W.S., 1997. Theory of Thermoluminescence and Related Phenomena.
#' WORLD SCIENTIFIC. \doi{10.1142/2781}
#'
#' @examples
#' run_MC_ISO_DELOC(
#'  s = 3.5e12,
#'  E = 1.45,
#'  T = 200,
#'  R = 1,
#'  method = 'seq',
#'  times = 0:100) %>%
#' plot_RLumCarlo(legend = TRUE)
#'
#' @keywords models data
#' @encoding UTF-8
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

# Integrity checks ----------------------------------------------------------------------------
  if(!output %in% c("signal", "remaining_e"))
    stop("[run_MC_ISO_DELOC()] Allowed keywords for 'output' are either 'signal' or 'remaining_e'!", call. = FALSE)

# Register multi-core back end ----------------------------------------------------------------
cl <- .registerClusters(method, ...)
on.exit(parallel::stopCluster(cl))

# Enable dosimetric cluster system -----------------------------------------
if(class(clusters)[1] == "RLumCarlo_ClusterSystem"){
  n_filled <- .distribute_electrons(
    clusters = clusters,
    N_system = n_filled[1])[["e_in_cluster"]]
  clusters <- clusters$cl_groups

}

# Expand parameters -------------------------------------------------------
n_filled <- rep(n_filled, length.out = max(clusters))
N_e <- rep(N_e, length.out = max(clusters))

# Run model -----------------------------------------------------------------------------------
  temp <- foreach(c = 1:max(clusters),
                  .packages = 'RLumCarlo',
                  .combine = 'comb_array',
                  .multicombine = TRUE) %dopar% {

    results <- MC_C_ISO_DELOC(
      times = times,
      N_e = N_e[c],
      n_filled = n_filled[c],
      R = R[1],
      E = E[1],
      T = T[1],
      s = s[1])

    return(results[[output]])

  }  # end c-loop

# Return --------------------------------------------------------------------------------------
.return_ModelOutput(signal = temp, time = times)
}
