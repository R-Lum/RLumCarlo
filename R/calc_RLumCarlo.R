#' Plot results from Monte-Carlo simulations with RLumCarlo
#'
#' @param results [list] of class RLumCarlo_Model_Output: RLumCarlo simulation output object
#' produced by all
#'
#' @return This function returns a [data.frame]
#'
#' @section Function version: 0.1.0
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany), Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS-Université Bordeaux Montagine, France
#'
#' @md
#' @export
calc_RLumCarlo <- function(results){

  # Pre-checks ----------------------------------------------------------------------------------
  if(class(results) != "RLumCarlo_Model_Output")
    stop("[calc_RLumCarlo()] Input must be of class 'RLumModel_Model_Output' created by functions starting with 'run_'!",
         call. = FALSE)

  # copy input into new objects
  signal <- results[[1]]
  times <- results[[2]]

  # melt objects
  if(length(dim(results)) == 2) {
      sum_signal <- vapply(1:length(times), function(x){
        sum(signal[x,])
      }, FUN.VALUE = 1)

      avg <- y_min <- y_max <- sum_signal

    } else {
      ## extract number of clusters
      clusters <- dim(signal)[3]

      ##get sum signal
      sum_signal <- sapply(1:clusters, function(y){
        vapply(1:length(times), function(x){
          sum(signal[x,,y])
        }, FUN.VALUE = 1)

      })

      avg <- rowMeans(sum_signal)
      y_min <- apply(sum_signal, 1, min)
      y_max <- apply(sum_signal, 1, max)

    }

  ## set output data.frame
  output <- data.frame(time = times, avg = avg, y_min = y_min, y_max = y_max)
  attr(output, "model") <- attributes(results)$model

  ## return
  return(output)
}
