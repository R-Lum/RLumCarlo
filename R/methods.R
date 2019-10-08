#' @title Summarize RLumCarlo Modelling Results
#'
#' @description Summarize RLumCarlo Modelling results, so that they can be plotted easily
#'
#' @param object [list] of class RLumCarlo_Model_Output: RLumCarlo simulation output object
#' produced by all
#'
#' @param ... further arguments passed to the method
#'
#' @return This function returns a [data.frame]
#'
#' @section Function version: 0.1.0
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany), Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS-Université Bordeaux Montagine, France
#'
#' @method summary RLumCarlo_Model_Output
#' @md
#' @export
summary.RLumCarlo_Model_Output <- function(object, ...){

  # copy input into new objects
  signal <- object[[1]]
  times <- object[[2]]

  # melt objects
  if(length(dim(object)) == 2) {
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
  attr(output, "model") <- attributes(object)$model

  ## return
  return(output)
}
