#' methods_RLumCarlo
#'
#' @keywords internal
#' @md
#' @name methods_RLumCarlo
NULL

#' @title Summarize RLumCarlo Modelling Results
#'
#' @description Summarize RLumCarlo modelling results for easy plotting
#'
#' @param object [list] of class RLumCarlo_Model_Output: RLumCarlo simulation output object
#' produced by all `run_` functions
#'
#' @param verbose [logical] (*with default*): enable/disable verbose mode
#'
#' @param ... further arguments passed to the method
#'
#' @return This function returns a [data.frame]
#'
#' @section Function version: 0.1.0
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany),
#' Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#' @md
#' @method summary RLumCarlo_Model_Output
#' @rdname methods_RLumCarlo
#' @export
summary.RLumCarlo_Model_Output <- function(object, verbose = FALSE, ...){

  # copy input into new objects
  signal <- object[[1]]
  times <- object[[2]]

  # melt objects
  # the first case is the 1-cluster case
  if(length(dim(object$signal)) == 2) {
      sum_signal <- vapply(1:length(times), function(x){
        sum(signal[x,])
      }, FUN.VALUE = 1)

      mean <- y_min <- y_max <- sd <- var <- sum_signal

    } else {
      ## extract number of clusters
      clusters <- dim(signal)[3]

      ##get sum signal
      sum_signal <- sapply(1:clusters, function(y){
        vapply(1:length(times), function(x){
          sum(signal[x,,y])
        }, FUN.VALUE = 1)

      })

      mean <- rowMeans(sum_signal)
      sd <- apply(sum_signal, 1, sd)
      var <- apply(sum_signal, 1, var)
      y_min <- apply(sum_signal, 1, min)
      y_max <- apply(sum_signal, 1, max)

    }


  ## set output data.frame
  output <- data.frame(time = times, mean = mean, y_min = y_min, y_max = y_max, sd = sd, var = var)
  attr(output, "model") <- attributes(object)$model

  ## return the summary as terminal output from the data.frame
  if(verbose) print(summary(output))

  ##add class
  class(output) <- c("data.frame", class(object))

  ## return
  invisible(output)
}
