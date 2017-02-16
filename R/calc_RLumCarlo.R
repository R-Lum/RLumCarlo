#' Plot results from Monte-Carlo simulations with RLumCarlo
#'
#' @param results \code{\link{array}}:
#'
#' @return This function returns a \code{\link{data.frame}}
#'
#' @section Function version: 0.0.1 [2017-01-27]
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany)
#'
#' @export
calc_RLumCarlo <- function(results){

  signal <- results[["signal"]]
  times <-   results[["time"]]

  if(length(dim(results)) == 2) {


      sum_signal <- vapply(1:length(times), function(x){
        sum(signal[x,])
      }, FUN.VALUE = 1)

      avg <- y_min <- y_max <- sum_signal


    } else {

      clusters <- dim(signal)[3]

      sum_signal <- sapply(1:clusters, function(y){

        vapply(1:length(times), function(x){
          sum(signal[x,,y])
        }, FUN.VALUE = 1)

      })

      avg <- rowMeans(sum_signal)
      y_min <- apply(sum_signal, 1, min)
      y_max <- apply(sum_signal, 1, max)
    }

  return(data.frame(avg = avg, y_min = y_min, y_max = y_max, time = times))
}
