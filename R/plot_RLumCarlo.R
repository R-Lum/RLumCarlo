#' Plot results from Monte-Carlo simulations with RLumCarlo
#'
#' @param results \code{\link{data.frame}}
#' @param times \code{\link{vector}} (with default):
#' @param norm \code{\link{character}} (with default):
#' @param legend \code{\link{logical}} (with default):
#' @param add \code{\link{logical}} (with default):
#' @param \dots further arguments
#'
#' @return This function returns a graphical output
#'
#' @section Function version: 0.0.1 [2017-01-27]
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany)
#'
#' @export
plot_RLumCarlo <- function(
  results,
  times = NULL,
  norm = FALSE,
  legend = FALSE,
  add = FALSE,
  ...){

  avg <- results[,"avg"]
  y_min <- results[,"y_min"]
  y_max <- results[,"y_max"]

  if(is.null(times)) times <- results[,"time"]

  if(norm){ # normalization
    avg <- avg/max(avg)
    y_min  <-  y_min/max(y_min)
    y_max <- y_max/max(y_max)
    ylab = "normalized average signal"
  } else {

    ylab = "average signal [a.u.]"
  }

  if(add == FALSE){

    plot(
      x = times,
      y = avg,
      xlab = "Time [s]",
      ylab = ylab,
      type = "l",
      ylim = c(0, max(y_max)),
      lwd = 2,
      ...)
  } else {

    lines(
      x = times,
      y = avg,
      xlab = "Time [s]",
      ylab = ylab,
      type = "l",
      ylim = c(0, max(y_max)),
      lwd = 2,
      ...)
  }

  polygon(x = c(times, rev(times)),
          y = c(y_min, rev(y_max)),
          col = adjustcolor("red",alpha.f=0.5),
          border = "red")

  if(legend){
    legend("topright",
           legend = c("average", "min-max"),
           lwd = 1,
           col = c("black", "red"))
  }

}
