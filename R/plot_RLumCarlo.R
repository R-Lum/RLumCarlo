#' Plot results from Monte-Carlo simulations with RLumCarlo
#'
#' @param results [data.frame] (**required**)
#'
#' @param times [numeric] (*optinal*): Optional vector for the x-axis
#'
#' @param norm  [logical] (*with default*): Normalise curve to the highest intensity
#'
#' @param legend [logical] (*with default*): Enable/disable legend
#'
#' @param add [logical] (*with default*): allow overplotting of results
#'
#' @param \dots further arguments that can be passed to control the plot output. Currently supported
#' are: `xlab`, `xlim`, `ylim`, `main`, `lwd`, `type`, `col`, `grid`
#'
#' @return This function returns a graphical output
#'
#' @section Function version: 0.1.0
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany), Sebastian Kreutzer, IRAMAT-CRP2A, Université
#' Bordeaux Montaigne (France)
#'
#' @md
#' @export
plot_RLumCarlo <- function(
  results,
  times = NULL,
  norm = FALSE,
  legend = FALSE,
  add = FALSE,
  ...){


 # Preset --------------------------------------------------------------------------------------
  avg <- results[["avg"]]
  y_min <- results[["y_min"]]
  y_max <- results[["y_max"]]

  if(is.null(times))
    times <- results[["time"]]

  if(norm){ # normalization
    avg <- avg/max(avg)
    y_min  <-  y_min/max(y_min)
    y_max <- y_max/max(y_max)
    ylab = "Normalized average signal"
  } else {
    ylab = "Averaged signal [a.u.]"
  }

  ##default plot settings
  plot_settings <-
    modifyList(x = list(
      main = "",
      xlim = range(times),
      ylim = c(0, max(y_max)),
      xlab = if(length(grep(pattern = "TL", attributes(results)$model, fixed = TRUE) == 1)) {
        "Temperature [a.u.]"
      } else {
        "Time [s]"
      },
      type = "l",
      lwd = 2,
      grid = TRUE,
      col = "red"
    ), val = list(...))

  # Plotting ------------------------------------------------------------------------------------
  ## check if plot was already called, if not just plot
    if(add == FALSE || is.null(tryCatch(par(new =TRUE), warning = function(w) NULL))){
      plot(NA, NA,
        main = plot_settings$main,
        xlab = plot_settings$xlab,
        ylab = ylab,
        type = plot_settings$type,
        xlim = plot_settings$xlim,
        ylim = plot_settings$ylim
      )

    }

  ##add grid
  if(plot_settings$grid) grid()

  ## draw error pologyon
  polygon(x = c(times, rev(times)),
          y = c(y_min, rev(y_max)),
          col = adjustcolor(plot_settings$col, alpha.f=0.2),
          border = NA)

  ## add average lines
  lines(
    x = times,
    y = avg,
    main = plot_settings$main,
    xlab = plot_settings$xlab,
    ylab = ylab,
    col = adjustcolor(plot_settings$col, alpha.f=1),
    type = plot_settings$type,
    xlim = plot_settings$xlim,
    ylim = plot_settings$ylim,
    lwd = plot_settings$lwd)

  if(legend){
    legend(
      "topright",
      legend = c("average", "min-max"),
      lwd = 1,
      bty = "n",
      col = c(
        adjustcolor(plot_settings$col, alpha.f = 1),
        adjustcolor(plot_settings$col, alpha.f = 0.3)
      )
    )
  }

}
