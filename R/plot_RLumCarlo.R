#' @title Plot results from Monte-Carlo simulations with RLumCarlo
#'
#'
#' @param results [data.frame] (**required**)
#'
#' @param times [numeric] (*optinal*): Optional vector for the x-axis
#'
#' @param plot_uncertainty [logical] (*with default*): Enable/disable uncertainty polygon plot
#'
#' @param norm  [logical] (*with default*): Normalise curve to the highest intensity
#'
#' @param add [logical] (*with default*): allow overplotting of results
#'
#' @param \dots further arguments that can be passed to control the plot output. Currently supported
#' are: `xlab`, `xlim`, `ylim`, `main`, `lwd`, `type`, `pch`, `lty`,`col`, `grid`, `legend`
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
  plot_uncertainty = "range",
  norm = FALSE,
  add = FALSE,
  ...){


 # Preparation ---------------------------------------------------------------------------------

 ## try to summarise whenenver possible
 if(class(results) == "RLumCarlo_Model_Output")
    results <- summary(results, verbose = FALSE)

 ## extract correct columns
 avg <- results[["mean"]]
 y_min <- y_max <- NULL

 ## extract values for the uncertainties
 if(!is.null(plot_uncertainty)){
  if(plot_uncertainty == "sd") {
    y_min <-  avg -  results[["sd"]] / 2
    y_max <-  avg +  results[["sd"]] / 2


  } else if (plot_uncertainty == "var") {
    y_min <-  avg -  results[["var"]] / 2
    y_max <-  avg +  results[["var"]] / 2


  } else {
    y_min <- results[["y_min"]]
    y_max <- results[["y_max"]]

  }

 }

  if(is.null(times))
    times <- results[["time"]]

  if(norm){ # normalization
    avg <- avg / max(avg)

    if(!is.null(plot_uncertainty)){
      y_min <- y_min / max(y_min)
      y_max <- y_max / max(y_max)

    }

    ylab <- "Normalized average signal"
  } else {
    ylab <- "Averaged signal [a.u.]"

  }

  ##default plot settings
  plot_settings <-
    modifyList(x = list(
      main = "",
      xlim = range(times),
      ylim = if(is.null(y_max)) c(0, max(avg)) else c(0, max(y_max)),
      xlab = if(length(grep(pattern = "TL", attributes(results)$model, fixed = TRUE) == 1)) {
        "Temperature [\u00b0 C]"
      } else {
        "Time [s]"
      },
      ylab = ylab,
      type = "l",
      lwd = 2,
      pch = 1,
      lty = 1,
      grid = TRUE,
      col = "red",
      legend = TRUE
    ), val = list(...))


  # Plotting ------------------------------------------------------------------------------------
  ## check if plot was already called, if not just plot
    if(add == FALSE || is.null(tryCatch(par(new =TRUE), warning = function(w) NULL))){
      plot(NA, NA,
        main = plot_settings$main,
        xlab = plot_settings$xlab,
        ylab = plot_settings$ylab,
        type = plot_settings$type,
        xlim = plot_settings$xlim,
        ylim = plot_settings$ylim
      )

    }

  ##add grid
  if(plot_settings$grid) grid()

  ## draw error pologyon
  if(!is.null(plot_uncertainty)){
  polygon(x = c(times, rev(times)),
          y = c(y_min, rev(y_max)),
          col = adjustcolor(plot_settings$col, alpha.f=0.3),
          border = NA)

  }

  ## add average lines
  lines(
    x = times,
    y = avg,
    main = plot_settings$main,
    xlab = plot_settings$xlab,
    ylab = ylab,
    col = adjustcolor(plot_settings$col, alpha.f=1),
    type = plot_settings$type,
    pch = plot_settings$pch,
    lty = plot_settings$lty,
    xlim = plot_settings$xlim,
    ylim = plot_settings$ylim,
    lwd = plot_settings$lwd)

  if(plot_settings$legend && !is.null(plot_uncertainty)){
    legend(
      "topright",
      legend = c("mean", plot_uncertainty),
      lwd = 1,
      bty = "n",
      col = c(
        adjustcolor(plot_settings$col, alpha.f = 1),
        adjustcolor(plot_settings$col, alpha.f = 0.3)
      )
    )
  }

}
