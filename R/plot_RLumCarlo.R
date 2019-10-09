#' @title Plot results from Monte-Carlo simulations with RLumCarlo
#'
#'
#' @param object [data.frame] (**required**)
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
  object,
  times = NULL,
  plot_uncertainty = "range",
  norm = FALSE,
  add = FALSE,
  ...){


 # Self-call -----------------------------------------------------------------------------------
 if(class(object) == "list"){

    ## double check the objects in the list
    if(!all(sapply(object, class) == "RLumCarlo_Model_Output"))
      stop("[plot_RLumCarlo()] At least one element in the list is not of class RLumCarlo_Model_Output!", call. = FALSE)

    ##make sure the plot_settings do not interfere
    plot_settings <- list(...)

    ## overwrite or extract col
    if("col" %in% plot_settings){
      col <- rep(plot_settings$col, length.out = length(object))
      plot_settings$col <- NULL

    } else {
     col <- 2:(length(object) + 1)

    }

    ## set add
    add <- c(FALSE, rep(TRUE,length(object) - 1))

    for(i in 1:length(object)){
      do.call(what = plot_RLumCarlo, args = c(list(
        object[[i]],
        times = times,
        plot_uncertainty = plot_uncertainty,
        norm = norm,
        col = col[i],
        add = add[i]),
        plot_settings
      ))

    }

   return(invisible(NULL))
 }

 # Preparation ---------------------------------------------------------------------------------

 ## try to summarise whenenver possible
 if(class(object) != "RLumCarlo_Model_Output")
   stop("[plot_RLumCarlo()] 'object' needs to be of class RLumCarlo_Model_Output!", call. = FALSE)


 ## summarize
 object <- summary(object, verbose = FALSE)

 ## extract correct columns
 avg <- object[["mean"]]
 y_min <- y_max <- NULL

 ## extract values for the uncertainties
 if(!is.null(plot_uncertainty)){
  if(plot_uncertainty == "sd") {
    y_min <-  avg -  object[["sd"]]
    y_max <-  avg +  object[["sd"]]


  } else if (plot_uncertainty == "var") {
    y_min <-  avg -  object[["var"]]
    y_max <-  avg +  object[["var"]]


  } else {
    y_min <- object[["y_min"]]
    y_max <- object[["y_max"]]

  }

 }

  if(is.null(times))
    times <- object[["time"]]

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
      xlab = if(length(grep(pattern = "TL", attributes(object)$model, fixed = TRUE) == 1)) {
        "Temperature [\u00b0C]"
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
    col = adjustcolor(plot_settings$col, alpha.f=1),
    type = plot_settings$type,
    pch = plot_settings$pch,
    lty = plot_settings$lty,
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

