#' methods_RLumCarlo
#'
#' Methods for S3-generics implemented for the package 'RLumCarlo'.
#'
#' @keywords internal
#'
#' @examples
#'
#' ##create object
#' object <- run_MC_TL_DELOC(
#'  s = 3.5e12,
#'  E = 1.45,
#'  R = 0.1,
#'  method = 'seq',
#'  clusters = 100,
#'  times = 150:350)
#'
#' ##summary
#' summary(object)
#'
#' ##combine
#' c(objects,objects)
#'
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
summary.RLumCarlo_Model_Output <- function(object, verbose = TRUE, ...){

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
      sum_signal <- vapply(1:clusters, function(y){
        vapply(1:length(times), function(x){
          sum(signal[x,,y])
        }, FUN.VALUE = 1)

      }, numeric(length(times)))

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

#' @title Combine modelling results
#'
#' @param ... objects to be concatenated
#'
#' @return This function returns a the same as the input objects
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#' @md
#' @method c RLumCarlo_Model_Output
#' @rdname methods_RLumCarlo
#' @export
c.RLumCarlo_Model_Output <- function(...){
  ## remove everything that does not belong into this list
  objects <- lapply(list(...), function(x) if(class(x) == "RLumCarlo_Model_Output") x)
  objects[sapply(objects, is.null)] <- NULL

  ## we can only merge results from the same stimulation mode
  ## get modes
  modes <- vapply(objects, attr, character(1), which = "model")
  modes <- vapply(strsplit(modes, "_", fixed = TRUE), function(x) x[3], character(1))

  ## get the one we remove
  if(length(rm_objects <- which(modes[1] != modes)) != 0){
    warning(paste0("Object(s) ", paste(rm_objects, collapse = ", "), " do not have the same stimulation mode as the first object and were removed from the input!"), call. = FALSE)
    objects[rm_objects] <- NULL

  }

  ## make sure that the time vector is identically
  if(!unique(vapply(objects, function(x) length(x$time), numeric(1))))
    stop("[c.RLumCarlo_Model_Output()] You cannot combine objects with different time vectors!", call. = FALSE)

  ## in order to combine, we have to treat values coming from the tunnelling
  ## process differently ...
  objects <- lapply(objects, function(x){
    if(dim(x$signal)[2] > 1){
      x$signal <- vapply(1:dim(x$signal)[3], function(o)
        matrix(rowSums(x$signal[, , o]), ncol = 1),
        matrix(numeric(nrow(x$signal[,,1])), ncol = 1))
    }
    return(x)
  })

  # finally combine the objects
  .return_ModelOutput(
    signal = comb_array(lapply(objects, function(x) x$signal)),
    time = objects[[1]]$time,
    model = attr(objects[[1]], which = "model"))
}
