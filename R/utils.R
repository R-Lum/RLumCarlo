## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Internal, non-exported package functions
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Combine Model Ouput Array
#'
#' @param ... Arrrays to be combined
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany)
#'
#' @noRd
comb_array <- function(...) abind::abind(..., along = 3)


#' Create RLumCarlo Model Output List
#'
#' @param signal [numeric] (**required**): signal vector
#'
#' @param time [numeric] (**required**): time vector
#'
#' @param model [character] (*with default*): the name of the model, the functions tries
#' to set this automatically.
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS-Université Bordeaux-Montaigne (France)
#'
#' @md
#' @noRd
.return_ModelOutput <- function(signal, time, model = as.character(sys.call(which = -1))[1]){
  list <- list(signal = signal, time = time)
  class(list) <- "RLumCarlo_Model_Output"
  attr(list, "model") <- if(!is.null(model)) model else NA_character_

  ## return
  return(list)
}


