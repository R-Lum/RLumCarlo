## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Internal, non-exported package functions
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Combine Model Output Array
#'
#' @param ... Arrays to be combined
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
#' @author Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
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

#' @title Register Multi-Core back end (helper function)
#'
#' @param method [character] (*with default*): Sequential `'seq'` or parallel `'par'`processing. In
#' the parallel mode the function tries to run the simulation on multiple CPU cores (if available) with
#' a positive effect on the computation time.
#'
#'@md
#'@noRd
.registerClusters <- function(method){
  ## check the method parameter
  if(!method %in% c("par", "seq"))
    stop(paste0("[",as.character(sys.call(which = -1))[1],"()] Allowed keywords for 'method' are either 'par' or 'seq'!"),
             call. = FALSE)

  ##get number of cores
  cores <- parallel::detectCores()
  if(cores == 1) method <- "seq"

  if(method != "par"){
    cl <- parallel::makeCluster(1)
    doParallel::registerDoParallel(cl)
    ##ensures that we do not have any particular problems
    foreach::registerDoSEQ()

  } else {
    cl <- parallel::makeCluster(cores-1)
    doParallel::registerDoParallel(cl)

  }

  return(cl)
}

