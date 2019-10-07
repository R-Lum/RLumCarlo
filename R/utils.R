#' Combine Model Ouput Array
#'
#' @noRd
comb_array <- function(...) abind::abind(..., along = 3)

#' Create RLumCarlo Model Output List
#'
#' @md
#' @noRd
.return_ModelOutput <- function(signal, time, model = as.character(sys.call(which = -1))[1]){
  list <- list(signal = signal, time = time)
  attributes(list) <- list(
    class = "RLumCarlo_Model_Output",
    model = if(!is.null(model)) model else NA_character_

  )

  return(list)
}


