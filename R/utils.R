#' Combine Model Ouput Array
#'
#' @noRd
comb_array <- function(...) abind::abind(..., along = 3)

#' Create RLumCarlo Model Output List
#'
#' @md
#' @noRd
.return_ModelOutput <- function(signal, time, model = sys.call(which = -1)){
  temp <- list(signal = signal, time = time)
  attributes(temp) <- list(
    class = "RLumCarlo_Model_Output",
    model = if(!is.null(model)) model else NA_character_

  )

  return(temp)
}


