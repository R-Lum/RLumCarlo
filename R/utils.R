#' @importFrom magrittr %>%
#' @export
magrittr::"%>%"

comb_array <- function(...) abind::abind(..., along = 3)
