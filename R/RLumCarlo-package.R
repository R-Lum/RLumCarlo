#' @title Monte-Carlo Methods for Simulating Luminescence Phenomena
#' \cr
#' \if{html}{
#' \figure{Logo_RLumCarlo.png}{options: width="50px" alt="https://github.com/R-Lum/RLumCarlo"}\cr
#' }
#'
#' @description A collection of functions to simulate luminescence production in dosimetric materials
#' using Monte-Carlo methods. Implemented are models for delocalised, localised and tunnelling
#' transitions. Supported stimulation modes are TL, CW-OSL, LM-OSL, LM-IRSL, and ITL (ISO-TL).
#'
#' @details
#'
#' \bold{Funding}
#'
#' The development of RLumCarlo benefitted from the support by various funding bodies:
#'
#'  * The initial work by Johannes Friedrich, Sebastian Kreutzer and Christoph Schmidt
#'  was supported by the Deutsche Forschungsgemmeinschaft (DFG, 2015–2018, SCHM 3051/4-1,
#'  "Modelling quartz luminescence signal dynamics relevant for dating and dosimetry", SCHM 3051/4-1).
#'
#'  * Later work (2018-2019) was secured through the project "ULTIMO: Unifying Luminescence Models of
#'  quartz and feldspar DAAD: Deutscher Akademischer Austauschdienst
#'  (German Academic Exchange Service). Framework: DAAD PPP USA 2018, ID: 57387041.
#'
#'  * The work of Sebastian Kreutzer as maintainer of the package was supported
#'  by LabEx LaScArBx (ANR - n. ANR-10-LABX-52) between 2017 and 2019.
#'
#' @name RLumCarlo-package
#' @aliases RLumCarlo
#' @keywords package
#' @docType package
#'
#' @author Johannes Friedrich (University of Bayreuth, Germany), \cr
#' Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS-Université Bordeaux Montaigne (France), \cr
#' Vasilis Pagonis, McDaniel College Westminster (MD, USA), \cr
#' Christoph Schmidt, University of Bayreuth (Germany), \cr
#' Ena Rajovic, University of Bayreuth (Germany), \cr
#' Alex Roy Duncan, University of Bayreuth (Germany), \cr
#' Christian Laag, University of Bayreuth (Germany)
#'
#' @references
#'
#' Pagonis, V., Friedrich, J., Discher, M., Müller-Kirschbaum, A., Schlosser, V., Kreutzer, S.,
#' Chen, R., Schmidt, C., 2019. Excited state luminescence signals from a random distribution of
#' defects_ A new Monte Carlo simulation approach for feldspar.
#' Journal of Luminescence 207, 266–272. \doi{doi:10.1016/j.jlumin.2018.11.024}
#'
#' @import methods magrittr foreach parallel doParallel
#' @importFrom grDevices adjustcolor rainbow
#' @importFrom graphics plot lines polygon grid par legend
#' @importFrom Rcpp evalCpp
#' @importFrom utils modifyList
#' @useDynLib RLumCarlo, .registration = TRUE
#' @md
NULL
