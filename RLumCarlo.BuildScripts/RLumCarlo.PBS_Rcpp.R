### ===============================================================================================
### R package RLumCarlo BUILDSCRIPTS
### Rcpp
### johannes.friedrich@uni-bayreuth.de
### 2017-02-01
### Note: Previous version based on code of Sebastian Kreutzer
### ===============================================================================================

if(!require("Rcpp"))
  install.packages("Rcpp")

##compile new attributes
Rcpp::compileAttributes(verbose = TRUE)
