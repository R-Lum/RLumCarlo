### ===============================================================================================
### R package RLumCarlo BUILDSCRIPTS
### roxygen2
### johannes.friedrich@uni-bayreuth.de
### 2017-02-01
### Note: Previous version based on code of Sebastian Kreutzer
### ===============================================================================================

if(!require("devtools"))
  install.packages("devtools")

library(devtools)

document(pkg = ".", roclets = NULL)
