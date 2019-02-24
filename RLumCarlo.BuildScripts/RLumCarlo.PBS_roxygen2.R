### ===============================================================================================
### R package RLumCarlo BUILDSCRIPTS
### roxygen2
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2018-11-25
### ===============================================================================================
##updated using the suggestions in
##https://github.com/klutometis/roxygen/issues/822


# Load packages -------------------------------------------------------------------------------

if(!require("pkgbuild"))
  install.packages("pkgbuild")

if(!require("devtools"))
  install.packages("devtools")


# Create documentation  -----------------------------------------------------------------------
pkgbuild::compile_dll(quiet = TRUE)
devtools::document()
