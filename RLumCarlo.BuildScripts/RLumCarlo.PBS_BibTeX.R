### ===============================================================================================
### R package RLumCarlo BUILDSCRIPTS
### BibTeX
### johannes.friedrich@uni-bayreuth.de
### 2017-02-01
### Note: Previous version based on code of Sebastian Kreutzer
### ===============================================================================================

library(tools)
library(RLumCarlo)

##get version number
temp <- readLines("DESCRIPTION")
temp <- temp[grep("Version", temp)]
temp.version <- sub(" ","",unlist(strsplit(temp,":"))[2])

package.citation <- toBibtex(citation("RLumCarlo"))
write(package.citation, file=paste0("RLumCarlo.BuildResults/RLumCarlo_", temp.version,"-bibliography.bib"))
