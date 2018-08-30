### ===============================================================================================
### R package RLumCarlo BUILDSCRIPTS
### Function_Arguments
### christoph.burow@uni-koeln.de
### 2017-01-10
### Note: Previous versions were written by Michael Dietze and Sebastian Kreutzer.
### This version programmatically fetches all functions and arguments and does
### not rely on parsing the .Rd files with regular expressions.
### ===============================================================================================
library(RLumCarlo)
temp.version <- packageVersion("RLumCarlo")

args <- sapply(lsf.str("package:RLumCarlo"), formalArgs)
M <- matrix(NA, ncol = length(args), nrow = max(sapply(args, length)))

for (i in 1:ncol(M))
  M[1:length(args[[i]]), i] <- args[[i]]

colnames(M) <- names(args)

write.table(x = M,
            row.names = FALSE,
            file = paste0("RLumCarlo.BuildResults/RLumCarlo_",temp.version,"-Function_Arguments.csv"),
            sep = ",")
