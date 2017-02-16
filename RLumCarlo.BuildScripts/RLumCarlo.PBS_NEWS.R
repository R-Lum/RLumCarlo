### ===============================================================================================
### R package RLumCarlo BUILDSCRIPTS
### NEWS
### johannes.friedrich@uni-bayreuth.de
### 2017-02-01
### Note: Previous version based on code of Sebastian Kreutzer
### ===============================================================================================

require(tools)

NEWS.txt <- capture.output(tools:::Rd2txt("inst/NEWS.Rd", outputEncoding = "UTF-8"))
NEWS.txt <- gsub( "_\b","", NEWS.txt) #remove unwanted characters
write(NEWS.txt,"NEWS")
