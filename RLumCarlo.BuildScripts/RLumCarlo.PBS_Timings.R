### ===============================================================================================
### R package RLumCarlo BUILDSCRIPTS
### Timings
### johannes.friedrich@uni-bayreuth.de
### 2017-02-01
### Note: Previous version based on code of Sebastian Kreutzer
### ===============================================================================================
library(tools)


##get version number
temp <- readLines("DESCRIPTION")
temp <- temp[grep("Version", temp)]
temp.version <- sub(" ","",unlist(strsplit(temp,":"))[2])

# CHECK EXAMPLE TIMING ----------------------------------------------------
timing.threshold <- 5
if (Sys.info()[["sysname"]] == "Windows") {
  temp <- read.table("RLumCarlo.Rcheck/RLumCarlo-Ex.timings", header=TRUE)
} else {
  temp <- read.table("RLumCarlo.Rcheck/RLumCarlo-Ex.timings", header=TRUE)
}

##plot values for the functions
pdf(file=paste0("RLumCarlo.BuildResults/RLumCarlo-TimingExamples.",temp.version,".pdf"), paper="special")

values <- barplot(rev(temp$elapsed), horiz=TRUE, xlim=c(0,10), cex.names=0.7,
                  beside=TRUE, names.arg=c(length(temp$name):1), las=1,
                  xlab="elapsed test time [s]", ylab="function number",
                  main="run time for function examples")

abline(v=timing.threshold, col="red")

for (i in 1:length(temp$name)){

  if(temp$elapsed[i] > timing.threshold){
    text(temp$elapsed[i],values[length(values)+1-i],temp$name[i], pos=4, cex=.7)
  }

}
dev.off()

temp.threshold <- temp[temp$elapsed > timing.threshold, ]
write.table(x=temp.threshold[,c(1,4)],
            file=paste0("RLumCarlo.BuildResults/RLumCarlo-Ex.timings.",temp.version,".WARNING"),
            row.names=FALSE, quote=FALSE, col.names=FALSE, sep=" >> ")


