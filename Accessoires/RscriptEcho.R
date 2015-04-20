## Utility to redirect the output of Rscript in a log file
# adapted from http://stackoverflow.com/questions/14167178/passing-command-line-arguments-to-r-cmd-batch
Args <- commandArgs(TRUE)
srcFile <- Args[1] # first argument: script to run
outFile <- file.path(Args[2],paste0(make.names(date()),".Rout")) # second argument: directory to store the outputs
Args <- Args[-c(1,2)] # next arguments: passed to srcFile

sink(outFile, split = TRUE)
source(srcFile, echo = TRUE)