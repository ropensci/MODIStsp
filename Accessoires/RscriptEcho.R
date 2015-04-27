## Utility to redirect the output of Rscript in a log file
# adapted from http://stackoverflow.com/questions/14167178/passing-command-line-arguments-to-r-cmd-batch
Args <- commandArgs(TRUE)
moddwl_dir = Args[1] # main directory of the tool
outFile <- file.path(moddwl_dir,'Log',paste0(make.names(date()),".Rout")) # directory to store the outputs
if (length(Args)>=2) gui = as.logical(Args[2])
if (length(Args)>=3) settings = Args[3]

source(file.path(moddwl_dir,'R/Moddwl_Main.R'), echo = TRUE)
sink(outFile, split = TRUE)
output = moddwl_main(gui=gui, settings=settings, moddwl_dir=moddwl_dir)