## Utility to redirect the output of Rscript in a log file
# adapted from http://stackoverflow.com/questions/14167178/passing-command-line-arguments-to-r-cmd-batch
Args <- commandArgs(TRUE)
moddwl_dir = Args[1] # main directory of the tool
outFile <- file.path(moddwl_dir,'Log',paste0(make.names(date()),".Rout")) # directory to store the outputs
gui = if (length(Args)>=2) as.logical(Args[2]) else TRUE
settings = if (length(Args)>=3) Args[3] else NULL

source(file.path(moddwl_dir,'R/moddwl_main.R'), echo = TRUE)
sink(outFile, split = TRUE)
output = moddwl_main(gui=gui, settings=settings, moddwl_dir=moddwl_dir)