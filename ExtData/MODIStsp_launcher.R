## Utility to redirect the output of Rscript in a log file
# adapted from http://stackoverflow.com/questions/14167178/passing-command-line-arguments-to-r-cmd-batch
Args <- commandArgs(TRUE)

gui = if (length(Args)>=1) as.logical(Args[1]) else TRUE
options_file = if (length(Args)>=2) Args[2] else NULL
if (length(Args)>=2) {
  load (options_file)
  log_dir = file.path(general_opts$out_folder,'Log')
  browser()
  dir.create(log_dir)
  outFile = file.path(log_dir,paste0(make.names(date()),".Rout"))
} else {
  outFile <- file.path(MODIStsp_dir,'Log',paste0(make.names(date()),".Rout")) # directory to store the Log
}

require(MODIStsp)
MODIStsp_dir = system.file(package = 'MODIStsp')

# source(file.path(moddwl_dir,'R/moddwl_main.R'), echo = TRUE)
sink(outFile, split = FALSE, type = c("output"))
output = MODIStsp(gui=gui, options_file=options_file)
