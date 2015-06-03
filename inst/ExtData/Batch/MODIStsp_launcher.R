## Utility to launch MODIStsp from bathc/bash file
# (do not run it; launch from MODIStsp.sh or MODIStsp.bat)

Args <- commandArgs(TRUE)

MODIStsp_dir <- Args[1]
gui = if (length(Args)>=2) as.logical(Args[2]) else TRUE
options_file = if (length(Args)>=3) Args[3] else NULL
if (length(Args)>=3) {
	load (options_file)
	log_dir = file.path(general_opts$out_folder,'Log')
} else {
	log_dir = file.path(MODIStsp_dir,'Log')
}
dir.create(log_dir,showWarnings=FALSE)
outFile <- file.path(log_dir,paste0('MODIStsp_',strftime(Sys.time(),'%y%m%d_%H%M%S'),".Rout")) # directory to store the Log

require(MODIStsp)
MODIStsp_dir = system.file(package = 'MODIStsp')

sink(outFile, split = FALSE, type = c("output"))
output = MODIStsp(gui=gui, options_file=options_file)
sink(type = c("output"))
