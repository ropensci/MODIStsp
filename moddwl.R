# Script used to launch the Moddwl application from R
# (see moddwl.bat to launch from the command line)
# 
# Author: lranghetti
###############################################################################

# Set parameters
gui=TRUE # Should the tool be launched with the GUI for the selection of the parameters? (default: TRUE)
settings=NULL # Path of the RData with the porcess parameters (default: NULL -> select the defaul one in the main script)

# Retrieve the corrent directory
rscript.stack <- function() {Filter(Negate(is.null), lapply(sys.frames(), function(x) x$ofile))}    			#	Returns the stack of RScript files
rscript.current <- function() {	stack <- rscript.stack()   ;	  as.character(stack[length(stack)])}		## Returns the current RScript file path
main_dir = dirname(rscript.current())
source(file.path(main_dir,'R/moddwl_main.R'))

# Launch it
cat('[',date(),'] Running MOD_DWL...\n')
moddwl_main(gui=gui,settings=settings,moddwl_dir=main_dir)
cat('[',date(),'] Done.\n\n')
