# Script used to launch the MODIStsp application from R (NO PACKAGE LOADING !) 
# (see MODIStsp.bat or MODIStsp.sh to launch from the command line)
#
# Author: Luigi Ranghetti <ranghetti.l@irea.cnr.it>
###############################################################################

# Set parameters
gui=TRUE # Should the tool be launched with the GUI for the selection of the parameters? (default: TRUE)
options_file=NULL # Path of the RData with the porcess parameters (default: NULL -> select the defaul one in the main script)
spatial_file_path=NULL # path of a spatial file to use as extent: if defined, the processing options which define the extent,
	# the selected tiles and the "Full Tile / Resized" options are not considered; instead, new files are created on the extent of the provided spatial file.

# Check sp version
sp_version <- packageVersion('sp')
sp_minversion <- package_version("1.0.17") # sp version used during the last test (for now used as minimum required version)
if (sp_version < sp_minversion) install.packages('sp',dep=TRUE,repos='http://stat.ethz.ch/CRAN')
require('sp')
# Check if needed packages are present. Install them otherwise
#			pkg_list = c('gWidgets','rgdal','plyr', 'reshape2','ggplot2','data.table','hash',
#					'raster','RCurl','stringr','tools','rts','RGtk2','gWidgetsRGtk2','spatial.tools', 'gdalUtils',')
pkg_list = c('XML','gWidgetsRGtk2','rgdal','rgeos','gdalUtils','hash','raster','RCurl','stringr','tools','plyr')
pkg_test <- function(x) {while (!require(x,character.only = TRUE)) {install.packages(x,dep=TRUE,repos='http://stat.ethz.ch/CRAN')}}
for (pkg in pkg_list) {pkg_test(pkg)}
# Retrieve the corrent directory
rscript.stack <- function() {Filter(Negate(is.null), lapply(sys.frames(), function(x) x$ofile))}    			#	Returns the stack of RScript files
rscript.current <- function() {	stack <- rscript.stack()   ;	  as.character(stack[length(stack)])}		## Returns the current RScript file path
main_dir = dirname(rscript.current())
src_dir = file.path(main_dir,'R')
source(file.path(main_dir,'R/MODIStsp.R'))
# Sourcing of needed R scripts (Remove when building package !!!!)-----
source(file.path(src_dir,'MODIStsp_addindex.R'))
source(file.path(src_dir,'MODIStsp_lpdaac_accessoires.R'))
source(file.path(src_dir,'MODIStsp_check_files.R'))
source(file.path(src_dir,'MODIStsp_process.R'))
source(file.path(src_dir,'MODIStsp_set_opts.R'))
source(file.path(src_dir,'MODIStsp_readxml.R'))
source(file.path(src_dir,'MODIStsp_GUI.R'))
source(file.path(src_dir,'MODIStsp_process_QA_bits.R'))
source(file.path(src_dir,'MODIStsp_vrt_create.R'))
source(file.path(src_dir,'MODIStsp_process_indexes.R'))
source(file.path(src_dir,'MODIStsp_reproj_bbox.R'))
source(file.path(src_dir,'MODIStsp_bbox_from_file.R'))

# Launch it
cat('[',date(),'] Running MOD_DWL...\n')
MODIStsp(gui=gui,options_file=options_file,spatial_file_path=spatial_file_path,MODIStsp_dir=main_dir)
cat('[',date(),'] Done.\n\n')



