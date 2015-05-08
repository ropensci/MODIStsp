#'  MODIStsp_main
#' @description Main function for the MODIS Time Series Processing Tool (MOD_TSP)
#' @details The function is used to initialize the processing (folder names, packages, etc.), to launch the GUI (MODIStsp_GUI) and receive its outputs,
#'  and to launch the required routines for downloading and processing the requested datasets.
#' @param gui logical parameters (TRUE: the GUI is opened before processing; FALSE: the saved parameters are directly passed)
#' @param options_file settings (optional): full path of the RData file containing the processing options (default: Previous.RData in subdir Previous);
#' @param MODIStsp_dir main directory of the local installation of the tool(if not gived, it is automatically retrieved, but this works only if the function is launched from here!)
#' @return NULL
#'
#' @author Lorenzo Busetto, phD (2014-2015)
#' email: busetto.l@@irea.cnr.it
#' Luigi Ranghetti, phD (2015)
#' license CC BY-NC 3.0
#' @export
#' @importFrom hash hash
#' @import gdalUtils
#' @import rgdal
#' @import raster

#run=F # workaround usable to TRUE to run directly the function from here
# (It would be better to launch from ../MODIStsp.R, in order not to edit this)
# IMPORTANT: retrieve to FALSE after using it!

MODIStsp= function(gui=TRUE, options_file=NULL, MODIStsp_dir=NA) {

	MODIStsp_dir = system.file(package = "MODIStsp")
	print(MODIStsp_dir)

	#- ------------------------------------------------------------------------------- -#
	#  Initialize project
	#- ------------------------------------------------------------------------------- -#

	# Check sp version
# 	sp_version <- packageVersion('sp')
# 	sp_minversion <- package_version("1.0.17") # sp version used during the last test (for now used as minimum required version)
# 	if (sp_version < sp_minversion) install.packages('sp',dep=TRUE,repos='http://stat.ethz.ch/CRAN')
# 	require('sp')
# 	{{# Check if needed packages are present. Install them otherwise
# #			pkg_list = c('gWidgets','rgdal','plyr', 'reshape2','ggplot2','data.table','hash',
# #					'raster','RCurl','stringr','tools','rts','RGtk2','gWidgetsRGtk2','spatial.tools', 'gdalUtils',')
# 	pkg_list = c('XML','gWidgetsRGtk2','rgdal','gdalUtils','hash','raster','RCurl','stringr','tools','plyr')
# 	pkg_test <- function(x) {while (!require(x,character.only = TRUE)) {install.packages(x,dep=TRUE,repos='http://stat.ethz.ch/CRAN')}}
#	for (pkg in pkg_list) {pkg_test(pkg)}
# 			# Check GDAL version
	if (is.null(getOption('gdalUtils_gdalPath'))) {gdal_setInstallation(ignore.full_scan=FALSE)}
	gdal_version <- package_version(gsub('^GDAL ([0-9.]*)[0-9A-Za-z/., ]*','\\1',getGDALVersionInfo(str = "--version")))
	gdal_minversion <- package_version("1.11.1") # GDAL version used during the last test (for now used as minimum required version)
	if (gdal_version < gdal_minversion) stop(paste0("GDAL version must be at least ",gdal_minversion,". Please update it."))
# 		}}
	print(gdal_version)
  require(gWidgetsRGtk2)
	options("guiToolkit"="RGtk2")
	memory.limit(8000)							# Increase maximum allocsable memory
	rasterOptions(setfileext = F)				# Make so that "raster" functions doesn't automatically add extensions on output files
	# Folder Initialization -----

	## Retrieve parameters passed by batch launcher
	# If the script has been launched from R, "gui" and "options_file" are passed from a global var, and src_dir is computed as below;
	# if it is launched from a bat script, they are saved as "args" list by an intermediate "RscriptEcho.R" script.
# 	if (is.na(MODIStsp_dir)) {
# 		rscript.stack <- function() {Filter(Negate(is.null), lapply(sys.frames(), function(x) x$ofile))}    			#	Returns the stack of RScript files
# 		rscript.current <- function() {	stack <- rscript.stack()   ;	  as.character(stack[length(stack)])}		## Returns the current RScript file path
# 		src_dir = dirname(rscript.current())
# 		main_dir = dirname(src_dir)
# 	} else {
# 		main_dir = MODIStsp_dir; src_dir = file.path(main_dir,'R')
# 	}


# 	setwd(main_dir);   main_dir = getwd()   ;
#   log_dir =  file.path(main_dir,'Log')   ; log_file =  file.path(log_dir,'Log.txt')
	previous_dir = if (is.null(options_file)) {file.path(MODIStsp_dir,'Previous')} else {dirname(options_file)}   # Folder in which the previous options file is saved
	dir.create(previous_dir, showWarnings = FALSE, recursive = TRUE) #; dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
	previous_file = if (is.null(options_file)) {file.path(previous_dir, 'MODIStsp_Previous.RData')} else {options_file}  # TODO fix to accept relative paths
	xml_file= file.path(MODIStsp_dir,'ExtData','MODIStsp_ProdOpts.xml')  #XML file describing MODIS products

#- ------------------------------------------------------------------------------- -#
#  Set general processing options
#- ------------------------------------------------------------------------------- -#
	{out_proj_names = c("Sinusoidal","UTM 32N","Latlon WGS84","User Defined" )
		out_proj_list = hash("Sinusoidal" = "",
				"UTM 32N" = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
				"Latlon WGS84" = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
				"User Defined" = "")
		MOD_proj_str = '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs '

		# Create the general_opts structure used to communicate with the GUI and set default values
		general_opts = list(MODIStsp_dir = MODIStsp_dir, previous_file=previous_file,xml_file = xml_file, out_proj_list = out_proj_list, out_proj_names = out_proj_names, MOD_proj_str = MOD_proj_str,
				sel_prod = 'Surf_Ref_8Days_500m (MOD09A1)',sensor = 'Terra',start_day = 1, start_month = 1,start_year = 2000,end_day = 1, end_month = 1, end_year = 2000,
				start_x = 18, end_x =18, start_y = 4, end_y = 4,
				proj = 'Sinusoidal',out_res_sel = 'Native', out_res = '',full_ext = 'Full Tiles Extent', resampling = 'near',out_format = 'ENVI',ts_format = 'ENVI Meta Files', compress = 'None',
				nodata_change = 'No',delete_hdf = 'No',reprocess ='No', bbox = c('','','',''), out_folder = '', out_folder_mod = '')
	}
	#launch the GUI if on an interactive session (i.e., gui = T) ----
	if (gui) {GUI = MODIStsp_GUI(general_opts)} else {Quit<<-FALSE}

	start.time <- Sys.time()
	# If not Quit selected, restore the user selected options from previous file and launch the processing ----
	if (!Quit) {

		if (file.exists(general_opts$previous_file)) {load(general_opts$previous_file)} else {print('Download Options file not found ! Exiting !'); stop()}

		prod_opts = prod_opt_list[[general_opts$sel_prod]]  # retrieve options relative to the selected product from the "prod_opt_list" data frame

		# Create variables needed to launch the processing

		start_date = paste(general_opts$start_year, general_opts$start_month, general_opts$start_day, sep = '.')
		end_date = paste(general_opts$end_year, general_opts$end_month, general_opts$end_day, sep = '.')

		if (general_opts$proj != "User Defined") {outproj_str = general_opts$out_proj_list[[general_opts$proj]]   # get proj4 string (if needed, from user proj4)
		} else { outproj_str = general_opts$user_proj4}
		if (outproj_str =='') {outproj_str = general_opts$MOD_proj_str}			# If out_proj = useer but empty, set to sinusoidal

		# If the product is NOT tiled, change or_proj to WGS84 and or_res to 0.05
		if (prod_opts$tiled == 0) {
			general_opts$MOD_proj_str = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
			prod_opts$native_res = "0.05"
		}


		if(general_opts$out_res == '' | general_opts$out_res_sel == 'Native'  ) {general_opts$out_res = prod_opts$native_res}   # get native resolution if out_res empty
		# launch MODIStsp_process to Download and preprocess the selected images
		output = with(general_opts, MODIStsp_process(sel_prod = sel_prod, start_date = start_date,end_date = end_date,
						out_folder = out_folder, out_folder_mod = out_folder_mod, reprocess = reprocess,
						delete_hdf = delete_hdf, sensor = sensor, https = prod_opts$http,
						start_x = start_x,start_y = start_y, end_x = end_x, end_y = end_y,
						full_ext = full_ext, bbox = bbox,out_format = out_format, out_res = as.numeric(out_res), native_res = prod_opts$native_res,  tiled = prod_opts$tiled,
						resampling = resampling, ts_format = ts_format, compress = compress,
						MOD_proj_str = MOD_proj_str,outproj_str = outproj_str,
						nodata_in = prod_opts$nodata_in, nodata_out = prod_opts$nodata_out,nodata_change = nodata_change,
						datatype =prod_opts$datatype,	bandsel = prod_opts$bandsel, bandnames = prod_opts$bandnames,
						indexes_bandsel = prod_opts$indexes_bandsel, indexes_bandnames = prod_opts$indexes_bandnames,
						indexes_formula = prod_opts$indexes_formula, indexes_nodata_out =prod_opts$indexes_nodata_out,
						quality_bandnames = prod_opts$quality_bandnames,quality_bandsel = prod_opts$quality_bandsel, quality_bitN = prod_opts$quality_bitN,
						quality_source = prod_opts$quality_source, quality_nodata_in =prod_opts$quality_nodata_in,
						quality_nodata_out =prod_opts$quality_nodata_out,
						file_prefixes = prod_opts$file_prefix, main_out_folder =prod_opts$main_out_folder))

	} # End If on "Quit" --> If "Quit" above is skipped and program terminates

	if (exists('Quit')) {rm(Quit, envir = globalenv())}    # Remove Quit if defined
	# End of processing
	end.time <- Sys.time()
	time.taken <- end.time - start.time
	print(time.taken)
}

#if (run) {output = MODIStsp_main()} # run the function if run has been set to TRUE
