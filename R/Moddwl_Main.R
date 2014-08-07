#' @Title moddwl_main
#' @Description Main function for the MODIS download and preprocessing functions
#' @details 
#'  The function is used to initialize the processing (folder names, packages, etc.), to launch the GUI (moddwl_GUI) and receive outputs,
#'  and to launch the required routines for downloading and processing the requested datasets.
#' @return 
#' NULL
#' @author Lorenzo Busetto, phD (2014)
#' email: busetto.l@@irea.cnr.it
#'
#' @license GPL(>2)
#' @export
moddwl_main = function() {
	
	#- ------------------------------------------------------------------------------- -#
	#  Initialize project
	#- ------------------------------------------------------------------------------- -#
	
	# Check if needed packages are present. Install them otherwise
	pkg_list = c('gWidgets','rgdal','plyr', 'reshape2','ggplot2','data.table','hash','raster','RCurl','stringr')
	pkg_test <- function(x) {if (!require(x,character.only = TRUE)) {install.packages(x,dep=TRUE)}}
	for (pkg in pkg_list) {pkg_test(pkg)}
#	options(error = browser)
	
	options("guiToolkit"="RGtk2")
	memory.limit(6000)							# Increase maximum allocable memory
	rasterOptions(setfileext = F)
	
	
	
	# Folder Initialization -----
	
#   rscript.stack <- function() {Filter(Negate(is.null), lapply(sys.frames(), function(x) x$ofile))}    			#	Returns the stack of RScript files
#   rscript.current <- function() {	stack <- rscript.stack()   ;	  as.character(stack[length(stack)])}		## Returns the current RScript file path
#   src_dir = dirname(rscript.current())
	
	src_dir = "D:/Documents/Source_Code/R/LB_MOD_DWL/R"
	setwd(file.path(src_dir,'..'))       ;   main_dir = getwd()   ;   previous_dir = file.path(main_dir,'/Previous')   ; log_dir =  file.path(main_dir,'/Log')
	dir.create(previous_dir, showWarnings = FALSE, recursive = TRUE) ; dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
	#   IDL_Dir = file.path(main_dir,'IDL-FRG')
	
	# Sourcing of needed R scripts (Remove when building package !!!!)-----
	source(file.path(src_dir,'moddwl_accessoires.R'))
	source(file.path(src_dir,'moddwl_process.R'))
	source(file.path(src_dir,'moddwl_set_opts.R'))
	source(file.path(src_dir,'Moddwl_QA_convert.R'))
	source(file.path(src_dir,'Moddwl_GUI.R'))
	source(file.path(src_dir,'moddwl_process_NDVI.R'))
	
	
	
	# Create the moddwl_opts structure used to communicate with the GUI and launch the GUI ----
	moddwl_opts = list(main_dir = main_dir, previous_dir = previous_dir, log_file = file.path(log_dir,paste(Sys.Date(),'log.txt', sep='_')),
			previous_file= file.path(previous_dir, 'Moddwl_Previous.RData'))
	
	moddwl_GUI(moddwl_opts)
	# If not Quit selected, restore the user selected options and launch the processing ----
	
	if (!exists('Quit')){
		
		if (file.exists(moddwl_opts$previous_file)) {load(moddwl_opts$previous_file)} else {print('Download Options file not found ! Exiting !'); stop()}
		
		moddwl_opts = c(moddwl_opts, opts)  # Join "general" and user selected opts
		pos_prod = which(names(opts$prod_opt_list) == opts$modprod)	# Find index of selected product in prod_opt_list
		
		# Create variables needed to launch the processing
		
		start_date = paste(moddwl_opts$start_year, moddwl_opts$start_month, moddwl_opts$start_day, sep = '.')
		end_date = paste(moddwl_opts$end_year, moddwl_opts$end_month, moddwl_opts$end_day, sep = '.')
		
		outproj_str = moddwl_opts$out_proj_list[[moddwl_opts$proj]]   ;  if (outproj_str =='') {outproj_str = moddwl_opts$MOD_prj_str}
		
		if(moddwl_opts$out_res == '') {moddwl_opts$out_res = moddwl_opts$prod_opt_list[[pos_prod]]$native_res}   # get native resolution f out_res empty
		
		if (opts$sensor == 'Both') {sensor = c('Terra','Aqua')} else {sensor = opts$sensor}  # selected sensors
		
		for (sens_sel in sensor) {		# cycle on selected sensors
			
			# get http site addresses and file prefixes
			if (sens_sel == "Terra") {FTP = moddwl_opts$prod_opt_list[[pos_prod]]$FTP[["Terra"]]} else {FTP = moddwl_opts$prod_opt_list[[pos_prod]]$FTP[["Aqua"]]}
			if (sens_sel == "Terra") {file_prefix = moddwl_opts$prod_opt_list[[pos_prod]]$file_prefix[["Terra"]]} else {file_prefix = moddwl_opts$prod_opt_list[[pos_prod]]$file_prefix[["Aqua"]]}
			
		# launch moddwl_opts to Download and preprocess the selected images
			output = with(moddwl_opts, moddwl_process(product = modprod, start_date,end_date,
							out_folder, MRTpath,reproj,reprocess, FTP,sensor,
							start_x,start_y, end_x, end_y,
							bbox,format, out_res = as.numeric(out_res),
							MOD_prj_str,outproj_str,
							nodata_in = prod_opt_list[[pos_prod]]$nodata_in, nodata_out= prod_opt_list[[pos_prod]]$nodata_out,derived_nodata_out= prod_opt_list[[pos_prod]]$derived_nodata_out,
							datatype =prod_opt_list[[pos_prod]]$datatype,
							bandsel = prod_opt_list[[pos_prod]]$bandsel, bandnames = prod_opt_list[[pos_prod]]$bandnames,
							derived_bandsel = prod_opt_list[[pos_prod]]$derived_bandsel, derived_bandnames = prod_opt_list[[pos_prod]]$derived_bandnames,
							derived_category = prod_opt_list[[pos_prod]]$derived_category, derived_bitN = prod_opt_list[[pos_prod]]$derived_bitN,
							derived_source = prod_opt_list[[pos_prod]]$derived_source,
							file_prefix = file_prefix, main_out_folder =prod_opt_list[[pos_prod]]$main_out_folder,
							multiband_bsq = T))  	
			
		} # End for on selected sensor
		
	} # End If on "Quit"
	
	if (exists('Quit')) {rm(Quit, envir = globalenv())}    # Remove Quit if defined 
} # End of processing

output = moddwl_main()


