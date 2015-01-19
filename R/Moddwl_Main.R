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
	
	{{# Check if needed packages are present. Install them otherwise
			pkg_list = c('gWidgets','rgdal','plyr', 'reshape2','ggplot2','data.table','hash',
					'raster','RCurl','stringr','tools','rts','RGtk2','gWidgetsRGtk2','spatial.tools', 'gdalUtils')
			pkg_test <- function(x) {if (!require(x,character.only = TRUE)) {install.packages(x,dep=TRUE)}}
			for (pkg in pkg_list) {pkg_test(pkg)}
#	options(error = browser)
			
			options("guiToolkit"="RGtk2")
			memory.limit(6000)							# Increase maximum allocable memory
			rasterOptions(setfileext = F)
			# Folder Initialization -----
			
   rscript.stack <- function() {Filter(Negate(is.null), lapply(sys.frames(), function(x) x$ofile))}    			#	Returns the stack of RScript files
   rscript.current <- function() {	stack <- rscript.stack()   ;	  as.character(stack[length(stack)])}		## Returns the current RScript file path
   src_dir = dirname(rscript.current())
			
#			src_dir = "D:/Documents/Source_Code/R/LB_MOD_DWL/R"
			setwd(file.path(src_dir,'..'))       ;   main_dir = getwd()   ;   previous_dir = file.path(main_dir,'/Previous')   ; log_dir =  file.path(main_dir,'/Log')
			dir.create(previous_dir, showWarnings = FALSE, recursive = TRUE) ; dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
			previous_file= file.path(previous_dir, 'Moddwl_Previous.RData')
#browser()		
log_file = file.path(log_dir,paste(Sys.Date(),'log.txt', sep='_'))
			#   IDL_Dir = file.path(main_dir,'IDL-FRG')
			
			# Sourcing of needed R scripts (Remove when building package !!!!)-----
			source(file.path(src_dir,'moddwl_accessoires.R'))
			source(file.path(src_dir,'moddwl_process.R'))
			source(file.path(src_dir,'moddwl_set_opts.R'))
			source(file.path(src_dir,'Moddwl_QA_convert.R'))
			source(file.path(src_dir,'Moddwl_GUI.R'))
# 			source(file.path(src_dir,'moddwl_process_NDVI.R'))
# 			source(file.path(src_dir,'moddwl_process_QA_bits.R'))
			source(file.path(src_dir,'moddwl_META_create.R'))
			source(file.path(src_dir,'moddwl_process_indexes.R'))
		}}
#- ------------------------------------------------------------------------------- -#
#  Set general processing options
#- ------------------------------------------------------------------------------- -#
	{ MRTpath='D:/MRT/bin'
		out_proj_names = c("Sinusoidal","UTM 32N","Latlon WGS84","User Defined" )
		out_proj_list = hash("Sinusoidal" = "",
				"UTM 32N" = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
				"Latlon WGS84" = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ",
				"User Defined" = "")
		MOD_proj_str = '+proj=sinu +R=6371007.181 +nadgrids=@null +wktext'
		
		# Create the general_opts structure used to communicate with the GUI and 
		general_opts = list(main_dir = main_dir, previous_file=previous_file, log_file = log_file, MRTpath = MRTpath, out_proj_list = out_proj_list, out_proj_names = out_proj_names, MOD_proj_str = MOD_proj_str, 
				sel_prod = 'Surf_Ref_Daily_250 (MOD09GQ)',sensor = 'Terra',start_day = 1, start_month = 1,start_year = 2000,end_day = 1, end_month = 1, end_year = 2000,
				start_x = 18, end_x =18, start_y = 4, end_y = 4, 
				proj = 'Sinusoidal',out_res_sel = 'Native', out_res = '',full_ext = 'Full Tiles Extent', resampling = 'near',out_format = 'ENVI',ts_format = 'ENVI Meta Files', 
				reprocess ='No', bbox = c('','','',''), out_folder = '', out_folder_mod = '')
	}	
	#launch the GUI ----
	GUI = moddwl_GUI(general_opts)
	print(Quit)
	start.time <- Sys.time()
	# If not Quit selected, restore the user selected options and launch the processing ----
	{{if (Quit == F) {
				{{
						if (file.exists(general_opts$previous_file)) {load(general_opts$previous_file)} else {print('Download Options file not found ! Exiting !'); stop()}
						
						prod_opts = prod_opt_list[[general_opts$sel_prod]]
						
						# Create variables needed to launch the processing
						
						start_date = paste(general_opts$start_year, general_opts$start_month, general_opts$start_day, sep = '.')
						end_date = paste(general_opts$end_year, general_opts$end_month, general_opts$end_day, sep = '.')
						
						
						if (general_opts$proj != "User Defined") {outproj_str = general_opts$out_proj_list[[general_opts$proj]] 
						} else { outproj_str = general_opts$user_proj4}
						if (outproj_str =='') {outproj_str = general_opts$MOD_proj_str}
						
						if(general_opts$out_res == '' | general_opts$out_res_sel == 'Native'  ) {general_opts$out_res = prod_opts$native_res}   # get native resolution if out_res empty
# 					browser()
							# launch moddwl_process to Download and preprocess the selected images
							{{output = with(general_opts, moddwl_process(sel_prod = sel_prod, start_date = start_date,end_date = end_date,
													out_folder = out_folder, out_folder_mod = out_folder_mod, MRTpath = MRTpath,reproj = reproj,reprocess = reprocess,sensor = sensor, FTPs = prod_opts$FTP,
													start_x = start_x,start_y = start_y, end_x = end_x, end_y = end_y,
													full_ext = full_ext, bbox = bbox,out_format = out_format, out_res_sel = out_res_sel, out_res = as.numeric(out_res),
													resampling = resampling, ts_format = ts_format, 
													MOD_proj_str = MOD_proj_str,outproj_str = outproj_str,
													nodata_in = prod_opts$nodata_in, nodata_out = prod_opts$nodata_out,
													datatype =prod_opts$datatype,	bandsel = prod_opts$bandsel, bandnames = prod_opts$bandnames,
													reflbands=prod_opts$reflbands, reflorder=prod_opts$reflorder,
													indexes_bandsel = prod_opts$indexes_bandsel, indexes_bandnames = prod_opts$indexes_bandnames,
													indexes_formula = prod_opts$indexes_formula, indexes_nodata_out =prod_opts$indexes_nodata_out,
													quality_bandnames = prod_opts$quality_bandnames,quality_bandsel = prod_opts$quality_bandsel, quality_bitN = prod_opts$quality_bitN,
													quality_source = prod_opts$quality_source, quality_nodata_in =prod_opts$quality_nodata_in,
													quality_nodata_out =prod_opts$quality_nodata_out,
													file_prefixes = prod_opts$file_prefix, main_out_folder =prod_opts$main_out_folder,
													multiband_bsq = T))  	
								}}
						} # End for on selected sensor
						
					} # End If on "Quit"
				}}
			if (exists('Quit')) {rm(Quit, envir = globalenv())}    # Remove Quit if defined 
		} # End of processing
		end.time <- Sys.time()
		time.taken <- end.time - start.time
		print(time.taken)
	}

output = moddwl_main()


