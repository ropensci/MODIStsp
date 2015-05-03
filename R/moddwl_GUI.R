#' @Title moddwl_GUI
#' @Description
#'	Function used to generate and handle the GUI used to allow selection of moddwl processing parameters
#'  If the "previous options" file (moddwl_Previous.RData) already exists, it is loaded and used to reinstate
#' 	the GUI to its last state. Otherwise, the previous options file is created by launching the moddwl_read_xml fucntion 
#' 
#' @details
#'
#' @param general_opts General options data frame passed by moddwl_main. Contains paths and other variables used to initialize the GUI
#' 						if a previous options file is not existing. 
#' 			 
#' @returnType 
#' @return 
#'		NONE - Processing options are saved in "previous" file and (if "Save options" is pressed) in user's selected file
#' @author Lorenzo Busetto, phD (2014)
#' email: busetto.l@@irea.cnr.it
#'
#' @license GPL(>2)
#' @export
moddwl_GUI = function (general_opts){
	
	# Restore previous options file if existing, otherwise create a "new" one with default values, by retrieving data from xml file
	if (file.exists(general_opts$previous_file)) {
		load(general_opts$previous_file)
	} else { moddwl_read_xml_opts(previous_file = general_opts$previous_file,xml_file = general_opts$xml_file ) ; load(general_opts$previous_file)}
	
	assign("Quit", T, envir=globalenv())	# Assigng "Quit" to true 
	
#- ------------------------------------------------------------------------------- -#
#  Start Building the GUI
#- ------------------------------------------------------------------------------- -#
	main_win <- gbasicdialog(title = "Select Main Processing Options", parent=NULL, do.buttons=F, 
			visible = F, spacing = 10)
	main_group <- ggroup(container = main_win, horizontal = FALSE, expand = T)
	sel_prod <- general_opts$sel_prod # get the product name selectedin the previous options file
	
	
#- ------------------------------------------------------------------------------- -#
# Widgets for product selection and bands selection  
#- ------------------------------------------------------------------------------- -#
	satprod_frame <- gframe(text ='<span foreground="blue" size="large">MODIS Product, Satellites and Layers selection</span>', markup = T,horizontal = F, container=main_group, spacing = 5)
	
	checked <- which(mod_prod_list == general_opts$sel_prod)
	
	temp_wid_bands <<- prod_opt_list[[checked]]$bandsel				# set dummy global variables holding the initial values of selected bands
	temp_wid_bands_indexes <<- prod_opt_list[[checked]]$indexes_bandsel
	temp_wid_bands_quality <<- prod_opt_list[[checked]]$quality_bandsel
	
	labels_group = ggroup(horizontal = T, container=satprod_frame)
	label = glabel (text = '<span weight = "bold">			       		 Product </span>', markup = T, container=labels_group)
	addSpace(labels_group, 190 )
	label2 = glabel (text = '<span weight = "bold">Satellites </span>', markup = T, container=labels_group)
	addSpace(labels_group, 5 )
	label3 = glabel (text = '<span weight = "bold">      Processing Layers</span>', markup = T, container=labels_group)
	
	#- ------------------------------------------------------------------------------- -#
	# Widgets for Product selection  
	#- ------------------------------------------------------------------------------- -#
	
	prod_group = ggroup(horizontal = T, container=satprod_frame)
	prod_wid <- gdroplist(items = mod_prod_list, container=prod_group, horizontal = T, selected = checked,  handler = function(h,...) {
				checked <- mod_prod_list[which(mod_prod_list == svalue(prod_wid))]		# find index of sel. product
				sel_prod <- svalue(prod_wid)
				temp_wid_bands <<- rep(0, length(prod_opt_list[[checked]]$bandsel))					# reset dummy variables for band selection to 0 on product change
				temp_wid_bands_indexes <<- rep(0, length(prod_opt_list[[checked]]$indexes_bandsel))
				temp_wid_bands_quality <<- rep(0, length(prod_opt_list[[checked]]$quality_bandsel))
				
			})
	#- ------------------------------------------------------------------------------- -#
	# Widgets for Sensor selection  
	#- ------------------------------------------------------------------------------- -#
	sens_wid <- gdroplist(items = c("Terra","Aqua", "Both"), container = prod_group, text = 'Select', 
			selected = match(general_opts$sensor, c("Terra","Aqua", "Both")))
	addSpace(satprod_frame, 20, horizontal=TRUE)
	#- ------------------------------------------------------------------------------- -#
	# Widgets for Layers selection  
	#- ------------------------------------------------------------------------------- -#
	band_wid <- gbutton(text = 'Click To Select', border = T,				# Child widget for processing bands selection
			handler = function(h,....) {
				
				checked <- mod_prod_list[which(mod_prod_list == svalue(prod_wid))]		# find index of sel. product
				check_names <- prod_opt_list[[checked]]$band_fullnames					# retrieve band names of sel. product
				check_wid <- temp_wid_bands												# retrieve currently selected original layers
				selgroup <- gbasicdialog(title = "Select Processing Layers						", parent=NULL, do.buttons=F, width = 450, horizontal = T)
				# widgets for band selection - original
				cbox_total <- gframe(text = '', container = selgroup, horizontal = T, width = 450)
				cbox<- gframe(text = '<span foreground="blue" size="large">		Original MODIS Layers			</span>', markup = T, container = cbox_total, horizontal = T, width = 450)
				bands_wid <- gcheckboxgroup(items = check_names, checked = as.logical(check_wid), container = cbox, use.table = F, width = 450)
				# widgets for band selection - quality
				
				check_names_quality <- prod_opt_list[[checked]]$quality_fullnames # retrieve quality band names (if existing for sel. product)
				if (!is.null(check_names_quality)) {
					check_wid_quality<- temp_wid_bands_quality						    # retrieve currently selected quality layers (if existing for sel. product)
					cbox_quality<- gframe(text = '<span foreground="blue" size="large">		Quality Indicators				</span>', markup = T, container = cbox_total, horizontal = FALSE, width = 450)
					bands_wid_quality <- gcheckboxgroup(items = check_names_quality, checked = as.logical(check_wid_quality), container = cbox_quality, use.table = F, width = 450)
				}
				
				# widgets for band selection - indexes
				check_names_indexes <- prod_opt_list[[checked]]$indexes_fullnames # retrieve indexes band names
				if (!is.null(check_names_indexes)) {
					check_wid_indexes<- temp_wid_bands_indexes							# retrieve currently selected indexes layers 
					cbox_indexes<- gframe(text = '<span foreground="blue" size="large">		Additional Spectral Indexes				</span>', markup = T, container = cbox_total, horizontal = FALSE, width = 450)
					bands_wid_indexes <- gcheckboxgroup(items = check_names_indexes, checked = as.logical(check_wid_indexes), container = cbox_indexes, use.table = F, width = 450)
				} 
				
				# Start/Cancel widgets
				bands_group <- ggroup(container = selgroup, horizontal = TRUE)
				accept_but <- gbutton(text = 'Start', container = bands_group, handler = function(button,...){ # On accept, retrieve and save selected layers
							
							pos_wid <- which(check_names %in% svalue (bands_wid))   # ? which layers selected ? --> store in temp_wid_bands array
							tmp_arr_bands <- array(data = 0 , dim = length(check_names))		
							tmp_arr_bands[pos_wid] <- 1   ;	temp_wid_bands <<- tmp_arr_bands
							if (length(which(check_names_indexes != '') > 0)) {    # ? which indexes selected ? --> store in temp_wid_bands_indexes array
								pos_wid <- which(check_names_indexes %in% svalue (bands_wid_indexes))   ;		tmp_arr_ind <- array(data = 0 , dim = length(check_names_indexes))  
								tmp_arr_ind[pos_wid] <- 1	; temp_wid_bands_indexes <<- tmp_arr_ind	
							}
							if (length(which(check_names_quality != '') > 0)) {    # ? which quality selected ? --> store in temp_wid_bands_quality array
								pos_wid <- which(check_names_quality %in% svalue(bands_wid_quality))   ;		tmp_arr_qual <- array(data = 0 , dim = length(check_names_quality))  
								tmp_arr_qual[pos_wid] <- 1	; temp_wid_bands_quality <<- tmp_arr_qual	
								
							}
							dispose(selgroup)			 # close layers selection child widget 
							
						})
				cancel_but <- gbutton(text = 'Cancel', container = bands_group, handler = function(button,...){  # if Cancel, reset selected layers to previous choice and exit
							
							if (exists('check_wid')) {temp_wid_bands <<- check_wid}
							if (exists('check_wid_indexes')) {temp_wid_bands_indexes <<- check_wid_indexes}
							if (exists('check_wid_quality')) {temp_wid_bands_quality <<- check_wid_quality}
							dispose(selgroup)
						})
				# Widget for "www" button
				addSpace(bands_group, 760, horizontal=TRUE)
				www_but <- gbutton(text = 'Product details', container = bands_group, handler = function(button,...) browseURL(prod_opt_list[[checked]]$www))
				
				visible(selgroup, set=TRUE)    # visualize band selection widgets
				
			},container =prod_group, width = 120, expand = T)
	
	
#- ------------------------------------------------------------------------------- -#
# Widgets for Dates selection	  
#- ------------------------------------------------------------------------------- -#
	{{dates_frame <- gframe(text = '<span foreground="blue" size="large">Processing Period</span>', markup = T, container = main_group, horizontal = T, expand = T, spacing = 10)
			# start date ----
			start_date_lab <- glabel(text = '<span weight = "bold" >Starting Date:</span>',markup = T, container = dates_frame)  ; size (start_date_lab ) = c(120,20) 
			start_day_wid <- gspinbutton(1,31,  container=dates_frame , value = general_opts$start_day)
			start_month_wid <- gspinbutton(1,12,  container=dates_frame , value = general_opts$start_month)
			start_year_wid <- gspinbutton(2000 ,2020,  container=dates_frame , value = general_opts$start_year, horizontal = T)
			size (start_day_wid) = c(35,25)    ; size (start_month_wid) = c(35,25)   ; size (start_year_wid) = c(55,25) 
			
			# End date ----
			end_date_lab <- glabel(text = '<span weight = "bold" >Ending Date:</span>', markup = T,container = dates_frame)  ; size (end_date_lab ) = c(120,20) 
			end_day_wid <- gspinbutton(1,31,  container=dates_frame , value = general_opts$end_day)
			end_month_wid <- gspinbutton(1,12,  container=dates_frame , value = general_opts$end_month)
			end_year_wid <- gspinbutton(2000,2020,  container=dates_frame , value = general_opts$end_year)
			size (end_day_wid) <- c(35,25)    ; size (end_month_wid) <- c(35,25)    ; size (end_year_wid) <- c(55,25) 
		}}
	
#- ------------------------------------------------------------------------------- -#
# Widgets for Tiles selection	  
#- ------------------------------------------------------------------------------- -#
	tiles_group <- gframe(text = '<span foreground="blue" size="large">Required MODIS Tiles </span>', markup = T, container = main_group, horizontal = F, expand = T, spacing = 10)
	x_group <- ggroup(container = tiles_group, horizontal = TRUE)
	# horizontal
	start_x_lab <- glabel(text = '<span weight = "bold" >Horizontal Tiles:</span>',markup = T, container = x_group) ; size(start_x_lab) = c(120,20)
	start_x_start <- glabel(text = 'Start: ', container = x_group) ; size(start_x_start) = c(35,25) 
	start_x_wid <- gspinbutton(1, 35, text = 'Select', container=x_group, value = general_opts$start_x)
	end_x_lab <- glabel(text = 'End: ', container = x_group) ; size(end_x_lab) = c(35,25) 
	end_x_wid <- gspinbutton(1, 35, text = 'Select', container=x_group, value = general_opts$end_x)
	size (start_x_wid) = c(35,25)    ; size (end_x_wid) = c(35,25)  
	show_map <- gbutton(text = 'Show Tiles Map', border = T,
			handler = function(h,....) {x11(10,6, )
				plot(raster(file.path(general_opts$main_dir, "/Accessoires/MODIS_Tiles.gif")))},
			container =x_group )
	# vertical
	y_group <- ggroup(container = tiles_group, horizontal = TRUE)
	start_y_lab <- glabel(text = '<span weight = "bold" >Vertical Tiles:</span>',markup = T, container = y_group) ; size(start_y_lab) = c(120,20) 
	start_y_start <- glabel(text = 'Start: ', container = y_group) ; size(start_y_start) = c(35,25) 
	start_y_wid <- gspinbutton(0,17, text = 'Select', container=y_group, value = general_opts$start_y)
	end_y_lab <- glabel(text = 'End: ', container = y_group); size(end_y_lab) = c(35,25) 
	end_y_wid <- gspinbutton(0,17, text = 'Select', container=y_group, value = general_opts$end_y)
	size (start_y_wid) <- c(35,25)   ; size (end_y_wid) <- c(35,25) 
	
	
#- ------------------------------------------------------------------------------- -#
# Widgets for Projection, resolution and bbox selection 
#- ------------------------------------------------------------------------------- -#
	output_proj_frame <- gframe(text = '<span foreground="blue" size="large">Reprojection and Resize Options</span>',markup = T, container = main_group, horizontal = FALSE, expand = T, spacing = 10)
	output_proj_group <- ggroup (container = output_proj_frame, horizontal = TRUE, spacing = 5)
	font (output_proj_frame) <- list(weight = 'bold', color  = 'blue')
	
	# Projection ----
	output_proj_lab <- glabel(text = '<span weight = "bold" >Output Projection:</span>', container = output_proj_group,markup = T) 
	size(output_proj_lab) = c(120,20)
	proj_wid <- gcombobox(general_opts$out_proj_names, container=output_proj_group, selected = match(general_opts$proj, general_opts$out_proj_names), handler = function(h,....) {
				current_sel <- svalue(proj_wid) 
				if (current_sel != 'User Defined') { enabled(output_proj4_wid) <- F} else {(enabled(output_proj4_wid) <- T)}
			})
	size(proj_wid) = c(120,20)
	
	outproj_user_lab<- glabel(text = '<span weight = "bold" >PROJ4 String:</span>', container = output_proj_group,markup = T) 
	size(outproj_user_lab) = c(120,20)
	
	output_proj4_wid <- gedit(text = general_opts$user_proj4, container = output_proj_group, width = 30)
	if (general_opts$proj == 'User Defined') { enabled(output_proj4_wid) <- T} else {(enabled(output_proj4_wid) <- F)}
	size(output_proj4_wid) = c(120,20)
	
	# Resolution ----
	output_res_group <- ggroup (container = output_proj_frame, horizontal = TRUE, spacing = 5)
	output_res_lab <- glabel(text = '<span weight = "bold" >Output Resolution:</span>',markup = T, container = output_res_group) 
	size(output_res_lab) = c(120,20)
	
	output_res_sel_wid  <-  gcombobox(c('Native','Resampled'), container=output_res_group, 
			selected = match(general_opts$out_res_sel, c('Native','Resampled')), handler = function(h,....) {
				current_sel <- svalue(output_res_sel_wid) 
				if (current_sel == 'Native') { enabled(output_res_wid) <- F} else {(enabled(output_res_wid) <- T)}
			}) 
	size(output_res_sel_wid) = c(120,20)
	
	pixsize_lab <- glabel(text = '<span weight = "bold" >Pixel Size:</span>',markup = T, container = output_res_group) 
	size (pixsize_lab) <- c(120,20)
	
	output_res_wid <- gedit(text = general_opts$out_res , container = output_res_group)   
	size(output_res_wid) <- c(120,20)
	if (general_opts$out_res_sel == 'Native') { enabled(output_res_wid) <- F} else {(enabled(output_res_wid) <- T)}
	pixsize2_lab <- glabel(text = '(IN OUTPUT PROJECTION !)', container = output_res_group)
	
	# Resampling ----
	resopts_group <- ggroup (container = output_proj_frame, horizontal = TRUE)
	resmeth_lab <- glabel(text = '<span weight = "bold" >Resampling Method:</span>',markup = T, container = resopts_group) 
	size(resmeth_lab) = c(120,20)
	resamp_array <- c('near','bilinear','cubic', 'cubicspline','lanczos','average','mode')
	output_resmeth_wid <-  gcombobox(resamp_array, container=resopts_group, selected = match(general_opts$resampling, resamp_array)) 
	size (output_resmeth_wid) <- c(120,20)
	
	# Extent ----
	output_ext_lab <- glabel(text = '<span weight = "bold" >Output Extent:</span>',markup = T, container = resopts_group) 
	size(output_ext_lab) <- c(120,20)
	output_ext_wid <-  gcombobox(c('Full Tiles Extent','Resized'), container=resopts_group, 
			selected = match(general_opts$full_ext, c('Full Tiles Extent','Resized')), handler = function(h,....) {
				current_sel = svalue(output_ext_wid) 
				if (current_sel == 'Full Tiles Extent') { enabled(bbox_group) <- F} else {(enabled(bbox_group) <- T)}
			}) 
	size (output_ext_wid) <- c(120,20)
	
	# bounding box ----
	bbox_group <- ggroup (horizontal = FALSE, container=output_proj_frame)
	output_bbox_lab <- glabel(text = '<span weight = "bold" > Bounding Box for output images (IN OUTPUT PROJECTION !) </span>',markup = T, container = bbox_group) 
	size(output_ext_lab) <- c(120,15)
	Lon_group <- ggroup (horizontal = TRUE, container=bbox_group)
	output_ULeast_lab <- glabel('Upper Left Easting (xmin)', container = Lon_group)   
	size (output_ULeast_lab) <- c(160,20)
	output_ULeast_wid <- gedit(text = general_opts$bbox[1], container = Lon_group, width = 10)
	addSpace(Lon_group, 30, horizontal=TRUE)
	output_LReast_lab <- glabel('Lower Right Easting (xmax)', container = Lon_group)   
	size (output_LReast_lab) <- c(160,20)
	output_LReast_wid <- gedit(text = general_opts$bbox[2], container = Lon_group, width = 10)
	
	Lat_group <- ggroup (horizontal = TRUE, container=bbox_group)
	
	output_LRnorth_lab <- glabel('Lower Right Northing (ymin)', container = Lat_group) 
	size (output_LRnorth_lab) = c(160,20)
	output_LRnorth_wid <- gedit(text = general_opts$bbox[3], container = Lat_group, width = 10)
	addSpace(Lat_group, 30, horizontal=TRUE)
	output_ULnorth_lab <- glabel('Upper Left Northing (ymax)', container = Lat_group) 
	size (output_ULnorth_lab) <- c(160,20)
	output_ULnorth_wid <- gedit(text = general_opts$bbox[4], container = Lat_group, width = 10)
	
	if (general_opts$full_ext == 'Full Tiles Extent') { enabled(bbox_group) <- F} else {(enabled(bbox_group) <- T)}  # Grey out bbox if NAtive extent
	
	
	
#- ------------------------------------------------------------------------------- -#
# Widgets for Format and reprocess options 
#- ------------------------------------------------------------------------------- -#
	options_frame <- gframe(text = '<span foreground="blue" size="large">Processing Options</span>', markup = T, container = main_group, expand = T,spacing = 10, horizontal = F)
	opt_group <- ggroup(container = options_frame, horizontal = T, expand = T)
	
	# Out format ----
	format_lab <- glabel(text = '<span weight = "bold" >Output Files Format </span>',markup = T, container = opt_group)
	format_wid <- gdroplist(items = c('ENVI','GTiff'), text = 'Select', container=opt_group, selected = match(general_opts$out_format, c('ENVI','GTiff')),handler = function(h,....) {
				current_sel <- svalue(format_wid)
				if (current_sel != 'GTiff') { enabled(compress_wid) <- F} else {(enabled(compress_wid) <- T)}
			})
	addSpace(opt_group, 100, horizontal=TRUE)
	
	# Compression ----	
	compress_dict <- c('NONE','PACKBITS','LZW','DEFLATE'); names(compress_dict) = c('None','Low (PACKBITS)','Medium (LZW)','High (DEFLATE)')
	compress_lab <- glabel(text = '<span weight = "bold" >GTiff Compression: </span>',markup = T, container = opt_group)
	compress_wid <- gcombobox( names(compress_dict), container=opt_group,selected <- match(general_opts$compress, names(compress_dict)))
	if (general_opts$out_format == 'GTiff') { enabled(compress_wid) <- T} else {(enabled(compress_wid) <- F)} # grey out compression if ENVI output
	
	# virtual and NODATA ----		
	other_group <- ggroup(container = options_frame, horizontal = T)
	timeseries_lab <- glabel(text = '<span weight = "bold" >Virtual Time Series: </span>',markup = T, container = other_group)
	timeseries_wid <- gcombobox( c('None','ENVI Meta Files','GDAL vrt Files','ENVI and GDAL'), container=other_group, 
			selected <- match(general_opts$ts_format, c('None','ENVI Meta Files','GDAL vrt Files','ENVI and GDAL')), handler = function(h,....) {
				current_sel <- svalue(timeseries_wid)
			})
	addSpace(other_group,34 )
	nodata_lab <- glabel(text = '<span weight = "bold" >Change Original NODATA values</span>',markup = T, container = other_group)
	nodata_wid <- gradio(items = c('Yes','No'), text = 'Select', container=other_group, selected = match(general_opts$nodata_change, c('Yes','No')), horizontal = T)
	
#- ------------------------------------------------------------------------------- -#
# Widgets for output folders selection  
#- ------------------------------------------------------------------------------- -#
	
	# Main output folder ----
	outfold_frame <- gframe(text = '<span foreground="blue" size="large">Main Output Folder for Time Series storage</span>', markup = T, container=main_group, expand = T,spacing = 10)    			# Frame group
	outfold_group <- ggroup(horizontal = TRUE, container=outfold_frame)  				# Main group
	outfold_wid <- gedit(text = format(general_opts$out_folder, justify = "right") , container=outfold_group, width = 46)			# Selected file
	fold_choose <- gbutton("Browse", handler=function(h,...) {choice<-gfile(type="selectdir", text="Select the Output Folder for MODIS data...")		# File selection widget
				if(! is.na(choice)){svalue(outfold_wid)<-choice						## On new selection, set value of the label widget
					general_opts$out_folder = format(choice, justify = "left")	# 	On new selection,  Set value of the selected variable
				}}, container=outfold_group)
	
	# Reprocessing options chackbox ----
	reprocess_lab <- glabel(text = '<span weight = "bold" >ReProcess Existing Data</span>',markup = T, container = outfold_group)
	reprocess_wid <- gradio(items = c('Yes','No'), text = 'Select', container=outfold_group, selected = match(general_opts$reprocess, c('Yes','No')), horizontal = T)
	addSpace(outfold_group, 5, horizontal=TRUE)
	
	
	# HDF output folder ----
	outfoldmod_frame <- gframe(text = '<span foreground="blue" size="large">Output Folder for Original HDF files download</span>', markup = T, container=main_group, expand = T,spacing = 10)    			# Frame group
	outfoldmod_group <- ggroup(horizontal = TRUE, container=outfoldmod_frame)  				# Main group
	outfoldmod_wid <- gedit(text = format(general_opts$out_folder_mod, justify = "right") , container=outfoldmod_group, width = 46)			# Selected file
	fold_choose <- gbutton("Browse", handler=function(h,...) {choice<-gfile(type="selectdir", text="Select the Output Folder for storage of original HDFs...")		# File selection widget
				if(! is.na(choice)){svalue(outfoldmod_wid)<-choice						## On new selection, set value of the label widget
					general_opts$out_folder_mod = format(choice, justify = "left")	# 	On new selection,  Set value of the selected variable
				}}, container=outfoldmod_group)
	
	# HDF delete option checkbox ----
	delete_group <- ggroup(container = outfoldmod_group, horizontal = T)
	delete_lab <- glabel(text = '<span weight = "bold" >Delete original HDF files</span>',markup = T, container = delete_group)
	delete_wid <- gradio(items = c('Yes','No'), text = 'Select', container=delete_group, selected = 2, horizontal = T)
	
	
#- ------------------------------------------------------------------------------- -#
# Start/Quit/Save/Load buttons 
#- ------------------------------------------------------------------------------- -#
	but_group <- ggroup(container = main_group, horizontal = TRUE)
	
	start_but <- gbutton(text = 'Start', container = but_group, handler = function (h,....) {# If "Start" pressed, retrieve selected values and save in previous file
				general_opts$sel_prod <- mod_prod_list[which(mod_prod_list == svalue(prod_wid))]						# Products options
				general_opts$sensor <- svalue(sens_wid)
				
				if (exists ('temp_wid_bands')) {
					prod_opt_list[[general_opts$sel_prod]]$bandsel <- temp_wid_bands			#retrieve selected bands
					
				}
				
				if (exists ('temp_wid_bands_indexes')) {
					prod_opt_list[[general_opts$sel_prod]]$indexes_bandsel <- temp_wid_bands_indexes #retrieve selected indexes
					
				}
				
				if (exists ('temp_wid_bands_quality')) {
					prod_opt_list[[general_opts$sel_prod]]$quality_bandsel <- temp_wid_bands_quality 	#retrieve selected quality ind.
					
				}
				
				general_opts$start_day <- svalue(start_day_wid)		# Retrieve Dates options
				general_opts$start_month <- svalue(start_month_wid)
				general_opts$start_year <- svalue(start_year_wid)
				general_opts$end_day <- svalue(end_day_wid)
				general_opts$end_month <- svalue(end_month_wid)
				general_opts$end_year <- svalue(end_year_wid)
				
				general_opts$start_x <- svalue(start_x_wid)		# Retrieve Tiles options
				general_opts$end_x <- svalue(end_x_wid)
				general_opts$start_y <- svalue(start_y_wid)
				general_opts$end_y <- svalue(end_y_wid)
				
				general_opts$proj <- svalue(proj_wid)		# Retrieve Proj and extent options
				general_opts$user_proj4 <- svalue(output_proj4_wid)	
				general_opts$out_res_sel <- (svalue(output_res_sel_wid))			
				general_opts$out_res <- (svalue(output_res_wid))
				general_opts$resampling <- svalue(output_resmeth_wid)
				general_opts$full_ext <- svalue(output_ext_wid)
				general_opts$bbox <- ((c(svalue(output_ULeast_wid),svalue(output_LReast_wid),
										svalue(output_LRnorth_wid),svalue(output_ULnorth_wid))))
				
				general_opts$reprocess <- svalue(reprocess_wid)  # Retrieve reprocess, delete and nodata 
				general_opts$delete_hdf <- svalue(delete_wid)
				general_opts$nodata_change <- svalue(nodata_wid)
				
				general_opts$out_format <- svalue(format_wid)   # Retrieve format, virtual and compression
				general_opts$ts_format <- svalue(timeseries_wid)
				general_opts$compress <- compress_dict[svalue(compress_wid)]
				
				general_opts$out_folder <- svalue(outfold_wid)		# # Retrieve Folder options
				general_opts$out_folder_mod <- svalue(outfoldmod_wid) 
				
				check <- T
				# Send warning if HDF deletion selected 
				if (general_opts$delete_hdf == 'Yes') {res = gconfirm('Warning ! HDF files in Original MODIS folder will be deleted at the end of processing ! Are you sure ? ', title = 'Warning', icon = 'warning')
				}	
				
				#- Perform checks on options consistency ---------------
				
				# Check if at least 1 layer selected
				if (max(prod_opt_list[[general_opts$sel_prod]]$bandsel)+
						max(prod_opt_list[[general_opts$sel_prod]]$indexes_bandsel)+
						max(prod_opt_list[[general_opts$sel_prod]]$quality_bandsel) == 0) {gmessage('No Output bands or indexes selected - Please Correct !', title = 'Warning') ; check <- F}
				
				# Check if dates, processing extent and tiles selection make sense
				if (as.Date(paste(general_opts$start_year, general_opts$start_month, general_opts$start_day, sep = '-')) >
						as.Date(paste(general_opts$end_year, general_opts$end_month, general_opts$end_day, sep = '-'))) {gmessage('Error in Selected Dates', title = 'Warning'); check <- F}
				
				if ((general_opts$start_x > general_opts$end_x ) | (general_opts$start_y > general_opts$end_y )) {gmessage('Error in Selected Tiles', title = 'Warning') ; check <- F}
				
				# Check if bbox is consistent	
				general_opts$bbox <- as.numeric(general_opts$bbox)
				n_bbox_compiled <- length(which(is.finite(general_opts$bbox)))
				
				if (general_opts$full_ext != 'Full Tiles Extent') {
					if (n_bbox_compiled == 4){
						if ((general_opts$bbox[1] > general_opts$bbox[2]) | (general_opts$bbox[3] > general_opts$bbox[4])) {gmessage('Error in Selected Output extent', title = 'Warning') ; check <- F}}
					if ((n_bbox_compiled < 4) & (n_bbox_compiled >= 0 )) {gmessage('Error in Selected Output extent', title = 'Warning') ; check <- F}
				}
				
				# check if folders are defined
				if (general_opts$out_folder == ''){gmessage('Please Select an output folder !', title = 'Warning') ; check <- F}
				if (general_opts$out_folder_mod == ''){gmessage('Please Select an output folder for storing original HDFs!', title = 'Warning') ; check <- F}
				
				if (check == T) {					# If check passed, save previous file and return
#					dir.create(file.path(getwd(),'Previous'),showWarnings=FALSE)	
					
					save(general_opts,prod_opt_list,mod_prod_list, file = general_opts$previous_file) # Save options to previous file 
					assign("Quit", F, envir=globalenv()) # If "Start", set "Quit to F
					rm(temp_wid_bands, envir = globalenv())
					rm(temp_wid_bands_indexes, envir = globalenv())
					rm(temp_wid_bands_quality, envir = globalenv())
					dispose(main_win)
				}
			})
	
	# On "Quit", exit
	quit_but <- gbutton(text = 'Quit', container = but_group, handler = function(h,...){ # If "Quit", set "Quit to T and exit
				assign("Quit", T, envir=globalenv())
				dispose(main_win)
			})
	
	addSpace(but_group, 280, horizontal=TRUE)
	
	# On "Load", ask for a old options file and load it --------
	load_but <- gbutton(text = 'Load Options from File', container = but_group, handler = function (h,....){
				
				choice<-gfile(type="open", text="Select file for loading processing options...")	# ask for file		 	
				
				if(! is.na(choice)){
					load(choice)  # load file and reset all widgets to values found in the loaded file
					svalue(prod_wid) <- general_opts$sel_prod
					
					svalue(sens_wid)<-general_opts$sensor 
					
					temp_wid_bands <<- prod_opt_list[[general_opts$sel_prod]]$bandsel				# set dummy variables holding the initial values of selected bands
					temp_wid_bands_indexes <<- prod_opt_list[[general_opts$sel_prod]]$indexes_bandsel
					temp_wid_bands_quality <<- prod_opt_list[[general_opts$sel_prod]]$quality_bandsel
					
					svalue(start_day_wid)<-general_opts$start_day  	# Dates options
					svalue(start_month_wid)<- general_opts$start_month  
					svalue(start_year_wid) <- general_opts$start_year 
					svalue(end_day_wid) <- general_opts$end_day 
					svalue(end_month_wid) <-general_opts$end_month  
					svalue (end_year_wid) <-general_opts$end_year   
					
					svalue(start_x_wid)<- general_opts$start_x  		# Tiles options
					svalue(end_x_wid) <-general_opts$end_x  
					svalue(start_y_wid) <-general_opts$start_y   
					svalue(end_y_wid) <-general_opts$end_y   
					
					svalue(proj_wid)	 <-general_opts$proj 	# Proj and extent options
					svalue(output_proj4_wid)	 <- general_opts$user_proj4 
					svalue(output_res_sel_wid) <-general_opts$out_res_sel 			
					svalue(output_res_wid) <-general_opts$out_res 
					svalue(output_resmeth_wid) <- general_opts$resampling
					svalue(output_ext_wid) <-general_opts$full_ext 
					svalue(output_ULeast_wid) <- general_opts$bbox [1]
					svalue(output_LReast_wid) <- general_opts$bbox [2]
					svalue(output_LRnorth_wid) <- general_opts$bbox [3]
					svalue(output_ULnorth_wid) <- general_opts$bbox [4]
					svalue(reprocess_wid) <- general_opts$reprocess
					svalue(delete_wid) <- general_opts$delete_hdf 
					svalue(nodata_wid) <- general_opts$nodata_change 
					
					
					svalue(format_wid) <- general_opts$out_format 
					svalue(timeseries_wid) <- general_opts$ts_format 
					svalue(compress_wid) <- general_opts$compress  
					
					svalue(outfold_wid) <- 	general_opts$out_folder # Folder options
					svalue(outfoldmod_wid) <- general_opts$out_folder_mod
				}
			})
	
	# On "Save", ask for a file name and save options (must be a RData file !)  --------
	save_but <- gbutton(text = 'Save Options', container = but_group, handler = function (h,....) {
				
				choice<-gfile(type="save", text="Select file for saving processing options...")		# File selection widget
				
				if(!is.na(choice)){
					
					# If "Start" pressed, retrieve selected values and save in RData file
					general_opts$sel_prod <- mod_prod_list[which(mod_prod_list == svalue(prod_wid))]						# Products options
					sel_prod = general_opts$sel_prod    # When saving, set sel_prod to current selection (may be good to change) 
					general_opts$sensor <- svalue(sens_wid)
					
					if (exists ('temp_wid_bands')) {
						prod_opt_list[[sel_prod]]$bandsel <- temp_wid_bands			#retrieve selected bands
					}
					
					if (exists ('temp_wid_bands_indexes')) {
						prod_opt_list[[sel_prod]]$indexes_bandsel <- temp_wid_bands_indexes #retrieve selected indexes
					}
					
					if (exists ('temp_wid_bands_quality')) {
						prod_opt_list[[sel_prod]]$quality_bandsel <- temp_wid_bands_quality 	#retrieve selected quality ind.
					}
					
					general_opts$start_day <- svalue(start_day_wid)		# Dates options
					general_opts$start_month <- svalue(start_month_wid)
					general_opts$start_year <- svalue(start_year_wid)
					general_opts$end_day <- svalue(end_day_wid)
					general_opts$end_month <- svalue(end_month_wid)
					general_opts$end_year <- svalue(end_year_wid)
					
					general_opts$start_x <- svalue(start_x_wid)		# Tiles options
					general_opts$end_x <- svalue(end_x_wid)
					general_opts$start_y <- svalue(start_y_wid)
					general_opts$end_y <- svalue(end_y_wid)
					
					general_opts$proj <- svalue(proj_wid)		# Proj and extent options
					general_opts$user_proj4 <- svalue(output_proj4_wid)	
					general_opts$out_res_sel<- (svalue(output_res_sel_wid))			
					general_opts$out_res <- (svalue(output_res_wid))
					general_opts$resampling <- svalue(output_resmeth_wid)
					general_opts$full_ext <- svalue(output_ext_wid)
					general_opts$bbox <- ((c(svalue(output_ULeast_wid),svalue(output_LReast_wid),
											svalue(output_LRnorth_wid),svalue(output_ULnorth_wid))))
					
					general_opts$reprocess <- svalue(reprocess_wid)
					general_opts$delete_hdf <- svalue(delete_wid)
					general_opts$nodata_change <- svalue(nodata_wid)
					
					general_opts$out_format <- svalue(format_wid)
					general_opts$ts_format <- svalue(timeseries_wid)
					general_opts$compress <- svalue(compress_wid)
					
					general_opts$out_folder <- svalue(outfold_wid)		# Folder options
					general_opts$out_folder_mod <- svalue(outfoldmod_wid)		# Folder options
					check <- T
					
					# Check if at least 1 layer selected
					if (max(prod_opt_list[[general_opts$sel_prod]]$bandsel)+
							max(prod_opt_list[[general_opts$sel_prod]]$indexes_bandsel)+
							max(prod_opt_list[[general_opts$sel_prod]]$quality_bandsel) == 0) {gmessage('No Output bands or indexes selected - Please Correct !', title = 'Warning') ; check <- F}
					
					# Check if dates, processing extent and tiles selection make sense
					if (as.Date(paste(general_opts$start_year, general_opts$start_month, general_opts$start_day, sep = '-')) >
							as.Date(paste(general_opts$end_year, general_opts$end_month, general_opts$end_day, sep = '-'))) {gmessage('Error in Selected Dates', title = 'Warning'); check <- F}
					
					if ((general_opts$start_x > general_opts$end_x ) | (general_opts$start_y > general_opts$end_y )) {gmessage('Error in Selected Tiles', title = 'Warning') ; check <- F}
					
					general_opts$bbox <- as.numeric(general_opts$bbox)
					n_bbox_compiled <- length(which(is.finite(general_opts$bbox)))
					
					# Check if bbox is consistent	
					general_opts$bbox <- as.numeric(general_opts$bbox)
					n_bbox_compiled <- length(which(is.finite(general_opts$bbox)))
					
					if (general_opts$full_ext != 'Full Tiles Extent') {
						if (n_bbox_compiled == 4){
							if ((general_opts$bbox[1] > general_opts$bbox[2]) | (general_opts$bbox[3] > general_opts$bbox[4])) {gmessage('Error in Selected Output extent', title = 'Warning') ; check <- F}}
						if ((n_bbox_compiled < 4) & (n_bbox_compiled >= 0 )) {gmessage('Error in Selected Output extent', title = 'Warning') ; check <- F}
					}
					
					# check if folders are defined
					if (general_opts$out_folder == ''){gmessage('Please Select an output folder !', title = 'Warning') ; check <- F}
					if (general_opts$out_folder_mod == ''){gmessage('Please Select an output folder for storing original HDFs!', title = 'Warning') ; check <- F}
					
					
					if (check == T) {					# If check passed, save previous file and return
						save(general_opts,prod_opt_list,mod_prod_list, file = choice)
					}
				}
			})		
	
	visible(main_win, set=TRUE) ## show the selection GUI
	
}  # END 
