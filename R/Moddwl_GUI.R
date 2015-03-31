#' @Title moddwl_GUI
#' @Description
#'	Function used to generate and handle the GUI used to allow selection of   
#' @details
#'
#' @param general_opts General options (paths and other stuff) passed by the moddwl_main 
#' @returnType 
#' @return 
#'		NONE - Processing options are saved in "previous" file and (if "Save options" is pressed) in user's selected file
#' @author Lorenzo Busetto, phD (2014)
#' email: busetto.l@@irea.cnr.it
#'
#' @license GPL(>2)
#' @export
moddwl_GUI = function (general_opts){
		
	# Restore previous options file if existing, otherwise create a "dummy" one with default values
	if (file.exists(general_opts$previous_file)) {
		
		load(general_opts$previous_file)
	} else {
		
#		moddwl_set_opts(previous_file = general_opts$previous_file) ; load(general_opts$previous_file)}
		moddwl_read_xml_opts(previous_file = general_opts$previous_file,xml_file = general_opts$xml_file ) ; load(general_opts$previous_file)}
	
	assign("Quit", T, envir=globalenv())	
	
#- ------------------------------------------------------------------------------- -#
#  Start Building the GUI
#- ------------------------------------------------------------------------------- -#
	{{main_win <- gbasicdialog(title = "Select Main Processing Options", parent=NULL, do.buttons=F, 
					visible = F, spacing = 10)
			main_group <- ggroup(container = main_win, horizontal = FALSE, expand = T)
			sel_prod <- general_opts$sel_prod
		}}	
	
	
	
#- ------------------------------------------------------------------------------- -#
# Widgets for product selection and bands selection  
#- ------------------------------------------------------------------------------- -#
	{{satprod_frame <- gframe(text ='<span foreground="blue" size="large">MODIS Product, bands and satellites selection</span>', markup = T,horizontal = FALSE, container=main_group, spacing = 15)
			prod_frame <- gframe(text ="Select MODIS Product and bands",horizontal = TRUE, container=satprod_frame, spacing = 15)
			checked <- which(mod_prod_list == general_opts$sel_prod)

			temp_wid_bands <<- prod_opt_list[[checked]]$bandsel				# set dummy variables holding the initial values of selected bands
			temp_wid_bands_indexes <<- prod_opt_list[[checked]]$indexes_bandsel
			temp_wid_bands_quality <<- prod_opt_list[[checked]]$quality_bandsel
			
			prod_wid <- gdroplist(items = mod_prod_list, container=prod_frame, horizontal = T, selected = checked,  handler = function(h,...) {
						checked <- mod_prod_list[which(mod_prod_list == svalue(prod_wid))]		# find index of sel. product
						temp_wid_bands <<- rep(0, length(prod_opt_list[[checked]]$bandsel))					# reset dummy variables for band selection to 0 on product change
						temp_wid_bands_indexes <<- rep(0, length(prod_opt_list[[checked]]$indexes_bandsel))
						temp_wid_bands_quality <<- rep(0, length(prod_opt_list[[checked]]$quality_bandsel))
						
					})
			addSpace(prod_frame, 100, horizontal=TRUE)
			band_wid <- gbutton(text = 'Select Processing Bands', border = T,				# Child widget for processing bands selection
					handler = function(h,....) {
						
						checked <- mod_prod_list[which(mod_prod_list == svalue(prod_wid))]		# find index of sel. product
						check_names <- prod_opt_list[[checked]]$band_fullnames						# retrieve band names
						check_wid <- temp_wid_bands																				# retrieve selected at the time
						selgroup <- gbasicdialog(title = "Select Processing Bands						", parent=NULL, do.buttons=F, width = 500, horizontal = T)
						# widgets for band selection - original
						cbox_total <- gframe(text = "Select Processing Bands", container = selgroup, horizontal = T, width = 500)
						cbox<- gframe(text = "Original MODIS Bands					   ", container = cbox_total, horizontal = T, width = 500)
						bands_wid <- gcheckboxgroup(items = check_names, checked = as.logical(check_wid), container = cbox, use.table = F, width = 500)
						# widgets for band selection - indexes
						check_names_indexes <- prod_opt_list[[checked]]$indexes_fullnames # retrieve indexes band names
						
						if (!is.null(check_names_indexes)) {
							check_wid_indexes<- temp_wid_bands_indexes													# retrieve indexes fullnames selected at the time
							cbox_indexes<- gframe(text = "Spectral Indexes						", container = cbox_total, horizontal = FALSE, width = 500)
							bands_wid_indexes <- gcheckboxgroup(items = check_names_indexes, checked = as.logical(check_wid_indexes), container = cbox_indexes, use.table = F, width = 500)
						} 
						
						# widgets for band selection - quality
						check_names_quality <- prod_opt_list[[checked]]$quality_fullnames # retrieve quality band names
						
						if (!is.null(check_names_quality)) {
							check_wid_quality<- temp_wid_bands_quality													# retrieve quality fullnames selected at the time
							cbox_quality<- gframe(text = "Quality Indicators						", container = cbox_total, horizontal = FALSE, width = 500)
							bands_wid_quality <- gcheckboxgroup(items = check_names_quality, checked = as.logical(check_wid_quality), container = cbox_quality, use.table = F, width = 500)
						}
						
						# Start/Cancel widgets
						bands_group <- ggroup(container = selgroup, horizontal = TRUE)
						accept_but <- gbutton(text = 'Start', container = bands_group, handler = function(button,...){
									
									pos_wid <- which(check_names %in% svalue (bands_wid))   ;		tmp_arr_bands <- array(data = 0 , dim = length(check_names))		
									tmp_arr_bands[pos_wid] <- 1   ;	temp_wid_bands <<- tmp_arr_bands
									if (length(which(check_names_indexes != '') > 0)) {
										pos_wid <- which(check_names_indexes %in% svalue (bands_wid_indexes))   ;		tmp_arr_ind <- array(data = 0 , dim = length(check_names_indexes))  
										tmp_arr_ind[pos_wid] <- 1	; temp_wid_bands_indexes <<- tmp_arr_ind	
									}
									if (length(which(check_names_quality != '') > 0)) {
										pos_wid <- which(check_names_quality %in% svalue(bands_wid_quality))   ;		tmp_arr_qual <- array(data = 0 , dim = length(check_names_quality))  
										tmp_arr_qual[pos_wid] <- 1	; temp_wid_bands_quality <<- tmp_arr_qual	
										
									}
									dispose(selgroup)			
									print(checked)
									print(temp_wid_bands)
									print(temp_wid_bands_indexes)
									print(temp_wid_bands_quality)
									
									
								})
						cancel_but <- gbutton(text = 'Cancel', container = bands_group, handler = function(button,...){
									
									if (exists('check_wid')) {temp_wid_bands <<- check_wid}
									if (exists('check_wid_indexes')) {temp_wid_bands_indexes <<- check_wid_indexes}
									if (exists('check_wid_quality')) {temp_wid_bands_quality <<- check_wid_quality}
									dispose(selgroup)
								})
						addSpace(bands_group, 760, horizontal=TRUE)
						www_but <- gbutton(text = 'Product details', container = bands_group, handler = function(button,...) browseURL(prod_opt_list[[checked]]$www))
						
						visible(selgroup, set=TRUE)    # visualize band selection widgets
						
					},container =prod_frame)
		}}
	
#- ------------------------------------------------------------------------------- -#
# Widgets for Sensor selection  
#- ------------------------------------------------------------------------------- -#	
	{{sens_frame <- gframe(text ="Select satellites to be processed",horizontal = TRUE, container=satprod_frame, spacing = 15, expand = T)
			sens_wid <- gradio(items = c("Terra","Aqua", "Both"),selected = which(c("Terra","Aqua", "Both") == general_opts$sensor),
					container = sens_frame, horizontal = T)
			
		}}
	
#- ------------------------------------------------------------------------------- -#
# Widgets for Dates selection	  
#- ------------------------------------------------------------------------------- -#
	{{dates_frame <- gframe(text = '<span foreground="blue" size="large">Processing Period</span>', markup = T, container = main_group, horizontal = T, expand = T, spacing = 15)
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
	{{tiles_group <- gframe(text = '<span foreground="blue" size="large">Required MODIS Tiles </span>', markup = T, container = main_group, horizontal = F, expand = T, spacing = 15)
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
		}}
	
#- ------------------------------------------------------------------------------- -#
# Widgets for Projection, resolution and bbox selection 
#- ------------------------------------------------------------------------------- -#
	{{output_proj_frame <- gframe(text = '<span foreground="blue" size="large">Reprojection and Resize Options</span>',markup = T, container = main_group, horizontal = FALSE, expand = T, spacing = 30)
			output_proj_group <- ggroup (container = output_proj_frame, horizontal = TRUE, spacing = 15)
			font (output_proj_frame) <- list(weight = 'bold', color  = 'blue')
			# Projection ----
			output_proj_lab <- glabel(text = '<span weight = "bold" >Output Projection:</span>', container = output_proj_group,markup = T) ; size(output_proj_lab) = c(120,20)
			proj_wid <- gcombobox(general_opts$out_proj_names, container=output_proj_group, selected = match(general_opts$proj, general_opts$out_proj_names), handler = function(h,....) {
						current_sel <- svalue(proj_wid) 
						if (current_sel != 'User Defined') { enabled(output_proj4_wid) <- F} else {(enabled(output_proj4_wid) <- T)}
					})
			size (proj_wid) <- c(100,20)
			
			outproj_user_lab<- glabel(text = '<span weight = "bold" >PROJ4 String:</span>', container = output_proj_group,markup = T) ; size(output_proj_lab) = c(120,20)
			output_proj4_wid <- gedit(text = general_opts$user_proj4, container = output_proj_group, width = 40)
			if (general_opts$proj == 'User Defined') { enabled(output_proj4_wid) <- T} else {(enabled(output_proj4_wid) <- F)}
			
			# Resolution ----
			output_res_group <- ggroup (container = output_proj_frame, horizontal = TRUE)
			output_res_lab <- glabel(text = '<span weight = "bold" >Output Resolution:</span>',markup = T, container = output_res_group) ; size(output_res_lab) = c(120,20)
			output_res_sel_wid  <-  gcombobox(c('Native','Resampled'), container=output_res_group, 
					selected = match(general_opts$out_res_sel, c('Native','Resampled')), handler = function(h,....) {
						current_sel <- svalue(output_res_sel_wid) 
						if (current_sel == 'Native') { enabled(output_res_wid) <- F} else {(enabled(output_res_wid) <- T)}
					}) 
			
			pixsize_lab <- glabel(text = '<span weight = "bold" >Pixel Size:</span>',markup = T, container = output_res_group) ; 	size (pixsize_lab) <- c(120,20)
			output_res_wid <- gedit(text = general_opts$out_res , container = output_res_group)   ; 	size(output_res_wid) <- c(90,20)
			if (general_opts$out_res_sel == 'Native') { enabled(output_res_wid) <- F} else {(enabled(output_res_wid) <- T)}
			
			# Resampling ----
			resopts_group <- ggroup (container = output_proj_frame, horizontal = TRUE)
			resmeth_lab <- glabel(text = '<span weight = "bold" >Resampling Method:</span>',markup = T, container = resopts_group) ; size(resmeth_lab) = c(120,20)
			resamp_array <- c('near','bilinear','cubic', 'cubicspline','lanczos','average','mode')
			output_resmeth_wid <-  gcombobox(resamp_array, container=resopts_group, selected = match(general_opts$resampling, resamp_array)) 
			size (output_resmeth_wid) <- c(80,20)
			
			# Extent ----
			output_ext_group <- ggroup (container = output_proj_frame, horizontal = TRUE, expand = T)
			output_ext_lab <- glabel(text = '<span weight = "bold" >Output Extent:</span>',markup = T, container = output_ext_group) ; size(output_ext_lab) <- c(120,20)
			output_ext_wid <-  gcombobox(c('Full Tiles Extent','Resized'), container=output_ext_group, 
					selected = match(general_opts$full_ext, c('Full Tiles Extent','Resized')), handler = function(h,....) {
						current_sel = svalue(output_ext_wid) 
						if (current_sel == 'Full Tiles Extent') { enabled(bbox_group) <- F} else {(enabled(bbox_group) <- T)}
					}) 
			size (output_ext_wid) <- c(120,20)
			
			{# bounding box ----
				bbox_group <- ggroup (horizontal = FALSE, container=output_proj_frame)
				UL_group <- ggroup (horizontal = TRUE, container=bbox_group)
				
				output_ULeast_lab <- glabel('Upper Left Easting (xmin)', container = UL_group)   ;    size (output_ULeast_lab) <- c(160,20)
				output_ULeast_wid <- gedit(text = general_opts$bbox[1], container = UL_group, width = 10)
				addSpace(UL_group, 30, horizontal=TRUE)
				output_LReast_lab <- glabel('Lower Right Easting (xmax)', container = UL_group)   ;    size (output_LReast_lab) <- c(160,20)
				output_LReast_wid <- gedit(text = general_opts$bbox[2], container = UL_group, width = 10)
				
				LR_group <- ggroup (horizontal = TRUE, container=bbox_group)
				
				output_LRnorth_lab <- glabel('Lower Right Northing (ymin)', container = LR_group) ;    size (output_LRnorth_lab) = c(160,20)
				output_LRnorth_wid <- gedit(text = general_opts$bbox[3], container = LR_group, width = 10)
				addSpace(LR_group, 30, horizontal=TRUE)
				output_ULnorth_lab <- glabel('Upper Left Northing (ymax)', container = LR_group) ;    size (output_ULnorth_lab) <- c(160,20)
				output_ULnorth_wid <- gedit(text = general_opts$bbox[4], container = LR_group, width = 10)
				
				
				
				if (general_opts$full_ext == 'Full Tiles Extent') { enabled(bbox_group) <- F} else {(enabled(bbox_group) <- T)}
			}
		}}
	
#- ------------------------------------------------------------------------------- -#
# Widgets for Format and reprocess options 
#- ------------------------------------------------------------------------------- -#
	{{options_frame <- gframe(text = '<span foreground="blue" size="large">Processing Options</span>', markup = T, container = main_group, expand = T,spacing = 15, horizontal = F)
			opt_group <- ggroup(container = options_frame, horizontal = T, expand = T)
			format_lab <- glabel(text = '<span weight = "bold" >Output Format for single images</span>',markup = T, container = opt_group)
			format_wid <- gdroplist(items = c('ENVI','GTiff'), text = 'Select', container=opt_group, selected = match(general_opts$out_format, c('ENVI','GTiff')))
			
			timeseries_lab <- glabel(text = '<span weight = "bold" >Format of Time Series Images</span>',markup = T, container = opt_group)
			timeseries_wid <- gcombobox( c('ENVI Meta Files','R Stack Files','Both'), container=opt_group, 
					selected <- match(general_opts$ts_format, c('ENVI Meta Files','R Stack Files','Both')), handler = function(h,....) {
						current_sel <- svalue(timeseries_wid)
					})
			repro_group <- ggroup(container = options_frame, horizontal = T)
			reprocess_lab <- glabel(text = '<span weight = "bold" >ReDownload Existing Images</span>',markup = T, container = repro_group)
			reprocess_wid <- gradio(items = c('Yes','No'), text = 'Select', container=repro_group, selected = match(general_opts$reprocess, c('Yes','No')), horizontal = T)
			
			
		}}	
	
#- ------------------------------------------------------------------------------- -#
# Widgets for options file saving and loading
#- ------------------------------------------------------------------------------- -#
#	{{outprev_frame <- gframe(text = '<span foreground="blue" size="large">File For Saving-loading processing options</span>', markup = T, container=main_group, expand = T,spacing = 15)    			# Frame group
#			outprev_group <- ggroup(horizontal = TRUE, container=outprev_frame)  				# Main group
#			outprev_wid <- gedit(text = format(general_opts$previous_file, justify = "right") , container=outprev_group, width = 57)			# Selected file
#			outprev_choose <- gbutton("Browse", handler=function(h,...) {choice<-gfile(type="open", text="Select the file for saving-loading processing options...")		# File selection widget
#						
#						if(!is.na(choice)){svalue(outprev_wid)<-choice						## On new selection, set value of the label widget
#							general_opts$previous_file = format(choice, justify = "left")	# 	On new selection,  Set value of the selected variable
#						
#						}}, container=outprev_group)
#			butprev_group <- ggroup(container = outprev_group, horizontal = TRUE)
#			load_but <- gbutton(text = 'Load Oprions from File', container = butprev_group, handler = function (h,....){
#						
#						choice<-gfile(type="open", text="Select file for loading processing options...")		# File selection widget
#						
#						if(! is.na(choice)){
#							svalue(outprev_wid)<-choice						## On new selection, set value of the label widget
#							general_opts$previous_file = format(choice, justify = "left")	# 	On new selection,  Set value of the selected variable
#							
#						
#												
	##						browser()
#						load(svalue(outprev_wid))
#						svalue(prod_wid) <- general_opts$sel_prod
	##						general_opts$sel_prod <- mod_prod_list[which(mod_prod_list == svalue(prod_wid))]						# Products options
#						svalue(sens_wid)<-general_opts$sensor 
#						
#						temp_wid_bands <<- prod_opt_list[[general_opts$sel_prod]]$bandsel				# set dummy variables holding the initial values of selected bands
#						temp_wid_bands_indexes <<- prod_opt_list[[general_opts$sel_prod]]$indexes_bandsel
#						temp_wid_bands_quality <<- prod_opt_list[[general_opts$sel_prod]]$quality_bandsel
#							
#						svalue(start_day_wid)<-general_opts$start_day  	# Dates options
#						svalue(start_month_wid)<- general_opts$start_month  
#						svalue(start_year_wid) <- general_opts$start_year 
#						svalue(end_day_wid) <- general_opts$end_day 
#						svalue(end_month_wid) <-general_opts$end_month  
#						svalue (end_year_wid) <-general_opts$end_year   
#						
#						svalue(start_x_wid)<- general_opts$start_x  		# Tiles options
#						svalue(end_x_wid) <-general_opts$end_x  
#						svalue(start_y_wid) <-general_opts$start_y   
#						svalue(end_y_wid) <-general_opts$end_y   
#						
#						svalue(proj_wid)	 <-general_opts$proj 	# Proj and extent options
#						svalue(output_proj4_wid)	 <- general_opts$user_proj4 
#						svalue(output_res_sel_wid) <-general_opts$out_res_sel 			
#						svalue(output_res_wid) <-general_opts$out_res 
#						svalue(output_resmeth_wid) <- general_opts$resampling
#						svalue(output_ext_wid) <-general_opts$full_ext 
#						svalue(output_ULeast_wid) <- general_opts$bbox [1]
#						svalue(output_LReast_wid) <- general_opts$bbox [2]
#						svalue(output_LRnorth_wid) <- general_opts$bbox [3]
#						svalue(output_ULnorth_wid) <- general_opts$bbox [4]
#						svalue(reprocess_wid) <- general_opts$reprocess
#						svalue(format_wid) <- general_opts$out_format 
#						svalue(timeseries_wid) <- general_opts$ts_format 
#						
#						svalue(outfold_wid) <- 	general_opts$out_folder # Folder options
#						svalue(outfoldmod_wid) <- general_opts$out_folder_mod
#						}
#					})
#			
#	
#		}}
#	
	
#- ------------------------------------------------------------------------------- -#
# Widgets for output folders selection  
#- ------------------------------------------------------------------------------- -#
	{{outfold_frame <- gframe(text = '<span foreground="blue" size="large">Main Output Folder for Time Series storage</span>', markup = T, container=main_group, expand = T,spacing = 15)    			# Frame group
			outfold_group <- ggroup(horizontal = TRUE, container=outfold_frame)  				# Main group
			outfold_wid <- gedit(text = format(general_opts$out_folder, justify = "right") , container=outfold_group, width = 57)			# Selected file
			fold_choose <- gbutton("Browse", handler=function(h,...) {choice<-gfile(type="selectdir", text="Select the Output Folder for MODIS data...")		# File selection widget
						if(! is.na(choice)){svalue(outfold_wid)<-choice						## On new selection, set value of the label widget
							general_opts$out_folder = format(choice, justify = "left")	# 	On new selection,  Set value of the selected variable
						}}, container=outfold_group)
		}}
	
	{{outfoldmod_frame <- gframe(text = '<span foreground="blue" size="large">Output Folder for Original hdf storage</span>', markup = T, container=main_group, expand = T,spacing = 15)    			# Frame group
			outfoldmod_group <- ggroup(horizontal = TRUE, container=outfoldmod_frame)  				# Main group
			outfoldmod_wid <- gedit(text = format(general_opts$out_folder_mod, justify = "right") , container=outfoldmod_group, width = 57)			# Selected file
			fold_choose <- gbutton("Browse", handler=function(h,...) {choice<-gfile(type="selectdir", text="Select the Output Folder for storage of original HDFs...")		# File selection widget
						if(! is.na(choice)){svalue(outfoldmod_wid)<-choice						## On new selection, set value of the label widget
							general_opts$out_folder_mod = format(choice, justify = "left")	# 	On new selection,  Set value of the selected variable
						}}, container=outfoldmod_group)
		}}
	
#- ------------------------------------------------------------------------------- -#
# Start/Quit buttons 
#- ------------------------------------------------------------------------------- -#
	{{but_group <- ggroup(container = main_group, horizontal = TRUE)
			
			{{start_but <- gbutton(text = 'Start', container = but_group, handler = function (h,....) {# If "Start" pressed, retrieve selected values and save in previous file
								general_opts$sel_prod <- mod_prod_list[which(mod_prod_list == svalue(prod_wid))]						# Products options
								general_opts$sensor <- svalue(sens_wid)
#								browser()
								if (exists ('temp_wid_bands')) {
									prod_opt_list[[general_opts$sel_prod]]$bandsel <- temp_wid_bands			#retrieve selected bands
									
								}
								
								if (exists ('temp_wid_bands_indexes')) {
									prod_opt_list[[general_opts$sel_prod]]$indexes_bandsel <- temp_wid_bands_indexes #retrieve selected indexes
									
								}
								
								if (exists ('temp_wid_bands_quality')) {
									prod_opt_list[[general_opts$sel_prod]]$quality_bandsel <- temp_wid_bands_quality 	#retrieve selected quality ind.
									
								}
								print(temp_wid_bands)
								print(temp_wid_bands_quality)
								print(temp_wid_bands_indexes)
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
								general_opts$out_res_sel <- (svalue(output_res_sel_wid))			
								general_opts$out_res <- (svalue(output_res_wid))
								general_opts$resampling <- svalue(output_resmeth_wid)
								general_opts$full_ext <- svalue(output_ext_wid)
								general_opts$bbox <- ((c(svalue(output_ULeast_wid),svalue(output_LReast_wid),
														svalue(output_LRnorth_wid),svalue(output_ULnorth_wid))))
								
								general_opts$reprocess <- svalue(reprocess_wid)
								general_opts$out_format <- svalue(format_wid)
								general_opts$ts_format <- svalue(timeseries_wid)
								
								general_opts$out_folder <- svalue(outfold_wid)		# Folder options
								general_opts$out_folder_mod <- svalue(outfoldmod_wid) 
								
								check <- T
								# Check if dates, processing extent and tiles selection make sense
								if (as.Date(paste(general_opts$start_year, general_opts$start_month, general_opts$start_day, sep = '-')) >
										as.Date(paste(general_opts$end_year, general_opts$end_month, general_opts$end_day, sep = '-'))) {gmessage('Error in Selected Dates', title = 'Warning'); check <- F}
								
								if ((general_opts$start_x > general_opts$end_x ) | (general_opts$start_y > general_opts$end_y )) {gmessage('Error in Selected Tiles', title = 'Warning') ; check <- F}
								
								if (max(prod_opt_list[[general_opts$sel_prod]]$bandsel)+
										max(prod_opt_list[[general_opts$sel_prod]]$indexes_bandsel)+
										max(prod_opt_list[[general_opts$sel_prod]]$quality_bandsel) == 0) {gmessage('No Output bands or indexes selected - Please Correct !', title = 'Warning') ; check <- F}
								
								general_opts$bbox <- as.numeric(general_opts$bbox)
								n_bbox_compiled <- length(which(is.finite(general_opts$bbox)))
								
								if (general_opts$full_ext != 'Full Tiles Extent') {
									if (n_bbox_compiled == 4){
										if ((general_opts$bbox[1] > general_opts$bbox[2]) | (general_opts$bbox[3] > general_opts$bbox[4])) {gmessage('Error in Selected Output extent', title = 'Warning') ; check <- F}}
									if ((n_bbox_compiled < 4) & (n_bbox_compiled >= 0 )) {gmessage('Error in Selected Output extent', title = 'Warning') ; check <- F}
								}
								if (general_opts$out_folder == ''){gmessage('Please Select an output folder !', title = 'Warning') ; check <- F}
								if (general_opts$out_folder_mod == ''){gmessage('Please Select an output folder for storing original HDFs!', title = 'Warning') ; check <- F}
								
								if (check == T) {					# If check passed, save previous file and return
									dir.create(file.path(getwd(),'Previous'))
									
									save(general_opts,prod_opt_list,mod_prod_list, file = general_opts$previous_file)
									assign("Quit", F, envir=globalenv())
									rm(temp_wid_bands, envir = globalenv())
									rm(temp_wid_bands_indexes, envir = globalenv())
									rm(temp_wid_bands_quality, envir = globalenv())
									dispose(main_win)
								}
							})
				}}	
			
			quit_but <- gbutton(text = 'Quit', container = but_group, handler = function(h,...){
						assign("Quit", T, envir=globalenv())
						dispose(main_win)
					})
			
			addSpace(but_group, 300, horizontal=TRUE)
			
			{{load_but <- gbutton(text = 'Load Options from File', container = but_group, handler = function (h,....){
								
								choice<-gfile(type="open", text="Select file for loading processing options...")#, initialfilename = general_opts$previous_file)		# File selection widget
								
								#			outprev_choose <- gbutton("Browse", handler=function(h,...) {choice<-gfile(type="open", text="Select the file for saving-loading processing options...")		# File selection widget
#						
#						if(!is.na(choice)){svalue(outprev_wid)<-choice						## On new selection, set value of the label widget
#							general_opts$previous_file = format(choice, justify = "left")	# 	On new selection,  Set value of the selected variable
#						
#						}}, container=outprev_group)
								
								
								if(! is.na(choice)){
#							svalue(outprev_wid)<-choice						## On new selection, set value of the label widget
#							general_opts$previous_file = format(choice, justify = "left")	# 	On new selection,  Set value of the selected variable
									load(choice)
									svalue(prod_wid) <- general_opts$sel_prod
#						general_opts$sel_prod <- mod_prod_list[which(mod_prod_list == svalue(prod_wid))]						# Products options
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
									svalue(format_wid) <- general_opts$out_format 
									svalue(timeseries_wid) <- general_opts$ts_format 
									
									svalue(outfold_wid) <- 	general_opts$out_folder # Folder options
									svalue(outfoldmod_wid) <- general_opts$out_folder_mod
								}
							})
				}}
			{{save_but <- gbutton(text = 'Save Options', container = but_group, handler = function (h,....) {
								
								choice<-gfile(type="save", text="Select file for saving processing options...", initialfilename = general_opts$previous_file)		# File selection widget
#								browser()
								if(!is.na(choice)){
									
#											svalue(outprev_wid)<-choice						## On new selection, set value of the label widget
#											general_opts$previous_file = format(choice, justify = "left")	# 	On new selection,  Set value of the selected variable
									
									
									# If "Start" pressed, retrieve selected values and save in previous file
									general_opts$sel_prod <- mod_prod_list[which(mod_prod_list == svalue(prod_wid))]						# Products options
									general_opts$sensor <- svalue(sens_wid)
									if (exists ('temp_wid_bands')) {
										prod_opt_list[[sel_prod]]$bandsel <- temp_wid_bands			#retrieve selected bands
										rm(temp_wid_bands, envir = globalenv())
									}
									
									if (exists ('temp_wid_bands_indexes')) {
										prod_opt_list[[sel_prod]]$indexes_bandsel <- temp_wid_bands_indexes #retrieve selected indexes
										rm(temp_wid_bands_indexes, envir = globalenv())
									}
									
									if (exists ('temp_wid_bands_quality')) {
										prod_opt_list[[sel_prod]]$quality_bandsel <- temp_wid_bands_quality 	#retrieve selected quality ind.
										rm(temp_wid_bands_quality, envir = globalenv())
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
									general_opts$out_format <- svalue(format_wid)
									general_opts$ts_format <- svalue(timeseries_wid)
									
									general_opts$out_folder <- svalue(outfold_wid)		# Folder options
									general_opts$out_folder_mod <- svalue(outfoldmod_wid)		# Folder options
									check <- T
									# Check if dates, processing extent and tiles selection make sense
									if (as.Date(paste(general_opts$start_year, general_opts$start_month, general_opts$start_day, sep = '-')) >
											as.Date(paste(general_opts$end_year, general_opts$end_month, general_opts$end_day, sep = '-'))) {gmessage('Error in Selected Dates', title = 'Warning'); check <- F}
									
									if ((general_opts$start_x > general_opts$end_x ) | (general_opts$start_y > general_opts$end_y )) {gmessage('Error in Selected Tiles', title = 'Warning') ; check <- F}
									
									general_opts$bbox <- as.numeric(general_opts$bbox)
									n_bbox_compiled <- length(which(is.finite(general_opts$bbox)))
									
									if (n_bbox_compiled == 4){
										if ((general_opts$bbox[1] > general_opts$bbox[2]) | (general_opts$bbox[3] > general_opts$bbox[4])) {gmessage('Error in Selected Output extent', title = 'Warning') ; check <- F}}
									if ((n_bbox_compiled < 4) & (n_bbox_compiled > 0 )) {gmessage('Error in Selected Output extent', title = 'Warning') ; check <- F}
									
									if (general_opts$out_folder == ''){gmessage('Please Select an output folder !', title = 'Warning') ; check <- F}
									
									if (check == T) {					# If check passed, save previous file and return
										
										dir.create(file.path(getwd(),'Previous'))
										
										save(general_opts,prod_opt_list,mod_prod_list, file = choice)
									}
								}
							})		
				}}
			
		}}
	
	visible(main_win, set=TRUE) ## show the selection GUI
	
	
}  # END 
