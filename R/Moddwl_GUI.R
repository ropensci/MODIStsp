#' @Title moddwl_GUI
#' @Description
#' 
#' @details
#'
#' @param general_opts General options (paths) passed by the moddwl_main launcher
#' @returnType 
#'
#' @return 
#' 
#' @author Lorenzo Busetto, phD (2014)
#' email: busetto.l@@irea.cnr.it
#'
#' @license GPL(>2)
#' @export
moddwl_GUI = function (general_opts){
	
	# Restore previous options file if existing, otherwise create a "dummy" one with default values
	if (file.exists(general_opts$previous_file)) {load(general_opts$previous_file)
	} else {moddwl_set_opts(previous_file = general_opts$previous_file) ; load(general_opts$previous_file)}
	
#- ------------------------------------------------------------------------------- -#
#  Start Building the GUI
#- ------------------------------------------------------------------------------- -#
	
	main_win <- gbasicdialog(title = "Select Main Processing Oprions", parent=NULL, do.buttons=FALSE)
	main_group = ggroup(container = main_win, horizontal = FALSE)
	sel_prod = general_opts$sel_prod
	# Widgets for product selection and bands selection
	prod_frame <- gframe(text ="MODIS Products",horizontal = TRUE, container=main_group)
	
	checked = which(mod_prod_list == general_opts$sel_prod)
	pos_prod = which(names(general_opts$prod_opt_list) == general_opts$sel_prod)
	
	temp_wid_bands <<- prod_opt_list[[sel_prod]]$bandsel				# set dummy variables holding the initial values of selected bands
	temp_wid_bands_indexes <<- prod_opt_list[[sel_prod]]$indexes_bandsel
	temp_wid_bands_quality <<- prod_opt_list[[sel_prod]]$quality_bandsel
	
	prod_wid <- gdroplist(items = mod_prod_list, container=prod_frame, horizontal = T, selected = checked,  handler = function(h,....) {
				sel_prod = mod_prod_list[which(mod_prod_list == svalue(prod_wid))]		# find index of sel. product
				temp_wid_bands <<- prod_opt_list[[sel_prod]]$bandsel					# update dummy variables
				temp_wid_bands_indexes <<- prod_opt_list[[sel_prod]]$indexes_bandsel
			})
	
	band_wid = gbutton(text = 'Select Processing Bands', border = T,				# Child widget for processing bands selection
			handler = function(h,....) {
				
				sel_prod = mod_prod_list[which(mod_prod_list == svalue(prod_wid))]		# find index of sel. product
				check_names = prod_opt_list[[sel_prod]]$nicknames						# retrieve band names
				check_wid = temp_wid_bands																				# retrieve selected at the time
				selgroup = gbasicdialog(title = "Select Processing Bands						", parent=NULL, do.buttons=F, width = 500, horizontal = T)
				# widgets for band selection - original
				cbox_total = gframe(text = "Select Processing Bands", container = selgroup, horizontal = T, width = 500)
				cbox= gframe(text = "Original MODIS Bands					   ", container = cbox_total, horizontal = T, width = 500)
				bands_wid = gcheckboxgroup(items = check_names, checked = as.logical(check_wid), container = cbox, use.table = F, width = 500)
				# widgets for band selection - indexes
				check_names_indexes = prod_opt_list[[sel_prod]]$indexes_nicknames # retrieve indexes band names
				check_wid_indexes= temp_wid_bands_indexes													# retrieve indexes nicknames selected at the time
				if (length(which(check_names_indexes != '') > 0)) {
					cbox_indexes= gframe(text = "Spectral Indexes						", container = cbox_total, horizontal = FALSE, width = 500)
					bands_wid_indexes = gcheckboxgroup(items = check_names_indexes, checked = as.logical(check_wid_indexes), container = cbox_indexes, use.table = F, width = 500)
				}
				
				# widgets for band selection - quality
				check_names_quality = prod_opt_list[[sel_prod]]$quality_nicknames # retrieve quality band names
				check_wid_quality= temp_wid_bands_quality													# retrieve quality nicknames selected at the time
				if (length(which(check_names_quality != '') > 0)) {
					cbox_quality= gframe(text = "Quality Indicators						", container = cbox_total, horizontal = FALSE, width = 500)
					bands_wid_quality = gcheckboxgroup(items = check_names_quality, checked = as.logical(check_wid_quality), container = cbox_quality, use.table = F, width = 500)
				}
				
				# Start/Cancel widgets
				bands_group = ggroup(container = selgroup, horizontal = TRUE)
				accept_but = gbutton(text = 'Start', container = bands_group, handler = function(button,...){
							
							pos_wid = which(check_names %in% svalue (bands_wid))   ;		tmp_arr = array(data = 0 , dim = length(check_names))		
							tmp_arr[pos_wid] = 1   ;	temp_wid_bands <<- tmp_arr
							if (length(which(check_names_indexes != '') > 0)) {
								pos_wid = which(check_names_indexes %in% svalue (bands_wid_indexes))   ;		tmp_arr = array(data = 0 , dim = length(check_names_indexes))  
								tmp_arr[pos_wid] = 1	; temp_wid_bands_indexes <<- tmp_arr	
							}
							if (length(which(check_names_quality != '') > 0)) {
								pos_wid = which(check_names_quality %in% svalue (bands_wid_quality))   ;		tmp_arr = array(data = 0 , dim = length(check_names_quality))  
								tmp_arr[pos_wid] = 1	; temp_wid_bands_quality <<- tmp_arr	
							}
							dispose(selgroup)			
						})
				cancel_but = gbutton(text = 'Cancel', container = bands_group, handler = function(button,...){
							temp_wid_bands <<- check_wid
							temp_wid_bands_indexes <<- check_wid_indexes
							temp_wid_bands_quality <<- check_wid_quality
							dispose(selgroup)
						})
				
				visible(selgroup, set=TRUE)    # visualize band selection widgets
				
			},container =prod_frame)
	
# Widgets for Sensor selection
	sens_frame <- gframe(text ="Satellites",horizontal = TRUE, container=main_group)
	sens_wid = gradio(items = c("Terra","Aqua", "Both"),selected = which(c("Terra","Aqua", "Both") == general_opts$sensor),
			container = sens_frame, horizontal = T)
	
# Widgets for Dates selection	
	dates_frame = gframe(text = 'Processing period', container = main_group, horizontal = T)
	start_date_lab = glabel(text = 'Starting Date', container = dates_frame)
	start_day_wid <- gspinbutton(1,31,  container=dates_frame , value = general_opts$start_day)
	start_month_wid <- gspinbutton(1,12,  container=dates_frame , value = general_opts$start_month)
	start_year_wid <- gspinbutton(2000 ,2020,  container=dates_frame , value = general_opts$start_year, horizontal = T)
	
	end_date_lab = glabel(text = 'Ending Date', container = dates_frame)
	end_day_wid <- gspinbutton(1,31,  container=dates_frame , value = general_opts$end_day)
	end_month_wid <- gspinbutton(1,12,  container=dates_frame , value = general_opts$end_month)
	end_year_wid <- gspinbutton(2000,2020,  container=dates_frame , value = general_opts$end_year)
	
# Widgets for Tiles selection	
	tiles_group = gframe(text = 'Processing extent', container = main_group, horizontal = FALSE)
	x_group = ggroup(container = tiles_group, horizontal = TRUE)
	start_x_lab = glabel(text = 'h_start', container = x_group)
	start_x_wid <- gspinbutton(1, 35, text = 'Select', container=x_group, value = general_opts$start_x)
	end_x_lab = glabel(text = 'h_end', container = x_group)
	end_x_wid <- gspinbutton(1, 35, text = 'Select', container=x_group, value = general_opts$end_x)
	
	y_group = ggroup(container = tiles_group, horizontal = TRUE)
	start_y_lab = glabel(text = 'v_start', container = y_group)
	start_y_wid <- gspinbutton(1,18, text = 'Select', container=y_group, value = general_opts$start_y)
	end_y_lab = glabel(text = 'v_end', container = y_group)
	end_y_wid <- gspinbutton(1,18, text = 'Select', container=y_group, value = general_opts$end_y)
	
	show_map = gbutton(text = 'Show Tiles Map', border = T,
			handler = function(h,....) {x11()
				plot(raster(file.path(general_opts$main_dir, "/Accessoires/MODIS_Tiles.gif")))},
			container =x_group )
	
	# Widgets for Projection, resolution and bbox selection
	output_proj_frame = gframe(text = 'Output Projection and Extent', container = main_group)
	output_proj_group = ggroup (container = output_proj_frame, horizontal = FALSE)
	proj_wid <- gcombobox(names(general_opts$out_proj_list), container=output_proj_group, selected = match(general_opts$proj, names(general_opts$out_proj_list)))
	
	output_res_lab = glabel(text = 'Output Resolution (in OUTPUT proj measure units - Blank for Native Resolution)', container = output_proj_group)
	output_res_wid = gedit(text = general_opts$out_res , container = output_proj_group, width = 15)
	
	output_ext_lab = glabel(text = 'Output Bounding Box (in OUTPUT proj coordinates) - Leave Blank if no resize needed', container = output_proj_group)
	
	output_lab_group =  ggroup(horizontal = TRUE, container=output_proj_group, width = 200)
	output_xmin_lab = glabel('      Minimum X      ', container = output_lab_group, width = 15)
	output_xmax_lab = glabel('      Maximum X      ', container = output_lab_group)
	output_ymin_lab = glabel('      Minimum Y      ', container = output_lab_group)
	output_ymax_lab = glabel('      Maximum Y      ', container = output_lab_group)
	
	output_ext_group =  ggroup(horizontal = TRUE, container=output_proj_group)
	output_xmin_wid = gedit(text = general_opts$bbox[1], container = output_ext_group, width = 15)
	output_xmax_wid = gedit(text = general_opts$bbox[2], container = output_ext_group, width = 15)
	output_ymin_wid = gedit(text = general_opts$bbox[3], container = output_ext_group, width = 15)
	output_ymax_wid = gedit(text = general_opts$bbox[4], container = output_ext_group, width = 15)
	
	# Widgets for Format and reprocess options
	options_frame = gframe(text = 'Processing Options', container = main_group)
	opt_group = ggroup(container = options_frame)
	format_lab = glabel(text = 'Output Format', container = opt_group)
	format_wid <- gdroplist(items = c('ENVI','GTiff'), text = 'Select', container=opt_group, selected = match(general_opts$out_format, c('ENVI','GTiff')))
	
	reprocess_lab = glabel(text = 'ReDownload Existing', container = opt_group)
	reprocess_wid <- gradio(items = c('Yes','No'), text = 'Select', container=opt_group, selected = match(general_opts$reprocess, c('Yes','No')), horizontal = T)
	
	# Widgets for output folder selection 
	outfold_frame = ggroup(text = "Output Folder", container=main_group)    			# Frame group
	outfold_group = ggroup(horizontal = TRUE, container=outfold_frame)  				# Main group
	outfold_lab <- glabel(text ='Main Folder for download of MODIS data', container=outfold_group, editable =FALSE)  # Label
	outfold_wid<- gedit(text = format(general_opts$out_folder, justify = "right") , container=outfold_group, width = 40)			# Selected file
	fold_choose <- gbutton("Browse", handler=function(h,...) {choice<-gfile(type="selectdir", text="Select the Output Folder for MODIS data...")		# File selection widget
				if(! is.na(choice)){svalue(outfold_wid)<-choice						## On new selection, set value of the label widget
					general_opts$out_folder = format(choice, justify = "right")	# 	On new selection,  Set value of the selected variable
				}}, container=outfold_group)
	
	# Widgets for Start/cancel
	start_frame = gframe (container = main_group)
	start_group = ggroup(container = start_frame, horizontal = TRUE)
	start_but = gbutton(text = 'Start', container = start_group, handler = function(button,...){
				
				# If "Start" pressed, retrieve selected values and save in previous file
				general_opts$sel_prod = svalue(prod_wid)						# Products options
				general_opts$sensor = svalue(sens_wid)
				if (exists ('temp_wid_bands')) {
					prod_opt_list[[sel_prod]]$bandsel = temp_wid_bands			#retrieve selected bands
					rm(temp_wid_bands, envir = globalenv())
				}
				
				if (exists ('temp_wid_bands_indexes')) {
					prod_opt_list[[sel_prod]]$indexes_bandsel = temp_wid_bands_indexes #retrieve selected indexes
					rm(temp_wid_bands_indexes, envir = globalenv())
				}
				
				if (exists ('temp_wid_bands_quality')) {
					prod_opt_list[[sel_prod]]$quality_bandsel = temp_wid_bands_quality 	#retrieve selected quality ind.
					rm(temp_wid_bands_quality, envir = globalenv())
				}
				
				general_opts$start_day = svalue(start_day_wid)		# Dates options
				general_opts$start_month = svalue(start_month_wid)
				general_opts$start_year = svalue(start_year_wid)
				general_opts$end_day = svalue(end_day_wid)
				general_opts$end_month = svalue(end_month_wid)
				general_opts$end_year = svalue(end_year_wid)
				
				general_opts$start_x = svalue(start_x_wid)		# Tiles options
				general_opts$end_x = svalue(end_x_wid)
				general_opts$start_y = svalue(start_y_wid)
				general_opts$end_y = svalue(end_y_wid)
				
				general_opts$proj = svalue(proj_wid)		# Proj and extent options
				general_opts$bbox = ((c(svalue(output_xmin_wid),svalue(output_xmax_wid),svalue(output_ymin_wid),svalue(output_ymax_wid))))
				general_opts$out_res = (svalue(output_res_wid))
				
				general_opts$out_folder = svalue(outfold_wid)		# Fodler options
				general_opts$reprocess = svalue(reprocess_wid)
				general_opts$out_format = svalue(format_wid)
				
				check = T
				# Check if dates, processing extent and tiles selection make sense
				if (as.Date(paste(general_opts$start_year, general_opts$start_month, general_opts$start_day, sep = '-')) >
						as.Date(paste(general_opts$end_year, general_opts$end_month, general_opts$end_day, sep = '-'))) {gmessage('Error in Selected Dates', title = 'Warning'); check = F}
				
				if ((general_opts$start_x > general_opts$end_x ) | (general_opts$start_y > general_opts$end_y )) {gmessage('Error in Selected Tiles', title = 'Warning') ; check = F}
				
				general_opts$bbox = as.numeric(general_opts$bbox)
				n_bbox_compiled = length(which(is.finite(general_opts$bbox)))
				
				if (n_bbox_compiled == 4){
					if ((general_opts$bbox[1] > general_opts$bbox[2]) | (general_opts$bbox[3] > general_opts$bbox[4])) {gmessage('Error in Selected Output extent', title = 'Warning') ; check = F}}
				if ((n_bbox_compiled < 4) & (n_bbox_compiled > 0 )) {gmessage('Error in Selected Output extent', title = 'Warning') ; check = F}
				
				if (general_opts$out_folder == ''){gmessage('Please Select an output folder !', title = 'Warning') ; check = F}
									
				
#				browser()
				if (check == T) {					# If check passed, save previous file and retu 
					dir.create(file.path(getwd(),'Previous'))
					save(general_opts,mod_prod_list, prod_opt_list, file = general_opts$previous_file)
#					assign("Quit", F, envir=globalenv())
					dispose(main_win)
				}
				
			})
	quit_but = gbutton(text = 'Cancel', container = start_group, handler = function(button,...){ # If Quit pressed, Quit
				assign("Quit", T, envir=globalenv())		# Set the "Quit" variable in GloblEnv. used in Main to exit on "Cancel"
				dispose(main_win)})
	
	visible(main_win, set=TRUE) ## show the selection GUI
	
}  # END 
