moddwl_GUI = function (moddwl_opts){

  # Restore previous options file if existing, otherwise create a "dummy" one with default values
  if (file.exists(moddwl_opts$previous_file)) {load(moddwl_opts$previous_file)
  } else {moddwl_set_opts(previous_file = moddwl_opts$previous_file) ; load(moddwl_opts$previous_file)}

  main_win <- gbasicdialog(title = "Select Main Processing Oprions", parent=NULL, do.buttons=FALSE)

  main_group = ggroup(container = main_win, horizontal = FALSE)
  prod_frame <- gframe(text ="MODIS Products",horizontal = TRUE, container=main_group)
  checked = which(opts$mod_prod_list == opts$modprod)
  pos_prod = which(names(opts$prod_opt_list) == opts$modprod)
  prod_sel <- gdroplist(items = opts$mod_prod_list, container=prod_frame, horizontal = T, selected = checked)

  temp_sel_bands <<- opts$prod_opt_list[[pos_prod]]$bandsel
  temp_sel_bands_derived <<- opts$prod_opt_list[[pos_prod]]$bandsel_derived

  band_sel = gbutton(text = 'Select Processing Bands', border = T,
                     handler = function(h,....) {

                       pos_prod = which(names(opts$prod_opt_list) ==svalue(prod_sel))
                       check_names = opts$prod_opt_list[[pos_prod]]$bandnames
                       check_sel = temp_sel_bands

                       selgroup = gbasicdialog(title = "Select Processing Bands", parent=NULL, do.buttons=F, width = 500, horizontal = T)
                       #                             cfake = gtext(text = '', editable = F, width = 700, container = selgroup)
                       cbox_total = gframe(text = "Select Processing Bands                     ", container = selgroup, horizontal = T, width = 500)

                       cbox= gframe(texrt = "Original    ", container = cbox_total, horizontal = T, width = 500)
                       bands_sel = gcheckboxgroup(items = check_names, checked = as.logical(check_sel), container = cbox, use.table = F, width = 500)

                       cbox_derived= gframe(text = "Derived      ", container = cbox_total, horizontal = FALSE, width = 500)

                       check_names_derived = opts$prod_opt_list[[pos_prod]]$derived_bandnames
                       check_sel_derived= temp_sel_bands_derived
                       bands_sel_derived = gcheckboxgroup(items = check_names_derived, checked = as.logical(check_sel_derived), container = cbox_derived, use.table = F, width = 500)

                       start_group_2 = ggroup(container = selgroup, horizontal = TRUE)
                       start_but_2 = gbutton(text = 'Start', container = start_group_2, handler = function(button,...){
#                          browser()
                         # Retrieve selected values
#                          temp_sel_bands =
#

#                          opts$prod_opt_list[[pos_prod]]$bandsel = b

                         a = which(opts$prod_opt_list[[pos_prod]]$bandnames %in% svalue (bands_sel))
                         b = array(data = 0 , dim = length(opts$prod_opt_list[[pos_prod]]$bandsel))
                         b[a] = 1
                         temp_sel_bands <<- b

                         temp_sel_bands_derived = svalue (bands_sel_derived)
                         a = which(opts$prod_opt_list[[pos_prod]]$derived_bandnames %in%  svalue (bands_sel_derived))
                         b = array(data = 0 , dim = length(opts$prod_opt_list[[pos_prod]]$bandsel_derived))
                         b[a] = 1
                        temp_sel_bands_derived <<- b
#
                         dispose(selgroup)

                       })
                       quit_but_2 = gbutton(text = 'Cancel', container = start_group_2, handler = function(button,...){
                         temp_sel_bands <<- check_sel
                         temp_sel_bands_derived <<- check_sel_derived
                         dispose(selgroup)}
                       )


                       visible(selgroup, set=TRUE)

                     },container =prod_frame)
# browser()
  sens_frame <- gframe(text ="Satellites",horizontal = TRUE, container=main_group)
  sens_sel = gradio(items = c("Terra","Aqua", "Both"),selected = which(c("Terra","Aqua", "Both") == opts$sensor),
                    container = sens_frame, horizontal = T)


  dates_frame = gframe(text = 'Processing period', container = main_group, horizontal = T)
  start_date_lab = glabel(text = 'Starting Date', container = dates_frame)
  start_day_sel <- gspinbutton(1,31,  container=dates_frame , value = opts$start_day)
  start_month_sel <- gspinbutton(1,12,  container=dates_frame , value = opts$start_month)
  start_year_sel <- gspinbutton(2000 ,2020,  container=dates_frame , value = opts$start_year, horizontal = T)

  end_date_lab = glabel(text = 'Ending Date', container = dates_frame)
  end_day_sel <- gspinbutton(1,31,  container=dates_frame , value = opts$end_day)
  end_month_sel <- gspinbutton(1,12,  container=dates_frame , value = opts$end_month)
  end_year_sel <- gspinbutton(2000,2020,  container=dates_frame , value = opts$end_year)

  tiles_group = gframe(text = 'Processing extent', container = main_group, horizontal = FALSE)
  x_group = ggroup(container = tiles_group, horizontal = TRUE)
  start_x_lab = glabel(text = 'h_start', container = x_group)
  start_x_sel <- gspinbutton(1, 35, text = 'Select', container=x_group, value = opts$start_x)
  end_x_lab = glabel(text = 'h_end', container = x_group)
  end_x_sel <- gspinbutton(1, 35, text = 'Select', container=x_group, value = opts$end_x)

  y_group = ggroup(container = tiles_group, horizontal = TRUE)
  start_y_lab = glabel(text = 'v_start', container = y_group)
  start_y_sel <- gspinbutton(1,18, text = 'Select', container=y_group, value = opts$start_y)
  end_y_lab = glabel(text = 'v_end', container = y_group)
  end_y_sel <- gspinbutton(1,18, text = 'Select', container=y_group, value = opts$end_y)

  show_map = gbutton(text = 'Show Tiles Map', border = T,
                     handler = function(h,....) {x11()
                                                 plot(raster(file.path(moddwl_opts$main_dir, "/Accessoires/MODIS_Tiles.gif")))},
                     container =x_group )

  output_proj_frame = gframe(text = 'Output Projection and Extent', container = main_group)
  output_proj_group = ggroup (container = output_proj_frame, horizontal = FALSE)
  proj_sel <- gcombobox(names(opts$out_proj_list), container=output_proj_group, selected = match(opts$proj, names(opts$out_proj_list)))

  output_res_lab = glabel(text = 'Output Resolution (in OUTPUT proj measure units - Blank for Native Resolution)', container = output_proj_group)
  output_res_sel = gedit(text = opts$out_res , container = output_proj_group, width = 15)

  output_ext_lab = glabel(text = 'Output Bounding Box (in OUTPUT proj coordinates) - Leave Blank if no resize needed', container = output_proj_group)

  output_lab_group =  ggroup(horizontal = TRUE, container=output_proj_group, width = 200)
  output_xmin_lab = glabel('      Minimum X      ', container = output_lab_group, width = 15)
  output_xmax_lab = glabel('      Maximum X      ', container = output_lab_group)
  output_ymin_lab = glabel('      Minimum Y      ', container = output_lab_group)
  output_ymax_lab = glabel('      Maximum Y      ', container = output_lab_group)

  output_ext_group =  ggroup(horizontal = TRUE, container=output_proj_group)
  output_xmin_sel = gedit(text = opts$bbox[1], container = output_ext_group, width = 15)
  output_xmax_sel = gedit(text = opts$bbox[2], container = output_ext_group, width = 15)
  output_ymin_sel = gedit(text = opts$bbox[3], container = output_ext_group, width = 15)
  output_ymax_sel = gedit(text = opts$bbox[4], container = output_ext_group, width = 15)

  options_frame = gframe(text = 'Processing Options', container = main_group)
  opt_group = ggroup(container = options_frame)
  format_lab = glabel(text = 'Output Format', container = opt_group)
  format_sel <- gdroplist(items = c('ENVI','GTiff'), text = 'Select', container=opt_group, selected = match(opts$format, c('ENVI','GTiff')))

  reprocess_lab = glabel(text = 'ReDownload Existing', container = opt_group)
  reprocess_sel <- gradio(items = c('Yes','No'), text = 'Select', container=opt_group, selected = match(opts$reprocess, c('Yes','No')), horizontal = T)

  outfold_frame = ggroup(text = "Output Folder", container=main_group)    			# Main group
  outfold_group = ggroup(horizontal = TRUE, container=outfold_frame)  				# Main group
  outfold_lab <- glabel(text ='Main Folder for download of MODIS data', container=outfold_group, editable =FALSE)  # Label
  #   size(Out_Lab) <- c(280,8)																		# Set label width
  outfold_sel<- gedit(text = format(opts$out_folder, justify = "right") , container=outfold_group, width = 40)			# Selected file
  #   size(MOD_Dir) <-c(600,20)																			# Set field width
  fold_choose <- gbutton("Browse", handler=function(h,...) {choice<-gfile(type="selectdir", text="Select the Output Folder for MODIS data...")		# File selection widget
                                                            if(! is.na(choice)){svalue(outfold_sel)<-choice						## On new selection, set value of the label widget
                                                                                opts$out_folder = format(choice, justify = "right")	# 	On new selection,  Set value of the selected variable
                                                            }}, container=outfold_group)

  start_frame = gframe (container = main_group)
  start_group = ggroup(container = start_frame, horizontal = TRUE)
  start_but = gbutton(text = 'Start', container = start_group, handler = function(button,...){

    # Retrieve selected values
    opts$modprod = svalue(prod_sel)
    opts$sensor = sens_sel
    opts$start_day = svalue(start_day_sel)
    opts$start_month = svalue(start_month_sel)
    opts$start_year = svalue(start_year_sel)
    opts$end_day = svalue(end_day_sel)
    opts$end_month = svalue(end_month_sel)
    opts$end_year = svalue(end_year_sel)

    opts$start_x = svalue(start_x_sel)
    opts$end_x = svalue(end_x_sel)
    opts$start_y = svalue(start_y_sel)
    opts$end_y = svalue(end_y_sel)

    opts$proj = svalue(proj_sel)
    opts$format = svalue(format_sel)
    opts$reprocess = svalue(reprocess_sel)
    opts$bbox = ((c(svalue(output_xmin_sel),svalue(output_xmax_sel),svalue(output_ymin_sel),svalue(output_ymax_sel))))
    opts$out_res = (svalue(output_res_sel))
    opts$out_folder = svalue(outfold_sel)

    if (exists ('temp_sel_bands')) {

#       a = which(opts$prod_opt_list[[pos_prod]]$bandnames %in% temp_sel_bands)
#       b = array(data = 0 , dim = length(opts$prod_opt_list[[pos_prod]]$bandsel))
#       b[a] = 1
      opts$prod_opt_list[[pos_prod]]$bandsel = temp_sel_bands
      rm(temp_sel_bands, envir = globalenv())
    }

    if (exists ('temp_sel_bands_derived')) {

#       a = which(opts$prod_opt_list[[pos_prod]]$derived_bandnames %in% temp_sel_bands_derived)
#       b = array(data = 0 , dim = length(opts$prod_opt_list[[pos_prod]]$bandsel_derived))
#       b[a] = 1
      opts$prod_opt_list[[pos_prod]]$derived_bandsel = temp_sel_bands_derived
      rm(temp_sel_bands_derived, envir = globalenv())
    }


    check = T
    # Check if dates, processing extent and tilews selection make sense
    if (as.Date(paste(opts$start_year, opts$start_month, opts$start_day, sep = '-')) >=
          as.Date(paste(opts$end_year, opts$end_month, opts$end_day, sep = '-'))) {gmessage('Error in Selected Dates', title = 'Warning'); check = F}

    if ((opts$start_x > opts$end_x ) | (opts$start_y > opts$end_y )) {gmessage('Error in Selected Tiles', title = 'Warning') ; check = F}

    n_bbox_compiled = length(which(is.finite(opts$bbox)))

    if (n_bbox_compiled == 4){
      if ((opts$bbox[1] > opts$bbox[2]) | (opts$bbox[3] > opts$bbox[4])) {gmessage('Error in Selected Output extent', title = 'Warning') ; check = F}}
    if ((n_bbox_compiled < 4) & (n_bbox_compiled > 0 )) {gmessage('Error in Selected Output extent', title = 'Warning') ; check = F}


    if (check == T) {
      dir.create(file.path(getwd(),'Previous'))
      save(opts, file = moddwl_opts$previous_file)
      assign("Quit", F, envir=globalenv())
      dispose(main_win)
    }

  })
  quit_but = gbutton(text = 'Cancel', container = start_group, handler = function(button,...){
    assign("Quit", T, envir=globalenv())
    dispose(main_win)}
  )


  visible(main_win, set=TRUE) ## show the selection GUI

}
