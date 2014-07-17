Moddwl_GUI = function (Moddwl_opts){

  if (file.exists(Moddwl_opts$Previous_File)) {load (Moddwl_opts$Previous_File)
  } else {opts =  list(modprod = '',
                       start_day = 1,
                       start_month = 11,
                       start_year = 2003,
                       end_day = 1,
                       end_month = 1,
                       end_year = 2000,
                       start_x = 1,
                       end_x =1,
                       start_y = 1,
                       end_y = 1,
                       proj = 'Sinusoidal',
                       format = 'GTiff',
                       reprocess ='No',
                       bbox = c('100','','300',''),
                       out_folder = '')}

  main_win <- gbasicdialog(title = "Select Main Processing Oprions", parent=NULL, do.buttons=FALSE)

  main_group = ggroup(container = main_win, horizontal = FALSE)
  prod_frame <- gframe(text ="MODIS Products",horizontal = TRUE, container=main_group)
  checked = rep(F, length(Moddwl_opts$mod_prod_list)) ; checked[match(opts$modprod, Moddwl_opts$mod_prod_list)] = T
  prod_sel <- gcheckboxgroup(items = Moddwl_opts$mod_prod_list, container=prod_frame, horizontal = T, checked = checked)


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
                                                 plot(raster(file.path(Moddwl_opts$Main_Dir, "/Accessoires/MODIS_Tiles.gif")))},
                     container =x_group )

  output_proj_frame = gframe(text = 'Output Projection and Extent', container = main_group)
  output_proj_group = ggroup (container = output_proj_frame, horizontal = FALSE)
  proj_sel <- gcombobox(names(Moddwl_opts$out_proj_list), container=output_proj_group, selected = match(opts$proj, names(Moddwl_opts$out_proj_list)))

  output_ext_lab = glabel(text = 'Select Output Bounding Box (in OUTPUT proj coordinates) - Leave Blank if no resize needed', container = output_proj_group)

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
    opts$bbox = as.numeric((c(svalue(output_xmin_sel),svalue(output_xmax_sel),svalue(output_ymin_sel),svalue(output_ymax_sel))))

    opts$out_folder = svalue(outfold_sel)

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
      save(opts, file = Moddwl_opts$Previous_File)
      dispose(main_win)
    }

  })
  visible(main_win, set=TRUE) ## show the selection GUI

}
