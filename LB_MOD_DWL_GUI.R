LB_MOD_DWL_GUI = function (out_proj_list, mod_prod_list){

  if (file.exists(file.path(getwd(),'Previous','MOD_DWL_Previous.RData'))) {load (file.path(getwd(),'Previous','MOD_DWL_Previous.RData'))
  } else {opts =  list(modprod = '',
        startdate = '2000-01-01',
        enddate = '2015-12-31',
        start_x_sel = 1,
        end_x_sel =1,
        start_y_sel = 1,
        end_y_sel = 1,
        proj_sel = 'Sinusoidal',
        format_sel = 'GTiff',
        repro_sel ='No',
        bbox = c('100','','300',''),
        out_folder = '')}

  library(gWidgets)
  options("guiToolkit"="RGtk2")

  main_win <- gbasicdialog(title = "Dialog", parent=NULL, do.buttons=FALSE)

  main_group = ggroup(container = main_win, horizontal = FALSE)
  prod_frame <- gframe(text ="MODIS Products",horizontal = TRUE, container=main_group)
  checked = rep(F, length(mod_prod_list)) ; checked[match(opts$modprod, mod_prod_list)] = T
  prod_sel <- gcheckboxgroup(items = mod_prod_list, container=prod_frame, horizontal = T, checked = checked)


  dates_frame = gframe(text = 'Processing period', container = main_group, horizontal = TRUE)
  start_date_lab = glabel(text = 'Starting Date', container = dates_frame)
  start_date_sel <- gcalendar( container=dates_frame, text = opts$startdate)

  end_date_lab = glabel(text = 'Ending Date', container = dates_frame)
  end_date_sel <- gcalendar(container=dates_frame, text = opts$enddate)

  tiles_group = gframe(text = 'Processing extent', container = main_group, horizontal = FALSE)
  x_group = ggroup(container = tiles_group, horizontal = TRUE)
  start_x_lab = glabel(text = 'h_start', container = x_group)
  start_x_sel <- gdroplist(items = seq(1:35), text = 'Select', container=x_group, selected = opts$start_x_sel)
  end_x_lab = glabel(text = 'h_end', container = x_group)
  end_x_sel <- gdroplist(items = seq(1:35), text = 'Select', container=x_group, selected = opts$end_x_sel)

  y_group = ggroup(container = tiles_group, horizontal = TRUE)
  start_y_lab = glabel(text = 'v_start', container = y_group)
  start_y_sel <- gdroplist(items = seq(1:18), text = 'Select', container=y_group, selected = opts$start_y_sel)
  end_y_lab = glabel(text = 'v_end', container = y_group)
  end_y_sel <- gdroplist(items = seq(1:18), text = 'Select', container=y_group, selected = opts$end_y_sel)

  show_map = gbutton(text = 'Show Tiles Map', border = T,
                     handler = function(h,....) {x11()
                                    plot(raster("D:/Documents/Source_Code/R/LB_MOD_DWL/LB_MOD_DWL/Accessoires/MODIS_Tiles.gif"))},
                     container =tiles_group )

  output_proj_frame = gframe(text = 'Output Projection and Extent', container = main_group)
  output_proj_group = ggroup (container = output_proj_frame, horizontal = FALSE)
  proj_sel <- gcombobox(out_proj_list, container=output_proj_group, selected = match(opts$proj_sel, out_proj_list) )

  output_ext_lab = glabel(text = 'Select Output Bounding Box (in OUTPUT proj coordinates) - Leave Blank if no resize needed', container = output_proj_group)

  output_lab_group =  ggroup(horizontal = TRUE, container=output_proj_group, width = 200)
  output_xmin_lab = glabel('              Minimum X              ', container = output_lab_group, width = 40)
  output_xmax_lab = glabel('              Maximum X              ', container = output_lab_group)
  output_ymin_lab = glabel('              Minimum Y              ', container = output_lab_group)
  output_ymax_lab = glabel('              Maximum Y              ', container = output_lab_group)

  output_ext_group =  ggroup(horizontal = TRUE, container=output_proj_group)
  output_xmin_sel = gedit(text = opts$bbox[1], container = output_ext_group)
  output_xmax_sel = gedit(text = opts$bbox[2], container = output_ext_group)
  output_ymin_sel = gedit(text = opts$bbox[3], container = output_ext_group)
  output_ymax_sel = gedit(text = opts$bbox[4], container = output_ext_group)

  options_frame = gframe(text = 'Processing Options', container = main_group)
  opt_group = ggroup(container = options_frame)
  format_lab = glabel(text = 'Output Format', container = opt_group)
  format_sel <- gdroplist(items = c('ENVI','GTiff'), text = 'Select', container=opt_group, selected = match(opts$format_sel, c('ENVI','GTiff')))

  repro_lab = glabel(text = 'ReDownload Existing', container = opt_group)
  repro_sel <- gradio(items = c('Yes','No'), text = 'Select', container=opt_group, selected = match(opts$repro_sel, c('Yes','No')))

  outfold_frame = ggroup(text = "Output Folder", container=main_group)    			# Main group
  outfold_group = ggroup(horizontal = TRUE, container=outfold_frame)  				# Main group
  outfold_lab <- glabel(text ='Main Folder for download of MODIS data', container=outfold_group, editable =FALSE)  # Label
#   size(Out_Lab) <- c(280,8)																		# Set label width
  outfold_sel<- gedit(text = format(opts$out_folder, justify = "right") , container=outfold_group)			# Selected file
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
    opts$startdate = svalue(start_date_sel)
    opts$enddate = svalue(end_date_sel)
    opts$start_x_sel = svalue(start_x_sel)
    opts$end_x_sel = svalue(end_x_sel)
    opts$start_y_sel = svalue(start_y_sel)
    opts$end_y_sel = svalue(end_y_sel)

    opts$proj_sel = svalue(proj_sel)
    opts$format_sel = svalue(format_sel)
    opts$repro_sel = svalue(repro_sel)
    opts$bbox = (c(svalue(output_xmin_sel),svalue(output_xmax_sel),svalue(output_ymin_sel),svalue(output_ymax_sel)))

    opts$out_folder = svalue(outfold_sel)
    dir.create(file.path(getwd(),'Previous'))
    dispose(main_win)
    save(opts, file = file.path(getwd(),'Previous','MOD_DWL_Previous.RData'))
    opts<<- opts

  })
visible(main_win, set=TRUE) ## show the selection GUI
opts
}