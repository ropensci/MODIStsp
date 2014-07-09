LB_MOD_DWL_GUI = function {
  
  library(gWidgets)
  options("guiToolkit"="RGtk2")
  
  main_win <- gwindow("MODIS DOWNLOAD Tool", visible=TRUE)
  group <- gframe(text ="MODIS Products",horizontal = FALSE, container=main_win)
  prod_sel <- gcheckboxgroup(c("MOD13/MYD13","MOD09/MYD09","MOD11/MYD11"), container=group)
  
  dates_group = gframe(text = 'Procesing period', container = main_win)
  start_date_lab = glabel(text = 'Starting Date', container = dates_group)
  start_date_sel <- gcalendar(text = 'Select', container=dates_group)
  
  end_date_lab = glabel(text = 'Ending Date', container = dates_group)
  end_date_sel <- gcalendar(text = 'Select', container=dates_group)
  
  output_proj_lab = glabel(text = 'Select Output Projection', container = group)
  proj_sel <- gcombobox(c("UTM 32N","latlon WGS84",'Other'), container=group)
  output_proj_lab = glabel(text = 'Proj4 String', container = group)
  output_proj_edit = gedit( container = group)
  
  output_proj_lab = glabel(text = 'Select Output Resolution', container = group)
  output_res_edit = gedit(container = main_win)
  
  output_ext_lab = glabel(text = 'Select Output Bounding Box (in output proj coordinates)', container = group)
  output_ext_group =  ggroup(horizontal = TRUE, container=main_win)
  
  output_xmin_edit = gedit(initial.msg = "Minimum X", container = output_ext_group)
  output_xmax_edit = gedit(text = "Maximum X", container = output_ext_group)
  output_ymin_edit = gedit(text = "Minimum Y", container = output_ext_group)
  output_ymax_edit = gedit(text = "Maximum Y", container = output_ext_group)
  
}