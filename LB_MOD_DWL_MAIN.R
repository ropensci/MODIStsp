# LB_MOD_DWL_Main = function() {

  out_proj_list = c("Sinusoidal","UTM 32N","Latlon WGS84")
  mod_prod_list = c("MOD13/MYD13","MOD09/MYD09","MOD11/MYD11")
  LB_MOD_DWL_GUI(out_proj_list,mod_prod_list)
  opts$modprod

# }
  a = gbasicdialog(title = "Dialog", widget, parent=NULL, do.buttons=TRUE, handler = NULL, action=NULL,  toolkit=guiToolkit())
