#'@title FRG_Full_Proc_GUI
#'@description Function used to create the GUI for selecting the parameters for full FRG procesing
#'@details
#'
#' @return
#' If the user chooses 'Start' and all processing parameters are OK, the selected parameters are saved in the 'FRG_Full_Previous.RData' file
#'
#' @author Lorenzo Busetto (2012)
#' email: lorenzo.busetto@@jrc.ec.europa.eu
#'
#' Created Date: Oct 26, 2012
#' @export



Moddwl_Main = function() {


  #- ------------------------------------------------------------------------------- -#
  #  Initialize project
  #- ------------------------------------------------------------------------------- -#

  # Check if needed packages are present. Install them otherwise
  pkgList = c('tools','debug','gWidgets','gWidgetsRGtk2','RCurl','rgdal','reshape2','ggplot2','tcltk',
              'gdata','abind', 'data.table','hash','plyr','car')
  for (pkg in pkgList) {pkgTest(pkg)  }
  options("guiToolkit"="RGtk2")

  memory.limit(6000)							# Increase maximum allocable memory

  # Folder initialization
  rscript.current()
  Src_Dir = dirname(rscript.current())
  print(Src_Dir)
  setwd(file.path(Src_Dir,'../..')); 	Main_Dir = getwd()
  Previous_Dir = file.path(Main_Dir,'R-FRG/Previous')
  IDL_Dir = file.path(Main_Dir,'IDL-FRG')
  IDL_Dir
  # Sourcing of needed R scripts (Remove when building package !!!!)

  source(file.path(Src_Dir,'GUI','FRG_MOD_GUI.R'))
  source(file.path(Src_Dir,'Processing','FRG_MOD_Comp_RDVI.R'))
  source(file.path(Src_Dir,'Processing','FRG_MOD_Comp_UI.R'))
  source(file.path(Src_Dir,'Processing','FRG_MOD_Comp_MeanInd.R'))
  source(file.path(Src_Dir,'Processing','FRG_MOD_Download.R'))
  source(file.path(Src_Dir,'Processing','FRG_MOD_Proc.R'))

  source(file.path(Src_Dir,'GUI','FRG_SVI_GUI.R'))
  source(file.path(Src_Dir,'Processing','FRG_MOD_Comp_SVI.R'))

  source(file.path(Src_Dir,'Processing','FRG_Extr_Stats.R'))
  source(file.path(Src_Dir,'GUI','FRG_Extr_Stats_GUI.R'))

  source(file.path(Src_Dir,'Processing','FRG_Significance_Matrix.R'))
  source(file.path(Src_Dir,'Processing','FRG_Comp_Sig_Matrix.R'))
  source(file.path(Src_Dir,'GUI','FRG_Regr_GUI.R'))

  source(file.path(Src_Dir,'Processing','FRG_Full_Processing.R'))
  source(file.path(Src_Dir,'GUI','FRG_Full_Proc_GUI.R'))



library(gWidgets)
options("guiToolkit"="RGtk2")

  out_proj_list = c("Sinusoidal","UTM 32N","Latlon WGS84")
  mod_prod_list = c("MOD13/MYD13","MOD09/MYD09","MOD11/MYD11")
  Moddwl_GUI(out_proj_list,mod_prod_list)
  opts$modprod
  if (file.exists(file.path(getwd(),'Previous','Moddwl_Previous.RData'))) {load (file.path(getwd(),'Previous','MOD_DWL_Previous.RData'))
  } else {dialog_}
# }

