#'@title Moddwl_Main
#'@description Main function for the MODIS download and preprocessing functions
#'@details
#'  The function is used to initialize the processing (folder names, packages, etc.), to launch the GUI (Moddwl_GUI) and receive outputs,
#'  and to launch the required routines for downloading and processing the requested datasets.
#' @return
#' NULL
#'
#'
#' @author Lorenzo Busetto (2014)
#' email: lorenzo.busetto@@jrc.ec.europa.eu
#'

#' @export



Moddwl_Main = function() {

  #- ------------------------------------------------------------------------------- -#
  #  Initialize project
  #- ------------------------------------------------------------------------------- -#

  # Check if needed packages are present. Install them otherwise
  pkgList = c('gWidgets','rgdal','plyr', 'reshape2','ggplot2','data.table','hash','raster','RCurl')
  pkgTest <- function(x) {if (!require(x,character.only = TRUE)) {install.packages(x,dep=TRUE)}}
  for (pkg in pkgList) {pkgTest(pkg)}

  options("guiToolkit"="RGtk2")
  memory.limit(6000)							# Increase maximum allocable memory

# Folder Inittialization -----

  rscript.stack <- function() {Filter(Negate(is.null), lapply(sys.frames(), function(x) x$ofile))}    			#	Returns the stack of RScript files
  rscript.current <- function() {	stack <- rscript.stack()   ;	  as.character(stack[length(stack)])}		## Returns the current RScript file path
  Src_Dir = dirname(rscript.current())
  setwd(file.path(Src_Dir,'..'))       ;   Main_Dir = getwd()   ;   Previous_Dir = file.path(Main_Dir,'/Previous')   ; Log_Dir =  file.path(Main_Dir,'/Log')
  dir.create(Previous_Dir, showWarnings = FALSE, recursive = TRUE) ; dir.create(Log_Dir, showWarnings = FALSE, recursive = TRUE)
  #   IDL_Dir = file.path(Main_Dir,'IDL-FRG')

# Sourcing of needed R scripts (Remove when building package !!!!)-----

  source(file.path(Src_Dir,'Moddwl_GUI.R'))
  source(file.path(Src_Dir,'Moddwl_Accessoires.R'))
  source(file.path(Src_Dir,'Moddwl_Process.R'))
  source(file.path(Src_Dir,'Moddwl_prodopts.R'))

# Define arrays of possible MODIS products and Projections ------

  mod_prod_list = c("MOD13Q1/MYD13Q1","MOD09GK/MYD09GK","MOD09GA/MYD09GA","MOD11/MYD11")
  out_proj_list = hash("Sinusoidal" = "",
                       "UTM 32N" = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                       "Latlon WGS84" = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")

# Create the MOddwl_opts structure used to communicate with the GUI and launch the GUI ----
  Moddwl_opts = list (Main_Dir = Main_Dir, Previous_Dir = Previous_Dir,  MRTpath='C:/MRT/bin', log_file = file.path(Log_Dir,paste(Sys.Date(),'log.txt', sep='_')),
                      Previous_File = file.path(Previous_Dir, 'Moddwl_Previous.RData'), out_proj_list = out_proj_list,mod_prod_list = mod_prod_list,
                      MRTpath = 'C:/MRT/bin', MOD_prj_str = '+proj=sinu +R=6371007.181 +nadgrids=@null +wktext')



# Restore the user selected options and launch the processing ----
  Moddwl_GUI(Moddwl_opts)
  if (file.exists(Moddwl_opts$Previous_File)) {load (Moddwl_opts$Previous_File)
  } else {print('Download Options file not found ! Exiting !')   ; stop()}
  Moddwl_opts = c(Moddwl_opts, opts)
# Start cycling on selected MODIS products -----

  for(product in Moddwl_opts$modprod) {
    # Prompt user for selection of downloading options

    prod_opts = Moddwl_prodopts(product)
    Moddwl_opts = c(Moddwl_opts, prod_opts)

    # Launch the processing
    Start_Date = paste(Moddwl_opts$start_year, Moddwl_opts$start_month, Moddwl_opts$start_day, sep = '.')
    End_Date = paste(Moddwl_opts$end_year, Moddwl_opts$end_month, Moddwl_opts$end_day, sep = '.')
    if (Moddwl_opts$proj == 'Sinusoidal') {reproj = F} else {reproj = T}
    outproj_str = Moddwl_opts$out_proj_list[[Moddwl_opts$proj]]
    sensor = 'Terra'

    if (sensor == "Terra") {FTP = Moddwl_opts$FTP[["Terra"]]} else {FTP = Moddwl_opts$FTP[["Aqua"]]}

    res = Moddwl_Process(product = product, Start_Date=Start_Date,End_Date = End_Date,out_folder = Moddwl_opts$out_folder, MRTpath = Moddwl_opts$MRTpath,
                         reproj = reproj, reprocess = Moddwl_opts$reprocess, FTP = FTP,
                         bbox = Moddwl_opts$bbox, format = Moddwl_opts$format, start_x = Moddwl_opts$start_x, start_y = Moddwl_opts$start_y,
                         end_x = Moddwl_opts$end_x, end_y = Moddwl_opts$end_y, nodata_in = Moddwl_opts$nodata_in, nodata_out= Moddwl_opts$nodata_out,
                         bandnames = Moddwl_opts$bandnames, bands_subset = Moddwl_opts$bands_subset, MOD_prj_str = Moddwl_opts$MOD_prj_str, outproj_str = outproj_str,
                         out_res = Moddwl_opts$out_res, sensor = sensor
                         )		# Download the images for the year

#     Moddwl_Process <- function(Start_Date=Start_Date,End_Date = End_Date, OutPath = OutPath,  ovr = ovr, del = del, reproj = reproj, opts = opts,
#                                bandnames = bandnames,bands_subset=bands_subset,nodatas=nodatas,nodata_out=nodata_out) {

  }

return(opts)

}

a = Moddwl_Main()


