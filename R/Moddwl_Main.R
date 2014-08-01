#'@title Moddwl_Main
#'@description Main function for the MODIS download and preprocessing functions
#'@details
#'  The function is used to initialize the processing (folder names, packages, etc.), to launch the GUI (moddwl_GUI) and receive outputs,
#'  and to launch the required routines for downloading and processing the requested datasets.
#' @return
#' NULL
#'
#'
#' @author Lorenzo Busetto (2014)
#' email: lorenzo.busetto@@jrc.ec.europa.eu
#'

#' @export



moddwl_main = function() {

  #- ------------------------------------------------------------------------------- -#
  #  Initialize project
  #- ------------------------------------------------------------------------------- -#

  # Check if needed packages are present. Install them otherwise
  pkg_list = c('gWidgets','rgdal','plyr', 'reshape2','ggplot2','data.table','hash','raster','RCurl')
  pkg_test <- function(x) {if (!require(x,character.only = TRUE)) {install.packages(x,dep=TRUE)}}
  for (pkg in pkg_list) {pkg_test(pkg)}

  options("guiToolkit"="RGtk2")
  memory.limit(4000)							# Increase maximum allocable memory

  # Folder Inittialization -----

#   rscript.stack <- function() {Filter(Negate(is.null), lapply(sys.frames(), function(x) x$ofile))}    			#	Returns the stack of RScript files
#   rscript.current <- function() {	stack <- rscript.stack()   ;	  as.character(stack[length(stack)])}		## Returns the current RScript file path
#   src_dir = dirname(rscript.current())

  src_dir = "D:/Documents/Source_Code/R/LB_MOD_DWL/R"
  setwd(file.path(src_dir,'..'))       ;   main_dir = getwd()   ;   previous_dir = file.path(main_dir,'/Previous')   ; log_dir =  file.path(main_dir,'/Log')
  dir.create(previous_dir, showWarnings = FALSE, recursive = TRUE) ; dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
  #   IDL_Dir = file.path(main_dir,'IDL-FRG')

  # Sourcing of needed R scripts (Remove when building package !!!!)-----

  source(file.path(src_dir,'moddwl_GUI.R'))
  source(file.path(src_dir,'moddwl_accessoires.R'))
  source(file.path(src_dir,'moddwl_process.R'))
  source(file.path(src_dir,'moddwl_set_opts.R'))
  source(file.path(src_dir,'Moddwl_QA_convert.R'))

  # Create the moddwl_opts structure used to communicate with the GUI and launch the GUI ----
  moddwl_opts = list(main_dir = main_dir, previous_dir = previous_dir, log_file = file.path(log_dir,paste(Sys.Date(),'log.txt', sep='_')),
                      previous_file = file.path(previous_dir, 'Moddwl_Previous.RData'))
  moddwl_GUI(moddwl_opts)
  # If not Quit selected, restore the user selected options and launch the processing ----

  if (Quit == F) {
    rm(Quit, envir = globalenv())

    if (file.exists(moddwl_opts$previous_file)) {load (moddwl_opts$previous_file)
    } else {print('Download Options file not found ! Exiting !')   ; stop()}
    moddwl_opts = c(moddwl_opts, opts)
    # Start cycling on selected MODIS products -----

    for(product in moddwl_opts$modprod) {
      # Prompt user for selection of downloading options

      moddwl_opts = c(moddwl_opts, opts)

      pos_prod = which(names(opts$prod_opt_list) == opts$modprod)

      # Launch the processing
      start_date = paste(moddwl_opts$start_year, moddwl_opts$start_month, moddwl_opts$start_day, sep = '.')
      end_date = paste(moddwl_opts$end_year, moddwl_opts$end_month, moddwl_opts$end_day, sep = '.')
#       if (moddwl_opts$proj == 'Sinusoidal' & moddwl_opts$out_res != '') {reproj = F} else {reproj = T}
      reproj = T
      outproj_str = moddwl_opts$out_proj_list[[moddwl_opts$proj]]   ;  if (outproj_str =='') {outproj_str = moddwl_opts$MOD_prj_str}


      if(moddwl_opts$out_res == '') {moddwl_opts$out_res = moddwl_opts$prod_opt_list[[pos_prod]]$native_res}
      sensor = 'Terra'

      if (sensor == "Terra") {FTP = moddwl_opts$prod_opt_list[[pos_prod]]$FTP[["Terra"]]} else {FTP = moddwl_opts$prod_opt_list[[pos_prod]]$FTP[["Aqua"]]}
      if (sensor == "Terra") {file_prefix = moddwl_opts$prod_opt_list[[pos_prod]]$file_prefix[["Terra"]]} else {file_prefix = moddwl_opts$prod_opt_list[[pos_prod]]$file_prefix[["Aqua"]]}

      bandsel = moddwl_opts$prod_opt_list[[pos_prod]]$bandsel
      res = with(moddwl_opts, moddwl_process(product = product, start_date=start_date,end_date = end_date,out_folder = out_folder, MRTpath = MRTpath,
                           reprocess = reprocess, FTP = FTP,sensor = sensor,
                           start_x = start_x, start_y = start_y, end_x = end_x, end_y = end_y,
                           bbox = bbox, format = format, out_res = as.numeric(out_res),
                           MOD_prj_str = MOD_prj_str,outproj_str = outproj_str, reproj = reproj,
                           nodata_in = prod_opt_list[[pos_prod]]$nodata_in, nodata_out= prod_opt_list[[pos_prod]]$nodata_out,
                           bandsel = prod_opt_list[[pos_prod]]$bandsel, bandnames = prod_opt_list[[pos_prod]]$bandnames,
                           datatype =prod_opt_list[[pos_prod]]$datatype,
                           file_prefix = file_prefix, main_out_folder =prod_opt_list[[pos_prod]]$main_out_folder,
                           multiband_bsq = T)
      )		# Download the images for the year

      #     moddwl_process <- function(start_date=start_date,end_date = end_date, OutPath = OutPath,  ovr = ovr, del = del, reproj = reproj, opts = opts,
      #                                bandnames = bandnames,bands_subset=bands_subset,nodatas=nodatas,nodata_out=nodata_out) {

    }

    return(opts)

  }
  rm(Quit, envir = globalenv())
}

a = moddwl_main()


