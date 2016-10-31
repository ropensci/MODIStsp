#' MODIStsp
#' @description Main function for the MODIS Time Series Processing Tool (MODIStsp)
#' @details The function is used to initialize the processing (folder names, packages, 
#'  etc.), to launch the GUI (MODIStsp_GUI) and receive its outputs, and to launch the 
#'  required routines for downloading and processing the requested datasets.
#' @param gui logical parameter (TRUE: the GUI is opened before processing; FALSE: the 
#'  saved parameters are retrieved from "options_file")
#' @param options_file settings (optional): full path of the JSON file containing the 
#'  processing options (default: MODIStsp_Previous.json in subdir Previous);
#' @param spatial_file_path (optional): full path of a spatial file to use as extent 
#'  (default=NULL): if defined, the processing options which define the extent, the 
#'  selected tiles and the "Full Tile / Resized" options are not considered; instead, new 
#'  files are created on the extent of the provided spatial file.
#' @param scrollWindow (optional) logical parameter: if TRUE, the GUI window is opened 
#' fullscreen with scrollbars (this is useful on devices with small display). If using a
#'  device with a display resolution >= 1024x768, leaving this parameter to FALSE is 
#'  suggested.
#' @return NULL
#'
#' @author Lorenzo Busetto, phD (2014-2015) \email{busetto.l@@irea.cnr.it}
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
#' @import gWidgets2
#' @importFrom pacman p_load p_exists
#' @importFrom hash hash
#' @importFrom gdalUtils gdal_setInstallation gdalinfo
#' @importFrom rgdal getGDALVersionInfo
#' @importFrom raster extent rasterOptions
#' @importFrom tools file_path_sans_ext
#' @importFrom utils memory.limit packageVersion
#' @examples
#' # Running the tool without any option will start the GUI with the default or last used 
#' # settings
#' \dontrun{
#' MODIStsp()}
#'
#' # Run the tool using the settings previously saved in a specific option file
#' \dontrun{
#' MODIStsp(gui = FALSE, options_File = "X:/yourpath/youroptions.json")}
#'
#' # Run the tool using a previously saved options file,
#' # but editing it with the GUI before starting the processing
#' \dontrun{
#' MODIStsp(options_File = "X:/yourpath/youroptions.json")}
#'
#' # Run the tool using the settings previously saved in a specific option file
#' # and specifying the extent from a spatial file
#' \dontrun{
#' MODIStsp(gui = FALSE, options_File = "X:/yourpath/youroptions.json",
#'   spatial_file_path = "X:/yourpath/yourspatialfile.shp" )}
#'
#' # Run the tool in a batch mode, using the settings previously saved in a specific
#' # option file and specifying each time the extent from a different spatial file
#' \dontrun{
#' extent_list = list.files("X:/path/containing/some/shapefiles/", "\\.shp$")
#' for (single_shape in extent_list)
#'   MODIStsp(gui = FALSE, options_File = "X:/yourpath/youroptions.json",
#'     spatial_file_path = single_shape )}

MODIStsp <- function(gui=TRUE, options_file=NULL, spatial_file_path=NULL, scrollWindow=FALSE) {
  
  
  options("guiToolkit" = "RGtk2")
  
  MODIStsp.env <- new.env()
  MODIStsp.env$MODIStsp_dir <- system.file(package = "MODIStsp")
  
  #- ------------------------------------------------------------------------------- -#
  #  Initialize project
  #- ------------------------------------------------------------------------------- -#
  # On interactive execution, load Rgtk2
  # On interactive execution, load Rgtk2
  if (gui) {
    if (!pacman::p_exists("gWidgets2RGtk2", local = TRUE)) {
      #inst_gw <- utils::winDialog("Library 'gWidgetsRgtk2' is not installed. It is required to run MODIStsp ! \n \n Do you want to install it now ?", type = "yesno")
      message("Library 'gWidgetsRgtk2' is not installed. It is required to run MODIStsp! 
              \n\nDo you want to install it now?", type = " y / n")
      inst_gw <- readline()
      if (inst_gw =="y") {
        pacman::p_load("gWidgets2RGtk2")
        suppressWarnings("gWidgets2RGtk2")
      } else {
        stop("MODIStsp can not work in Interactive mode withouth gWidgets2RGtk2 ! Exiting !")
      }
      
    }
    # requireNamespace("gWidgetsRGtk2")
    options("guiToolkit" = "RGtk2")
  }
  
  # Check GDAL version
  if (is.null(getOption("gdalUtils_gdalPath"))) {
    # gdal_setInstallation(ignore.full_scan = FALSE)
    welcome_text <- "Welcome to MODIStsp!\n\nWe will now search for a valid GDAL installation - please wait\n(this will happen only once)"
    if (gui) {
      welcome_win <- gwindow(title = "Welcome", width = 400, height = 100)
      welcome_lab <- glabel(welcome_text, editable = FALSE, container = welcome_win)
      # mess <- gmessage(title = "Welcome", welcome_text, do.buttons = FALSE)
    } else {
      message("[",date(),"]",welcome_text)
    }
    gdal_setInstallation(ignore.full_scan = TRUE, verbose = TRUE)
  }
  gdal_version    <- package_version(gsub("^GDAL ([0-9.]*)[0-9A-Za-z/., ]*","\\1", getGDALVersionInfo(str = "--version")))
  gdal_minversion <- package_version("1.11.1") # GDAL version used during the last test (for now used as minimum required version)
  gdal_HDFsupport <- length(grep("HDF4", gdalinfo(formats = TRUE))) > 0
  
  if (gdal_version < gdal_minversion) {
    stop(paste0("GDAL version must be at least ",gdal_minversion,". Please update it."))
  }
  
  if (!gdal_HDFsupport) {
    stop("Your local GDAL installation does not support HDF4 format. Please install HDF4 support and recompile GDAL.")
  }
  
  cat("GDAL version in use:",as.character(gdal_version),"\n")
  
  # Increase memory limit on windows
  if (Sys.info()["sysname"] == "Windows") {
    memory.limit() # Increase maximum allocsable memory
  }							
  rasterOptions(setfileext = FALSE)				# Make so that "raster" functions doesn't automatically add extensions on output files
  
  # Parameter retrieval and Folder Initialization -----
  if (is.null(options_file) & gui == FALSE) {
    stop("Please provide a valid \"option_file\" path value (or run with gui=TRUE).")
  }
  
  # Folders in which the JSON/RData files (previous settings and product descriptions) are saved
  if (is.null(options_file)) {
    previous_dir <- file.path(MODIStsp.env$MODIStsp_dir,"Previous")
  } else {
    previous_dir <- dirname(options_file)
  }
  prodopts_dir <- file.path(MODIStsp.env$MODIStsp_dir,"Previous")
  dir.create(previous_dir, showWarnings = FALSE, recursive = TRUE) #; dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(prodopts_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Previous options file (or file passed by user in non-interactive mode)
  previous_jsfile <- if (is.null(options_file)) {
    file.path(previous_dir, "MODIStsp_Previous.json")
  } else {
    options_file
  }
  xml_file <- file.path(MODIStsp.env$MODIStsp_dir,"ExtData","MODIStsp_ProdOpts.xml")  #XML file describing MODIS products
  prodopts_file <- file.path(prodopts_dir, "MODIStsp_ProdOpts.RData") # this is created to speed up XML reading (done only the first time)
  
  #- ------------------------------------------------------------------------------- -#
  #  Set general processing options - used at first execution to initialize GUI
  #- ------------------------------------------------------------------------------- -#
  
  MOD_proj_str <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
  
  # Load options if existing, otherwise initialise them ----
  if (file.exists(previous_jsfile)) {
    general_opts <- RJSONIO::fromJSON(previous_jsfile)
  } 
  if (!exists("general_opts")) {
    # Create the general_opts structure used to communicate with the GUI and set default values
    general_opts <- list(#previous_jsfile = previous_jsfile, prodopts_file = prodopts_file, xml_file = xml_file, #out_proj_list = out_proj_list, out_proj_names = out_proj_names, MOD_proj_str = MOD_proj_str,
      sel_prod = "Surf_Ref_8Days_500m (M*D09A1)", sensor = "Terra", prod_version = "6", start_date = strftime(Sys.Date(),"%Y-01-01"), end_date = as.character(Sys.Date()),
      bandsel = rep(0,13), indexes_bandsel = rep(0,11), quality_bandsel = rep(0,21), # lenghts refearred to "Surf_Ref_8Days_500m (M*D09A1)" v6!
      start_x = 18, end_x = 18, start_y = 4, end_y = 4, user = "", password = "", download_server = "http",
      proj = "Sinusoidal", user_proj4 = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs",
      out_res_sel = "Native", out_res = "", full_ext = "Full Tiles Extent", resampling = "near", out_format = "ENVI", ts_format = "ENVI Meta Files", rts = "Yes", compress = "None",
      nodata_change = "No", delete_hdf = "No", reprocess = "No", bbox = c("","","",""), out_folder = "", out_folder_mod = "",
      MODIStspVersion = as.character(packageVersion("MODIStsp")), custom_indexes = list())
    write(RJSONIO::toJSON(general_opts),previous_jsfile)
  } else if (is.null(general_opts$MODIStspVersion)) {
    stop(paste0("The option file in use (",previous_jsfile,") was created with an too old MODIStsp version (<=1.2.2), and can not be used with the current version. Please delete it or specify a different value for option_file parameter."))
  } else if (general_opts$MODIStspVersion<packageVersion("MODIStsp")) {
    warning(paste0("The option file in use (",previous_jsfile,") was created with an old MODIStsp version (",general_opts$MODIStspVersion,"): this could lead to errors!"))
  }
  
  # Restore MODIS products if existing, otherwise retrieve data from xml file ----
  if (file.exists(prodopts_file)) {
    prod_opt_list <- get(load(prodopts_file))
  }
  if (!exists("prod_opt_list") | if (exists("prod_opt_list")) {
    is.null(attr(prod_opt_list,"MODIStspVersion")) | if (!is.null(attr(prod_opt_list,"MODIStspVersion"))) {
      attr(prod_opt_list,"MODIStspVersion")<packageVersion("MODIStsp")
    } else {FALSE}
  } else {FALSE}) {
    mess_text <- "Waiting while reading the MODIS products list..."
    if (gui) {
      mess     <- gwindow(title = "Please wait...", width = 400, height = 40)
      mess_lab <- glabel(text = mess_text, editable = FALSE, container = mess)
    } else {message(mess_text)}
    MODIStsp_read_xml(prodopts_file = prodopts_file, xml_file = xml_file )
    load(prodopts_file)
    if (gui) {
      addHandlerUnrealize(mess_lab, handler = function(h,...) {return(FALSE)})
      dispose(mess_lab)
    }
  }
  
  #launch the GUI if on an interactive session (i.e., gui = T) and wait for return----
  if (gui) {
    if (exists("welcome_lab")) {dispose(welcome_lab)}
    Quit = MODIStsp_GUI(general_opts = general_opts, prod_opt_list = prod_opt_list, MODIStsp_dir = MODIStsp.env$MODIStsp_dir,
                        previous_jsfile = previous_jsfile, prodopts_file = prodopts_file,
                        scrollWindow = scrollWindow)
    
  } else {
    Quit <- FALSE
  }
  start.time <- Sys.time()
  
  # Launch the processing ----
  # When GUI is closed (or in a non-interactive run): If not Quit selected, restore the user selected options from previous file and launch the processing ----
  if (!Quit) {
    
    if (file.exists(previous_jsfile)) {
      general_opts <- RJSONIO::fromJSON(previous_jsfile)
    } else {
      cat("[",date(),"] Processing Options file not found! Exiting!\n"); stop()
    }
    if (file.exists(prodopts_file)) {
      prod_opt_list <- get(load(prodopts_file))
    } else {
      cat("[",date(),"] Product information file not found! Exiting!\n"); stop()
    }
    prod_opts  <- prod_opt_list[[general_opts$sel_prod]][[general_opts$prod_version]]  # retrieve options relative to the selected product from the "prod_opt_list" data frame
    custom_idx <- general_opts$custom_indexes[[general_opts$sel_prod]][[general_opts$prod_version]]
    
    # Create variables needed to launch the processing
    
    general_opts$start_date <- as.character(format(as.Date(general_opts$start_date), "%Y.%m.%d"))
    general_opts$end_date   <- as.character(format(as.Date(general_opts$end_date),   "%Y.%m.%d"))
    
    # If the product is NOT tiled, change or_proj to WGS84 and or_res from metres to degrees
    if (prod_opts$tiled == 0) {
      MOD_proj_str         <- "+init=epsg:4008 +proj=longlat +ellps=clrk66 +no_defs"
      prod_opts$native_res <- format(as.numeric(prod_opts$native_res)*(0.05/5600))
    }
    # get native resolution if out_res empty (Probably obsolete...)
    if (general_opts$out_res == "" | general_opts$out_res_sel == "Native") {
      general_opts$out_res <- prod_opts$native_res
    }
    
    # Changes to perform in the case spatial_file_path is defined
    if (!is.null(spatial_file_path)) {
      
      # Check if the input file is a valid spatial file and redefine the bounding box
      external_bbox <- try(bbox_from_file(file_path = spatial_file_path, out_crs = general_opts$user_proj4),silent = TRUE)
      if (class(external_bbox) == "try-error") {
        stop(external_bbox)
      }
      general_opts$bbox <- external_bbox
      
      # Redefine the out_folder including the file name as subfolder
      # (this to avoid that, running in a cycle, files are overwritten every time)
      general_opts$out_folder <- file.path(general_opts$out_folder,file_path_sans_ext(basename(spatial_file_path)))
      if (file.exists(general_opts$out_folder)) {  # If out_folder already exists, create a new one with a suffix
        tmp_counter   <- 1
        out_newfolder <- paste(general_opts$out_folder,tmp_counter,sep = "_")
        while (file.exists(out_newfolder)) {
          tmp_counter   <- tmp_counter + 1
          out_newfolder <- paste(general_opts$out_folder,tmp_counter,sep = "_")
        }
        general_opts$out_folder <- out_newfolder
      }
      
      # Overwrite the full_ext option (avoids that , if the options_file specifies a full processing,
      # the incorrect parameter is passed)
      general_opts$full_ext <- "Resized"
      
      # Automatically retrieve the tiles requested to cover the extent
      modis_grid <- get(load(file.path(MODIStsp.env$MODIStsp_dir, "ExtData/MODIS_Tiles.RData")))
      external_bbox_mod    <- reproj_bbox(external_bbox, general_opts$user_proj4, MOD_proj_str, enlarge = TRUE)
      d_bbox_mod_tiled     <- intersect(modis_grid, extent(external_bbox_mod))
      general_opts$start_x <- min(d_bbox_mod_tiled$H)
      general_opts$end_x   <- max(d_bbox_mod_tiled$H)
      general_opts$start_y <- min(d_bbox_mod_tiled$V)
      general_opts$end_y   <- max(d_bbox_mod_tiled$V)
      
    }
    
    # launch MODIStsp_process to Download and preprocess the selected images ----
    output <- MODIStsp_process(sel_prod = general_opts$sel_prod, start_date = general_opts$start_date, end_date = general_opts$end_date,
                               out_folder = general_opts$out_folder, out_folder_mod = general_opts$out_folder_mod, reprocess = general_opts$reprocess,
                               delete_hdf = general_opts$delete_hdf, sensor = general_opts$sensor, download_server = general_opts$download_server, 
                               user = general_opts$user, password = general_opts$password,
                               https = prod_opts$http, ftps = prod_opts$ftp,
                               start_x = general_opts$start_x, start_y = general_opts$start_y, end_x = general_opts$end_x, end_y = general_opts$end_y,
                               full_ext = general_opts$full_ext, bbox = general_opts$bbox, out_format = general_opts$out_format, out_res_sel = general_opts$out_res_sel,
                               out_res = as.numeric(general_opts$out_res), native_res = prod_opts$native_res,  tiled = prod_opts$tiled,
                               resampling = general_opts$resampling, ts_format = general_opts$ts_format, compress = general_opts$compress,
                               MOD_proj_str = MOD_proj_str, outproj_str = general_opts$user_proj4,
                               nodata_in = prod_opts$nodata_in, nodata_out = prod_opts$nodata_out, rts = general_opts$rts, nodata_change = general_opts$nodata_change,
                               datatype = prod_opts$datatype,	bandsel = general_opts$bandsel, bandnames = prod_opts$bandnames,
                               indexes_bandsel = general_opts$indexes_bandsel, indexes_bandnames = c(prod_opts$indexes_bandnames,custom_idx$indexes_bandnames),
                               indexes_formula = c(prod_opts$indexes_formula,custom_idx$indexes_formula), indexes_nodata_out = c(prod_opts$indexes_nodata_out,custom_idx$indexes_nodata_out),
                               quality_bandnames = prod_opts$quality_bandnames,quality_bandsel = general_opts$quality_bandsel, quality_bitN = prod_opts$quality_bitN,
                               quality_source = prod_opts$quality_source, quality_nodata_in = prod_opts$quality_nodata_in, quality_nodata_out = prod_opts$quality_nodata_out,
                               file_prefixes = prod_opts$file_prefix, main_out_folder = prod_opts$main_out_folder, gui = gui)
    
    # At end of succesfull execution, save the options used in the main output folder
    general_opts <- RJSONIO::fromJSON(previous_jsfile)
    optfilename = file.path(general_opts$out_folder,paste0('MODIStsp_', Sys.Date(),'.json'))
    write(RJSONIO::toJSON(general_opts),optfilename)
    
    # Clean up at end of processing ----
    # End of processing
    end.time   <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)
    gc()
  } else {# End If on "Quit" --> If "Quit" above is skipped and program terminates
    message("[",date(),"] "," You Selected to Quit! Goodbye!")
  }
  
}
