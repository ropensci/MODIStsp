#' @title MODIStsp
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
#'  fullscreen with scrollbars (this is useful on devices with small display). If using a
#'  device with a display resolution >= 1024x768, leaving this parameter to FALSE is 
#'  suggested.
#' @param test (optional) integer: if set, the tool is run in test mode, using a
#'  preset Option File instead than opening the GUI or accepting the option_file parameter.
#'  The number indicates which preset file to be used (five files are available).
#'  If test=0, the files sis selected randomly. Default value (-1) indicates that the 
#'  tool is executed normally (not in test mode).
#'  This modality is useful to test the tool in case of errors.
#' @return NULL
#'
#' @author Lorenzo Busetto, phD (2014-2015) \email{busetto.l@@irea.cnr.it}
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
#' @seealso 
#'  \code{\link[MODIStsp]{MODIStsp_GUI}},\code{\link[MODIStsp]{MODIStsp_Process}}
#' @rdname MODIStsp
#' @importFrom gdalUtils gdal_setInstallation gdalinfo
#' @importFrom gWidgets gwindow glabel addHandlerUnrealize dispose
#' @importFrom pacman p_exists p_load
#' @importFrom raster rasterOptions crop extent
#' @importFrom rgdal getGDALVersionInfo
#' @importFrom stringr str_pad
#' @importFrom RJSONIO fromJSON toJSON
#' @importFrom tools file_path_sans_ext
#' @importFrom utils packageVersion
#' @importFrom utils unzip
#' @examples
#' # Running the tool without any option will start the GUI with the default or last used 
#' # settings
#' \dontrun{
#' MODIStsp()}
#'
#' # Run the tool using the settings previously saved in a specific option file
#' \dontrun{
#' MODIStsp(gui = FALSE, options_file = "X:/yourpath/youroptions.json")}
#'
#' # Run the tool using a previously saved options file,
#' # but editing it with the GUI before starting the processing
#' \dontrun{
#' MODIStsp(options_file = "X:/yourpath/youroptions.json")}
#'
#' # Run the tool using the settings previously saved in a specific option file
#' # and specifying the extent from a spatial file
#' \dontrun{
#' MODIStsp(gui = FALSE, options_file = "X:/yourpath/youroptions.json",
#'   spatial_file_path = "X:/yourpath/yourspatialfile.shp" )}
#'
#' # Run the tool in a batch mode, using the settings previously saved in a specific
#' # option file and specifying each time the extent from a different spatial file
#' \dontrun{
#' extent_list = list.files("X:/path/containing/some/shapefiles/", "\\.shp$")
#' for (single_shape in extent_list)
#'   MODIStsp(gui = FALSE, options_file = "X:/yourpath/youroptions.json",
#'     spatial_file_path = single_shape )}

MODIStsp <- function(gui = TRUE, options_file = NULL, 
                     spatial_file_path = NULL, scrollWindow = FALSE, test = -1) {
  
  options("guiToolkit" = "RGtk2")
  # Make so that "raster" functions doesn't automatically add extensions on output files
  # prevopt <- raster::rasterOptions()$setfileext
  raster::rasterOptions(setfileext = FALSE)
  
  #- ------------------------------------------------------------------------ -#
  #  Initialize project
  #- ------------------------------------------------------------------------ -#

  # If test mode is selected, change other parameters
  if (test > -1) {
    gui <- FALSE
    message("MODIStsp is running in test mode.")
    # read names of available json
    test_files <- sort(list.files(system.file("Test_files", package = "MODIStsp"), 
                                  "^test[0-9]{2}\\.json$",
                                  full.names = TRUE))
    # if test=0, select the test file randomly
    if (test == 0) {
      test <- sample(seq_along(test_files),1)
      message(paste0("Randomly selected test ",test,"."))
    } else if (test > length(test_files)) {
      stop(paste0("Value of argument 'test' is too high: only ",length(test_files),
                  " test Option Files are available."))
    }
    # check that the offline HDF files were unzipped
    tests_hdf_zipped <- list.files(system.file("Test_files", package = "MODIStsp"), 
                                   "\\.hdf\\.zip$",
                                   full.names = TRUE)
    for (test_hdf in gsub("\\.zip$","",tests_hdf_zipped)) {
      if (!file.exists(test_hdf)) {
        unzip(zipfile = paste0(test_hdf,".zip"),
              files   = basename(test_hdf),
              exdir   = dirname(test_hdf),
              unzip   = "internal")
      }
    }
    # Assign the selected test Option File
    options_file <- test_files[test]
    # Workaround: if a test with http download was selected,
    # open the GUI so that the user can insert his credentials.
    if (test %in% c(4,5)) {
      direct_username <- readline(prompt = "Enter your USGS username: ")
      direct_password <- readline(prompt = "Enter your password: ")
    }
  }
  
  # On interactive execution, load Rgtk2

  if (gui) {
    if (!pacman::p_exists("gWidgetsRGtk2", local = TRUE)) {
      
      message(paste0("Library 'gWidgetsRGtk2' is not installed. It is required to run MODIStsp!\n\n",
                     "Do you want to install it now?"), type = " y / n")
      inst_gw <- readline()
      if (inst_gw == "y") {
        pacman::p_load("gWidgetsRGtk2")
      } else {
        stop("MODIStsp can not work in Interactive mode without gWidgetsRGtk2! Exiting!")
      }
      
    }
    options("guiToolkit" = "RGtk2")
  }
  
  # Check GDAL version
  if (is.null(getOption("gdalUtils_gdalPath"))) {
    
    welcome_text <- paste0("Welcome to MODIStsp!\n\nWe will now search for a valid GDAL ",
                           "installation - please wait\n(this will happen only once)")
    if (gui) {
      welcome_win       <- gWidgets::gwindow(title = "Welcome", width = 400, height = 100)
      welcome_lab       <- gWidgets::glabel(text = welcome_text, container = welcome_win, editable = FALSE)
      font(welcome_lab) <- list(family = "sans", style = "italic", size = 10)
      Sys.sleep(0.05)
      message("[", date(), "]", welcome_text)
    } else {
      message("[", date(), "]", welcome_text)
    }
    gdalUtils::gdal_setInstallation(ignore.full_scan = TRUE, verbose = TRUE)
  }
  gdal_version <- package_version(gsub("^GDAL ([0-9.]*)[0-9A-Za-z/., ]*", "\\1",
                                       rgdal::getGDALVersionInfo(str = "--version")))
  # GDAL version used during the last test (for now used as minimum required version)
  gdal_minversion <- package_version("1.11.1")
  gdal_HDFsupport <- length(grep("HDF4", gdalUtils::gdalinfo(formats = TRUE))) > 0
  
  if (gdal_version < gdal_minversion) {
    stop(paste0("GDAL version must be at least ", gdal_minversion, ". Please update it."))
  }
  
  if (!gdal_HDFsupport) {
    stop("Your local GDAL installation does not support HDF4 format. Please install HDF4 support and recompile GDAL.")
  }
  
  message("GDAL version in use: ", as.character(gdal_version))
  
  # Parameter retrieval and Folder Initialization -----
  if (is.null(options_file) & gui == FALSE) {
    stop("Please provide a valid \"option_file\" path value (or run with gui=TRUE).")
  }
  
  # Folders in which the JSON/RData files (previous settings and product descriptions) are saved
  if (is.null(options_file)) {
    previous_dir <- system.file("Previous", package = "MODIStsp")
  } else {
    previous_dir <- dirname(options_file)
  }
  prodopts_dir <- system.file("Previous", package = "MODIStsp")
  dir.create(previous_dir, showWarnings = FALSE, recursive = TRUE) #; dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(prodopts_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Previous options file (or file passed by user in non-interactive mode)
  previous_jsfile <- if (is.null(options_file)) {
    file.path(previous_dir, "MODIStsp_Previous.json")
  } else {
    options_file
  }
  xml_file      <- system.file("ExtData","MODIStsp_ProdOpts.xml", package = "MODIStsp")  #XML file describing MODIS products
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
    general_opts <- list(
      sel_prod        = "Surf_Ref_8Days_500m (M*D09A1)",
      sensor          = "Terra",
      prod_version    = "6",
      start_date      = strftime(Sys.Date(), "%Y-01-01"),
      end_date        = as.character(Sys.Date()),
      bandsel         = rep(0, 13),
      indexes_bandsel = rep(0, 11),
      quality_bandsel = rep(0, 21), # lenghts refearred to "Surf_Ref_8Days_500m (M*D09A1)" v6!
      start_x         = 18,
      end_x           = 18,
      start_y         = 4,
      end_y           = 4,
      user            = "",
      password        = "",
      use_aria        = FALSE,
      download_server = "http",
      download_range  = "full",
      proj            = "Sinusoidal",
      user_proj4      = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs",
      out_res_sel     = "Native",
      out_res         = "",
      full_ext        = "Full Tiles Extent",
      resampling      = "near",
      out_format      = "ENVI",
      ts_format       = "ENVI Meta Files",
      rts             = "Yes",
      compress        = "None",
      nodata_change   = "No",
      scale_val       = "No",
      delete_hdf      = "No",
      reprocess       = "No",
      bbox            = c("", "", "", ""),
      out_folder      = "",
      out_folder_mod  = "",
      MODIStspVersion = as.character(utils::packageVersion("MODIStsp")),
      custom_indexes  = list()
    )
    write(RJSONIO::toJSON(general_opts), previous_jsfile)
  } else {
    if (is.null(general_opts$MODIStspVersion)) {
      stop(paste0("The option file in use (", previous_jsfile, ") was created with an 
                too old MODIStsp version (<=1.2.2), and can not be used with the current 
                version. Please delete it or specify a different value for the `option_file`
                parameter."))
    } else {
      if (general_opts$MODIStspVersion < utils::packageVersion("MODIStsp")) {
        warning(paste0("The option file in use (", previous_jsfile, ") was created with an 
                       old MODIStsp version (",
                       general_opts$MODIStspVersion, "): this could lead to errors!"))
      }
    }
  }
  # Restore MODIS products if existing, otherwise retrieve data from xml file ----
  if (file.exists(prodopts_file)) {
    prod_opt_list <- get(load(prodopts_file))
    if (is.null(attr(prod_opt_list, "MODIStspVersion"))) {
      reload_prodlist <- TRUE
    } else {
      # load if prod_opt_list is old
      reload_prodlist <- attr(prod_opt_list, "MODIStspVersion") < utils::packageVersion("MODIStsp")
    }
  } else {
    reload_prodlist <- TRUE
  }
  if (reload_prodlist) {
    mess_text <- "Waiting while reading the MODIS products list..."
    if (gui) {
      mess     <- gWidgets::gwindow(title = "Please wait...", width = 400, height = 40)
      mess_lab <- gWidgets::glabel(text = mess_text, editable = FALSE, container = mess)
      Sys.sleep(0.05)
      message(mess_text)
    } else {
      message(mess_text)
    }
    MODIStsp_read_xml(prodopts_file = prodopts_file, xml_file = xml_file)
    load(prodopts_file)
    if (gui) {
      gWidgets::addHandlerUnrealize(mess_lab, handler = function(h, ...) {
        return(FALSE)
      })
      gWidgets::dispose(mess_lab)
    }
  }
  
  #launch the GUI if on an interactive session (i.e., gui = T) and wait for return----
  if (gui) {
    if (exists("welcome_lab")) {
      gWidgets::dispose(welcome_lab)
    }
    Quit <- MODIStsp_GUI(general_opts    = general_opts,
                         prod_opt_list   = prod_opt_list,
                         MODIStsp_dir    = system.file(package = "MODIStsp"),
                         previous_jsfile = previous_jsfile,
                         prodopts_file   = prodopts_file,
                         scrollWindow    = scrollWindow
    )
    
  } else {
    Quit <- FALSE
  }
  start.time <- Sys.time()
  
  # Launch the processing ----
  # When GUI is closed (or in a non-interactive run): If not Quit selected, restore the
  # user selected options from previous file and launch the processing ----
  
  if (!Quit) {
    
    if (file.exists(previous_jsfile)) {
      general_opts <- RJSONIO::fromJSON(previous_jsfile)
    } else {
      message("[", date(), "] Processing Options file not found! Exiting!")
      stop()
    }
    if (file.exists(prodopts_file)) {
      prod_opt_list <- get(load(prodopts_file))
    } else {
      message("[", date(), "] Product information file not found! Exiting!")
      stop()
    }
    # retrieve options relative to the selected product from the "prod_opt_list" data frame
    prod_opts  <- prod_opt_list[[general_opts$sel_prod]][[general_opts$prod_version]]
    custom_idx <- general_opts$custom_indexes[[general_opts$sel_prod]][[general_opts$prod_version]]
    
    # Workaround to avoid error if only one custom index exists
    if (class(custom_idx) == "character") {
      custom_idx <- data.frame(indexes_bandnames = custom_idx["indexes_bandnames"],
                               indexes_fullnames  = custom_idx["indexes_fullnames"],
                               indexes_formulas   = custom_idx["indexes_formulas"],
                               indexes_nodata_out = custom_idx["indexes_nodata_out"],
                               stringsAsFactors   = FALSE)
    }
    
    # Create variables needed to launch the processing
    
    general_opts$start_date <- as.character(format(as.Date(general_opts$start_date), "%Y.%m.%d"))
    general_opts$end_date   <- as.character(format(as.Date(general_opts$end_date),   "%Y.%m.%d"))
    
    # If the product is NOT tiled, change or_proj to WGS84 and or_res from metres to degrees
    if (prod_opts$tiled == 0) {
      MOD_proj_str         <- "+init=epsg:4008 +proj=longlat +ellps=clrk66 +no_defs"
      prod_opts$native_res <- format(as.numeric(prod_opts$native_res) * (0.05 / 5600))
    }
    # get native resolution if out_res empty (Probably obsolete...)
    if (general_opts$out_res == "" | general_opts$out_res_sel == "Native") {
      general_opts$out_res <- prod_opts$native_res
    }
    
    # Changes to perform in the case spatial_file_path is defined
    if (!is.null(spatial_file_path)) {
      
      # Check if the input file is a valid spatial file and redefine the bounding box
      external_bbox <- try(bbox_from_file(file_path = spatial_file_path,
                                          out_crs   = general_opts$user_proj4),
                           silent = TRUE)
      if (class(external_bbox) == "try-error") {
        stop(external_bbox)
      }
      general_opts$bbox <- external_bbox
      
      # Redefine the out_folder including the file name as subfolder
      # (this to avoid that, running in a cycle, files are overwritten every time)
      general_opts$out_folder <- file.path(general_opts$out_folder,
                                           tools::file_path_sans_ext(basename(spatial_file_path)))
      
      # # If out_folder already exists, create a new one with a suffix
      # if (file.exists(general_opts$out_folder)) {
      #   tmp_counter   <- 1
      #   out_newfolder <- paste(general_opts$out_folder, tmp_counter, sep = "_")
      #   while (file.exists(out_newfolder)) {
      #     tmp_counter   <- tmp_counter + 1
      #     out_newfolder <- paste(general_opts$out_folder, tmp_counter, sep = "_")
      #   }
      #   general_opts$out_folder <- out_newfolder
      # }
      
      # Overwrite the full_ext option (avoids that , if the options_file specifies a full processing,
      # the incorrect parameter is passed)
      general_opts$full_ext <- "Resized"
      
      # Automatically retrieve the tiles requested to cover the extent
      modis_grid           <- get(load(system.file("ExtData","MODIS_Tiles.RData", package = "MODIStsp")))
      external_bbox_mod    <- reproj_bbox(external_bbox, general_opts$user_proj4, MOD_proj_str, enlarge = TRUE)
      d_bbox_mod_tiled     <- raster::crop(modis_grid, raster::extent(external_bbox_mod))
      general_opts$start_x <- min(d_bbox_mod_tiled$H)
      general_opts$end_x   <- max(d_bbox_mod_tiled$H)
      general_opts$start_y <- min(d_bbox_mod_tiled$V)
      general_opts$end_y   <- max(d_bbox_mod_tiled$V)
      
    }
    
    # if out_folder[_mod] is set to "$tempdir" or "$modispath", define the path (used in test mode)
    for (sel_out_folder in c("out_folder","out_folder_mod")) {
      if (general_opts[[sel_out_folder]] == "$tempdir") {
        general_opts[[sel_out_folder]] <- tempdir()
      }
      if (general_opts[[sel_out_folder]] == "$modistest") {
        general_opts[[sel_out_folder]] <- system.file("Test_files", package="MODIStsp")
      }
    }
    
    # launch MODIStsp_process to Download and preprocess the selected images ----
    MODIStsp_process(
      sel_prod           = general_opts$sel_prod,
      start_date         = general_opts$start_date,
      end_date           = general_opts$end_date,
      out_folder         = general_opts$out_folder,
      out_folder_mod     = general_opts$out_folder_mod,
      reprocess          = general_opts$reprocess,
      delete_hdf         = general_opts$delete_hdf,
      sensor             = general_opts$sensor,
      download_server    = general_opts$download_server,
      user               = if (exists("direct_username")) {direct_username} else {general_opts$user},
      password           = if (exists("direct_password")) {direct_password} else {general_opts$password},
      https              = prod_opts$http,
      ftps               = prod_opts$ftp,
      start_x            = general_opts$start_x,
      start_y            = general_opts$start_y,
      end_x              = general_opts$end_x,
      end_y              = general_opts$end_y,
      full_ext           = general_opts$full_ext,
      bbox               = general_opts$bbox,
      out_format         = general_opts$out_format,
      out_res_sel        = general_opts$out_res_sel,
      out_res            = as.numeric(general_opts$out_res),
      native_res         = prod_opts$native_res,
      tiled              = prod_opts$tiled,
      resampling         = general_opts$resampling,
      ts_format          = general_opts$ts_format,
      compress           = general_opts$compress,
      MOD_proj_str       = MOD_proj_str,
      outproj_str        = general_opts$user_proj4,
      nodata_in          = prod_opts$nodata_in,
      nodata_out         = prod_opts$nodata_out,
      rts                = general_opts$rts,
      nodata_change      = general_opts$nodata_change,
      scale_val          = general_opts$scale_val,
      scale_factor       = prod_opts$scale_factor,
      offset             = prod_opts$offset,
      datatype           = prod_opts$datatype,
      bandsel            = general_opts$bandsel,
      bandnames          = prod_opts$bandnames,
      indexes_bandsel    = c(general_opts$indexes_bandsel),
      indexes_bandnames  = c(prod_opts$indexes_bandnames,  custom_idx$indexes_bandnames),
      indexes_formula    = c(prod_opts$indexes_formula,    custom_idx$indexes_formulas),
      indexes_nodata_out = c(prod_opts$indexes_nodata_out, custom_idx$indexes_nodata_out),
      quality_bandnames  = prod_opts$quality_bandnames,
      quality_bandsel    = general_opts$quality_bandsel,
      quality_bitN       = prod_opts$quality_bitN,
      quality_source     = prod_opts$quality_source,
      quality_nodata_in  = prod_opts$quality_nodata_in,
      quality_nodata_out = prod_opts$quality_nodata_out,
      file_prefixes      = prod_opts$file_prefix,
      main_out_folder    = prod_opts$main_out_folder,
      gui                = gui,
      use_aria           = general_opts$use_aria,
      download_range     = general_opts$download_range
    )
    
    # At end of succesfull execution, save the options used in the main output folder
    optfilename  <- file.path(general_opts$out_folder, paste0("MODIStsp_", Sys.Date(), ".json"))
    general_opts <- RJSONIO::fromJSON(previous_jsfile)
    write(RJSONIO::toJSON(general_opts), optfilename)
    
    # If running in test mode, check the output created
    if (test>0) {
      MODIStest_check_md5(test = test)
    }
    
    # Clean up at end of processing ----
    end.time   <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)
    gc()
  } else {
    # End If on "Quit" --> If "Quit" above is skipped and program terminates
    message("[", date(), "] ", " You Selected to Quit! Goodbye!")
  }
  
  # on.exit(raster::rasterOptions(setfileext = prevopt))
}
