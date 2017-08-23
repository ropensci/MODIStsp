#' @title Main MODIStsp function
#' @description Main function for the MODIS Time Series Processing Tool
#'   (MODIStsp)
#' @details The function is used to:
#'  - initialize the processing (folder names, packages, etc.);
#'  - launch the GUI ([MODIStsp_GUI()]) and receive its outputs on interactive
#'    execution;
#'  - load the options file on non-interactive execution;
#'  - launch the routines for downloading and processing the requested datasets.
#'    ([MODIStsp_process()])
#' @param gui `logical` if TRUE: the GUI is opened before processing. If FALSE:
#'  processing parameters are retrieved from the provided `options_file`
#'  argument), Default: TRUE
#' @param options_file `character` full path to a JSON file
#'  containing MODIStsp processing options saved from the GUI. If NULL,
#'  parameters of the last succesfull run are retrieved from file
#'  "MODIStsp_Previous.json" in subdir Previous), Default: NULL
#' @param spatial_file_path `character` (optional) full path of a spatial file
#'  to use to derive te processing extent. If not NULL, the processing options
#'  which define the extent, the selected tiles and the "Full Tile / Resized"
#'  in the JSON options file are overwritten and new files are created on the
#'  extent of the provided spatial file, Default: NULL
#' @param scroll_window `logical` if TRUE, the GUI window is opened
#'  fullscreen with scrollbars (this is useful on devices with small displays).
#'  If using a device with a display resolution >= 1024x768, leaving this
#'  parameter to FALSE is suggested, Default: FALSE
#' @param test `integer` if set, MODIStsp is executed in "test mode", using a
#'  preset Options File instead than opening the GUI or accepting the
#'  `options_file` parameter. This allows both to check correct installation on
#'  user's machines, and to implement unit testing. The number indicates which
#'  tests to execute (six are available). If == 0, the test is selected
#'  randomly. If == -1, MODIStsp is executed in normal mode. Default: -1
#'
#' @return NULL
#'
#' @author Lorenzo Busetto, phD (2014-2015) \email{busetto.l@@irea.cnr.it}
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
#' @seealso
#'  [MODIStsp_GUI()], [MODIStsp_process()]
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
#' # Running the tool without any option will start the GUI with the default or
#' # last used settings
#' \dontrun{
#' MODIStsp()}
#'
#' # Run the tool using the settings previously saved in a specific options file
#' \dontrun{
#' MODIStsp(gui = FALSE, options_file = "X:/yourpath/youroptions.json")}
#'
#' # Run the tool using the settings previously saved in a specific option file
#' # and specifying the extent from a spatial file
#' \dontrun{
#' MODIStsp(gui = FALSE, options_file = "X:/yourpath/youroptions.json",
#'   spatial_file_path = "X:/yourpath/yourspatialfile.shp" )}
#'
#' # Run the tool in batch mode, using the settings previously saved in a
#' # specific options file and specifying each time the extent from a different
#' # spatial file (e.g., to perform the same processing on different extents)
#'
#' \dontrun{
#' extent_list = list.files("X:/path/containing/some/shapefiles/", "\\.shp$")
#' for (single_shape in extent_list)
#'   MODIStsp(gui = FALSE, options_file = "X:/yourpath/youroptions.json",
#'     spatial_file_path = single_shape )}

MODIStsp <- function(gui               = TRUE,
                     options_file      = NULL,
                     spatial_file_path = NULL,
                     scroll_window     = FALSE,
                     test              = -1) {
  
  options("guiToolkit" = "RGtk2")
  
  # Make so that "raster" functions doesn't automatically add extensions on
  # output files. This is automatically reset to TRUE at the end of the session
  raster::rasterOptions(setfileext = FALSE)
  
  #   __________________________________________________________________________
  #   Initialize processing                                                 ####
  
  #   __________________________________________________________________________
  #   If test mode is selected, select the options file for testing         ####
  #   and set other parameters
  if (test > -1) {
    
    gui <- FALSE
    message("MODIStsp is running in test mode.")
    # read names of available json test files
    test_files <- sort(list.files(
      path       = system.file("Test_files", package = "MODIStsp"),
      pattern    = "^test[0-9]{2}\\.json$",
      full.names = TRUE))
    # if test=0, select the test randomly
    if (test == 0) {
      test <- sample(seq_along(test_files), 1)
      message(paste0("Randomly selected test ", test, "."))
    } else {
      if (test > length(test_files)) {
        stop(paste0("Value of argument 'test' is too high: only ",
                    length(test_files), " test Option Files are available."))
      }
    }
    # check that the offline HDF files were unzipped - unzip them if not
    tests_hdf_zipped <- list.files(
      path       = system.file("Test_files", package = "MODIStsp"),
      pattern    = "\\.hdf\\.zip$",
      full.names = TRUE
    )
    for (test_hdf in gsub("\\.zip$", "", tests_hdf_zipped)) {
      if (!file.exists(test_hdf)) {
        unzip(zipfile = paste0(test_hdf, ".zip"),
              files   = basename(test_hdf),
              exdir   = dirname(test_hdf),
              unzip   = "internal")
      }
    }
    
    # Assign the selected test Option File
    options_file <- test_files[test]
    
    # If a test with http download was selected, ask credentials.
    if (test %in% c(4, 5)) {
      direct_username <- readline(prompt = "Enter your USGS username: ")
      direct_password <- readline(prompt = "Enter your password: ")
    }
  }
  
  
  #   __________________________________________________________________________
  #   On interactive execution, ensure that gWidgetsRGtk2 is avauilable     ####
  
  if (gui) {
    if (!pacman::p_exists("gWidgetsRGtk2", local = TRUE)) {
      
      message(strwrap("Library 'gWidgetsRGtk2' is not installed. It is required
                      to run MODIStsp!\n\n", "Do you want to install it now?"),
              type = " y / n")
      inst_gw <- readline()
      if (inst_gw == "y") {
        pacman::p_load("gWidgetsRGtk2")
      } else {
        stop(strwrap("MODIStsp can not work in Interactive mode without
                     gWidgetsRGtk2! Aborting!"))
      }
      
    }
    options("guiToolkit" = "RGtk2")
  }
  
  # Check GDAL version ----
  if (is.null(getOption("gdalUtils_gdalPath"))) {
    
    welcome_text <- strwrap("Welcome to MODIStsp!\n\nWe will now search for a
                            valid GDAL installation - please wait! (this will
                            happen only once)", width = 60)
    if (gui) {
      welcome_win       <- gWidgets::gwindow(title  = "Welcome", width = 400,
                                             height = 100)
      
      welcome_lab       <- gWidgets::glabel(text      = welcome_text,
                                            container = welcome_win,
                                            editable  = FALSE)
      font(welcome_lab) <- list(family = "sans", style = "italic", size = 10)
      Sys.sleep(0.05)
      message("[", date(), "]", welcome_text)
    } else {
      message("[", date(), "]", welcome_text)
    }
    gdalUtils::gdal_setInstallation(ignore.full_scan = TRUE, verbose = TRUE)
  }
  gdal_version <- package_version(
    gsub("^GDAL ([0-9.]*)[0-9A-Za-z/., ]*", "\\1",
         rgdal::getGDALVersionInfo(str = "--version"))
  )
  # GDAL version used during the last test (for now used as minimum required
  # version)
  gdal_minversion  <- package_version("1.11.1")
  gdal_hdf_support <- length(grep("HDF4",
                                  gdalUtils::gdalinfo(formats = TRUE))) > 0
  
  if (gdal_version < gdal_minversion) {
    stop(paste0("GDAL version must be at least ",
                gdal_minversion,
                ". Please update it."))
  }
  
  if (!gdal_hdf_support) {
    stop("Your local GDAL installation does not support HDF4 format.\n",  
      "Please install HDF4 support and recompile GDAL. See:\n",
      strwrap("http://lbusett.github.io/MODIStsp/articles/installation.html#
      installing-r-and-gdal", width = 200))
  }
  
  message("GDAL version in use: ", as.character(gdal_version))
  
  # Parameter retrieval and Folder Initialization -----
  if (is.null(options_file) & gui == FALSE) {
    stop("Please provide a valid \"options_file\" path value (or run ", 
         "with gui=TRUE).")
  }
  
  # Folders in which the JSON/RData files (previous settings and product
  # descriptions) are saved
  if (is.null(options_file)) {
    previous_dir <- system.file("Previous", package = "MODIStsp")
  } else {
    previous_dir <- dirname(options_file)
  }
  prodopts_dir <- system.file("Previous", package = "MODIStsp")
  dir.create(previous_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(prodopts_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Previous options file (or file passed by user in non-interactive mode)
  previous_jsfile <- if (is.null(options_file)) {
    file.path(previous_dir, "MODIStsp_Previous.json")
  } else {
    options_file
  }
  #XML file describing MODIS products
  xml_file <- system.file("ExtData", "MODIStsp_ProdOpts.xml",
                          package = "MODIStsp")
  
  # RData file created to speed up options reading (done only the first time)
  prodopts_file <- file.path(prodopts_dir, "MODIStsp_ProdOpts.RData")
  
  #   __________________________________________________________________________
  #   Set general processing options - used at first itneractive execution  ####
  #   to initialize the GUI
  
  # default projection string for MODIS gridded data
  mod_proj_str <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs" #nolint
  
  # Load options if existing, otherwise initialise them ----
  if (file.exists(previous_jsfile)) {
    general_opts <- RJSONIO::fromJSON(previous_jsfile)
  }
  
  # Create the general_opts structure used to communicate with the GUI and
  # set default values and save it as a JSON file (if needed - a.k.a. at
  # first execution)
  if (!exists("general_opts")) {
    general_opts <- list(
      sel_prod        = "Surf_Ref_8Days_500m (M*D09A1)",
      sensor          = "Terra",
      prod_version    = "6",
      start_date      = strftime(Sys.Date(), "%Y-01-01"),
      end_date        = as.character(Sys.Date()),
      # lenghts refearred to "Surf_Ref_8Days_500m (M*D09A1)" v6! (used as 
      # default product at first execution)
      bandsel         = rep(0, 13),
      indexes_bandsel = rep(0, 11),
      quality_bandsel = rep(0, 21),
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
      user_proj4      = mod_proj_str,
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
      stop(paste(
        "The option file in use (",
        previous_jsfile,
        strwrap(") was created with a old MODIStsp version (<=1.2.2), and can
                not be used with the current version. Please delete it or
                specify a different value for the `options_file` argument.")))
    } else {
      if (general_opts$MODIStspVersion < utils::packageVersion("MODIStsp")) {
        warning(paste0("The option file in use (",
                       previous_jsfile,
                       ") was created with an old MODIStsp version (",
                       general_opts$MODIStspVersion,
                       "): this could lead to errors!"))
      }
    }
  }
  
  #   __________________________________________________________________________
  #   Load options for MODIS files from the prodopt_file or (at first       ####
  #   execution) from the XML options file
  
  if (file.exists(prodopts_file)) {
    prod_opt_list <- get(load(prodopts_file))
    if (is.null(attr(prod_opt_list, "MODIStspVersion"))) {
      reload_prodlist <- TRUE
    } else {
      # load if prod_opt_list is old
      reload_prodlist <-
        attr(prod_opt_list,
             "MODIStspVersion") < utils::packageVersion("MODIStsp")
    }
  } else {
    reload_prodlist <- TRUE
  }
  if (reload_prodlist) {
    mess_text <- "Waiting while reading the MODIS products list..."
    if (gui) {
      mess     <- gWidgets::gwindow(title  = "Please wait...",
                                    width  = 400,
                                    height = 40)
      
      mess_lab <- gWidgets::glabel(text      = mess_text,
                                   editable  = FALSE,
                                   container = mess)
      Sys.sleep(0.05)
      message(mess_text)
    } else {
      message(mess_text)
    }
    MODIStsp_read_xml(prodopts_file = prodopts_file,
                      xml_file      = xml_file)
    load(prodopts_file)
    if (gui) {
      gWidgets::addHandlerUnrealize(mess_lab, handler = function(h, ...) {
        return(FALSE)
      })
      gWidgets::dispose(mess_lab)
    }
  }
  
  #   __________________________________________________________________________
  #   On interactive execution, launch the GUI and wait for user selection  ####
  #   otherwise, immediately start processing using `options_file` to load
  #   processing parameters
  
  if (gui) {
    if (exists("welcome_lab")) {
      gWidgets::dispose(welcome_lab)
    }
    quit <- MODIStsp_GUI(general_opts,
                         prod_opt_list,
                         MODIStsp_dir = system.file(package = "MODIStsp"),
                         previous_jsfile,
                         prodopts_file,
                         scroll_window)
    
  } else {
    quit <- FALSE
  }
  start_time <- Sys.time()
  
  if (!quit) {
    
    if (file.exists(previous_jsfile)) {
      general_opts <- RJSONIO::fromJSON(previous_jsfile)
    } else {
      message("[", date(), "] Processing Options file not found! Aborting!")
      stop()
    }
    if (file.exists(prodopts_file)) {
      prod_opt_list <- get(load(prodopts_file))
    } else {
      message("[", date(), "] Product information file not found! Aborting!")
      stop()
    }
    # retrieve options relative to the selected product and version from the
    # "prod_opt_list" data frame
    
    sel_prod   <- general_opts$sel_prod
    sel_ver    <- general_opts$prod_version
    prod_opts  <- prod_opt_list[[sel_prod]][[sel_ver]]
    custom_idx <- general_opts$custom_indexes[[sel_prod]][[sel_ver]]
    
    # Workaround to avoid error if only one custom index exists
    if (class(custom_idx) == "character") {
      custom_idx <- data.frame(
        indexes_bandnames  = custom_idx["indexes_bandnames"],
        indexes_fullnames  = custom_idx["indexes_fullnames"],
        indexes_formulas   = custom_idx["indexes_formulas"],
        indexes_nodata_out = custom_idx["indexes_nodata_out"],
        stringsAsFactors   = FALSE
      )
    }
    
    # Create variables needed to launch the processing
    
    general_opts$start_date <- as.character(
      format(as.Date(general_opts$start_date), "%Y.%m.%d")
    )
    general_opts$end_date  <- as.character(
      format(as.Date(general_opts$end_date), "%Y.%m.%d")
    )
    
    # If the product is NOT tiled, change or_proj to WGS84 and or_res from
    #  metres to degrees
    if (prod_opts$tiled == 0) {
      mod_proj_str <- "+init=epsg:4008 +proj=longlat +ellps=clrk66 +no_defs"
      prod_opts$native_res <- format(
        as.numeric(prod_opts$native_res) * (0.05 / 5600)
      )
    }
    # get native resolution if out_res empty (Probably obsolete...)
    if (general_opts$out_res == "" | general_opts$out_res_sel == "Native") {
      general_opts$out_res <- prod_opts$native_res
    }
    
    #   ________________________________________________________________________
    #   Modify processing options if `spatial_file_path` is passed          ####
    
    if (!is.null(spatial_file_path)) {
      
      # Check if the input file is a valid spatial file and redefine the
      # bounding box
      external_bbox <- try(bbox_from_file(file_path = spatial_file_path,
                                          out_crs   = general_opts$user_proj4),
                           silent = TRUE)
      if (class(external_bbox) == "try-error") {
        stop("Failed retrieving processing extent from ",
             spatial_file_path,
             " . Please check your inputs! Aborting. "
        )
      }
      general_opts$bbox <- external_bbox
      
      # Redefine the out_folder including the file name as subfolder
      # (this to avoid that, running in a cycle, files are overwritten
      # every time)
      general_opts$out_folder <- file.path(
        general_opts$out_folder,
        tools::file_path_sans_ext(basename(spatial_file_path))
      )
      
      # Overwrite the full_ext option (avoids that, if the options_file
      # specifies a full processing, the incorrect parameter is passed)
      general_opts$full_ext <- "Resized"
      
      # Automatically retrieve the tiles required to cover the extent
      modis_grid  <- get(load(system.file("ExtData", "MODIS_Tiles.RData",
                                          package = "MODIStsp")))
      external_bbox_mod    <- reproj_bbox(external_bbox,
                                          general_opts$user_proj4,
                                          mod_proj_str,
                                          enlarge = TRUE)
      d_bbox_mod_tiled     <- raster::crop(modis_grid,
                                           raster::extent(external_bbox_mod))
      general_opts$start_x <- min(d_bbox_mod_tiled$H)
      general_opts$end_x   <- max(d_bbox_mod_tiled$H)
      general_opts$start_y <- min(d_bbox_mod_tiled$V)
      general_opts$end_y   <- max(d_bbox_mod_tiled$V)
      
    }
    
    # if out_folder[_mod] is set to "$tempdir" or "$modispath", it means that
    # we are running in test mode. In that case, redefine the output folders
    # to use `R` temporary folder
    
    if (test != -1) {
      general_opts$out_folder     <- normalizePath(tempdir())
      general_opts$out_folder_mod <- system.file("Test_files",
                                                 package = "MODIStsp")
      if (exists("direct_username")) {
        general_opts$user <- direct_username
      }
      if (exists("direct_password")) {
        general_opts$password <- direct_password
      }
    }
    
    #   ________________________________________________________________________
    #   launch MODIStsp_process to Download and preprocess the selected     ####
    #   images. To do so, retrieve all processing parameters from either
    #   gemeral_opts (processing options), or prod_opts (characteristics of
    #   the selected product - band names, available indexes, etcetera.)
    
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
      user               = general_opts$user,
      password           = general_opts$password,
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
      MOD_proj_str       = mod_proj_str,
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
      indexes_bandnames  = c(prod_opts$indexes_bandnames,
                             custom_idx$indexes_bandnames),
      indexes_formula    = c(prod_opts$indexes_formula,
                             custom_idx$indexes_formulas),
      indexes_nodata_out = c(prod_opts$indexes_nodata_out,
                             custom_idx$indexes_nodata_out),
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
    
    # At end of succesfull execution, save the options used in the main output
    # folder as a JSON file with name containing the date of processing.
    # Also update "MODIStsp_previous.json.
    optfilename  <- file.path(general_opts$out_folder,
                              paste0("MODIStsp_", Sys.Date(), ".json"))
    general_opts <- RJSONIO::fromJSON(previous_jsfile)
    write(RJSONIO::toJSON(general_opts), optfilename)
    
    # If running in test mode, check the outputs created against MD5 of
    # "correct" outputs to verify that all went well
    # if (test != -1) {
    #   MODIStest_check_md5(test = test)
    # }
    
    # Clean up at end of processing ----
    end_time   <- Sys.time()
    time_taken <- end_time - start_time
    message(time_taken)
  } else {
    # If "quit" passed from the GUI, all of the above is skipped and program
    # terminates
    message("[", date(), "] ", " You Selected to quit! Goodbye!")
  }
}
