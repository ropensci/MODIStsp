#' MODIStsp
#' @description Main function for the MODIS Time Series Processing Tool (MODIStsp)
#' @details The function is used to initialize the processing (folder names, packages, etc.), to launch the GUI (MODIStsp_GUI) and receive its outputs,
#'  and to launch the required routines for downloading and processing the requested datasets.
#' @param gui logical parameter (TRUE: the GUI is opened before processing; FALSE: the saved parameters are retrieved from "options_file")
#' @param options_file settings (optional): full path of the RData file containing the processing options (default: Previous.RData in subdir Previous);
#' @param spatial_file_path (optional): full path of a spatial file to use as extent (default=NULL): if defined, the processing options which define the
#'  extent, the selected tiles and the "Full Tile / Resized" options are not considered; instead, new files are created on the extent of the provided
#'  spatial file.
#' @param MODIStsp_dir (optional): main folder containing MODIStsp R files (used only to launche MODSItsp from outside the package using MODIStsp_std.R)
#' @return NULL
#'
#' @author Lorenzo Busetto, phD (2014-2015) \email{busetto.l@@irea.cnr.it}
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
#' @importFrom hash hash
#' @importFrom gdalUtils gdal_setInstallation gdalinfo
#' @importFrom rgdal getGDALVersionInfo
#' @importFrom raster extent rasterOptions
#' @importFrom tools file_path_sans_ext
#' @importFrom utils memory.limit
#' @examples
#' # Running the tool without any option will start the GUI with the default or last used settings
#' \dontrun{
#' MODIStsp()}
#'
#' # Run the tool using the settings previously saved in a specific option file
#' \dontrun{
#' MODIStsp(gui = FALSE, options_File = "X:/yourpath/youroptions.RData")}
#'
#' # Run the tool using a previously saved options file,
#' # but editing it with the GUI before starting the processing
#' \dontrun{
#' MODIStsp(options_File = "X:/yourpath/youroptions.RData")}
#'
#' # Run the tool using the settings previously saved in a specific option file
#' # and specifying the extent from a spatial file
#' \dontrun{
#' MODIStsp(gui = FALSE, options_File = "X:/yourpath/youroptions.RData",
#'   spatial_file_path = "X:/yourpath/yourspatialfile.shp" )}
#'
#' # Run the tool in a batch mode, using the settings previously saved in a specific
#' # option file and specifying each time the extent from a different spatial file
#' \dontrun{
#' extent_list = list.files("X:/path/containing/some/shapefiles/", "\\.shp$")
#' for (single_shape in extent_list)
#'   MODIStsp(gui = FALSE, options_File = "X:/yourpath/youroptions.RData",
#'     spatial_file_path = single_shape )}

MODIStsp <- function(gui=TRUE, options_file=NULL, spatial_file_path=NULL, MODIStsp_dir=NA) {

  if (is.na(MODIStsp_dir)) {
    MODIStsp_dir <- system.file(package = "MODIStsp")
  }
  options("guiToolkit" = "RGtk2")
  #- ------------------------------------------------------------------------------- -#
  #  Initialize project
  #- ------------------------------------------------------------------------------- -#
  
  # On interactive execution, load Rgtk2
  if (gui) {
    requireNamespace("gWidgetsRGtk2")
    options("guiToolkit" = "RGtk2")
  }
  
  # Check GDAL version
  if (is.null(getOption("gdalUtils_gdalPath"))) {
    # gdal_setInstallation(ignore.full_scan = FALSE)
    welcome_text <- "Welcome to MODIStsp!\n\nWe will now search for a valid GDAL installation - please wait\n(this will happen only once)"
    if (gui) {
      welcome_win <- gwindow(title = "Welcome", container = TRUE, width = 400, height = 100)
      welcome_lab <- glabel(welcome_text, editable = FALSE, container = welcome_win)
      # mess <- gmessage(title = "Welcome", welcome_text, do.buttons = FALSE)
    } else {
      message("[",date(),"]",welcome_text)
    }
    gdal_setInstallation(ignore.full_scan = TRUE, verbose = TRUE)
  }
  gdal_version <- package_version(gsub("^GDAL ([0-9.]*)[0-9A-Za-z/., ]*","\\1", getGDALVersionInfo(str = "--version")))
  gdal_minversion <- package_version("1.11.1") # GDAL version used during the last test (for now used as minimum required version)
  gdal_HDFsupport <- length(grep("HDF4", gdalinfo(formats = TRUE))) > 0

  if (gdal_version < gdal_minversion) {
    stop(paste0("GDAL version must be at least ",gdal_minversion,". Please update it."))
  }

  if (!gdal_HDFsupport) {
    stop("Your local GDAL installation does not support HDF4 format. Please install HDF4 support and recompile GDAL.")
  }

  cat("GDAL version in use:",as.character(gdal_version),"\n")

  # Increase memory limit on winzozz
  if (Sys.info()["sysname"] == "Windows") {
    memory.limit(8000)
  }							# Increase maximum allocsable memory
  rasterOptions(setfileext = FALSE)				# Make so that "raster" functions doesn't automatically add extensions on output files

  # Parameter retrieval and Folder Initialization -----
  if (is.null(options_file) & gui == FALSE) {
    stop("Please provide a valid \"option_file\" path value (or run with gui=TRUE).")
  }

  # Folder in which the previous options file is saved
  if (is.null(options_file)) {
    previous_dir <- file.path(MODIStsp_dir,"Previous")
  } else {
    previous_dir <- dirname(options_file)
  }
  dir.create(previous_dir, showWarnings = FALSE, recursive = TRUE) #; dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)

  # Previous options file (or file passed by user in non-interactive mode)
  previous_file <- if (is.null(options_file)) {
    file.path(previous_dir, "MODIStsp_Previous.RData")
  } else {
    options_file
  }
  xml_file <- file.path(MODIStsp_dir,"ExtData","MODIStsp_ProdOpts.xml")  #XML file describing MODIS products

  #- ------------------------------------------------------------------------------- -#
  #  Set general processing options - used at first execution to initialize GUI
  #- ------------------------------------------------------------------------------- -#
  out_proj_names <- c("Sinusoidal","UTM 32N","Latlon WGS84","User Defined" )
  out_proj_list <- hash("Sinusoidal" = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs ",
                        "UTM 32N" = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                        "Latlon WGS84" = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                        "User Defined" = "")
  MOD_proj_str <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs "

  # Create the general_opts structure used to communicate with the GUI and set default values
  general_opts <- list(MODIStsp_dir = MODIStsp_dir, previous_file = previous_file,xml_file = xml_file, out_proj_list = out_proj_list, out_proj_names = out_proj_names, MOD_proj_str = MOD_proj_str,
                       sel_prod = "Surf_Ref_8Days_500m (M*D09A1)",sensor = "Terra",start_date = "2015-01-01",end_date = "2015-01-01",
                       start_x = 18, end_x = 18, start_y = 4, end_y = 4,
                       proj = "Sinusoidal",out_res_sel = "Native", out_res = "",full_ext = "Full Tiles Extent", resampling = "near",out_format = "ENVI",ts_format = "ENVI Meta Files", rts = "Yes",compress = "None",
                       nodata_change = "No",delete_hdf = "No",reprocess = "No", bbox = c("","","",""), out_folder = "", out_folder_mod = "")
  attr(general_opts,"GeneratedBy") <- "MODIStsp"

  #launch the GUI if on an interactive session (i.e., gui = T) and wait for return----
  if (gui) {
    if (exists("welcome_lab")) {dispose(welcome_lab)}
    MODIStsp_GUI(general_opts)
  } else {
    Quit <<- FALSE
  }
  start.time <- Sys.time()

  # Launch the processing ----
  # When GUI is closed (or in a non-interactive run): If not Quit selected, restore the user selected options from previous file and launch the processing ----
  if (!Quit) {

    if (file.exists(general_opts$previous_file)) {
      load(general_opts$previous_file)
    } else {
      cat("[",date(),"] Processing Options file not found ! Exiting !\n"); stop()
    }
    prod_opts <- prod_opt_list[[general_opts$sel_prod]]  # retrieve options relative to the selected product from the "prod_opt_list" data frame

    # Create variables needed to launch the processing
    # start_date <- paste(general_opts$start_year, general_opts$start_month, general_opts$start_day, sep = ".")
    # end_date <- paste(general_opts$end_year, general_opts$end_month, general_opts$end_day, sep = ".")
    #
    general_opts$start_date <- as.character(format(as.Date(general_opts$start_date), "%Y.%m.%d"))
    general_opts$end_date <- as.character(format(as.Date(general_opts$end_date), "%Y.%m.%d"))

    # If the product is NOT tiled, change or_proj to WGS84 and or_res to 0.05 deg
    if (prod_opts$tiled == 0) {
      general_opts$MOD_proj_str <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
      prod_opts$native_res <- "0.05"
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
        tmp_counter <- 1
        out_newfolder <- paste(general_opts$out_folder,tmp_counter,sep = "_")
        while (file.exists(out_newfolder)) {
          tmp_counter <- tmp_counter + 1
          out_newfolder <- paste(general_opts$out_folder,tmp_counter,sep = "_")
        }
        general_opts$out_folder <- out_newfolder
      }

      # Overwrite the full_ext option (avoids that , if the options_file specifies a full processing,
      # the incorrect parameter is passed)
      general_opts$full_ext <- "Resized"

      # Automatically retrieve the tiles requested to cover the extent
      load(file.path(MODIStsp_dir, "ExtData/MODIS_Tiles.RData"))
      external_bbox_mod <- reproj_bbox(external_bbox, general_opts$user_proj4, general_opts$MOD_proj_str, enlarge = TRUE)
      d_bbox_mod_tiled <- intersect(modis_grid, extent(external_bbox_mod))
      general_opts$start_x <- min(d_bbox_mod_tiled$H)
      general_opts$end_x <- max(d_bbox_mod_tiled$H)
      general_opts$start_y <- min(d_bbox_mod_tiled$V)
      general_opts$end_y <- max(d_bbox_mod_tiled$V)

    }

    # launch MODIStsp_process to Download and preprocess the selected images ----
    output <- with(general_opts, MODIStsp_process(sel_prod = sel_prod, start_date = start_date,end_date = end_date,
                                                  out_folder = out_folder, out_folder_mod = out_folder_mod, reprocess = reprocess,
                                                  delete_hdf = delete_hdf, sensor = sensor, https = prod_opts$http, ftps = prod_opts$ftp,
                                                  start_x = start_x,start_y = start_y, end_x = end_x, end_y = end_y,
                                                  full_ext = full_ext, bbox = bbox, out_format = out_format, out_res_sel = out_res_sel,
                                                  out_res = as.numeric(out_res), native_res = prod_opts$native_res,  tiled = prod_opts$tiled,
                                                  resampling = resampling, ts_format = ts_format, compress = compress,
                                                  MOD_proj_str = MOD_proj_str,outproj_str = user_proj4,
                                                  nodata_in = prod_opts$nodata_in, nodata_out = prod_opts$nodata_out,rts = rts, nodata_change = nodata_change,
                                                  datatype = prod_opts$datatype,	bandsel = prod_opts$bandsel, bandnames = prod_opts$bandnames,
                                                  indexes_bandsel = prod_opts$indexes_bandsel, indexes_bandnames = prod_opts$indexes_bandnames,
                                                  indexes_formula = prod_opts$indexes_formula, indexes_nodata_out = prod_opts$indexes_nodata_out,
                                                  quality_bandnames = prod_opts$quality_bandnames,quality_bandsel = prod_opts$quality_bandsel, quality_bitN = prod_opts$quality_bitN,
                                                  quality_source = prod_opts$quality_source, quality_nodata_in = prod_opts$quality_nodata_in,
                                                  quality_nodata_out = prod_opts$quality_nodata_out,
                                                  file_prefixes = prod_opts$file_prefix, main_out_folder = prod_opts$main_out_folder, gui = gui))

    # At end of succesfull execution, save he options used in the main output folder
    load(previous_file)
    optfilename = file.path(general_opts$out_folder,paste0('MODIStsp_', Sys.Date(),'.RData'))
    save(general_opts,prod_opt_list,mod_prod_list, file = optfilename)
  } # End If on "Quit" --> If "Quit" above is skipped and program terminates

  # Clean up at end of processing ----
  if (exists("Quit")) {
    rm(Quit, envir = globalenv())
  }    # Remove Quit if defined
  # End of processing
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
}
