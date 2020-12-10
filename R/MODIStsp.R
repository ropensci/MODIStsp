#' @title MODIStsp main function
#' @description Main function for the MODIS Time Series Processing Tool
#'   (MODIStsp)
#' @details The function is used to:
#'  - initialize the processing (folder names, packages, etc.);
#'  - launch the GUI ([MODIStsp_GUI()]) on interactive
#'    execution, or load an options file to set processing arguments and/or
#'    retrieve CLI inputs and run processing on non-interactive execution;
#'  - launch the routines for downloading and processing the requested datasets.
#'    ([MODIStsp_process()])
#'  - launching the function with GUI = FALSE and without specifying a opts_file
#'    initializes arguments with default values. This allows making a test run.
#' @param gui `logical` if TRUE: the GUI is opened before processing. If FALSE:
#'  processing parameters are retrieved from the provided `opts_file`
#'  argument), Default: TRUE
#' @param out_folder `character` Main output folder, default: NULL.
#' @param out_folder_mod `character` Output folder for original HDF storage.
#'  If `"$tempdir"` (default), a temporary directory is used.
#' @param opts_file `character` full path to a JSON file
#'  containing MODIStsp processing options saved from the GUI, Default: NULL
#' @param selprod `character` Name of selected MODIS product (e.g.,
#'   Vegetation Indexes_16Days_250m (M*D13Q1)). You can get
#'   a list of available product names using function `MODIStsp_get_prodnames`,
#'   Default: NULL
#' @param bandsel `character array` Original MODIS layers to be processed.
#'   You can get a list of available layers for a given product
#'   using function `MODIStsp_get_prodlayers` (e.g., MODIStsp_get_prodlayers("M*D13Q1")$bandnames),
#'   Default: NULL
#' @param quality_bandsel `character array` Quality Indicators to be computed starting from
#'   bit fields of original MODIS layers. You can get a list of available quality layers for a given product
#'   using function `MODIStsp_get_prodlayers` (e.g., MODIStsp_get_prodlayers("M*D13Q1")$quality_bandnames),
#'   Default: NULL
#' @param indexes_bandsel `character array`Spectral Indexes to be computed starting from reflectance bands.
#'   You can get a list of available quality layers for a given product
#'   using function `MODIStsp_get_prodlayers` (e.g., MODIStsp_get_prodlayers("M*D13Q1")$indexes_bandnames),
#'   Default: NULL
#' @param sensor `character ["Terra"| "Aqua" | "Both"]` MODIS platform to be considered.
#'   (Ignored for MCD* products). Default: "Both"
#' @param download_server `character ["http" | "offline"]` service to be used for
#'  download. Default: "http"
#' @param downloader download_server `character ["http" | "aria2"]` downloader to be used,
#'  Default: "http"
#' @param user `character` Username for NASA http server.
#'   ([urs.earthdata.nasa.gov/home](https://urs.earthdata.nasa.gov/home)).
#' @param password `character` Password for NASA http server
#'   ([urs.earthdata.nasa.gov/home](https://urs.earthdata.nasa.gov/home)).
#' @param download_range `character ["Full" | "Seasonal"]` If "full", all the
#'   available images between the starting and the ending dates are downloaded;
#'   If "seasonal", only the images included in the season are downloaded
#'   (e.g: if the starting date is 2005-12-01 and the ending is 2010-02-31, only
#'   the images of December, January and February from 2005 to 2010 - excluding
#'   2005-01, 2005-02 and 2010-12 - are downloaded), Default: Full
#' @param start_date `character` Start date for images download and preprocessing
#'  (yyyy.mm.dd), Default: NULL
#' @param end_date `character` End date for images download and preprocessing
#'  (yyyy.mm.dd), Default: NULL
#' @param spatmeth `character ["tiles" | "bbox" | "file"]`, indicates how the processing
#'  extent is retrieves. if "tiles", use the specified tiles (start_x....).
#'  If "file", retrieve extent from spatial file specifies in `spafile`. If
#'  "bbox", use the specified bounding box, Default: "tiles"
#' @param start_x `integer [0-35]` Start MODIS horizontal tile defining spatial extent.
#'  Ignored if spatmeth != "tiles", Default: 18
#' @param start_y `integer [0-17]` Start MODIS vertical tile defining spatial extent.
#'  Ignored if spatmeth != "tiles", Default: 4
#' @param end_x `integer [0-35]` End MODIS horizontal tile defining spatial extent.
#'  Ignored if spatmeth != "tiles", Default: 18
#' @param end_y `integer [0-17]` End MODIS vertical tile defining spatial extent.
#'  Ignored if spatmeth != "tiles", Default: 4
#' @param bbox `numeric(4)` Output bounding box (xmin, ymin, xmax, ymax) in
#'   out_proj coordinate system. Ignored if spatmeth == "tiles", Default: NULL
#' @param spafile `character` (optional) full path of a spatial file
#'  to use to derive the processing extent. If not NULL, the processing options
#'  which define the extent, the selected tiles and the "Full Tile / Custom"
#'  in the JSON options file are overwritten and new files are created on the
#'  extent of the provided spatial file. Ignored if spatmeth != "file", Default: NULL
#' @param out_projsel `character ["Native", "User Defined`] If "Native", the
#'   outputs keep the original resolution of MODIS HDF images. Otherwise, the value
#'    set in "out_res" is used, Default:Native
#' @param output_proj `character` either equal to "MODIS Sinusoidal",
#'  or to the code of a valid EPSG or to a WKT projection string.
#'  Ignored if outproj_sel == "Native", Default: NULL
#' @param out_res_sel `character ["Native", "User Defined`]. If "Native", the
#'   outputs keep the original resolution of MODIS HDF images. Otherwise, the value
#'    set in "out_res" is used.
#' @param out_res `float` Output resolution (in output projection measurement
#'  unit). Ignored if out_res_sel == "Native".
#' @param resampling `character ["near" | "bilinear" | "cubic" | "cubicspline",
#' |lanczos"|, "average"|, "mode", |"max"|, |"min"|, |"q1"|, |"q3"|, |"sum"|]`
#'   Resampling method to be used by `gdalwarp`.
#' @param reprocess `logical` If TRUE, reprocess data for already existing dates.
#' @param delete_hdf `logical` If TRUE, delete downloaded HDF files after completion.
#' @param nodata_change `logical` if TRUE, NoData values are set to the max value
#'  of the datatype of the layer on the MODIStsp output rasters. NOTE: If multiple
#'   nodata values are reported for a layer, all are reset to the new value.
#' @param scale_val `logical` If TRUE,  scale and offset are applied to
#'  original MODIS layers, and Spectral Indexes are saved as floating point. If
#'  FALSE, no rescaling is done and Spectral Indexes are saved as integer, with a
#'  10000 scaling factor.
#' @param ts_format `character array including ["R RasterStack" | "ENVI Meta Files" | "GDAL VRT" |
#'  "ENVI and GDAL"]` Selected virtual time series format.
#' @param out_format `character ["ENVI" | "GTiff"]` Desired output format.
#' @param compress `character ["None" | "PACKBITS" | "LZW" | "DEFLATE"]`
#'   Compression method for GTiff outputs (Ignored if `out_format == ENVI`)
#' @param test `integer | character  (e.g., "01a")` if set, MODIStsp is executed in
#'  "test mode", using a preset Options File instead than opening the GUI or accepting the
#'  `opts_file` parameter. This allows both to check correct installation on
#'  user's machines, and to implement unit testing.
#' @param n_retries `numeric` maximum number of retries on download functions.
#'   In case any download function fails more than `n_retries` times consecutively,
#'   MODIStsp_process will abort, Default: 20
#' @param verbose `logical` If FALSE, suppress processing messages,
#'  Default: TRUE
#' @param parallel `logical` If TRUE (default), the function is run using parallel
#'  processing, to speed-up the computation for large rasters (with a maximum
#'  of 8 cores).
#'  The number of cores is automatically determined; specifying it is also 
#'  possible (e.g. `parallel = 4`). In this case, more than 8 cores can be
#'  specified. If FALSE (default), single core processing is used.
#' @param ... not used for values, forces later arguments to bind by name
#' @return NULL
#'
#' @author Lorenzo Busetto, phD (2014-2017)
#' @author Luigi Ranghetti, phD (2015-2017) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0
#' @export
#' @seealso [MODIStsp_GUI()], [MODIStsp_process()]
#' @rdname MODIStsp
#' @importFrom raster rasterOptions
#' @importFrom sf sf_extSoftVersion
#' @importFrom jsonlite read_json write_json
#' @importFrom tools file_path_sans_ext
#' @importFrom utils unzip
#' @examples
#'
#' #' # - Running the tool using the GUI
#' # Running the tool without any option will start the GUI with the default or
#' # last used settings, in interactive mode (i.e., with gui = TRUE).
#' \donttest{
#' if (interactive()) {
#'   MODIStsp()
#' }
#' }
#'
#'
#' #' # - Running the tool specifying processing arguments in the call
#'
#' # **NOTE** Output files of examples are saved to file.path(tempdir(), "MODIStsp").
#'
#' # Here we process layers __NDVI__ and __EVI__ and quality indicator __usefulness__
#' # of product __M*D13Q1__, considering both Terra and Aqua platforms, for dates
#' # comprised between 2020-06-01 and 2020-06-15 and saves output to R tempdir
#' # --> See name and available layers for product M*D13Q1.
#' # Note that this example (as well as the following ones) is run in single
#' # core to follow CRAN policies, by setting parallel = FALSE.
#' # Users can exploit multicore functionalities skipping to set this argument.
#'
#' MODIStsp_get_prodlayers("M*D13A2")
#' \donttest{
#' MODIStsp(
#'   gui = FALSE,
#'   out_folder = "$tempdir",
#'   selprod = "Vegetation_Indexes_16Days_1Km (M*D13A2)",
#'   bandsel = c("EVI", "NDVI"),
#'   quality_bandsel = "QA_usef",
#'   indexes_bandsel = "SR",
#'   user = "mstp_test" ,
#'   password = "MSTP_test_01",
#'   start_date = "2020.06.01",
#'   end_date = "2020.06.15",
#'   verbose = FALSE,
#'   parallel = FALSE
#' )
#' }
#'
#'
#' #' # - Running the tool using the settings previously saved in a specific options file
#'
#' # **NOTE** Output files of examples are saved to file.path(tempdir(), "MODIStsp").
#' # You can run the examples with `gui = TRUE` to set a different output folder!
#'
#' # Here we use a test json file saved in MODIStsp installation folder which
#' # downloads and processed 3 MOD13A2 images over the Como Lake (Lombardy, Italy)
#' # and retrieves NDVI and EVI data, plus the Usefulness Index Quality Indicator.
#'
#' opts_file <- system.file("testdata/test_MOD13A2.json", package = "MODIStsp")
#' \donttest{
#' MODIStsp(gui = FALSE, opts_file = opts_file, verbose = TRUE, parallel = FALSE)
#' }
#'
#'
#' # Running the tool using the settings previously saved in a specific option file
#' # and specifying the extent from a spatial file allows to re-use the same
#' # processing settings to perform download and reprocessing on a different area
#'
#' opts_file <- system.file("testdata/test_MOD13A2.json", package = "MODIStsp")
#' spatial_file <- system.file("testdata/lakeshapes/garda_lake.shp", package = "MODIStsp")
#' \donttest{
#' MODIStsp(
#'   gui = FALSE, 
#'   opts_file = opts_file,
#'   spatmeth = "file",
#'   spafile = spatial_file, 
#'   verbose = TRUE,
#'   parallel = FALSE
#' )
#' }
#'
#'
#' # Running the tool using the settings previously saved in a
#' # specific options file and specifying each time the extent from a different
#' # spatial file (e.g., to perform the same processing on several extents)
#' # Note that you can also put all your extent files in a specific folder and
#' # create the extent list using for example.
#' 
#' extent_list = list.files(
#'   system.file("testdata/lakeshapes/", package = "MODIStsp"),
#'   "\\.shp$", 
#'   full.names = TRUE
#' )
#' extent_list
#' opts_file <- system.file("testdata/test_MOD13A2.json", package = "MODIStsp")
#' 
#' \donttest{
#' for (single_shape in extent_list) {
#'   MODIStsp(
#'     gui = FALSE, 
#'     opts_file = opts_file,
#'     spatmeth = "file",
#'     spafile = single_shape, 
#'     verbose = TRUE,
#'     parallel = FALSE
#'  )
#' }
#'
#' # output files are placed in separate folders:
#' outfiles_garda <- list.files(
#'   file.path(tempdir(), "MODIStsp/garda_lake/VI_16Days_1Km_v6/NDVI"),
#'   full.names = TRUE
#' )
#' outfiles_garda
#' library(raster)
#' plot(raster(outfiles_garda[1] ))
#'
#' outfiles_iseo <- list.files(
#'   file.path(tempdir(), "MODIStsp/iseo_lake/VI_16Days_1Km_v6/NDVI"),
#'   full.names = TRUE
#' )
#' outfiles_iseo
#' plot(raster(outfiles_iseo[1]))
#' }
#'
#' # See also https://docs.ropensci.org/MODIStsp/articles/noninteractive_execution.html

MODIStsp <- function(...,
                     gui             = TRUE,
                     out_folder      = NULL,
                     out_folder_mod  = NULL,
                     opts_file       = NULL,
                     selprod         = NULL,
                     bandsel         = NULL,
                     quality_bandsel = NULL,
                     indexes_bandsel = NULL,
                     sensor          = NULL,
                     download_server = NULL,
                     downloader      = NULL,
                     user            = NULL,
                     password        = NULL,
                     download_range  = NULL,
                     start_date      = NULL,
                     end_date        = NULL,
                     spatmeth        = NULL,
                     start_x         = NULL,
                     end_x           = NULL,
                     start_y         = NULL,
                     end_y           = NULL,
                     bbox            = NULL,
                     spafile         = NULL,
                     out_projsel     = NULL,
                     output_proj     = NULL,
                     out_res_sel     = NULL,
                     out_res         = NULL,
                     resampling      = NULL,
                     reprocess       = NULL,
                     delete_hdf      = NULL,
                     nodata_change   = NULL,
                     scale_val       = NULL,
                     ts_format       = NULL,
                     out_format      = NULL,
                     compress        = NULL,
                     test            = NULL,
                     n_retries       = 5,
                     verbose         = TRUE,
                     parallel        = TRUE) {

  # Make so that "raster" functions does not automatically add extensions on
  # output files. This is automatically reset to TRUE at the end of the session
  raster::rasterOptions(setfileext = FALSE)
  proc_opts <- NULL
  #   _________________________________________________________________________
  #   check arguments                                                       ####

  # if (is.null(opts_file) & gui == FALSE) {
  #   stop("You need to provide a valid `.json` options file to run MODIStsp",
  #        " in non-interactive mode. \n",
  #        "Please provide a valid \"opts_file\" path or run ",
  #        "with gui=TRUE to create and save one.")
  # }

  if (!is.null(opts_file)) {
    if (!file.exists(opts_file))
      stop("The specified `.json` options file was not found. \n",
           "Please provide a valid \"opts_file\" path or run ",
           "without specifying one to create and save one.")
  }

  #   __________________________________________________________________________
  #   Initialize processing                                                 ####

  #   __________________________________________________________________________
  #   If test mode is selected, select the options file for testing         ####
  #   and set other parameters
  if (!is.null(test)) {

    gui <- FALSE
    message("MODIStsp is running in test mode.")
    # read names of available json test files
    test_files <- sort(list.files(
      path       = system.file("testdata", package = "MODIStsp"),
      pattern    = "^test[0-9]{2}[a-zA-Z]?\\.json$",
      full.names = TRUE))

    if (is.numeric(test)) {
      test <- sprintf("%02d", test)
    }

    cur_test  <- paste0("test", test, ".json")
    avail_tests <- basename(test_files)
    #nocov start
    if (!cur_test %in% avail_tests) {
      stop(paste0("The test is not available. These are the currently available tests: ", #nolint
                  paste(avail_tests, collapse = ", ")))
    }
    #nocov end
    # check that the offline HDF files were unzipped - unzip them if not
    tests_hdf_zipped <- list.files(
      path       = system.file("testdata", package = "MODIStsp"),
      pattern    = "\\.hdf\\.zip$",
      full.names = TRUE
    )

    for (test_hdf in gsub("\\.zip$", "", tests_hdf_zipped)) {
      if (!file.exists(file.path(tempdir(), "MODIStsp/HDFs", basename(test_hdf)))) {
        unzip(zipfile = paste0(test_hdf, ".zip"),
              files   = basename(test_hdf),
              exdir   = file.path(tempdir(), "MODIStsp/HDFs"),
              unzip   = "internal")
      }
    }

    # Assign the selected test Option File

    opts_file <- list.files(
      path       = system.file("testdata", package = "MODIStsp"),
      pattern    = cur_test,
      full.names = TRUE)
  }

  #   __________________________________________________________________________
  #   On GUI execution, ensure that shiny libraries suggested are avauilable     ####

  if (gui) {
    #nocov start
    gui_deps <- c("leaflet", "shiny",
                  "shinydashboard","shinyFiles",
                  "shinyalert","rappdirs", "shinyjs",
                  "leafem", "mapedit",
                  "magrittr")
    gui_deps_missing <- !sapply(gui_deps, requireNamespace, quietly = TRUE)

    if (sum(gui_deps_missing) > 0) {
      stop("You need to install the following Suggested packages to use the MODIStsp GUI.
           Please install them with:
           install.packages(c(\"leaflet\", \"shiny\",\"shinydashboard\",\"shinyFiles\",
                              \"shinyalert\", \"rappdirs\",\"shinyjs\",
                              \"leafem\", \"mapedit\", \"magrittr\"))")
    } else {
      requireNamespace("leaflet")
      requireNamespace("shiny")
      requireNamespace("shinydashboard")
      requireNamespace("shinyFiles")
      requireNamespace("shinyalert")
      requireNamespace("shinyjs")
      requireNamespace("leafem")
      requireNamespace("rappdirs")
      requireNamespace("mapedit")
      requireNamespace("magrittr")
    }
    #nocov end
  }

  gdal_version <- sf::sf_extSoftVersion()[["GDAL"]]

  if (verbose) message("GDAL version in use: ", as.character(gdal_version))

  #   __________________________________________________________________________
  #   On interactive execution, launch the GUI and wait for user selection. ####
  #   On non-interactive immediately start processing using the processing
  #   options contained in the provided `opts_file`

  if (gui) {
    #nocov start
    MODIStsp_GUI()
    #nocov end
  } else {

    if (!is.null(opts_file)) {
      proc_opts <- try(jsonlite::read_json(opts_file))
      if(inherits(proc_opts, "try-error")) {
        stop("Unable to read the provided options file. Please check your ",
             "inputs!")
      }

      if(proc_opts$MODIStspVersion < 2.0) {
        stop("Options files saved with MODIStsp versions lower than 2.0
             are no longer supported. Please save a new file using MODIStsp
             GUI.")
      }
      #TODO: function to check if file is OK
    } else {
      # Load default values - to avoid overwriting always the loaded
      # parameters with default, we set the defaults here rather than
      # in the initialization of the function!
      proc_opts <- jsonlite::read_json(system.file("ExtData",
                                                   "mstp_defaults.json",
                                                   package = "MODIStsp"))
    }

    # update proc_opts based on arguments passed to the function ----

    if(!is.null(selprod)) {proc_opts$selprod <- selprod}

    if(!is.null(bandsel)) {proc_opts$bandsel <- bandsel}

    if(!is.null(quality_bandsel)) {proc_opts$quality_bandsel <- quality_bandsel}

    if(!is.null(indexes_bandsel)) {proc_opts$indexes_bandsel <- indexes_bandsel}

    if(!is.null(sensor)) {proc_opts$sensor <- sensor}
    #TODO check correctness of platform wrt product

    if(!is.null(download_server)) {proc_opts$download_server <- download_server}
    if(!is.null(downloader)) {proc_opts$downloader <- downloader}
    # TODO replace use_aria use

    if(!is.null(user)) {proc_opts$user     <- user}
    if(!is.null(password)) {proc_opts$password <- password}

    if(!is.null(download_range)) {proc_opts$download_range <- download_range}
    if(!is.null(start_date)) {proc_opts$start_date <- start_date}
    if(!is.null(end_date)) {proc_opts$end_date <- end_date}

    if(!is.null(spatmeth)) {proc_opts$spatmeth  <- spatmeth}

    if (proc_opts$spatmeth == "tiles") {
      if(!is.null(start_x)) {proc_opts$start_x  <- start_x}
      if(!is.null(end_x))   {proc_opts$end_x    <- end_x}
      if(!is.null(start_y)) {proc_opts$start_y  <- start_y}
      if(!is.null(end_y))   {proc_opts$end_y <- end_y}
    } else {
      if (proc_opts$spatmeth == "file") {
        if (!is.null(spafile)) {
          bbox <- bbox_from_file(spafile, crs_out = proc_opts$output_proj)
          proc_opts$bbox <- as.numeric(bbox)
          tiles <- tiles_from_bbox(bbox, proc_opts$output_proj)
          proc_opts$start_x <- tiles[1]
          proc_opts$start_y <- tiles[3]
          proc_opts$end_x  <- tiles[2]
          proc_opts$end_y  <- tiles[4]

        }

        #TODO update examples and website
      } else {
        if (proc_opts$spatmeth == "map") {
          # TODO how to deal with this?????
        } else {
          if (proc_opts$spatmeth == "bbox") {
            #TODO retrieve tiles from selection
            bbox  <- as.numeric(proc_opts$bbox)
            proc_opts$bbox <- bbox
            tiles <- tiles_from_bbox(bbox, proc_opts$output_proj)
            proc_opts$start_x <- tiles[1]
            proc_opts$start_y <- tiles[3]
            proc_opts$end_x  <- tiles[2]
            proc_opts$end_y  <- tiles[4]
          }
        }
      }
    }
    # proc_opts$bbox <- as.numeric(proc_opts$bbox)
    if(!is.null(out_projsel)) {proc_opts$out_projsel  <- out_projsel}
    if(!is.null(output_proj)) {proc_opts$output_proj  <- output_proj}

    if(!is.null(out_res_sel))  {proc_opts$out_res_sel  <- out_res_sel}
    if(!is.null(out_res))      {proc_opts$out_res  <- out_res}
    if(!is.null(resampling))  {proc_opts$resampling  <- resampling}

    if(!is.null(reprocess))  {proc_opts$reprocess  <- reprocess}
    if(!is.null(delete_hdf))  {proc_opts$delete_hdf  <- delete_hdf}
    if(!is.null(nodata_change)) {proc_opts$nodata_change  <- nodata_change}
    if(!is.null(scale_val))  {proc_opts$scale_val  <- scale_val}

    if(!is.null(out_format)) {proc_opts$out_format  <- out_format}
    if(!is.null(ts_format))  {proc_opts$ts_format  <- ts_format}
    if(!is.null(compress))   {proc_opts$compress  <- compress}

    if(!is.null(out_folder))     {proc_opts$out_folder  <- out_folder}
    if(!is.null(out_folder_mod)) {proc_opts$out_folder_mod  <- out_folder_mod}

    if (proc_opts$out_folder == "$modistest") {
      warning(paste0(
        "Argument out_folder = '$modistest' can no longer be used ",
        "due to CRAN policy; using '$tempdir'."
      ))
      proc_opts$out_folder <- "$tempdir"
    }
    if (proc_opts$out_folder == "$tempdir") {
      proc_opts$out_folder <- file.path(tempdir(), "MODIStsp")
    }

    if (proc_opts$out_folder_mod == "$modistest") {
      warning(paste0(
        "Argument out_folder_mod = '$modistest' can no longer be used ",
        "due to CRAN policy; using '$tempdir'."
      ))
      proc_opts$out_folder_mod <- "$tempdir"
    }

    if (proc_opts$out_folder_mod == "$tempdir") {
      proc_opts$out_folder_mod <- file.path(tempdir(), "MODIStsp/HDFs")
    }

    if (proc_opts$spatmeth == "file" & !missing(spafile)) {
      proc_opts$out_folder <- file.path(
        proc_opts$out_folder,
        tools::file_path_sans_ext(basename(spafile))
      )
    }

    # Create output folders if needed. No recursive to avoid creating big
    # folder trees by mistake

    dir.create(proc_opts$out_folder,
               recursive = FALSE,
               showWarnings = FALSE)

    dir.create(proc_opts$out_folder_mod,
               recursive = FALSE,
               showWarnings = FALSE)

    check_opts <- check_proc_opts(proc_opts)

    MODIStsp_process(proc_opts,
                     n_retries = n_retries,
                     verbose = verbose,
                     parallel = parallel)
  }

}
