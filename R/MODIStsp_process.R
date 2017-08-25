#' @title MODIStsp_process
#' @description main function of MODIStsp tool. Takes as input processing parameters
#' specified by the p_opts$user using MODIStsp_p_opts$gui and saved in MODIStsp_Previous.json
#' (Interactive use), or a p_opts$user specified JSON file (batch use)
#' (See MODIStsp_main for details ) and performs all required
#' processing.
#' @details After retrieving the input processing options, the function
#'   1. Accesses lpdaac http or ftp archive to determine the list of dates to be processed
#'   2. Performs all required processing steps on each date (download, reprojection,
#'      resize, mosaicing, p_opts$indexes computation, p_opts$quality indicators computation)
#'   3. Creates virtual files of the processed time series.
#'
#' Reprojection and resize is dealt with by accessing gdal routines through the `gdaUtils`
#' package.
#' Extraction of bitfields from p_opts$quality layers is done though fast bitwise computation
#' Checks are done in order to not re-download already existing HDF images, and not
#' reprocess already processed dates (if the p_opts$user did not specify that)
#'
#' @param p_opts$sel_prod string selected MODIS product
#' @param p_opts$start_date string p_opts$start_date for images download and preprocessing
#'  (yyyy.mm.dd)
#' @param p_opts$end_date string p_opts$end_date for images download and preprocessing
#'  (yyyy.mm.dd)
#' @param p_opts$out_folder  main output folder
#' @param p_opts$out_folder_mod  output folder for original HDF storage
#' @param reprocess string string ("Yes"/"No") If Yes, reprocess data for already existing
#'   dates (Default = 'Yes')
#' @param p_opts$delete_hdf string ("Yes"/"No") If Yes, delete original HDF after completion
#' @param p_opts$sensor string ("Terra" or "Aqua" or "Both")
#' @param https hash https site for download of HDF of selected product
#' @param p_opts$ftps hash p_opts$ftps site for download of HDF of selected product
#' @param p_opts$download_server service used to download MODIS tiles, one of: 'http', 'ftp', NA.
#' @param p_opts$user p_opts$username for http download
#'   ([urs.earthdata.nasa.gov/home](https://urs.earthdata.nasa.gov/home))
#' @param p_opts$password p_opts$password for http download
#'   ([urs.earthdata.nasa.gov/home](https://urs.earthdata.nasa.gov/home))
#' @param p_opts$start_x int start horizontal tile
#' @param p_opts$start_y int start vertical tile
#' @param p_opts$end_x int end horizontal tile
#' @param p_opts$end_y int end vertical tile
#' @param p_opts$bbox array output bounding box (xmin, xmax, ymin, ymax ) in out proj coords
#' @param p_opts$out_format string output raster format (ENVI or GTiff)
#' @param p_opts$compress string p_opts$compression for GTiff outputs (None, LZW, DEFLATE)
#' @param p_opts$native_res_sel string "Native" or "Resampled"
#' @param p_opts$native_res float Output resolution (in output projection measurement unit)
#' @param native_res float Native resolution of MODIS product
#' @param p_opts$tiled 0/1 1 = p_opts$tiled product; 0 = non-p_opts$tiled product (resolution 0.05 deg)
#' @param p_opts$mod_proj_str string proj4 string for MODIS product native projection
#' @param p_opts$outproj_str string proj4 string of selected output projection
#' @param p_opts$nodata_in array Original NoData for MODIS bands
#' @param p_opts$nodata_out Target NoData for MODIS bands
#' @param p_opts$nodata_out string (Yes/No) if Yes, NoData are set to p_opts$nodata_out in
#' output rasters
#' @param p_opts$scale_val string (Yes/No) if Yes, output values in are rescaled in the
#'  measure unit of the variable
#' @param p_opts$rts string ("Yes"/"No") If Yes, create p_opts$rts time series
#' @param p_opts$datatype string array p_opts$datatypes of MODIS bands
#' @param bandsel  array of length equal to number of original modis layers. set
#'  to 1 for bands to be processed
#' @param p_opts$bandnames array of Abbreviated Names of MODIS bands
#' @param p_opts$indexes_bandsel array of length equal to number of available spectral p_opts$indexes,
#'   set to  1 for p_opts$indexes to be processed
#' @param p_opts$indexes_bandnames array of Abbreviated Names of MODIS p_opts$indexes
#' @param p_opts$indexes_formula  array of p_opts$indexes formulas
#' @param p_opts$indexes_nodata_out NoData values for p_opts$indexes
#' @param p_opts$quality_bandnames array of  Names of MODIS p_opts$quality indicators
#' @param p_opts$quality_bandsel array of length equal to number of available p_opts$quality indicators,
#'   set to  1 for indicators to be processed
#' @param p_opts$quality_bitN list of strings with number of entries equal to number of
#'   p_opts$quality indicators. each entry contains position of bits corresponding to a QI
#'   (e.g., 0-1)
#' @param p_opts$quality_source list of strings which connects each p_opts$quality indicator to
#' its source aggregated p_opts$quality assurance layer
#' @param p_opts$quality_nodata_in Always 255
#' @param p_opts$full_ext string ("p_opts$full_ext" or "Resized")
#' @param p_opts$quality_nodata_out Always 255
#' @param p_opts$file_prefixes output file prefix according to selected product (e.g., MOD13Q1)
#' @param p_opts$main_out_folder Suffix to add to the overall p_opts$out_folder to create the out
#'   dir for the product (corresponds to an abbreviation of the selected product)
#' @param p_opts$resampling string p_opts$resampling method (near, bilinear, etc.)
#' @param p_opts$ts_format string format of virtual files (None, ENVI Meta Files, GDAL vrt files,
#'   ENVI and GDAL)
#' @param p_opts$gui logical indicates if processing was called within the p_opts$gui environment or not.
#'   If not, direct processing messages to the log
#' @param p_opts$use_aria logical if TRUE, then aria2c is used to accelerate download
#'   (if available !)
#' @param p_opts$download_range character if "full", all the available images between the
#'   starting and the ending dates are downloaded; if "seasonal", only the images
#'   included in the season (e.g: if the starting date is 2005-12-01 and the ending
#'   is 2010-02-31, only the images of December, January and February from 2005 to 2010
#'   - excluding 2005-01, 2005-02 and 2010-12 - are downloaded)
#' @param p_opts$scale_factor scale factor to be applied to MODIS layer to convert from
#'   scaled integer to correct measure units
#' @param p_opts$offset p_opts$offset to be applied to MODIS layer to convert form scaled integer
#'   to correct measure units
#' @return NULL
#'
#' @author Lorenzo Busetto, phD (2014-2015) \email{busetto.l@@irea.cnr.it}
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @note Thanks Tomislav Hengl and Babak Naimi, whose scripts made the starting point for
#'   development of this function ([ModisDownload](http://r-gis.net/?q=ModisDownload);
#'   [Download_and_p_opts$resampling_of_MODIS_images](http://spatial-analyst.net/wiki/index.php?title=Download_and_p_opts$resampling_of_MODIS_images))
#' @note License: GPL 3.0
#' @rdname MODIStsp_process
#' @importFrom gdalUtils gdalinfo gdal_translate gdalwarp gdalbuildvrt
#' @importFrom gWidgets gwindow glabel dispose gconfirm svalue addHandlerUnrealize
#' @importFrom httr content GET authenticate progress timeout
#' @importFrom raster raster writeRaster
#' @importFrom XML xmlToList xmlParse
#' @importFrom tools file_path_sans_ext
#' @importFrom parallel detectCores

MODIStsp_process <- function(p_opts) {
  
  # Intialize variables -----------------------------------------------------
  #^ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  
  # Fix on multiple NoData values
  suppressWarnings(
    p_opts$nodata_in[is.na(as.numeric(p_opts$nodata_in))] <- "None"
  )
  suppressWarnings(
    p_opts$quality_nodata_in[is.na(as.numeric(p_opts$quality_nodata_in))] <- "None" 
  )
  # FIXME: as.integer(NoData) cause NoData ranges (e.g. 249-255) to be suppressed.
  # So, in this cases NoData values will not
  # be recognised. This problem will be solved in future with a cycle on NoData range.
  
  if (p_opts$nodata_out == "No") {
    p_opts$nodata_out <- p_opts$nodata_in
  }  # if NoData chande set to no, set ou_nodata to in_nodata
  dir.create(p_opts$out_folder_mod, recursive = TRUE, showWarnings = FALSE) # create out folder if not existing
  out_prod_folder <- file.path(p_opts$out_folder, p_opts$main_out_folder)  # main output folder --> define on the basis of product name and create if necessary
  dir.create(out_prod_folder, showWarnings = FALSE, recursive = TRUE)
  # tmp_prod_folder <- file.path(out_prod_folder, "tmp") # directory to store temporary [virtual] rasters
  p_opts$start_year      <- unlist(strsplit(p_opts$start_date, "[.]"))[1]
  p_opts$end_year        <- unlist(strsplit(p_opts$end_date, "[.]"))[1]
  
  
  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### Initialize number of cores for gdalwarp (equal to ncpus - 2 OR 10 if ncp####
  
  ncores <- min(c(10, parallel::detectCores() - 2))
  
  #^ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  #  Verify if bands needed for computing spectral p_opts$indexes and/or p_opts$quality indicators are already selected
  #  if not, select them and set the "delete" option for them to 1
  #^ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  
  # patch not to generate error if no indexes and/or quality bands are present/computable
  # for the selected product
  
  if (length(p_opts$indexes_bandnames) == 0) {p_opts$indexes_bandsel <- integer(0)}
  if (length(p_opts$quality_bandnames) == 0) {p_opts$quality_bandsel <- integer(0)}
  # dummy matrix which associate, to each couple of index or quality band (col) - original band (row),
  # info on wether that band is required to build that index
  bands_indexes <- matrix(0, nrow     = length(bandsel),
                          ncol     = length(p_opts$indexes_bandsel) + length(p_opts$quality_bandsel),
                          dimnames = list(p_opts$bandnames, c(p_opts$indexes_bandnames, p_opts$quality_bandnames)))
  
  # Save original choice of bands in bandsel_orig_choice (bandsel is later modified to
  # set to 1 all bands needed for p_opts$indexes and p_opts$quality
  bandsel_orig_choice <- bandsel
  # cycle on selected spectral indexes
  for (band in which(p_opts$indexes_bandsel == 1)) {
    formula <- p_opts$indexes_formula[band]    # If an index is selected retrieve its formula
    # cycle on original bands
    for (bandorig in seq(along.with = p_opts$bandnames)) {
      # check if the original band is needed for the index
      if (length(grep(p_opts$bandnames[bandorig], formula)) > 0) {
        # if yes and band not set to be processed, set it to be processed
        if (bandsel[bandorig] == 0) {
          bands_indexes[bandorig, band] <- 1
        }
      }
    } #End Cycle on bandorig
  } #End If on bandsel[band] == 1
  # cycle on selected QIs
  for (band in which(p_opts$quality_bandsel == 1)) {
    bandorig <- which(p_opts$bandnames == p_opts$quality_source[band]) # Identify source band for the p_opts$quality indicator selected
    # if source not already selected to be processed, select it
    if (bandsel[bandorig] == 0) {
      bands_indexes[bandorig, length(p_opts$indexes_bandsel) + band] <- 1
    }
  } #End If on bandsel[band] == 1
  
  #TODO CUT HERE FUNCTION
  
  #^ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^
  #  Retrieving required files list from NASA server ----
  #^ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^
  
  mess_text <- "Retrieving list of files from NASA server"
  if (p_opts$gui) {
    mess     <- gWidgets::gwindow(title = "Processing Status", width = 400, height = 40)
    mess_lab <- gWidgets::glabel(text = paste("---", mess_text, "---"), editable = FALSE, container = mess)
    Sys.sleep(0.05)
    message("[", date(), "] ", mess_text)
  } else {
    message("[", date(), "] ", mess_text)
  }
  
  if (p_opts$sensor == "Both") {
    senslist <- c("Terra", "Aqua")
  } else {
    senslist <- p_opts$sensor
  }   # If both p_opts$sensor selected, do a cycle. Process first Terra then Aqua
  
  # cycle on selected p_opts$sensors
  for (sens_sel in senslist) {
    
    # get http site addresses and file prefixes
    if (sens_sel == "Terra") {
      http <- p_opts$https[["Terra"]]
      ftp  <- p_opts$ftps[["Terra"]]
      file_prefix <- p_opts$file_prefixes[["Terra"]]
    } else {
      http <- p_opts$https[["Aqua"]]
      ftp  <- p_opts$ftps[["Aqua"]]
      file_prefix <- p_opts$file_prefixes[["Aqua"]]
    }
    
    # Check if "aria2c" requested. If so, verify that the executable is on the path
    
    if (p_opts$use_aria == TRUE) {
      test_aria <- Sys.which("aria2c")
      if (test_aria == "") {
        if (p_opts$gui) {
          noaria <- gconfirm("aria2c was not found! It is either not installed or not
                             on your path!\nDo you want to proceed with normal download? ")
          if (noaria == TRUE) {
            p_opts$use_aria <- 0
          } else {
            gmessage("Please ensure that aria2c is installed and in your path!
                     - See http://aria2.github.io ")
            gWidgets::dispose(mess)
            stop("aria2c was not found! Ensure that aria2c is installed and in your
                 path! - See http://aria2.github.io ")
          }
        } else {
          message("aria2c was not found! It is either not installed or not on your path! -
                  Continuing with normal download... ")
        }
      }
    }
    
    # Retrieve available dates for the selected product
    date_dirs_all   <- lpdaac_getmod_dirs(p_opts$ftp, p_opts$http, p_opts$download_server,
                                          p_opts$user, p_opts$password, p_opts$gui,
                                          p_opts$out_folder_mod,
                                          .Platform = .Platform)
    # overwrite with the used setting (if already specified it does not change, if NA,
    # it is set with the working one)
    p_opts$download_server <- attr(date_dirs_all, "server")
    
    #TODO CUT HERE FUNCTION --> returns date_dirs_all
    
    # ---------------------------------- #
    # Start Cycle on required years
    # ---------------------------------- #
    
    for (yy in p_opts$start_year:p_opts$end_year) {
      
      if (p_opts$download_range == "full") {
        # Create string representing the dates to be processed in the case
        # of continuous processing
        
        if (yy == p_opts$start_year & yy == p_opts$end_year) {
          dates <- c(p_opts$start_date, p_opts$end_date)
        }
        
        if (yy == p_opts$start_year & yy != p_opts$end_year) {
          dates <- c(p_opts$start_date, paste0(as.character(yy), ".12.31"))
        }
        
        if (yy != p_opts$start_year & yy != p_opts$end_year) {
          dates <- c(paste0(as.character(yy), ".1.1"), paste0(as.character(yy), ".12.31"))
        }
        
        if (yy != p_opts$start_year & yy == p_opts$end_year) {
          dates <- c(paste0(as.character(yy), ".1.1"), p_opts$end_date)
        }
        
      } else if (p_opts$download_range == "seasonal") {
        # Create string representing the dates to be processed in the case
        # of splitted processing
        #
        # the starting month-day
        start_seas <- as.Date(strftime(as.Date(p_opts$start_date, format = "%Y.%m.%d"), "0-%m-%d"))
        # the ending month-day
        end_seas   <- as.Date(strftime(as.Date(p_opts$end_date, format = "%Y.%m.%d"), "0-%m-%d"))
        
        nye_incl   <- start_seas > end_seas # TRUE if the period includes new year's eve, fasle if not
        
        if (!nye_incl) {
          dates    <- c(gsub(paste0("^", p_opts$start_year), yy, p_opts$start_date), gsub(paste0("^", p_opts$end_year),
                                                                                          yy, p_opts$end_date))
        } else {
          
          if (yy == p_opts$start_year & yy != p_opts$end_year) {
            dates  <- c(gsub(paste0("^", p_opts$start_year), yy, p_opts$start_date), paste0(as.character(yy),
                                                                                            ".12.31"))
          }
          
          if (yy != p_opts$start_year & yy != p_opts$end_year) {
            dates  <- c(paste0(as.character(yy), ".1.1"), gsub(paste0("^", p_opts$end_year), yy, p_opts$end_date),
                        gsub(paste0("^", p_opts$start_year), yy, p_opts$start_date), paste0(as.character(yy),
                                                                                            ".12.31"))
          }
          
          if (yy != p_opts$start_year & yy == p_opts$end_year) {
            dates  <- c(paste0(as.character(yy), ".1.1"), gsub(paste0("^", p_opts$end_year), yy, p_opts$end_date))
          }
          
        }
        
      } else stop("download_range value not valid (only \"full\" and \"seasonal\" are admitted).")
      
      # Processing status message
      mess_text <- paste("Retrieving Files for Year", as.character(yy))
      if (p_opts$gui) {
        svalue(mess_lab) <- paste("---", mess_text, "---")
        Sys.sleep(0.05)
      } else {
        message("[", date(), "] ", mess_text)
      }
      
      # Get a list of the folders containing HDF images required (Corresponding to the
      # subfolders in lpdaac corresponding to
      # selected product, dates, and current year under processing)
      date_dirs <- lpdaac_getmod_dates(dates = dates, date_dirs =  date_dirs_all)  # First, find the folders in lpdaac corresponding to the required dates
      
      if (length(date_dirs) > 0) {
        modislist <- NULL
        # Start Cycling on directories containing images to be downloaded and identify
        # the required ones (i.e., the ones corresponding to selected tiles)
        for (date in seq_along(date_dirs)) {
          
          date_name <- sub(sub(pattern = "\\.", replacement = "_", date_dirs[date]),
                           pattern = "\\.", replacement = "_", date_dirs[date])  #Create the date string
          year      <- strftime(as.Date(date_name, "%Y_%m_%d" ), format = "%Y")  # transform date to year
          DOY       <- strftime(as.Date(date_name, "%Y_%m_%d" ), format = "%j")  # transform date to DOY
          
          # check if all foreseen output rasters already exist. If so, skip the date. Otherwise start proecssing
          check_files <- FALSE
          check_files <- MODIStsp_check_files(out_prod_folder,
                                              file_prefix,
                                              yy,
                                              DOY,
                                              p_opts$bandnames,
                                              bandsel_orig_choice,
                                              p_opts$indexes_bandnames,
                                              p_opts$indexes_bandsel,
                                              p_opts$quality_bandnames,
                                              p_opts$quality_bandsel, 
                                              p_opts$out_format)
          # If not all output files are already present or reprocess = "Yes", start downloading hdfs
          if (check_files == FALSE | p_opts$reprocess == "Yes") {
            
            # Create vector of image names required (corresponding to the selected tiles,
            # within current dir)
            modislist <- lpdaac_getmod_names(p_opts$http, p_opts$ftp, used_server = p_opts$download_server,
                                             p_opts$user, p_opts$password,
                                             date_dirs[date],
                                             v = seq(from = p_opts$start_y, to = p_opts$end_y),
                                             h = seq(from = p_opts$start_x, to = p_opts$end_x),
                                             p_opts$tiled, p_opts, p_opts$out_folder_mod,
                                             p_opts$gui)
            
            
              #TODO CUT HERE FUNCTION --> returns modislist
            
            # ---------------------------------- ----------------------------------------------#
            # Download and preprocess Imagesin modislist vector -----------
            # ---------------------------------- ----------------------------------------------#
            if (length(modislist) > 0) {
              
              #- ------------------------------------------------------------------------------- -#
              #  Download images (If HDF file already in out_mod_folder, it is not redownloaded !!!!
              #- ------------------------------------------------------------------------------- -#
              for (modisname in modislist) {
                # Check file size (if the local file size is different, re-download)
                local_filename  <- file.path(p_opts$out_folder_mod, modisname)
                local_filesize  <- file.info(local_filename)$size
                if (p_opts$download_server == "http") {remote_filename <- paste0(http, date_dirs[date], "/", modisname)}
                if (p_opts$download_server == "ftp")  {remote_filename <- paste0(ftp, year, "/", DOY, "/", modisname)}
                if (p_opts$download_server == "offline") {remote_filename <- NA}
                
                # in case of http or ftp download, try to catch size information from xml file ----
                if (p_opts$download_server != "offline") {
                  remote_size_tries  <- 30 # numbers of tryouts for xml metafile
                  size_string        <- NA
                  class(size_string) <- "try-error"
                  
                  while (remote_size_tries > 0) {
                    size_string <- if (p_opts$download_server == "http") {
                      try(GET(paste0(remote_filename, ".xml"), authenticate(p_opts$user, p_opts$password),
                              timeout(240)))
                    } else if (p_opts$download_server == "ftp") {
                      try((getURL(remote_filename, nobody = 1L, header = 1L,
                                  .opts = list(timeout = 240, maxredirs = 5, verbose = FALSE))))
                    }
                    # Check if download was good: check class of xmldown and status of xmldown
                    if (class(size_string) == "try-error") {
                      remote_size_tries <- remote_size_tries - 1
                    } else {
                      remote_size_tries <- 0
                    }
                  }
                  
                  # if p_opts$user/p_opts$password are not valid, notify
                  if (p_opts$download_server == "http" & size_string["status_code"] == 401) {
                    stop("p_opts$username and/or p_opts$password are not valid. Please retry with the
                         correct ones or try with ftp download.")
                  }
                  
                  # if the xml was available, check the size; otherwise, set as the local size to
                  # skip the check
                  if (class(size_string) == "try-error") {
                    remote_filesize <- local_filesize
                  } else {
                    remote_filesize <- if (p_opts$download_server == "http") {
                      as.integer(XML::xmlToList(XML::xmlParse(httr::content(size_string, encoding = "UTF-8"))
                      )[["GranuleURMetaData"]][["DataFiles"]][["DataFileContainer"]][["FileSize"]])
                    } else if (p_opts$download_server == "ftp") {
                      as.integer(gsub("[^:]+: ([0-9]+)\\r.*", "\\1", size_string))
                    }
                  }
                  
                } else {
                  # On offline mode, don't perform file size check.
                  remote_filesize <- local_filesize
                }
                
                # Perform download ----
                # If HDF not existing or with different size, download.
                if (!file.exists(local_filename) | local_filesize != remote_filesize) {
                  er <- 5; class(er) <- "try-error"; ce <- 0
                  
                  local_filesize <- 0
                  # while loop: Only exit if local file size equals remote filesize
                  while (local_filesize != remote_filesize) {
                    # repeat until no error or > 30 tryyouts
                    while (er != 0) {
                      mess_text <- paste("Downloading", sens_sel, "Files for date",
                                         date_name, ":", which(modislist == modisname),
                                         "of", length(modislist))
                      if (p_opts$gui) {
                        svalue(mess_lab) <- paste("---", mess_text, "---")
                        Sys.sleep(0.05)
                        message("[", date(), "] ", mess_text)
                      } else {
                        message("[", date(), "] ", mess_text)
                      } # Update progress window
                      if (p_opts$download_server == "http") {
                        # http download
                        if (p_opts$use_aria == TRUE) {
                          aria_string <- paste(Sys.which("aria2c"), " -x 6 -d ", dirname(local_filename),
                                               " -o ", basename(remote_filename), " ", remote_filename,
                                               " --allow-overwrite --file-allocation=none --retry-wait=2",
                                               " --http-p_opts$user=", p_opts$user, " --http-passwd=", p_opts$password, sep = "")
                          # intern=TRUE for Windows, FALSE for Unix
                          download <- try(system(aria_string, intern = Sys.info()["sysname"] == "Windows"))
                        } else {
                          download <- try(httr::GET(remote_filename, httr::authenticate(p_opts$user, p_opts$password),
                                                    httr::progress(), httr::timeout(2400)))
                        }
                      } else {
                        # ftp download
                        if (p_opts$use_aria == TRUE) {
                          aria_string <- paste(Sys.which("aria2c"), " -x 6 -d ",
                                               dirname(local_filename),
                                               " -o ", basename(remote_filename), " ", remote_filename,
                                               " --allow-overwrite --file-allocation=none --retry-wait=2", sep = "")
                          download <- try(system(aria_string, intern = Sys.info()["sysname"] == "Windows"))
                        } else {
                          download <- try(httr::GET(remote_filename, httr::progress(),
                                                    httr::timeout(2400)))
                        }
                      }
                      # Check for errors on download try
                      if (class(download) == "try-error" | !is.null(attr(download, "status"))) {
                        er <- 5
                        ce <- ce + 1
                        message("[", date(), "] Download Error - Retrying...")
                        unlink(local_filename)  # On download error, delete bad files
                        Sys.sleep(1)    # sleep for a while....
                      } else {
                        if (p_opts$download_server == "http" & p_opts$use_aria == FALSE) {
                          
                          if (download$status_code != 200 & length(content(download, "text",
                                                                           encoding = "UTF-8")) == 1) {
                            # on error, delete last HDF file (to be sure no incomplete
                            # files are left behind and send message)
                            message("[", date(), "] Download Error - Retrying...")
                            unlink(local_filename)
                            Sys.sleep(1)
                            er <- 5
                            ce <- ce + 1
                          } else {
                            writeBin(download$content, local_filename)
                            er <- 0
                          }
                        } else {
                          er <- 0
                          if (p_opts$use_aria == FALSE) {
                            writeBin(download$content, local_filename)
                          }
                        }
                      }
                      
                      if (ce == 30) {
                        # Ask if Stop after 30 failed attempts
                        if (p_opts$gui) {
                          confirm <- gWidgets::gconfirm(paste0(p_opts$download_server,
                                                               " server seems to be down! Do you want to retry?"),
                                                        icon = "question",
                                                        handler = function(h, ...){})
                        } else {
                          confirm <- "FALSE"
                        }
                        if (confirm == "FALSE") {
                          warning("[", date(), "] Error: server seems to be down! Please Retry Later!")
                          unlink(local_filename)
                          stop()
                        }
                      }
                    }  # end while on download tries
                    
                    # Futher check on downloaded file size
                    # # Find the size of the new file downloaded to allow comparison with remote
                    local_filesize <- file.info(local_filename)$size
                    if (is.na(local_filesize)) {
                      local_filesize <- 0
                      er <- 5
                      ce <- ce + 1
                    }
                  } # end here the while loop on file size check
                  
                }  # end IF on HDF existence
              } # End cycle for downloading the images in modislist vector
              
              message("[", date(), "] ", length(modislist), " files for date of ",
                      date_dirs[date], " were successfully downloaded!")
              
              # ^ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^
              # After all required tiles for the date are downloaded, start geoprocessing -----
              # ^ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^
              
              # STEP 0: patch to correct wrong resolution/p_opts$bbox in some HDF4 original layers (e.g. albedo) ----
              # Retrieve information from hdf4 with gdalinfo
              gdalinfo_hdf_raw      <- gdalUtils::gdalinfo(file.path(p_opts$out_folder_mod, modislist[1]))
              gdalinfo_hdf_1stlayer <- gsub("^ *SUBDATASET_1_NAME=", "",
                                            gdalinfo_hdf_raw[grep("^ *SUBDATASET_1_NAME",
                                                                  gdalinfo_hdf_raw)])
              gdalinfo_hdf_resunit  <- gsub("^ *NADIRDATARESOLUTION=[0-9.]+ ?", "",
                                            gdalinfo_hdf_raw[grep("^ *NADIRDATARESOLUTION",
                                                                  gdalinfo_hdf_raw)])
              gdalinfo_raw          <- if (length(gdalinfo_hdf_1stlayer) > 0) {
                # if more than a band is present, take gdalinfo from the first band
                gdalUtils::gdalinfo(gdalinfo_hdf_1stlayer)
              } else {
                # otherwise, take from the HDF directly
                gdalinfo_hdf_raw
              }
              gdalinfo_p_opts$bbox <- cbind(na.omit(as.numeric(unlist(strsplit(gsub("[^0-9.\\-]+", " ",
                                                                                    gdalinfo_raw[grep("^Lower Left", gdalinfo_raw)]),
                                                                               " "))))[1:2],
                                            na.omit(as.numeric(unlist(strsplit(gsub("[^0-9.\\-]+", " ",
                                                                                    gdalinfo_raw[grep("^Upper Right", gdalinfo_raw)]),
                                                                               " "))))[1:2])
              # if HDF file is in degrees and with a small bounding box, correct
              correct_hdf <- if (length(grep("(degree)|(Arc Second)",
                                             gdalinfo_hdf_resunit)) &
                                 all(gdalinfo_p_opts$bbox == c(-0.05, -0.025, 0.05, 0.025))) {
                TRUE
              } else {
                FALSE
              }
              
              # ^ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^
              # STEP 1: identify the layers (original, p_opts$indexes and p_opts$quality bands) to be created ----
              # ^ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^
              
              # at the end of this step, "bandsel" is recreated as the union of the bands selected
              # by the p_opts$user and the bands required
              # by p_opts$indexes and p_opts$quality bands, but only those ones which are not already present.
              
              # do a check to see if the product has at least one p_opts$quality Layer or Possible Index
              if (length(p_opts$indexes_bandnames) != 0 | length(p_opts$quality_bandnames) != 0 ) {
                
                req_bands_indexes <- bands_indexes
                for (i in seq_along(req_bands_indexes)) {
                  req_bands_indexes[i] <- 0
                } # matrix similar to band_p_opts$indexes, but specific for this year-doy process
                
                for (band in which(p_opts$indexes_bandsel == 1)) {
                  p_opts$indexes_band <- p_opts$indexes_bandnames[band]
                  out_filename <- file.path(
                    out_prod_folder,
                    p_opts$indexes_band,
                    paste0(file_prefix, "_", p_opts$indexes_band, "_", yy, "_", DOY, 
                           ifelse(p_opts$out_format == "GTiff", ".tif", ".dat"))
                  )
                  if (file.exists(out_filename) == FALSE | p_opts$reprocess == "Yes") {
                    # if the index does not exists then find out the original bands required for it
                    req_bands_indexes[, band] <- bands_indexes[, band]
                  }
                }
                
                for (band in which(p_opts$quality_bandsel == 1)) {
                  p_opts$quality_band <- p_opts$quality_bandnames[band]
                  out_filename <- file.path(
                    out_prod_folder, p_opts$quality_band,
                    paste0(file_prefix, "_", p_opts$quality_band, "_", yy, "_", DOY, 
                           ifelse(p_opts$out_format == "GTiff", ".tif", ".dat"))
                  )
                  if (p_opts$out_format == "GTiff") {
                    out_filename <- paste0(out_filename, ".tif")
                  } else {
                    out_filename <- paste0(out_filename, ".dat")
                  }
                  # if the index does not exists then find out the original bands required for it
                  if (file.exists(out_filename) == FALSE | p_opts$reprocess == "Yes") {
                    req_bands_indexes[, band + length(p_opts$indexes_bandsel)] <- bands_indexes[, band + length(p_opts$indexes_bandsel)]
                  }
                }
                
                # Create the final vector of bands required for processing (bands
                # chosen by the p_opts$user + bands required for p_opts$indexes and p_opts$quality bands)
                bandsel <- as.integer(as.logical(bandsel_orig_choice + apply(req_bands_indexes, 1, sum)))
              } #end check on existence of p_opts$quality/p_opts$indexes layers
              
              # dummy array set to 0 - will contain info on wether orignal
              # downloaded bands has to be deleted
              delbands <- bandsel - bandsel_orig_choice
              
              # ^ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^
              # STEP 2: process the required original MODIS layers ----
              # ^ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^
              
              # Cycle on MODIS original layers
              for (band in seq_along(p_opts$bandnames)) {
                
                # Create vector with length = bands, filled with zeroes
                bands <- numeric(length(p_opts$bandnames))
                # If band selected, process it
                if (bandsel[band] == 1) {
                  bands[band] <- 1    # IF band selected for processing, put its value to 1
                  dir.create(file.path(out_prod_folder, p_opts$bandnames[band]),
                             showWarnings = FALSE, recursive = TRUE)
                  bands       <- paste(as.character(bands), collapse = "", sep = " ")         # Convert to character
                  # outfile     <- paste0(tmp_prod_folder, "/", p_opts$bandnames[band], "_", yy, "_", DOY, ".tif")   # Create name for the temporary tif mosaic
                  outfile     <- paste0(p_opts$bandnames[band], "_", yy, "_", DOY, ".tif")   # Create name for the temporary tif mosaic
                  # NOTE: Change outrep_file to a list of rep files: only one for original bands, multiple for p_opts$indexes and p_opts$quality
                  
                  # Create name for the TIFF reprojected  mosaic
                  outrep_file   <- file.path(
                    out_prod_folder, p_opts$bandnames[band],
                    paste0(file_prefix, "_",
                           sub("[.][^.]*$", "", basename(outfile), perl = TRUE), 
                           ifelse(p_opts$out_format == "GTiff", ".tif", ".dat"))
                  )
                  
                  outfile_vrt <- tempfile(fileext = ".vrt")   # filename of temporary vrt file
                  
                  if (file.exists(outrep_file) == FALSE | p_opts$reprocess == "Yes") {
                    
                    files_in <- file.path(p_opts$out_folder_mod, modislist)
                    # dir.create(tmp_prod_folder, recursive = TRUE, showWarnings = FALSE)
                    # ---------------------------------------------------------------------------------#
                    # Convert to output projection, extent and format using gdalwarp ----
                    # ---------------------------------------------------------------------------------#
                    
                    if (p_opts$outproj_str != p_opts$mod_proj_str) {
                      mess_text <- paste("Processing and Reprojecting", sens_sel,
                                         p_opts$bandnames[band], "files for date:", date_name)
                    } else {
                      mess_text <- paste("Processing", sens_sel, p_opts$bandnames[band],
                                         "files for date:", date_name)
                    }
                    if (p_opts$gui) {
                      svalue(mess_lab) <- paste("---", mess_text, "---")
                      Sys.sleep(0.05)
                      message("[", date(), "] ", mess_text)
                    } else {
                      message("[", date(), "] ", mess_text)
                    }
                    
                    if (p_opts$datatype[band] == "UInt32") {
                      # fix due to bug in creation of vrt file for UIn32 data type - create tiff
                      # files from the original hdfs, and then use those to build the vrt
                      
                      files_out   <- NULL
                      for(file in seq_along(files_in)) {
                        file_out <- tempfile(fileext = ".tif")
                        gdalUtils::gdal_translate(files_in[file],
                                                  file_out,
                                                  sd_index  = band,
                                                  srcnodata = p_opts$nodata_in[band],
                                                  vrtnodata = p_opts$nodata_out[band], 
                                                  overwrite = TRUE)
                        files_in[file] <- file_out
                      }
                      
                      outfile_vrt <- paste0(str_sub(outfile_vrt, 1,-5), ".tif")
                      gdalwarp(files_in,
                               outfile_vrt,
                               sd        = band,
                               srcnodata = p_opts$nodata_in[band],
                               vrtnodata = p_opts$nodata_out[band],
                               multi     = TRUE,
                               wo        = paste0("NUM_THREADS=", ncores), 
                               overwrite = TRUE
                      )
                      
                    } else {
                      # Create a GDAL vrt file corresponding to the original hdf4
                      gdalUtils::gdalbuildvrt(files_in,
                                              outfile_vrt,
                                              sd = band,
                                              srcnodata = p_opts$nodata_in[band],
                                              vrtnodata = p_opts$nodata_out[band])
                    }
                    
                    # apply the patch if an error in the original hdf4 file at step 0 was detected
                    if (correct_hdf) {
                      outfile_vrt_or        <- outfile_vrt
                      outfile_vrt           <- tempfile(fileext = ".vrt")   # filename of new temporary vrt file
                      outfile_vrt_cont      <- readLines(outfile_vrt_or)
                      outfile_vrt_linegeom  <- grep("<GeoTransform>", outfile_vrt_cont)
                      outfile_vrt_geom      <- as.numeric(unlist(strsplit(gsub("<GeoTransform>(.*)</GeoTransform>", "\\1",
                                                                               outfile_vrt_cont[outfile_vrt_linegeom]), ",")))
                      outfile_vrt_geom_corr <- outfile_vrt_geom * 3600
                      outfile_vrt_cont[outfile_vrt_linegeom] <- paste("<GeoTransform>",
                                                                      paste(outfile_vrt_geom_corr, collapse = ", "), "</GeoTransform>")
                      write(outfile_vrt_cont, outfile_vrt)
                    }
                    
                    
                    # If resize required,  convert p_opts$bbox coordinates from t_srs
                    # to modis_srs, to get the correct extent
                    if (p_opts$full_ext == "Resized") {
                      outfile_vrt_or <- outfile_vrt
                      outfile_vrt <- tempfile(fileext = ".vrt")   # filename of new temporary vrt file
                      # for resizing BEFORE reprojecting
                      p_opts$bbox_mod <- reproj_p_opts$bbox(p_opts$bbox, p_opts$outproj_str, p_opts$mod_proj_str, enlarge = TRUE)
                      # Create a resized and eventually mosaiced GDAL vrt file
                      
                      if (p_opts$datatype[band] == "UInt32") {
                        # fix to avoid bug on gdalbuildvrt for UInt32 datasets; create a tif
                        # instead than a vrt
                        outfile_vrt <- paste0(str_sub(outfile_vrt, 1,-5), ".tif")
                        gdalwarp(outfile_vrt_or,
                                 outfile_vrt,
                                 te        = c(p_opts$bbox_mod),
                                 tap       = TRUE,
                                 tr        = res(raster(outfile_vrt_or)),
                                 sd        = band,
                                 srcnodata = p_opts$nodata_in[band],
                                 vrtnodata = p_opts$nodata_out[band],
                                 ot        = p_opts$datatype[band],
                                 multi     = TRUE,
                                 wo        = c("INIT_DEST = NO_DATA",
                                               paste0("NUM_THREADS=", ncores)),
                                 overwrite  = TRUE)
                      } else {
                        gdalbuildvrt(outfile_vrt_or,
                                     outfile_vrt,
                                     te        = c(p_opts$bbox_mod),
                                     tap       = TRUE,
                                     tr        = res(raster(outfile_vrt_or)),
                                     srcnodata = p_opts$nodata_in[band],
                                     vrtnodata = p_opts$nodata_out[band],
                                     sd        = band,
                                     overwrite = TRUE)
                      }
                    }
                    
                    # Launch the reprojection - operations to be done depends on whether resize
                    # and/or reprojection and/or p_opts$resampling are required
                    
                    reproj_type <- if (p_opts$native_res_sel == "Native" & p_opts$outproj_str == p_opts$mod_proj_str) {
                      "GdalTranslate"
                    } else if (p_opts$native_res_sel == "Resampled" & p_opts$outproj_str == p_opts$mod_proj_str) {
                      "Resample1_Resize0"
                    } else if (p_opts$native_res_sel == "Native"    & p_opts$outproj_str != p_opts$mod_proj_str & p_opts$full_ext == "Full Tiles Extent") {
                      "Resample0_Resize0"
                    } else if (p_opts$native_res_sel == "Native"    & p_opts$outproj_str != p_opts$mod_proj_str & p_opts$full_ext == "Resized") {
                      "Resample0_Resize1"
                    } else if (p_opts$native_res_sel == "Resampled" & p_opts$outproj_str != p_opts$mod_proj_str & p_opts$full_ext == "Full Tiles Extent") {
                      "Resample1_Resize0"
                    } else if (p_opts$native_res_sel == "Resampled" & p_opts$outproj_str != p_opts$mod_proj_str & p_opts$full_ext == "Resized") {
                      "Resample1_Resize1"
                    } else {
                      "Error"
                    }
                    
                    # If p_opts$scale_factor="Yes", add a step before creating final files
                    outrep_file_0 <- if (
                      p_opts$scale_val == "Yes" &
                      !(p_opts$scale_factor[band] == 1 & p_opts$offset[band] == 0)
                    ) {
                      tempfile(fileext = ifelse(p_opts$out_format == "GTiff",
                                                ".tif", ".dat")
                      )
                    } else {
                      outrep_file
                    }
                    
                    if (p_opts$out_format == "GTiff") {
                      switch(reproj_type,
                             GdalTranslate = gdalUtils::gdal_translate(
                               outfile_vrt,
                               outrep_file_0,
                               a_srs = p_opts$mod_proj_str,
                               of = p_opts$out_format,
                               ot = p_opts$datatype[band],
                               a_nodata = p_opts$nodata_out[band],
                               co = paste("p_opts$compress", p_opts$compress, sep = "="),
                               overwrite = TRUE
                             ),
                             Resample0_Resize0 = gdalUtils::gdalwarp(
                               outfile_vrt, outrep_file_0,
                               s_srs     = p_opts$mod_proj_str,
                               t_srs = p_opts$outproj_str,
                               of        = p_opts$out_format,
                               r = p_opts$resampling,
                               co        = paste("p_opts$compress", p_opts$compress, sep = "="),
                               ot        = p_opts$datatype[band],
                               multi     = TRUE,
                               wo        = c("INIT_DEST = NO_DATA",
                                             paste0("NUM_THREADS=", ncores)),
                               overwrite = TRUE
                             ),
                             Resample0_Resize1 = gdalUtils::gdalwarp(
                               outfile_vrt, outrep_file_0,
                               s_srs      = p_opts$mod_proj_str,
                               t_srs = p_opts$outproj_str,
                               of         = p_opts$out_format,
                               r = p_opts$resampling,
                               te         = p_opts$bbox,
                               co         = paste("p_opts$compress", p_opts$compress, sep = "="),
                               ot         = p_opts$datatype[band],
                               multi      = TRUE,
                               wo         = c("INIT_DEST = NO_DATA",
                                              paste0("NUM_THREADS=", ncores)),
                               overwrite  = TRUE
                             ),
                             Resample1_Resize0 = gdalUtils::gdalwarp(
                               outfile_vrt, outrep_file_0,
                               s_srs      = p_opts$mod_proj_str,
                               t_srs = p_opts$outproj_str,
                               of         = p_opts$out_format,
                               r         = p_opts$resampling,
                               tr         = rep(p_opts$native_res, 2),
                               co         = paste("p_opts$compress", p_opts$compress, sep = "="),
                               ot        = p_opts$datatype[band],
                               multi     = TRUE,
                               wo        = c("INIT_DEST = NO_DATA",
                                             paste0("NUM_THREADS=", ncores)),
                               overwrite = TRUE
                             ),
                             Resample1_Resize1 =  gdalUtils::gdalwarp(
                               outfile_vrt,
                               outrep_file_0,
                               s_srs     = p_opts$mod_proj_str,
                               t_srs     = p_opts$outproj_str,
                               of        = p_opts$out_format,
                               r         = p_opts$resampling,
                               te        = p_opts$bbox,
                               tr        = rep(p_opts$native_res, 2),
                               co        = paste("p_opts$compress", p_opts$compress, sep = "="),
                               ot        = p_opts$datatype[band],
                               multi     = TRUE,
                               wo        = c("INIT_DEST = NO_DATA",
                                             paste0("NUM_THREADS=", ncores)),
                               overwrite = TRUE
                             ),
                             stop(
                               "Internal error in p_opts$native_res_sel, p_opts$outproj_str or p_opts$full_ext."))
                    } else {
                      switch(reproj_type,
                             GdalTranslate =  gdalUtils::gdal_translate(
                               outfile_vrt,  outrep_file_0,
                               a_srs = p_opts$mod_proj_str,
                               of = p_opts$out_format,
                               ot = p_opts$datatype[band],
                               a_nodata = p_opts$nodata_out[band],
                               overwrite = TRUE
                             ),
                             Resample0_Resize0  =  gdalUtils::gdalwarp(
                               outfile_vrt, outrep_file_0,
                               s_srs = p_opts$mod_proj_str,
                               t_srs = p_opts$outproj_str,
                               of = p_opts$out_format,
                               r = p_opts$resampling,
                               ot        = p_opts$datatype[band],
                               multi     = TRUE,
                               wo        = c("INIT_DEST = NO_DATA",
                                             paste0("NUM_THREADS=", ncores)),
                               overwrite = TRUE
                             ),
                             Resample0_Resize1  = gdalUtils::gdalwarp(
                               outfile_vrt, outrep_file_0,
                               s_srs = p_opts$mod_proj_str,
                               t_srs = p_opts$outproj_str,
                               of = p_opts$out_format,
                               r = p_opts$resampling,
                               te = p_opts$bbox,
                               ot        = p_opts$datatype[band],
                               multi     = TRUE,
                               wo        = c("INIT_DEST = NO_DATA",
                                             paste0("NUM_THREADS=", ncores)),
                               overwrite = TRUE
                             ),
                             Resample1_Resize0  =  gdalUtils::gdalwarp(
                               outfile_vrt, outrep_file_0,
                               s_srs = p_opts$mod_proj_str,
                               t_srs = p_opts$outproj_str,
                               of = p_opts$out_format,
                               r = p_opts$resampling,
                               tr = rep(p_opts$native_res, 2),
                               ot        = p_opts$datatype[band],
                               multi     = TRUE,
                               wo        = c("INIT_DEST = NO_DATA",
                                             paste0("NUM_THREADS=", ncores)),
                               overwrite = TRUE
                             ),
                             Resample1_Resize1  =  gdalUtils::gdalwarp(
                               outfile_vrt,
                               outrep_file_0,
                               s_srs = p_opts$mod_proj_str,
                               t_srs = p_opts$outproj_str,
                               of    = p_opts$out_format,
                               r     = p_opts$resampling,
                               te    = p_opts$bbox,
                               tr    = rep(p_opts$native_res, 2),
                               ot    = p_opts$datatype[band],
                               multi = TRUE,
                               wo    = c("INIT_DEST = NO_DATA",
                                         paste0("NUM_THREADS=", ncores)),
                               overwrite = TRUE
                             ),
                             quit("Internal error in p_opts$native_res_sel, p_opts$outproj_str or p_opts$full_ext."))
                    }
                    # TODO: Extract as function !
                    # If p_opts$scale_factor="Yes", create final files by rescaling values
                    if (p_opts$scale_val == "Yes"   & 
                        !(p_opts$scale_factor[band] == 1 & p_opts$offset[band] == 0)) {
                      # fixed: ignore scaling if slope = 1 AND p_opts$offset = 0
                      # mode with raster()
                      
                      outrep_0 <- raster::raster(outrep_file_0)
                      scl <- as.numeric(p_opts$scale_factor[band])
                      off <- as.numeric(p_opts$offset[band])
                      na  <- as.numeric(p_opts$nodata_out[band])
                      outrep <- raster::calc(x         = outrep_0,
                                             fun       = function(x) {
                                               x * scl + off
                                             }, 
                                             filename  = outrep_file,
                                             format    = p_opts$out_format,
                                             datatype  = "FLT4S", 
                                             options   = ifelse(
                                               p_opts$out_format == "GTiff",
                                               paste0("compress=", p_opts$compress),
                                               ""),                       
                                             NAflag    = na,
                                             overwrite = TRUE)
                      rm(outrep, outrep_0)
                    }
                    
                    # If output format is ENVI, add data ignore value to the header file
                    if (p_opts$out_format == "ENVI") {
                      fileConn_meta_hdr <- file(paste0(
                        tools::file_path_sans_ext(outrep_file), ".hdr"), "a")
                      writeLines(c("data ignore value = ",
                                   p_opts$nodata_out[band] ),
                                 fileConn_meta_hdr, sep = " ")
                      writeLines("", fileConn_meta_hdr)
                      close(fileConn_meta_hdr)
                    }
                    # xml_file <- paste0(outrep_file, ".aux.xml")   # Delete xml files created by gdalwarp
                    # unlink(tmp_prod_folder, recursive = TRUE)     # Delete temporary files in temp folder
                  }
                }  # ENDIF band selected for processing
              } # END Cycle on available MODIS Bands
              
              # ^ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^
              # If p_opts$indexes selected, then start creating them
              # ^ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ^
              
              for (band in which(p_opts$indexes_bandsel == 1)) {
                p_opts$indexes_band <- p_opts$indexes_bandnames[band]   # index name
                formula      <- p_opts$indexes_formula[band]       #index formula
                mess_text    <- paste("Computing", sens_sel, p_opts$indexes_band,
                                      "for date:", date_name)
                if (p_opts$gui) {
                  gWidgets::svalue(mess_lab) <- paste("---", mess_text, "---")
                  Sys.sleep(0.05)
                  message("[", date(), "] ", mess_text)
                } else {
                  message("[", date(), "] ", mess_text)
                }
                out_filename <- file.path(
                  out_prod_folder,
                  p_opts$indexes_band,
                  paste0(file_prefix, "_", p_opts$indexes_band, "_", yy, "_", DOY,
                         ifelse(p_opts$out_format == "GTiff", ".tif", ".dat"))
                )
                
                
                #If file not existing and p_opts$reprocess = No, compute the index and save it
                if (file.exists(out_filename) == FALSE | p_opts$reprocess == "Yes") {
                  MODIStsp_process_indexes(out_filename,
                                           formula,
                                           p_opts$p_opts$bandnames,
                                           p_opts$nodata_out,
                                           p_opts$indexes_nodata_out[band],
                                           out_prod_folder,
                                           file_prefix,
                                           p_opts$compress, 
                                           yy,
                                           p_opts$out_format,
                                           DOY,
                                           p_opts$scale_val
                  )
                }
              }
              
              # ---------------------------------- ----------------------------------------------#
              # If p_opts$quality indicators selected , then start creating them
              # ---------------------------------- ----------------------------------------------#
              
              for (band in which(p_opts$quality_bandsel == 1)) {
                
                mess_text     <- paste("Computing", p_opts$quality_band, "for date:", date_name)
                if (p_opts$gui) {
                  gWidgets::svalue(mess_lab) <- paste("---", mess_text, "---")
                  Sys.sleep(0.05)
                  message("[", date(), "] ", mess_text)
                } else {
                  message("[", date(), "] ", mess_text)
                }
                
                quality_band  <- p_opts$quality_bandnames[band]     # indicator name
                source        <- p_opts$quality_source[band]  #  Original MODIS layer containing data of the indicator
                bitN          <- p_opts$quality_bitN[band]      #  bitfields corresponding to indicator within source
                nodata_qa_in  <- p_opts$quality_nodata_in[band]
                nodata_qa_out <- p_opts$quality_nodata_out[band]
                nodata_source <- p_opts$nodata_out[grep(source, p_opts$bandnames)]
                
                out_filename <- file.path(
                  out_prod_folder, p_opts$quality_band,
                  paste0(file_prefix, "_", p_opts$quality_band, "_", yy, "_", DOY,
                         ifelse(p_opts$out_format == "GTiff", ".tif", ".dat")
                  )
                )
                
                # If file not existing or p_opts$reprocess = Yes, compute the indicator
                # and save it
                if (file.exists(out_filename) == FALSE | p_opts$reprocess == "Yes") {
                  
                  in_source_filename <- file.path(
                    out_prod_folder, source,
                    paste0(file_prefix, "_", source, "_", yy, "_", DOY, 
                           ifelse(p_opts$out_format == "GTiff", ".tif", ".dat"))
                  )
                  
                  MODIStsp_process_QA_bits(out_filename, 
                                           in_source_filename,
                                           bitN, 
                                           p_opts$out_format,
                                           nodata_source,
                                           nodata_qa_in,
                                           nodata_qa_out, 
                                           p_opts$compress)
                }
              }
              
              #- ------------------------------------------------------------------------------- -#
              #  Delete bands not needed (i.e., bands required for p_opts$indexes or p_opts$quality computation,
              # but not requested by the p_opts$user,
              #- ------------------------------------------------------------------------------- -#
              for (banddel in seq(along = delbands)) {
                
                if (delbands[banddel] == 1) {
                  out_filename <- file.path(out_prod_folder, p_opts$bandnames[banddel],
                                            paste0(file_prefix, "_", p_opts$bandnames[banddel], "_", yy, "_", DOY))
                  if (p_opts$out_format == "ENVI") {
                    out_filename_dat <- paste0(out_filename, ".dat")
                    unlink(out_filename_dat)
                    out_filename_hdr <- paste0(out_filename, ".hdr")
                    unlink(out_filename_hdr)
                    
                  }
                  if (p_opts$out_format == "GTiff") {
                    out_filename_tif <- paste0(out_filename, ".tif")
                    unlink(out_filename_tif)
                  }
                  unlink(dirname(out_filename), recursive = TRUE)
                } #End If on delbands[banddel] == 1
              } #End Cycle on banddel
              
            } else {
              message("[", date(), "] No available image for selected Tiles in ",
                      date_dirs[date])
            } # End check on at least one image available
            
          } else {
            message("[", date(), "] All Required output files for date ", date_name,
                    " are already existing - Doing Nothing!")
          } # End check on all data already processed for date or reprocees = Yes
          
          #- ------------------------------------------------------------------------------- -#
          # If deletion selected, delete the HDF files in p_opts$out_folder_mod directory
          #- ------------------------------------------------------------------------------- -#
          if (p_opts$delete_hdf == "Yes") {
            
            for (dir in seq_along(date_dirs)) {
              
              modislist <- lpdaac_getmod_names(p_opts$http,
                                               p_opts$ftp,
                                               p_opts$download_server,
                                               p_opts$user,
                                               p_opts$password,
                                               date_dirs[dir],
                                               v = seq(p_opts$start_y, p_opts$end_y, 1),
                                               h = seq(p_opts$start_x, p_opts$end_x, 1),
                                               p_opts$tiled,
                                               p_opts$out_folder_mod,
                                               p_opts$gui)
              for (modisname in modislist) {
                unlink(file.path(p_opts$out_folder_mod, modisname))
              }
            }
          } #end if on Delete original downloaded HDFs
          
        } # End cycling on available dates for selected year
        
      } else {
        message("[", date(), "] No available data for year: ",
                yy, " for p_opts$sensor ",
                sens_sel, " in selected dates.")
      }
      
    } # End Cycling on selected years
    
    bandsel <- bandsel_orig_choice  # reset bandsel to original p_opts$user's choice
    
  } # End cycling on p_opts$sensors
  
  #- ------------------------------------------------------------------------------- -#
  #  Create vrt files of time series - original and derived
  #- ------------------------------------------------------------------------------- -#
  
  if (p_opts$sensor == "Both") {
    senslist <- c("Terra", "Aqua", "Mixed")
  } # selected p_opts$sensors
  # cycle on selected p_opts$sensors
  for (sens_sel in senslist) {
    # Create virtual files for original layers
    for (band in which(bandsel == 1)) {
      message("[", date(), "] Creating Virtual Files and p_opts$rts time series for layer ",
              p_opts$bandnames[band])
      
      MODIStsp_vrt_create(out_prod_folder,
                          meta_band = p_opts$p_opts$bandnames[band],
                          p_opts$file_prefixes,
                          sens_sel,
                          p_opts$ts_format,
                          p_opts$nodata_out[band],
                          p_opts$out_format,
                          p_opts$rts)
    } #End Cycle on bandsel
    # Create virtual files for QI layers
    for (band in which(p_opts$indexes_bandsel == 1)) {
      message("[", date(), "] Creating Virtual Files and p_opts$rts time series for layer ",
              p_opts$indexes_bandnames[band])
      MODIStsp_vrt_create(out_prod_folder,
                          meta_band = p_opts$indexes_bandnames[band],
                          p_opts$file_prefixes,
                          sens_sel,
                          p_opts$ts_format,
                          p_opts$indexes_nodata_out[band],
                          p_opts$out_format,
                          p_opts$rts)
    } #End Cycle on p_opts$indexes_bandsel
    
    # Create virtual files for SI layers
    for (band in which(p_opts$quality_bandsel == 1)) {
      message("[", date(), "] Creating Virtual Files and p_opts$rts time series for layer ",
              p_opts$quality_bandnames[band])
      MODIStsp_vrt_create(out_prod_folder,
                          meta_band = p_opts$quality_bandnames[band]   ,
                          p_opts$file_prefixes,
                          sens_sel,
                          p_opts$ts_format,
                          p_opts$quality_nodata_out[band],
                          p_opts$out_format,
                          p_opts$rts)
    } #End Cycle on p_opts$quality_bandsel
    
  }
  #- ------------------------------------------------------------------------------- -#
  # Close p_opts$gui and clean-up
  #- ------------------------------------------------------------------------------- -#
  
  if (p_opts$gui) {
    gWidgets::addHandlerUnrealize(mess_lab, handler = function(h, ...) {
      return(FALSE)
    })    # Allow message lab to be closed since processing ended .
    gWidgets::dispose(mess_lab)
  }
  unlink(file.path(out_prod_folder, "Temp"), recursive = TRUE)
  return("DONE")
}



