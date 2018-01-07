#' @title MODIStsp main processing function
#' @description Main processing function of MODIStsp. Takes as input processing
#'   parameters specified by the user using MODIStsp_GUI and saved in
#'   MODIStsp_Previous.json (Interactive mode), or a user specified JSON file
#'   (non-interactive mode) and performs all required processing.
#' @details After retrieving the input processing options, the function
#'   1. Accesses NASA http or ftp archive to determine the list of files to be
#'      downloaded/processed (or, in case of offline processing, get the list
#'      of already available hdf files present in `out_mod_folder`);
#'   2. Performs all required processing steps on each date (download,
#'      reprojection, resize, mosaicing, Spectral Indexes and Quality indicators
#'      computation);
#'   3. Creates virtual files of the processed time series.
#'
#' Reprojection and resize is dealt with by accessing gdal routines through the
#' [`gdalUtils`](https://cran.r-project.org/web/packages/gdalUtils/index.html)
#' package.
#' Extraction of bitfields from Quality layers is done though bitwise computation
#' Checks are done in order to not re-download already existing HDF images, and not
#' reprocess already processed dates (if the user did not specify that)
#'
#' @param sel_prod `character` Name of selected MODIS product.
#' @param start_date `character` Start date for images download and preprocessing
#'  (yyyy.mm.dd).
#' @param end_date `character` End date for images download and preprocessing
#'  (yyyy.mm.dd).
#' @param out_folder `character` Main output folder.
#' @param out_folder_mod `character` Output folder for original HDF storage.
#' @param reprocess `character ["Yes" | "No"]` If Yes, reprocess data for already
#'  existing dates.
#' @param delete_hdf `character ["Yes" | "No"]` If Yes, delete original HDF after
#'  completion.
#' @param sensor `character ["Terra"| "Aqua" | "Both"]` MODIS platform to be considered.
#'   (Ignored for MCD* products).
#' @param https `list` http addresses for download of HDF of selected product.
#' @param ftps `list` ftp addresses for download of HDF of selected product.
#' @param download_server `character ["http" | "ftp" | "offline"]` service to be used for
#'  download.
#' @param user `character` Username for NASA http server.
#'   ([urs.earthdata.nasa.gov/home](https://urs.earthdata.nasa.gov/home)).
#' @param password `character` Password for NASA http server
#'   ([urs.earthdata.nasa.gov/home](https://urs.earthdata.nasa.gov/home)).
#' @param start_x `integer [0-35]` Start horizontal tile.
#' @param start_y `integer [0-17]` Start vertical tile.
#' @param end_x `integer [0-35]` End horizontal tile.
#' @param end_y `integer [0-17]` End vertical tile.
#' @param full_ext `characrter ["Full_Ext" | "Resized"]` If \"Full_Ext\", process
#'   the entire extent of the selected tiles. Otherwise, crop the output to
#'   output bbox.
#' @param bbox `numeric(4)` Output bounding box (xmin, xmax, ymin, ymax) in
#'   out_proj coordinate system.
#' @param out_format `character ["ENVI" | "GTiff"]` Desired output format.
#' @param compress `character ["None" | "PACKBITS" | "LZW" | "DEFLATE"]`
#'   Compression method for GTiff outputs (Ignored if `out_format == ENVI`)
#' @param out_res_sel `character ["Native" | "Resampled"]` Indicates if the native
#'  resolution of the product or a user supplied one is to be used.
#' @param out_res `float` Output resolution (in output projection measurement
#'  unit). Ignored if out_res_sel == "Native".
#' @param native_res `float` Native resolution of MODIS product to be processed.
#' @param tiled `integer [0 | 1]` 1 = tiled product; 0 = non-tiled product
#'   (resolution 0.05 deg - latlong projection).
#' @param mod_proj_str `character` proj4 string of MODIS product native projection.
#' @param outproj_str `character` proj4 string of selected output projection.
#' @param nodata_in `numeric array` Original NoData values of original layers of
#'  the selected MODIS product.
#' @param nodata_out `numeric array` Target NoData values of MODIS original layers
#'  (Ignored if nodata_change == FALSE).
#' @param nodata_change `character ["Yes" | "No"]` if Yes, NoData are set to nodata_out
#'   in output rasters.
#' @param scale_val `character ["Yes" | "No"]` If == "Yes", scale and offset are applied to
#'  original MODIS layers, and Spectral Indexes are saved as floating point. If
#'  == "No", no rescaling is done and Spectral Indexes are saved as integer, with a
#'  10000 scaling factor.
#' @param rts `character ["Yes" | "No"]` If "Yes", create RasterStack time series
#' @param datatype `character array` datatypes of original MODIS bands
#'  (e.g., "INT2S").
#' @param bandsel `integer 0/1 array` of length equal to number of original
#'  layers of the selected product set to 1 for bands to be processed.
#'@param bandnames `character array` Abbreviated Names of original layers of the
#'  selected product (used to build output file names).
#' @param indexes_bandsel `integer 0/1 array` array of length equal to the number
#'   of Spectral Indexes available for the product (standard + user-provided),
#'   set to  1 for indexes to be processed.
#' @param indexes_bandnames `character array` Abbreviated Names of SIs available
#'   for the selected product (used to build output file names of SIs).
#' @param indexes_formula `character array` formulas of SIs available for the
#'   selected product (standard and custom).
#' @param indexes_nodata_out `numeric array` NoData values to be used for SIs
#' @param quality_bandnames `character array` Abbreviated Names of Quality
#'  Indicators available for the selected product (used to build output file
#'  names of QIs).
#' @param quality_bandsel `integer 0|1 array` array of length equal to number
#'   of available QIs, set to  1 for indexes to be processed.
#' @param quality_bitN `character array` with length equal to the number QIs
#'   available for the selected product. Each entry contains the position of the
#'   bits corresponding to a QI (e.g., 0-1) in its "source" MODIS layer.
#' @param quality_source `character array` which connects each QI to its "source"
#'   original MODIS layer (multiple QIs share the same "source", since they are
#'   derived from different bits of the bit-encoded layer).
#' @param quality_nodata_in `integer` Always set to 255.
#' @param quality_nodata_out `integer` Always set to 255.
#' @param file_prefixes `character` output file prefix of selected product
#'   (e.g., MOD13Q1). Used to build output filenames.
#' @param main_out_folder `character` Main folder for storage of MODIStsp
#'   time series.
#' @param resampling `character ["near" | "bilinear" | "cubic" | "lanczos" | "mode"]`
#'   Resampling method to be used by `gdalwarp`.
#' @param ts_format `character ["None" | "ENVI Meta Files" | "GDAL vrt files" |
#'  "ENVI and GDAL"]` Selected virtual time series format.
#' @param gui `logical` Indicates if processing was called starting from an
#'  interactive environment or not. If FALSE, processing messages are sent to a
#'  log file instead than to the console, and gWidgets messages are suppressed
#' @param use_aria `logical` If TRUE, aria2c is used to accelerate download
#'   (if available !).
#' @param download_range `character ["full" | "seasonal"]` If "full", all the
#'   available images between the starting and the ending dates are downloaded;
#'   If "seasonal", only the images included in the season are downloaded
#'   (e.g: if the starting date is 2005-12-01 and the ending is 2010-02-31, only
#'   the images of December, January and February from 2005 to 2010 - excluding
#'   2005-01, 2005-02 and 2010-12 - are downloaded).
#' @param scale_factor `numeric array` of length equal to the number of original
#'   layers of the selected product, containing scale factors to be applied to each
#'   original layer to convert it to "correct" measure units.
#' @param offset `numeric array` of length equal to the number of original
#'   layers of the selected product, containing offsets to be applied to each
#'   original layer to convert it to "correct" measure units.
#' @param n_retries `numeric` maximum number of retries on download functions.
#'   In case any download function fails more than `n_retries` times consecutively,
#'   MODIStsp_process will abort, Default: 20
#' @param verbose `logical` If FALSE, suppress processing messages, Default: TRUE
#' @return The function is called for its side effects.
#' @author Lorenzo Busetto, phD (2014-2017) \email{lbusett@@gmail.com}
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @note Thanks Tomislav Hengl and Babak Naimi, whose scripts made the starting point for
#'   development of this function ([ModisDownload](http://r-gis.net/?q=ModisDownload);
#'   [Download_and_resampling_of_MODIS_images](http://spatial-analyst.net/wiki/index.php?title=Download_and_resampling_of_MODIS_images))
#' @note License: GPL 3.0
#' @rdname MODIStsp_process
#' @importFrom gdalUtils gdalinfo gdal_translate gdalwarp gdalbuildvrt
#' @importFrom gWidgets gwindow glabel dispose gconfirm svalue addHandlerUnrealize
#' @importFrom httr content GET authenticate progress timeout
#' @importFrom raster raster writeRaster
#' @importFrom tools file_path_sans_ext
#' @importFrom parallel detectCores
#' @importFrom stringr str_sub

MODIStsp_process <- function(sel_prod, start_date, end_date, out_folder,
                             out_folder_mod, reprocess = "Yes",
                             delete_hdf = "No",
                             sensor, download_server, user, password,
                             https, ftps, start_x, start_y, end_x, end_y,
                             full_ext, bbox, out_format, compress,
                             out_res_sel, out_res, native_res, tiled,
                             mod_proj_str, outproj_str,
                             nodata_in, nodata_out, nodata_change,
                             scale_val, scale_factor, offset,
                             rts, datatype,
                             bandsel, bandnames,
                             indexes_bandsel, indexes_bandnames,
                             indexes_formula, indexes_nodata_out,
                             quality_bandnames, quality_bandsel,
                             quality_bitN, quality_source,
                             quality_nodata_in, quality_nodata_out,
                             file_prefixes, main_out_folder,
                             resampling, ts_format,
                             use_aria       = TRUE,
                             download_range = "full",
                             gui            = TRUE,
                             n_retries, 
                             verbose) {
  
  mess_text <- "MODIStsp --> Starting processing"
  # initialize processing messages in case of interactive execution ----
  
  if (gui) {
    #nocov start
    mess     <- gWidgets::gwindow(title = "Processing Status",
                                  width = 400,
                                  height = 40)
    mess_lab <- gWidgets::glabel(text = paste("---", mess_text, "---"),
                                 editable = FALSE,
                                 container = mess)
    Sys.sleep(0.05)
    #nocov end
  } else {
    mess_lab <- NULL
  }
  
  process_message(mess_text, gui, mess_lab, verbose)
  
  #   __________________________________________________________________________
  #   Intialize processing variables                                        ####
  
  # as.integer(NoData) cause NoData ranges (e.g. 249-255) to be
  # suppressed. So, in this cases NoData values will not be recognised. This
  # problem will be solved in future with a cycle on NoData ranges.
  #
  # Fix for products with multiple NoData values
  if (any(is.na(as.numeric(nodata_in)))) {
    nodata_in[is.na(as.numeric(nodata_in))] <- "None"
  }
  
  if (any(is.na(as.numeric(quality_nodata_in)))) {
    quality_nodata_in[is.na(as.numeric(quality_nodata_in))] <- "None"
  }
  
  # if NoData change set to no, set out_nodata to nodata_in
  if (nodata_change == "No") {
    nodata_out <- nodata_in
  }
  
  # set-up processing folders ----
  
  # Folder for HDF storage
  dir.create(out_folder_mod, recursive = TRUE, showWarnings = FALSE)
  
  # main output folder --> subfolder of "out_folder" named after the selected
  # MODIS product
  out_prod_folder <- file.path(out_folder, main_out_folder)
  dir.create(out_prod_folder, showWarnings = FALSE, recursive = TRUE)
  
  # get start/end years from start_date/end_date
  start_year <- unlist(strsplit(start_date, "[.]"))[1]
  end_year   <- unlist(strsplit(end_date, "[.]"))[1]
  
  # workaround to avoid generating error if no indexes and/or quality bands are
  # present/computable for the selected product
  
  if (length(indexes_bandnames) == 0) indexes_bandsel <- integer(0)
  if (length(quality_bandnames) == 0) quality_bandsel <- integer(0)
  
  # Save original choice of bands in bandsel_orig_choice (bandsel is later
  # modified to set to 1 all bands needed for indexes and quality
  bandsel_orig_choice <- bandsel
  
  #  ___________________________________________________________________________
  #  Build a matrix which associates each SI or QI available for the selected
  #  product with the original layers required to compute it
  #  This allows later to force processing of layers needed to compute a QI or
  #  SI even if it was not selected by the user
  bands_indexes_matrix <- set_bandind_matrix(bandnames,
                                             bandsel,
                                             indexes_bandnames,
                                             indexes_bandsel,
                                             indexes_formula,
                                             quality_bandnames,
                                             quality_bandsel,
                                             quality_source)
  # ___________________________________________________________________________
  # Double check to see if aria2c executable is present and on the PATH. On non
  # interactive execution, if aria2c is not found, use_aria is forced to FALSE
  
  check_aria <- Sys.which("aria2c")
  if (check_aria == "") use_aria <- FALSE
  
  #   __________________________________________________________________________
  #   Start Working.                                                        ####
  
  combined   <- FALSE
  # check which platforms were selected,
  if (sensor[1] == "Both") {
    sensor   <- c("Terra", "Aqua")
  }
  
  if (sensor[1] == "Combined") {
    sensor   <- c("Terra")
    combined <- TRUE
  }
  
  #  If both platforms selected, do a cycle. Process first Terra then Aqua.
  
  for (sens_sel in sensor) {
    
    
    http        <- https[[sens_sel]]
    ftp         <- ftps[[sens_sel]]
    file_prefix <- file_prefixes[[sens_sel]]
    
    # check if product is available on ftp
    
    if (download_server == "ftp" & ftp == "Not Available") {
      if (gui) gWidgets::dispose(mess_lab) #nocov
      stop("Product ", sel_prod, " is not available over ftp.\n",
           "Please switch to http download! Aborting!")
    }
    
    
    # __________________________________________________________________________
    # Start Cycle on required years - needed since in case of "sesonal"     ####
    # download the dates to be downloaded need to be "tweaked" with respect
    # to start_date/end_date
    
    for (yy in start_year:end_year) {
      
      #   ______________________________________________________________________
      #   Retrieve list of files to be downloaded/processed from NASA       ####
      #   http/ftp servers
      
      # First, retrieve acquisition dates of all available MODIS hdfs for the
      # selected product in yy
      date_dirs_all   <- get_mod_dirs(http, ftp, download_server,
                                      user, password,
                                      yy,
                                      n_retries,
                                      gui,
                                      out_folder_mod,
                                      .Platform)
      
      # overwrite download_server with the setting used in the end to retrieve
      # folders. Used in scheduled execution in case http fails and download
      # switched automatically to ftp
      download_server <- attr(date_dirs_all, "server")
      
      dates <- get_yeardates(download_range,
                             yy,
                             start_year, end_year,
                             start_date, end_date)
      
      # Processing status message
      mess_text <- paste("Retrieving list of available `",
                         ifelse(combined, "Combined", sens_sel),
                         "` Files for Year",
                         as.character(yy))
      process_message(mess_text, gui, mess_lab, verbose)
      
      # Get a list of the folders containing HDF images required (Corresponding
      # to the subfolders in lpdaac corresponding to selected product, dates and
      # current year under processing)
      
      # First, find the folders in lpdaac corresponding to the required dates
      date_dirs <- get_mod_dates(dates = dates, date_dirs =  date_dirs_all)
      
      if (length(date_dirs) > 0 | download_server == "offline") {
        modislist <- NULL
        # Start Cycling on directories containing images to be downloaded and
        # identify the required ones (i.e., the ones corresponding to selected
        # dates)
        for (date in seq_along(date_dirs)) {
          #Create the date string
          date_name <- sub(sub(
            pattern = "\\.", replacement = "_", date_dirs[date]),
            pattern = "\\.", replacement = "_", date_dirs[date]
          )
          # transform date to year
          year      <- strftime(as.Date(date_name, "%Y_%m_%d" ), format = "%Y")
          # transform date to DOY
          DOY       <- strftime(as.Date(date_name, "%Y_%m_%d" ), format = "%j")
          
          # check if all foreseen output rasters already exist. If so, skip the
          # date. Otherwise start processing
          check_files <- FALSE
          check_files <- check_files_existence(out_prod_folder,
                                               file_prefix,
                                               yy,
                                               DOY,
                                               bandnames,
                                               bandsel_orig_choice,
                                               indexes_bandnames,
                                               indexes_bandsel,
                                               quality_bandnames,
                                               quality_bandsel,
                                               out_format)
          
          # If not all output files are already present or reprocess = "Yes",
          # start downloading hdfs
          if (check_files == FALSE | reprocess == "Yes") {
            
            # Create vector of image names required (corresponding to the
            # required tiles for the current date)
            modislist <- get_mod_filenames(http, ftp,
                                           used_server = download_server,
                                           user, password, n_retries,
                                           date_dir = date_dirs[date],
                                           v = seq(from = start_y, to = end_y),
                                           h = seq(from = start_x, to = end_x),
                                           tiled, out_folder_mod,
                                           gui)
            
            # -----------------------------------------------------------------#
            # Download and process Images in modislist vector               ####
            
            if (length(modislist) > 0) {
              
              #- ------------------------------------------------------------ -#
              #  STEP 1: Download images (If HDF file already in            ####
              #  out_mod_folder, it is not redownloaded !!!!
              
              MODIStsp_download(modislist, out_folder_mod,
                                download_server, http, ftp, n_retries, use_aria,
                                date_dirs[date], year,
                                DOY, user, password, sens_sel,
                                date_name, gui, mess_lab, verbose)
              
              mess_text <- paste0("[", date(), "] ", length(modislist),
                                  " files for date: ", date_dirs[date],
                                  " were successfully downloaded!")
              process_message(mess_text, gui, mess_lab, verbose)
              
              # ______________________________________________________________
              # After all required tiles for the date are downloaded, start
              # geoprocessin
              
              # ________________________________________________________________
              # STEP 2: identify the layers to be processed.                ####
              # (original, indexes and  quality bands).
              # At the end of this step, "bandsel" is recreated as the union of
              # the bands selected by the user and the bands required to
              # compute indexes and quality bands
              
              req_bands_indexes <- get_reqbands(bands_indexes_matrix,
                                                indexes_bandsel,
                                                indexes_bandnames,
                                                quality_bandsel,
                                                quality_bandnames,
                                                out_prod_folder,
                                                file_prefix,
                                                yy, DOY,
                                                out_format,
                                                reprocess)
              
              # Create the final vector of bands required for processing (
              # bands chosen by the user + bands required for indexes and
              # quality bands)
              bandsel <- as.integer(as.logical(
                bandsel_orig_choice + apply(req_bands_indexes, 1, sum)))
              
              # Create a delbands array. Contains info on wether original
              # downloaded bands has to be deleted
              delbands <- bandsel - bandsel_orig_choice
              
              # _______________________________________________________________
              # STEP 3: process the required original MODIS layers          ####
              
              # Cycle on MODIS original layers
              for (band in which(bandsel == 1)) {
                
                dir.create(file.path(out_prod_folder, bandnames[band]),
                           showWarnings = FALSE, recursive = TRUE)
                
                # Create name for the final file to be saved
                outrep_file   <- file.path(
                  out_prod_folder, bandnames[band],
                  paste0(file_prefix, "_",
                         paste0(bandnames[band], "_", yy, "_", DOY),
                         ifelse(out_format == "GTiff", ".tif", ".dat"))
                )
                
                if (!file.exists(outrep_file) | reprocess == "Yes") {
                  
                  MODIStsp_process_bands(
                    out_folder_mod, modislist,
                    outproj_str, mod_proj_str, sens_sel,
                    band, bandnames[band], date_name,
                    datatype[band],
                    nodata_in[band], nodata_out[band],
                    full_ext, bbox,
                    scale_val, scale_factor[band], offset[band],
                    out_format, outrep_file, compress,
                    out_res_sel, out_res, resampling,
                    gui, mess_lab, verbose
                  )
                }
              }	# END Cycle on available MODIS Bands
              
              #  --------------------------------------------------------------#
              # STEP 4: If any Indexes selected, compute them               ####
              
              # cycle on selected indexes
              for (band in which(indexes_bandsel == 1)) {
                indexes_band <- indexes_bandnames[band]
                formula      <- indexes_formula[band]
                mess_text    <- paste("Computing", sens_sel, indexes_band,
                                      "for date:", date_name)
                process_message(mess_text, gui, mess_lab, verbose)
                
                out_filename <- file.path(
                  out_prod_folder,
                  indexes_band,
                  paste0(file_prefix, "_", indexes_band, "_", yy, "_", DOY,
                         ifelse(out_format == "GTiff", ".tif", ".dat"))
                )
                
                # If file not existing and reprocess = No, compute the index and
                # save it
                if (!file.exists(out_filename) | reprocess == "Yes") {
                  MODIStsp_process_indexes(out_filename,
                                           out_prod_folder,
                                           formula,
                                           bandnames,
                                           nodata_out,
                                           indexes_nodata_out[band],
                                           file_prefix,
                                           compress,
                                           yy,
                                           out_format,
                                           DOY,
                                           scale_val)
                }
              }
              
              #  --------------------------------------------------------------#
              # STEP 5: If any Quality indicators selected, compute them    ####
              
              # cycle on selected quality indicators
              for (band in which(quality_bandsel == 1)) {
                
                # indicator name
                quality_band  <- quality_bandnames[band]
                mess_text     <- paste("Computing", quality_band, "for date:",
                                       date_name)
                process_message(mess_text, gui, mess_lab, verbose)
                #  Original MODIS layer containing data of the indicator
                source        <- quality_source[band]
                #  bitfields corresponding to indicator within source
                bitN          <- quality_bitN[band]
                nodata_qa_in  <- quality_nodata_in[band]
                nodata_qa_out <- quality_nodata_out[band]
                nodata_source <- nodata_out[grep(source, bandnames)]
                
                out_filename <- file.path(
                  out_prod_folder, quality_band,
                  paste0(file_prefix, "_", quality_band, "_", yy, "_", DOY,
                         ifelse(out_format == "GTiff", ".tif", ".dat")
                  )
                )
                
                # If file not existing or reprocess = Yes, compute the indicator
                # and save it
                if (!file.exists(out_filename) | reprocess == "Yes") {
                  
                  # get filename of the (processed) original MODIS layer which
                  # contains the required bit fields input data
                  in_source_file <- file.path(
                    out_prod_folder, source,
                    paste0(file_prefix, "_", source, "_", yy, "_", DOY,
                           ifelse(out_format == "GTiff", ".tif", ".dat"))
                  )
                  
                  MODIStsp_process_QA_bits(out_filename,
                                           in_source_file,
                                           bitN,
                                           out_format,
                                           nodata_source,
                                           nodata_qa_in,
                                           nodata_qa_out,
                                           compress)
                }
              }
              
              #  --------------------------------------------------------------#
              #  STEP 6: Delete files corresponding to bands not needed (i.e., 
              #  bands required for indexes or quality computation, but not
              #  requested by the user.
              
              unlink(file.path(out_prod_folder,
                               bandnames[which(delbands == 1)]),
                     recursive = TRUE)
              
              #- ------------------------------------------------------------ -#
              # If deletion selected, delete the HDF files in out_folder_mod####
              # directory
              
              if (delete_hdf == "Yes") {
                unlink(file.path(out_folder_mod, modislist))
              } 
              
            } else {
              mess_text <- paste0("[", date(),
                                  "] No images available for selected area",
                                  "in date ", date_dirs[date])
              process_message(mess_text, gui, mess_lab, verbose)
            }
            
          } else {
            mess_text <- paste0(
              "[", date(),
              "] All Required output files for date ",
              date_name, " are already existing - Doing Nothing!\n",
              "Set Reprocess to \"Yes\" to reprocess existing data!"
            )
            process_message(mess_text, gui, mess_lab, verbose)
            
          }
          
        }
        
      } else {
        mess_text <- paste0("[", date(), "] No available data for year: ", yy,
                            " for Sensor ", sens_sel, " in selected dates.")
        process_message(mess_text, gui, mess_lab, verbose)
      }
      
    }	# End Cycling on selected years
    
    bandsel <- bandsel_orig_choice  # reset bandsel to original user's choice
    
  } # End cycling on sensors
  
  #   __________________________________________________________________________
  #   STEP 7: Create vrt files of time series - original, SI and QI        ####
  
  MODIStsp_vrt_create(sensor,
                      out_prod_folder,
                      bandnames, bandsel, nodata_out,
                      indexes_bandnames, indexes_bandsel, indexes_nodata_out,
                      quality_bandnames, quality_bandsel, quality_nodata_out,
                      file_prefixes,
                      ts_format, out_format, rts, 
                      verbose = verbose)
  
  # ____________________________________________________________________________
  #  Close GUI and clean-up                                                 ####
  
  
  if (gui) {
    #nocov start
    gWidgets::addHandlerUnrealize(mess_lab, handler = function(h, ...) {
      return(FALSE)
    })		# Allow message lab to be closed since processing ended .
    gWidgets::dispose(mess_lab)
    #nocov end
  }
  unlink(file.path(out_prod_folder, "Temp"), recursive = TRUE)
  return("DONE")
}
