#' @title MODIStsp main processing function
#' @description Main processing function of MODIStsp. Takes as input processing
#'   parameters specified by the user and performs all required processing.
#' @details After retrieving the input processing options, the function
#'   1. Accesses NASA http archive to determine the list of files to be
#'      downloaded/processed (or, in case of offline processing, get the list
#'      of already available hdf files present in `out_mod_folder`);
#'   2. Performs all required processing steps on each date (download,
#'      reprojection, resize, mosaicing, Spectral Indexes and Quality indicators
#'      computation);
#'   3. Creates virtual files of the processed time series.
#'
#' Reprojection and resize is dealt with by accessing gdal routines through the
#' [`gdalUtilities`](https://CRAN.R-project.org/package=gdalUtilities)
#' package.
#' Extraction of bitfields from Quality layers is done though bitwise computation
#' Checks are done in order to not re-download already existing HDF images, and not
#' reprocess already processed dates (if the user did not specify that)
#' @param proc_opts `data.frame` containing all processing parameters, as
#'  passed from the MODIStsp GUI, or created in `MODIStsp` by joining
#'  explicitly passed arguments with a (not mandatory) options file.
#' @param n_retries `numeric` maximum number of retries on download functions.
#'   In case any download function fails more than `n_retries` times consecutively,
#'   MODIStsp_process will abort, Default: 20
#' @param verbose `logical` If FALSE, suppress processing messages, Default: TRUE
#' @param parallel `logical` If TRUE (default), the function is run using parallel
#'  processing, to speed-up the computation for large rasters (with a maximum
#'  of 8 cores).
#'  The number of cores is automatically determined; specifying it is also
#'  possible (e.g. `parallel = 4`). In this case, more than 8 cores can be
#'  specified. If FALSE (default), single core processing is used.
#' @return The function is called for its side effects.
#' @author Lorenzo Busetto, phD (2014-2017)
#' @author Luigi Ranghetti, phD (2015)
#' @note Thanks Tomislav Hengl and Babak Naimi, whose scripts made the starting point for
#'   development of this function ([ModisDownload](https://r-gis.net/?q=ModisDownload);
#'   [Download_and_resampling_of_MODIS_images](https://en.wikipedia.org/wiki/Regression-kriging?title=Download_and_resampling_of_MODIS_images))
#' @note License: GPL 3.0
#' @export
#' @importFrom jsonlite read_json
#' @importFrom sf st_crs

MODIStsp_process <- function(proc_opts,
                             n_retries,
                             verbose = TRUE,
                             parallel = TRUE) {

  # Based on sel_prod, retrieve needed variables from prod_opts file

  prod_opt_list <- load_prodopts()
  prod_opts <- prod_opt_list[[proc_opts$selprod]][[proc_opts$prod_version]]

  cust_ind      <- jsonlite::read_json(system.file("ExtData","MODIStsp_indexes.json",
                                                   package = "MODIStsp"))

  if (length(cust_ind) == 1) {
    cust_ind <- NULL
  } else {
    cust_ind  <- cust_ind[[proc_opts$selprod]][[proc_opts$selprod]]
  }

  prod_opts$indexes_bandnames <- c(prod_opts$indexes_bandnames,
                                   cust_ind$indexes_bandnames)
  prod_opts$indexes_fullnames <- c(prod_opts$indexes_fullnames,
                                   cust_ind$indexes_fullnames)
  prod_opts$indexes_formulas <- c(prod_opts$indexes_formulas,
                                  cust_ind$indexes_formulas)
  prod_opts$indexes_nodata_out <- c(prod_opts$indexes_nodata_out,
                                    cust_ind$indexes_nodata_out)

  if (prod_opts$tiled == 0) {
    tiled = FALSE
    # mod_proj_str <- CRS("+init=epsg:4008 +proj=longlat +ellps=clrk66 +no_defs") #nolint

    # EPSG for proj definition on latlon data (4008)
    mod_proj_str <- sf::st_crs(4008) #nolint

    prod_opts$native_res <- format(
      as.numeric(prod_opts$native_res) * (0.05 / 5600)
    )
  } else {
    tiled = TRUE
    # default WKT for MODIS gridded data
    mod_proj_str <- sf::st_crs("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")
  }

  if (proc_opts$output_proj == "MODIS Sinusoidal") {
    outproj_str <- sf::st_crs("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")
  } else {
    outproj_str <- sf::st_crs(check_projection(proc_opts$output_proj))
  }

  if (proc_opts$out_res_sel == "Native") {
    proc_opts$out_res <- prod_opts$native_res
  }

  nodata_in  <- prod_opts$nodata_in
  nodata_out <- prod_opts$nodata_out

  offset       <- prod_opts$offset
  scale_factor <- prod_opts$scale_factor
  datatype     <- prod_opts$datatype
  bandnames    <- prod_opts$bandnames

  indexes_bandnames  <- prod_opts$indexes_bandnames
  indexes_formula    <- prod_opts$indexes_formula
  indexes_nodata_out <- prod_opts$indexes_nodata_out

  quality_bandnames  <- prod_opts$quality_bandnames
  quality_bitN       <- prod_opts$quality_bitN
  quality_source     <- prod_opts$quality_source
  quality_nodata_in  <- prod_opts$quality_nodata_in
  quality_nodata_out <- prod_opts$quality_nodata_out

  file_prefixes      <- prod_opts$file_prefix
  https              <- prod_opts$http
  main_out_folder    <- prod_opts$main_out_folder

  start_time <- Sys.time()

  if (proc_opts$spatmeth == "tiles") {
    full_ext = TRUE
  } else {
    full_ext = FALSE
  }

  # outproj_str <- sf::st_as_text(sf::st_crs(check_projection(outproj_str)))
  #   __________________________________________________________________________
  #   Intialize processing variables                                        ####

  # Set unrecognised values to None.
  # Recognised values are numerics, integer ranges (separate by ":")
  # and integer vectors (separate by ",").
  nodata_in_unrecognised <- !grepl("^[0-9\\,\\:\\-]+$",nodata_in) &
    is.na(suppressWarnings(as.numeric(nodata_in)))
  if (any(nodata_in_unrecognised)) {
    nodata_in[nodata_in_unrecognised] <- "None"
  }

  quality_nodata_in_unrecognised <- !grepl("^[0-9\\,\\:\\-]+$",quality_nodata_in) & #nolint
    is.na(suppressWarnings(as.numeric(quality_nodata_in)))
  if (any(quality_nodata_in_unrecognised)) {
    quality_nodata_in[quality_nodata_in_unrecognised] <- "None"
  }

  # if NoData change set to no, set out_nodata to nodata_in
  # and take only the last values listed for each band

  if (!proc_opts$nodata_change) {
    nodata_out <- nodata_in
  }

  # set-up processing folders ----

  # Folder for HDF storage
  dir.create(proc_opts$out_folder_mod, recursive = FALSE, showWarnings = FALSE)

  # main output folder --> subfolder of "out_folder" named after the selected
  # MODIS product
  out_prod_folder <- file.path(proc_opts$out_folder, main_out_folder)

  # Preliminary check for MOD19 products: allow processing ONLY if a single tile
  # is involved.

  if (basename(out_prod_folder) == "MAIA_Land_Surf_BRF_500") {
    if ((proc_opts$start_x != proc_opts$end_x) | (proc_opts$start_y != proc_opts$end_y)) {
      warning("Processing for MCD19* products is possible only for one tile at a time! Processing aborted!") #nolint
      return()
    }
  }


  dir.create(out_prod_folder, showWarnings = FALSE, recursive = FALSE)

  mess_text <- "MODIStsp --> Starting processing"
  # initialize processing messages in case of interactive execution ----
  gui <- FALSE
  process_message(mess_text, verbose)

  # get start/end years from start_date/end_date
  start_year <- unlist(strsplit(proc_opts$start_date, "[.]"))[1]
  end_year   <- unlist(strsplit(proc_opts$end_date, "[.]"))[1]

  # workaround to avoid generating error if no indexes and/or quality bands are
  # present/computable for the selected product

  if (length(indexes_bandnames) == 0) indexes_bandsel <- integer(0)
  if (length(quality_bandnames) == 0) quality_bandsel <- integer(0)

  # Save original choice of bands in bandsel_orig_choice (bandsel is later
  # modified to set to 1 all bands needed for indexes and quality

  bandsel_ind <- which(bandnames %in% proc_opts$bandsel)
  bandsel <- rep(0, length(bandnames))
  bandsel[bandsel_ind] <- 1
  bandsel_orig_choice  <- bandsel

  indsel_ind <- which(indexes_bandnames %in% proc_opts$indexes_bandsel)
  indexes_bandsel <- rep(0, length(indexes_bandnames))
  indexes_bandsel[indsel_ind] <- 1

  qsel_ind <- which(quality_bandnames %in% proc_opts$quality_bandsel)
  quality_bandsel <- rep(0, length(quality_bandnames))
  quality_bandsel[qsel_ind] <- 1

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

  if (proc_opts$downloader == "aria2c") {
    use_aria <- TRUE
  } else {
    use_aria <- FALSE
  }
  check_aria <- Sys.which("aria2c")
  if (check_aria == "") use_aria <- FALSE


  #   __________________________________________________________________________
  #   Start Working.                                                        ####

  combined   <- FALSE

  # check which platforms were selected,
  if (proc_opts$sensor[1] == "Both") {
    sensor   <- c("Terra", "Aqua")
  } else {
    sensor <- proc_opts$sensor
  }

  if (proc_opts$sensor[1] == "Combined") {
    sensor   <- c("Terra")
    combined <- TRUE
  }
  #  If both platforms selected, do a cycle. Process first Terra then Aqua.

  for (sens_sel in sensor) {

    http        <- https[[sens_sel]]
    file_prefix <- file_prefixes[[sens_sel]]

    # __________________________________________________________________________
    # Start Cycle on required years - needed since in case of "sesonal"     ####
    # download the dates to be downloaded need to be "tweaked" with respect
    # to start_date/end_date

    # Processing status message
    mess_text <- paste("Accessing http server at: ", http)
    process_message(mess_text, verbose)

    for (yy in start_year:end_year) {

      #   ______________________________________________________________________
      #   Retrieve list of files to be downloaded/processed from NASA       ####
      #   http server

      # First, retrieve acquisition dates of all available MODIS hdfs for the
      # selected product in yy
      date_dirs_all   <- get_mod_dirs(http, proc_opts$download_server,
                                      proc_opts$user, proc_opts$password,
                                      yy,
                                      n_retries,
                                      gui,
                                      proc_opts$out_folder_mod)

      # overwrite download_server with the setting used in the end to retrieve
      # folders.

      download_server <- attr(date_dirs_all, "server")

      if (download_server == "unreachable") {return(invisible(NULL))}

      dates <- get_yeardates(proc_opts$download_range,
                             yy,
                             start_year, end_year,
                             proc_opts$start_date,
                             proc_opts$end_date)

      # Processing status message
      mess_text <- paste("Retrieving list of available `",
                         ifelse(combined, "Combined", sens_sel),
                         "` Files for Year",
                         as.character(yy))
      process_message(mess_text, verbose)

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
                                               proc_opts$out_format)

          # If not all output files are already present or reprocess = "Yes",
          # start downloading hdfs
          if (check_files == FALSE | proc_opts$reprocess) {

            # Create vector of image names required (corresponding to the
            # required tiles for the current date)
            modislist <- get_mod_filenames(http,
                                           used_server = download_server,
                                           proc_opts$user, proc_opts$password,
                                           n_retries,
                                           date_dir = date_dirs[date],
                                           v = seq(from = proc_opts$start_y, to = proc_opts$end_y),
                                           h = seq(from = proc_opts$start_x, to = proc_opts$end_x),
                                           tiled, proc_opts$out_folder_mod,
                                           gui)

            # -----------------------------------------------------------------#
            # Download and process Images in modislist vector               ####

            if (length(modislist) > 0) {

              #- ------------------------------------------------------------ -#
              #  STEP 1: Download images (If HDF file already in            ####
              #  out_folder_mod, it is not redownloaded !!!!

              MODIStsp_download(modislist, proc_opts$out_folder_mod,
                                download_server, http, n_retries, use_aria,
                                date_dirs[date], year,
                                DOY, proc_opts$user, proc_opts$password, sens_sel,
                                date_name, gui, verbose)

              mess_text <- paste0("[", date(), "] ", length(modislist),
                                  " files for date: ", date_dirs[date],
                                  " were successfully downloaded!")
              process_message(mess_text, verbose)

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
                                                proc_opts$out_format,
                                                proc_opts$reprocess)

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
                         ifelse(proc_opts$out_format == "GTiff", ".tif", ".dat"))
                )

                if (!file.exists(outrep_file) | proc_opts$reprocess) {

                  MODIStsp_process_bands(
                    proc_opts$out_folder_mod, modislist,
                    outproj_str, mod_proj_str, sens_sel,
                    band, bandnames[band], date_name,
                    datatype[band],
                    nodata_in[band], nodata_out[band],
                    full_ext, proc_opts$bbox,
                    proc_opts$scale_val, scale_factor[band], offset[band],
                    proc_opts$out_format, outrep_file, proc_opts$compress,
                    proc_opts$out_res_sel, proc_opts$out_res, proc_opts$resampling,
                    proc_opts$nodata_change,
                    gui, verbose, parallel
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
                process_message(mess_text, verbose)

                out_filename <- file.path(
                  out_prod_folder,
                  indexes_band,
                  paste0(file_prefix, "_", indexes_band, "_", yy, "_", DOY,
                         ifelse(proc_opts$out_format == "GTiff", ".tif", ".dat"))
                )

                # If file not existing and reprocess = No, compute the index and
                # save it
                if (!file.exists(out_filename) | proc_opts$reprocess) {
                  MODIStsp_process_indexes(out_filename,
                                           out_prod_folder,
                                           formula,
                                           bandnames,
                                           nodata_out,
                                           indexes_nodata_out[band],
                                           file_prefix,
                                           proc_opts$compress,
                                           yy,
                                           proc_opts$out_format,
                                           DOY,
                                           proc_opts$scale_val)
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
                process_message(mess_text, verbose)
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
                         ifelse(proc_opts$out_format == "GTiff", ".tif", ".dat")
                  )
                )

                # If file not existing or reprocess = Yes, compute the indicator
                # and save it
                if (!file.exists(out_filename) | proc_opts$reprocess) {

                  # get filename of the (processed) original MODIS layer which
                  # contains the required bit fields input data
                  in_source_file <- file.path(
                    out_prod_folder, source,
                    paste0(file_prefix, "_", source, "_", yy, "_", DOY,
                           ifelse(proc_opts$out_format == "GTiff", ".tif", ".dat"))
                  )

                  MODIStsp_process_QA_bits(out_filename,
                                           in_source_file,
                                           bitN,
                                           proc_opts$out_format,
                                           nodata_source,
                                           nodata_qa_in,
                                           nodata_qa_out,
                                           proc_opts$compress)
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

              if (proc_opts$delete_hdf) {
                unlink(file.path(proc_opts$out_folder_mod, modislist))
              }

            } else {
              mess_text <- paste0("[", date(),
                                  "] No images available for selected area ",
                                  "in date ", date_dirs[date])
              process_message(mess_text, verbose)
            }

          } else {
            mess_text <- paste0(
              "[", date(),
              "] All Required output files for date ",
              date_name, " are already existing - Doing Nothing!\n",
              "Set Reprocess to TRUE to reprocess existing data!"
            )
            process_message(mess_text, verbose)

          }

        }

      } else {
        mess_text <- paste0("[", date(), "] No available data for year: ", yy,
                            " for Sensor ", sens_sel, " in selected dates.")
        process_message(mess_text, verbose)
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
                      proc_opts$ts_format, proc_opts$out_format,
                      verbose = verbose)


  unlink(file.path(out_prod_folder, "Temp"), recursive = TRUE)

  # End-of-processing messages and clean-up ----
  end_time   <- Sys.time()
  time_taken <- end_time - start_time
  if (verbose) message("[", date(), "] ", "Total Processing Time: ",
                       time_taken)
  if (verbose) message("[", date(), "] ","MODIStsp processed files are in: `",
                       proc_opts$out_folder, "`")
  # if (!proc_opts$del)
  if (verbose) message("[", date(), "] ",
                       "Original downloaded MODIS HDF files are in: `",
                       proc_opts$out_folder_mod, "`")

  # Save previous options ----

  # At the end of a successful execution, save the options used in the main
  # output folder as a JSON file with name containing the date of processing.
  # Also update "MODIStsp_previous.json.

  if (!is.null(proc_opts)) {
    if (is.null(proc_opts$spafile))  proc_opts$spafile <- NA
    if (is.null(proc_opts$drawnext)) proc_opts$drawnext <- NA
    if (all(unlist(proc_opts$bbox) == NULL)) proc_opts$bbox <- c(NA, NA, NA, NA)
    opts_jsfile <- file.path(proc_opts$out_folder,
                             paste0("MODIStsp_", Sys.Date(), ".json"))
    jsonlite::write_json(proc_opts, opts_jsfile, pretty = TRUE,
                         auto_unbox = TRUE)

    if (verbose) message("[", date(), "] ",
                         "Processing options saved to: `",
                         opts_jsfile, "`")

  }

  return(invisible(NULL))
}
