#'
#' MODIStsp_process
#' @description main function of MODIStsp tool. Takes as input processing parameters specified by the user using MODIStsp_GUI and saved in
#' MODIStsp_Previous.json (Interactive use), or a user specified JSON file (batch use) (See MODIStsp_main for details ) and performs all required
#' processing.
#' @details After retrieving the input processing options, the function accesses lpdaac htttp archive to determine the list of dates
#' to be processed. It then perform all required processing steps on each date (download, reprojection, resize, mosaicing, indexes computation,
#' quality indicators computation), and finally performs virtual files creation. Checks are done in order to not re-download already existing HDF
#' images, and not reprocess already processed dates (if the user didn'specify that)
#' @param sel_prod string selected MODIS product
#' @param start_date string start_date for images download and preproc (yyyy.mm.dd)
#' @param end_date string end_date for images download and preproc (yyyy.mm.dd)
#' @param out_folder  main ouput folder
#' @param out_folder_mod  ouput folder for original HDF storage
#' @param reprocess string string ("Yes"/"No") If Yes, reprocess data for already existing dates (Default = 'Yes')
#' @param delete_hdf string ("Yes"/"No") If Yes, delete original hdf after completion
#' @param sensor string ("Terra" or "Aqua" or "Both")
#' @param https hash https site for download of hdf of selected product
#' @param ftps hash ftps site for download of hdf of selected product
#' @param download_server service used to download MODIS tiles, one of: 'http', 'ftp', NA.
#' @param user Username for http download (https://urs.earthdata.nasa.gov/home)
#' @param password Password for http download (https://urs.earthdata.nasa.gov/home)
#' @param start_x int start horiz. tile
#' @param start_y int start vertical. tile
#' @param end_x int end horiz. tile
#' @param end_y int end vertical. tile
#' @param bbox array output bounding box (xmin, xmax, ymin, ymax ) in out proj coords
#' @param out_format string output raster format (ENVI or GTiff)
#' @param compress string compression for GTiff outputs (None, LZW, DEFLATE)
#' @param out_res_sel string "Native" or "Resampled"
#' @param out_res float Output resolution (in output projection measurement unit)
#' @param native_res float Native resolution of MODIS product
#' @param tiled 0/1 1 = tiled product; 0 = nontiled product (resolution 0.05 deg)
#' @param MOD_proj_str string proj4 string for MODIS product native projection (? Check ! for product in geographic !)
#' @param outproj_str string proj4 string of selected output projection
#' @param nodata_in array Original nodata for MODIS bands
#' @param nodata_out Target nodata for MODIS bands
#' @param nodata_change string (Yes/No) if Yes, nodata are set to nodata_out in output rasters
#' @param rts string ("Yes"/"No") If Yes, create rts time series
#' @param datatype string array datatypes of MODIS bands
#' @param bandsel  array of lenght equal to number of original modis layers. set to 1 for bands to be processed
#' @param bandnames array of Abbreviated Names of MODIS bands
#' @param indexes_bandsel array of lenght equal to number of available spectral indexes, set to  1 for indexes to be processed
#' @param indexes_bandnames array of Abbreviated Names of MODIS indexes
#' @param indexes_formula  array of indexes formulas
#' @param indexes_nodata_out Nodata values for indexes
#' @param quality_bandnames array of  Names of MODIS quality indicators
#' @param quality_bandsel array of lenght equal to number of available quality indicators, set to  1 for indicators to be processed
#' @param quality_bitN list of strings with number of entries equal to number of quality indicators. each entry caontains position of bits corresponding to a QI (e.g., 0-1)
#' @param quality_source list of strings which connects each quality indicator to its source aggregated quality assurance layer
#' @param quality_nodata_in Always 255
#' @param full_ext string ("Full_Ext" or "Resized")
#' @param quality_nodata_out Always 255
#' @param file_prefixes output file prefix according to selelected product (e.g., MOD13Q1)
#' @param main_out_folder Suffix to add to the overall out_folder to create the out dir for the product (corresponds to an abbreviation of the selected product)
#' @param resampling string resampling method (near, bilinear, etc.)
#' @param ts_format string format of virtual files (None, ENVI Meta Files, GDAL vrt files, ENVI and GDAL)
#' @param gui logical indicates if processing was called within the GUI environment or not. If not, direct processing messages to the log
#' @return NULL
#'
#' @author Lorenzo Busetto, phD (2014-2015) \email{busetto.l@@irea.cnr.it}
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @note Thanks Tomislav Hengl and Babak Naimi, whose scripts made the starting point for development of this function ( http://r-gis.net/?q=ModisDownload ; .
#' http://spatial-analyst.net/wiki/index.php?title=Download_and_resampling_of_MODIS_images)
#' @note License: GPL 3.0
#' @importFrom gdalUtils gdal_translate gdalbuildvrt gdalwarp
#' @importFrom hash hash
#' @importFrom httr GET authenticate timeout content progress
#' @importFrom tools file_path_sans_ext
#' @importFrom XML xmlParse xmlRoot xmlToList
#' @importFrom RCurl getBinaryURL
#' @importFrom stringr str_locate
#' @import gWidgets

MODIStsp_process <- function(sel_prod, start_date, end_date ,out_folder, out_folder_mod, reprocess = "Yes", delete_hdf = "No", sensor, download_server, 
                             user, password, https, ftps,start_x, start_y, end_x, end_y, bbox, out_format, compress, out_res_sel, out_res, 
                             native_res, tiled, MOD_proj_str, outproj_str, nodata_in,nodata_out, nodata_change,rts, datatype,	bandsel, bandnames, 
                             indexes_bandsel, indexes_bandnames, indexes_formula, indexes_nodata_out, quality_bandnames, quality_bandsel, 
                             quality_bitN ,quality_source, quality_nodata_in, full_ext, quality_nodata_out, file_prefixes, main_out_folder, resampling, 
                             ts_format, gui=TRUE) {
  
  if (nodata_change == "No") {
    nodata_out <- nodata_in
  }  # if nodata chande set to no, set ou_nodata to in_nodata
  dir.create(out_folder_mod, recursive = TRUE, showWarnings = FALSE) # create out folder if not existing
  
  #Initialize some variables
  out_prod_folder <- file.path(out_folder,main_out_folder)  # main output folder --> define on the basis of product name and create if necessary
  dir.create(out_prod_folder, showWarnings = FALSE, recursive = TRUE)
  tmp_prod_folder <- file.path(out_prod_folder,"tmp") # directory to store temporary [virtual] rasters
  start_year <- unlist(strsplit(start_date, "[.]"))[1]
  end_year <- unlist(strsplit(end_date, "[.]"))[1]
  
  # Add a message window while the file is charging (TODO: create a function to pass the message to cat if gui=FALSE and to svalue(mess_lab) if gui=TRUE)
  mess_text <- "Processing"
  if (gui) {
    mess <- gwindow(title = "Processing Status", container = TRUE, width = 400, height = 40)
    mess_lab <- glabel(text = paste("---",mess_text,"---"), editable = FALSE, container = mess)
  } else {
    message("[",date(),"] ",mess_text)
  }
  
  if (sensor == "Both") {
    senslist <- c("Terra","Aqua")
  } else {
    senslist <- sensor
  }		# If both sensor selected, do a cycle. Process first Terra then Aqua
  
  for (sens_sel in senslist) {		# cycle on selected sensors
    
    # get http site addresses and file prefixes
    if (sens_sel == "Terra") {
      http <- https[["Terra"]]
      ftp <- ftps[["Terra"]]
      file_prefix <- file_prefixes[["Terra"]]
    } else {
      http <- https[["Aqua"]]
      ftp <- ftps[["Aqua"]]
      file_prefix <- file_prefixes[["Aqua"]]
    }
    
    #- ------------------------------------------------------------------------------- -#
    #  Verify if bands needed for computing spectral indexes and/or quality indicators are already selected
    #  if not, select them and set the "delete" option for them to 1
    #- ------------------------------------------------------------------------------- -#
    
    bands_indexes <- matrix(0, nrow = length(bandsel), ncol = length(indexes_bandsel) + length(quality_bandsel),# dummy matrix which associate, to each couple of index or quality band (col) - original band (row),
                            dimnames = list(bandnames,c(indexes_bandnames,quality_bandnames)))								# info on wether that band is required to build that index
    
    bandsel_orig_choice <- bandsel						# Save original choice of bands in bandsel_orig_choice (bandsel is later modified to set to 1 all bands needed for indexes and quality
    
    for (band in which(indexes_bandsel == 1)) {  # cycle on selected indexes
      formula <- indexes_formula[band]	  # If an index is selected retrieve its formula
      for (bandorig in seq(along = bandnames)) {		# cycle on original bands
        if (length(grep(bandnames[bandorig],formula)) > 0) {			# check if the original band is needed for the index
          if (bandsel[bandorig] == 0) {        # if yes and band not set to be processed, set it to be processed
            bands_indexes[bandorig,band] <- 1
          }
        }
      } #End Cycle on bandorig
    } #End If on bandsel[band] == 1
    
    for (band in which(quality_bandsel == 1)) {  # cycle on selected QIs
      bandorig <- which(bandnames == quality_source[band]) 		# Identify source band for the quality indicator selected
      if (bandsel[bandorig] == 0) {							# if ource not already selected to be processed, select it
        bands_indexes[bandorig,length(indexes_bandsel) + band] <- 1
      }
    } #End If on bandsel[band] == 1
    
    # ---------------------------------- #
    # Start Cycle on selected years
    # ---------------------------------- #
    
    # Get start and end years from start and end dates
    
    # start_year <- as.numeric(strsplit(start_date, ".", fixed = T)[[1]][1])
    # end_year <- as.numeric(strsplit(end_date, ".", fixed = T)[[1]][1])
    #
    for (yy in start_year:end_year) {
      
      # Create string representing the dates to be processed
      if (yy == start_year & yy == end_year) {
        dates <- c(start_date,end_date)
      }
      
      if (yy == start_year & yy != end_year) {
        dates <- c(start_date,paste0(as.character(yy),".12.31"))
      }
      
      if (yy != start_year & yy == end_year) {
        dates <- c(paste0(as.character(yy),".1.1"),end_date)
      }
      
      if (yy != start_year & yy != end_year) {
        dates <- c(paste0(as.character(yy),".1.1"),paste0(as.character(yy),".12.31"))
      }
      
      # Processing status message
      
      mes_text <- paste("Retrieving Files for Year",as.character(yy))
      if (gui) {
        svalue(mess_lab) <- paste("---",mess_text,"---")
      } else {
        message("[",date(),"] ",mess_text)
      }
      
      # Get a list of the folders containing hdf images required (Corresponding to the subfolders in lpdaac corresponding to
      # selected product, dates, and current year under processing)
      
      date_dirs_all <- lpdaac_getmod_dirs(ftp = ftp, http = http, used_server = download_server, user = user, password = password, gui = gui, 
                                          out_folder_mod = out_folder_mod, .Platform = .Platform)
      download_server <- attr(date_dirs_all, "server") # overwrite with the used setting (if already specified it does not change, if NA, it is set with the working one)
      date_dirs <- lpdaac_getmod_dates(dates = dates, date_dirs =  date_dirs_all)  # First, find the folders in lpdaac corresponding to the required dates
      
      if (length(date_dirs) > 0) {
        modislist <- NULL
        # Start Cycling on directories containing images to be downloaded and identify the required ones (i.e., the ones corresponding to selected tiles)
        for (date in 1:length(date_dirs)) {
          
          date_name <- sub(sub(pattern = "\\.", replacement = "_", date_dirs[date]), pattern = "\\.", replacement = "_", date_dirs[date])  #Create the date string
          YEAR <- strftime(as.Date(date_name,"%Y_%m_%d" ), format = "%Y")  # transform date to YEAR
          DOY <- strftime(as.Date(date_name,"%Y_%m_%d" ), format = "%j")  # transform date to DOY
          
          # check if all foreseen output rasters already exist. If so, skip the date. Otherwise start proecssing
          check_files <- FALSE
          check_files <- MODIStsp_check_files(out_prod_folder, file_prefix,bandnames,bandsel_orig_choice,yy,DOY,out_format,  indexes_bandnames, 
                                              indexes_bandsel, quality_bandnames, quality_bandsel)
          if (check_files == FALSE | reprocess == "Yes") {  		# If not all output files are already present or reprocess = "Yes", start downloading hdfs
            
            # Create vector of image names required (corresponding to the selected tiles, within current dir)
            modislist <- lpdaac_getmod_names(http = http, ftp = ftp, used_server = download_server, user = user, password = password, 
                                             date_dir = date_dirs[date], v = seq(from = start_y, to = end_y), h = seq(from = start_x, to = end_x), 
                                             tiled, out_folder_mod = out_folder_mod, gui = gui)
            
            # ---------------------------------- ----------------------------------------------#
            # Download and preprocess Imagesin modislist vector -----------
            # ---------------------------------- ----------------------------------------------#
            if (length(modislist) > 0) {
              
              #- ------------------------------------------------------------------------------- -#
              #  Download images (If HDF file already in out_mod_folder, it is not redownloaded !!!!
              #- ------------------------------------------------------------------------------- -#
              for (modisname in modislist) {
                # Check file size (if the local file size is differente, re-download)
                local_filename <- file.path(out_folder_mod,modisname)
                local_filesize <- file.info(local_filename)$size
                remote_filename <- if (download_server == "http") {
                  paste0(http,date_dirs[date], "/",modisname)
                } else if (download_server == "ftp") {
                  paste0(ftp,YEAR,"/",DOY,"/",modisname)
                } else if (download_server == "offline") {NA}
                
                if (download_server != "offline") { # in case of http or ftp download, try to catch size information from xml file
                  
                  # # Get remote file size
                  # res <-getURL(remote_filename, nobody=1L, header=1L)

                  # All this substituted with direct assessment of filesize from CURL call - left here while completing feature
                  remote_size_tries <- 30 # numbers of tryouts for xml metafile
                  size_string <- NA
                  class(size_string) <- "try-error"
                  
                  while (remote_size_tries > 0) {
                    size_string <- try(GET(paste0(remote_filename,".xml"), authenticate(user, password), progress(), timeout(600)))
                    # Check if download was good: check class of xmldown and status of xmldown
                    if (class(size_string) == "try-error") {
                      remote_size_tries <- remote_size_tries - 1
                    } else {
                      remote_size_tries <- 0
                    }
                  }
                  
                  # if the xml was available, check the size; otherwise, set as the local size to skip the check
                  if (class(size_string) == "try-error") {
                    remote_filesize <- local_filesize
                  } else {
                    remote_filesize <- as.integer(xmlToList(xmlParse(content(size_string)))[["GranuleURMetaData"]][["DataFiles"]][["DataFileContainer"]][["FileSize"]])
                  }
                  
                } else {  # On offline mode, don't perform file size check. 
                  remote_filesize <- local_filesize
                }
                
                if (!file.exists(local_filename) | local_filesize != remote_filesize) {		# If HDF not existing or with different size, download.
                  er <- 5		; 	class(er) <- "try-error" ;	ce <- 0
                  
                 
                  local_filesize = 0  
                  while (local_filesize != remote_filesize) {   # Add here a while loop: Only exit if local file size equals remote filesize
                    
                    while (er != 0) {   # repeat until no error or > 30 tryyouts
                      mess_text <- paste("Downloading", sens_sel, "Files for date", date_name, ":" ,which(modislist == modisname), "of", length(modislist))
                      if (gui) {
                        svalue(mess_lab) <- paste("---",mess_text,"---")
                        message("[",date(),"] ",mess_text)
                      } else {
                        message("[",date(),"] ",mess_text)
                      }	# Update progress window
                      
                      if (download_server == "http") {
                        download <- try(GET(remote_filename, authenticate(user, password), progress(), timeout(600)))
                      } else {
                        download <- try(download.file(url = remote_filename, destfile = local_filename, mode = "wb", quiet = TRUE, cacheOK = FALSE,
                                                      extra = c("-L")))
                      }
                      
                      if (class(download) == "try-error") {
                        er <- 5
                        ce <- ce + 1
                        message("[",date(),"] Download Error - Retrying...")
                        unlink(local_filename)  # On download error, delete bad files
                        Sys.sleep(10)    # sleep for a while....
                      } else {
                        if (download_server == "http") {
                          if (download$status_code != 200 & length(content(download, "text")) == 1) {	
                            message("[",date(),"] Download Error - Retrying...")
                            unlink(local_filename) # on error, delete last hdf file (to be sure no incomplete files are left behind and send message)
                            Sys.sleep(10)
                            er <- 5
                            ce <- ce + 1
                          } else {
                            writeBin(download$content, local_filename)
                            er <- 0 
                          }
                        } else {
                          er <- 0 
                        }
                      } 
                      
                      if (ce == 30) {
                        # Ask if Stop after 30 failed attempts
                        if (gui) {
                          confirm <- gconfirm(paste0(download_server," server seems to be down! Do you want to retry?"), icon = "question", 
                                              handler = function(h,...){})
                        } else {
                          confirm <- "FALSE"
                        }
                        if (confirm == "FALSE") {
                          warning("[",date(),"] Error: server seems to be down! Please Retry Later!")
                          unlink(local_filename)
                          stop()
                        }
                      }  
                    }  # end while on download tries
                    
                    local_filesize <- file.info(local_filename)$size    # Find the size of the new file downloaded to allow comparison with remote 
                    
                  } # end here the while loop on file size chek
                  
                }  # end IF on hdf existence
              } # End cycle for downloading the images in modislist vector
              
              message("[",date(),"] ",length(modislist)," files for date of ",date_dirs[date]," were successfully downloaded!")
              
              # -------------------------------------------------------------------------
              # After all required tiles for the date are downloaded, start geoprocessing
              # -------------------------------------------------------------------------
              
              # -------------------------------------------------------------------------
              # STEP 1: choose the layers (original, indexes and quality bands) to be created
              # -------------------------------------------------------------------------
              
              # at the end of this step, "bandsel" is recreated as the union of the bands selected by the user and the bands required
              # by indexes and quality bands, but only those ones which are not already present.
              
              # do a chack to see if the product has at least one Quality Layer or Possible Index
              if (length(indexes_bandnames) != 0 | length(quality_bandnames) != 0 ) {
                
                req_bands_indexes <- bands_indexes
                for (i in 1:length(req_bands_indexes)) {
                  req_bands_indexes[i] <- 0
                }	# matrix similar to band_indexes, but specific for this year-doy process
                
                for (band in which(indexes_bandsel == 1)) {
                  indexes_band <- indexes_bandnames[band]
                  out_filename <- file.path(out_prod_folder,indexes_band,paste0(file_prefix,"_",indexes_band,"_",yy,"_", DOY))
                  if (out_format == "GTiff") {
                    out_filename <- paste0(out_filename, ".tif")
                  } else {
                    out_filename <- paste0(out_filename, ".dat")
                  }
                  if (file.exists(out_filename) == FALSE | reprocess == "Yes") {
                    req_bands_indexes[,band] <- bands_indexes[,band] # if the index does not exists then find out the original bands required for it
                  }
                }
                
                for (band in which(quality_bandsel == 1)) {
                  quality_band <- quality_bandnames[band]
                  out_filename <- file.path(out_prod_folder,quality_band,paste0(file_prefix,"_",quality_band,"_",yy,"_", DOY))
                  if (out_format == "GTiff") {
                    out_filename <- paste0(out_filename, ".tif")
                  } else {
                    out_filename <- paste0(out_filename, ".dat")
                  }
                  if (file.exists(out_filename) == FALSE | reprocess == "Yes") {# if the index does not exists then find out the original bands required for it
                    req_bands_indexes[,band + length(indexes_bandsel)] <- bands_indexes[,band + length(indexes_bandsel)] 
                  }
                }
                
                # Create the final vector of bands required for processing (bands chosen by the user + bands required for indexes and quality bands)
                bandsel <- as.integer(as.logical(bandsel_orig_choice + apply(req_bands_indexes,1,sum)))
              } #end check on existance of quality/indexes layers
              
              
              delbands <- bandsel - bandsel_orig_choice    # dummy array set to 0 - will contain info on wether orignal downloaded bands has to be deleted
              
              # -----------------------------------
              # STEP 2: process the required original MODIS layers
              # -----------------------------------
              
              for (band in 1:length(bandnames)) {														# Cycle on MODIS original layers
                
                bands <- numeric(length(bandnames))													# Create vector with length = bands, filled with zeroes
                # er_mos = 1  														# dummies for error state
                if (bandsel[band] == 1) {					# If band selected, process it
                  # mess_text <- paste("Mosaicing ", bandnames[band]," files for date: ",date_name)
                  # if (gui) {
                  #   svalue(mess_lab) <- paste("---",mess_text,"---")
                  #   message("[",date(),"]",mess_text)
                  # } else {
                  #   message("[",date(),"]",mess_text)
                  # }
                  bands[band] <- 1																			# IF band selected for processing, put its value to 1
                  dir.create(file.path(out_prod_folder, bandnames[band]), showWarnings = FALSE, recursive = TRUE)
                  bands <- paste(as.character(bands), collapse = "", sep = " ")					# Convert to character
                  outfile <- paste0(tmp_prod_folder, "/",bandnames[band],"_",yy,"_",DOY,".tif")  	# Create name for the temporary tif mosaic
                  outfile <- paste0(bandnames[band],"_",yy,"_",DOY,".tif")  	# Create name for the temporary tif mosaic
                  # NOTE: Change outrep_file to a list of rep files: only one for original bands, multiple for indexes and quality
                  outrep_file <- file.path(out_prod_folder, bandnames[band], paste0(file_prefix,"_",sub("[.][^.]*$", "", 
                                                                                                        basename(outfile), perl = TRUE)))	# Create name for the TIFF reprojected  mosaic
                  if (out_format == "GTiff") {
                    outrep_file <- paste0(outrep_file, ".tif")
                  } else {
                    outrep_file <- paste0(outrep_file, ".dat")
                  }
                  
                  outfile_vrt <- tempfile(fileext = ".vrt")   # filename of temporary vrt file 
             
                  if (file.exists(outrep_file) == FALSE | reprocess == "Yes") {
                    
                    files_in <- file.path(out_folder_mod, modislist)
                    dir.create(tmp_prod_folder, recursive = TRUE, showWarnings = FALSE)
                    # ---------------------------------- ----------------------------------------------#
                    # Convert to output projection, extent and format using gdalwarp ----
                    # ---------------------------------- ----------------------------------------------#
                    
                    if (outproj_str != MOD_proj_str) {
                      mess_text <- paste("Processing and Reprojecting", sens_sel, bandnames[band], "files for date:", date_name)
                    } else {
                      mess_text <- paste("Processing", sens_sel, bandnames[band], "files for date:", date_name)
                    }
                    if (gui) {
                      svalue(mess_lab) <- paste("---",mess_text,"---")
                      message("[",date(),"] ",mess_text)
                    } else {
                      message("[",date(),"] ",mess_text)
                    }
                    
                    if (full_ext == "Resized") {
                      #If resize required,  convert bbox coordinates from t_srs to modis_srs, to get the correct extent
                      # for resizing BEFORE reprojecting
                      bbox_mod <- reproj_bbox( bbox, outproj_str, MOD_proj_str, enlarge = TRUE)
                      # Create a resized and eventually mosaiced GDAL vrt file
                      gdalbuildvrt(files_in, outfile_vrt, te = c(bbox_mod), tap = TRUE, tr = paste(rep(native_res,2),collapse = " "),
                                   srcnodata = nodata_in[band] ,vrtnodata = nodata_out[band], sd = band)
                    } else {
                      gdalbuildvrt(files_in, outfile_vrt,  sd = band,srcnodata = nodata_in[band] , vrtnodata = nodata_out[band])
                    }  # Create a resized and eventually mosaiced GDAL vrt file
                    
                    ## Launch the reprojection - operations to be done depends on whether resize and/or reprojection and/or
                    ## resampling are required
                    
                    
                    reproj_type <- if (out_res_sel == "Native" & outproj_str == MOD_proj_str) {
                      "GdalTranslate"
                    } else if (out_res_sel == "Resampled" & outproj_str == MOD_proj_str) {
                      "Resample1_Resize0"
                    } else if (out_res_sel == "Native"    & outproj_str != MOD_proj_str & full_ext == "Full Tiles Extent") {
                      "Resample0_Resize0"
                    } else if (out_res_sel == "Native"    & outproj_str != MOD_proj_str & full_ext == "Resized") {
                      "Resample0_Resize1"
                    } else if (out_res_sel == "Resampled" & outproj_str != MOD_proj_str & full_ext == "Full Tiles Extent") {
                      "Resample1_Resize0"
                    } else if (out_res_sel == "Resampled" & outproj_str != MOD_proj_str & full_ext == "Resized") {
                      "Resample1_Resize1"
                    } else {
                      "Error"
                    }
                    
                    if (out_format == "GTiff") {
                      switch( reproj_type,
                              GdalTranslate = gdal_translate(outfile_vrt,  outrep_file, a_srs = MOD_proj_str, of = out_format, ot = datatype[band], 
                                                             a_nodata = nodata_out[band], co = paste("COMPRESS",compress,sep = "="), overwrite = TRUE),
                              Resample0_Resize0 =  gdalwarp(outfile_vrt, outrep_file, s_srs = MOD_proj_str, t_srs = outproj_str, of = out_format, 
                                                            r = resampling, co = paste("COMPRESS",compress,sep = "="), wo = "INIT_DEST = NO_DATA", 
                                                            wt = datatype[band], overwrite = TRUE),
                              Resample0_Resize1 =  gdalwarp(outfile_vrt, outrep_file, s_srs = MOD_proj_str, t_srs = outproj_str, of = out_format, 
                                                            r = resampling, te = bbox, co = paste("COMPRESS",compress,sep = "="), wo = "INIT_DEST = NO_DATA", 
                                                            wt = datatype[band], overwrite = TRUE),
                              Resample1_Resize0 =  gdalwarp(outfile_vrt, outrep_file, s_srs = MOD_proj_str, t_srs = outproj_str, of = out_format, 
                                                            r = resampling, tr = rep(out_res,2), co = paste("COMPRESS",compress,sep = "="), 
                                                            wo = "INIT_DEST = NO_DATA", wt = datatype[band], overwrite = TRUE),
                              Resample1_Resize1 =  gdalwarp(outfile_vrt, outrep_file, s_srs = MOD_proj_str, t_srs = outproj_str, of = out_format, 
                                                            r = resampling, te = bbox, tr = rep(out_res,2), co = paste("COMPRESS",compress,sep = "="), 
                                                            wo = "INIT_DEST = NO_DATA", wt = datatype[band], overwrite = TRUE),
                              quit("Internal error in out_res_sel, outproj_str or full_ext."))
                    } else {
                      switch( reproj_type,
                              GdalTranslate =  gdal_translate(outfile_vrt,  outrep_file, a_srs = MOD_proj_str, of = out_format, ot = datatype[band], 
                                                              a_nodata = nodata_out[band], overwrite = TRUE),
                              Resample0_Resize0  =  gdalwarp(outfile_vrt, outrep_file, s_srs = MOD_proj_str, t_srs = outproj_str, of = out_format, 
                                                             r = resampling, wo = "INIT_DEST = NO_DATA", wt = datatype[band], overwrite = TRUE),
                              Resample0_Resize1  =  gdalwarp(outfile_vrt, outrep_file, s_srs = MOD_proj_str, t_srs = outproj_str, of = out_format, 
                                                             r = resampling, te = bbox, wo = "INIT_DEST = NO_DATA", wt = datatype[band], overwrite = TRUE),
                              Resample1_Resize0  =  gdalwarp(outfile_vrt, outrep_file, s_srs = MOD_proj_str, t_srs = outproj_str, of = out_format, 
                                                             r = resampling, tr = rep(out_res,2), wo = "INIT_DEST = NO_DATA", wt = datatype[band], 
                                                             overwrite = TRUE),
                              Resample1_Resize1  =  gdalwarp(outfile_vrt, outrep_file, s_srs = MOD_proj_str, t_srs = outproj_str, of = out_format, 
                                                             r = resampling, te = bbox, tr = rep(out_res,2), wo = "INIT_DEST = NO_DATA", wt = datatype[band], 
                                                             overwrite = TRUE),
                              quit("Internal error in out_res_sel, outproj_str or full_ext."))
                      
                      fileConn_meta_hdr <- file(paste0(tools::file_path_sans_ext(outrep_file),".hdr"), "a")  # If output format is ENVI, add data ignore value to the header file
                      writeLines(c("data ignore value = ", nodata_out[band] ), fileConn_meta_hdr, sep = " ")		# Data Ignore Value
                      writeLines("", fileConn_meta_hdr)
                      close(fileConn_meta_hdr)
                      
                    }
                    
                    gc()
                    xml_file <- paste0(outrep_file,".aux.xml")		# Delete xml files created by gdalwarp
                    # unlink(xml_file)
                    unlink(tmp_prod_folder, recursive = TRUE)					# Delete temporary files in temp folder
                  }
                }  # ENDIF band selected for processing
              }	# END Cycle on available MODIS Bands
              
              # ---------------------------------- ----------------------------------------------#
              # If Indexes selected, then start creating them
              # ---------------------------------- ----------------------------------------------#
              
              for (band in which(indexes_bandsel == 1)) {
                indexes_band <- indexes_bandnames[band] 	# index name
                formula <- indexes_formula[band]				#index formula
                mess_text <- paste("Computing", sens_sel, indexes_band, "for date:", date_name)
                if (gui) {
                  message("[",date(),"] ",mess_text)
                } else {
                  message("[",date(),"] ",mess_text)
                }
                out_filename <- file.path(out_prod_folder,indexes_band,paste0(file_prefix,"_",indexes_band,"_",yy,"_", DOY))
                if (out_format == "GTiff") {
                  out_filename <- paste0(out_filename, ".tif")
                } else {
                  out_filename <- paste0(out_filename, ".dat")
                }
                dir.create(file.path(out_prod_folder,indexes_band), showWarnings = FALSE, recursive = TRUE) # create folder for index
                if (file.exists(out_filename) == FALSE | reprocess == "Yes") { #If file not existing and reprocess = No, compute the index and save it
                  
                  MODIStsp_process_indexes(out_filename = out_filename, formula = formula,bandnames = bandnames, nodata_out = nodata_out,
                                           indexes_nodata_out = indexes_nodata_out[band],out_prod_folder = out_prod_folder, file_prefix = file_prefix,
                                           yy = yy,out_format = out_format, DOY = DOY )
                }
              }
              
              # ---------------------------------- ----------------------------------------------#
              # If Quality indicators selected , then start creating them
              # ---------------------------------- ----------------------------------------------#
              
              for (band in which(quality_bandsel == 1)) {
                quality_band <- quality_bandnames[band]		 # indicator name
                source <- quality_source[band]  #  Original MODIS layer containing data of the indicator
                bitN <- quality_bitN[band]      #  bitfields corresponding to indicator within source
                nodata_qa_in <- quality_nodata_in[band]
                nodata_qa_out <- quality_nodata_out[band]
                mess_text <- paste("Computing", quality_band, "for date:", date_name)
                if (gui) {
                  svalue(mess_lab) <- paste("---",mess_text,"---")
                  message("[",date(),"] ",mess_text)
                } else {
                  message("[",date(),"] ",mess_text)
                }
                out_filename <- file.path(out_prod_folder,quality_band,paste0(file_prefix,"_",quality_band,"_",yy,"_", DOY))
                if (out_format == "GTiff") {
                  out_filename <- paste0(out_filename, ".tif")
                } else {
                  out_filename <- paste0(out_filename, ".dat")
                }
                dir.create(file.path(out_prod_folder,quality_band), showWarnings = FALSE, recursive = TRUE)
                if (file.exists(out_filename) == FALSE | reprocess == "Yes") { #If file not existing and reprocess = No, compute the indicator and save it
                  MODIStsp_process_QA_bits(out_filename,in_raster_name = bandnames[grep(source,bandnames)], bitN, source,
                                           out_prod_folder, file_prefix, yy, DOY, out_format, nodata_source = nodata_out[grep(source,bandnames)],
                                           nodata_qa_in , nodata_qa_out  )
                }
              }
              
              #- ------------------------------------------------------------------------------- -#
              #  Delete bands not needed (i.e., bands required for indexes or quality computation,
              # but not requested by the user,
              #- ------------------------------------------------------------------------------- -#
              for (banddel in seq(along = delbands)) {
                
                if (delbands[banddel] == 1) {
                  out_filename <- file.path(out_prod_folder,bandnames[banddel],paste0(file_prefix,"_",bandnames[banddel],"_",yy,"_", DOY))
                  if (out_format == "ENVI") {
                    out_filename_dat <- paste0(out_filename, ".dat")
                    unlink(out_filename_dat)
                    out_filename_hdr <- paste0(out_filename, ".hdr")
                    unlink(out_filename_hdr)
                    
                  }
                  if (out_format == "GTiff") {
                    out_filename_tif <- paste0(out_filename, ".tif")
                    unlink(out_filename_tif)
                  }
                  unlink(dirname(out_filename),recursive = TRUE)
                } #End If on delbands[banddel] == 1
              } #End Cycle on banddel
              
            } else {
              message("[",date(),"] No available image for selected Tiles in ",date_dirs[date])
            } # End check on at least one image available
            
          } else {
            message("[",date(),"] All Required output files for date ",date_name, " are already existing - Doing Nothing!")
          } # End check on all data already processed for date or reprocees = Yes
          
          #- ------------------------------------------------------------------------------- -#
          # If deletion selected, delete the HDF files in out_folder_mod directory
          #- ------------------------------------------------------------------------------- -#
          if (delete_hdf == "Yes") {
            
            for (dir in 1:length(date_dirs)) {
              # modislist <- lpdaac_getmod_names(http = ftp, date_dirs = date_dirs,  date = date , v = seq(from = start_y, to =  end_y), h = seq(from = start_x, to = end_x), tiled)
              modislist <- lpdaac_getmod_names(http = http, ftp = ftp, used_server = download_server, user = user, password = password, 
                                               date_dir = date_dirs[dir], v = seq(from = start_y, to =  end_y), h = seq(from = start_x, to = end_x), 
                                               tiled, gui = gui)
              for (modisname in modislist) {
                unlink(file.path(out_folder_mod,modisname))
              }
            }
          } #end if on Delete original downloaded HDFs
          
        }   # End cycling on available dates for selected year
        
      } else {
        message("[",date(),"] No available data for year: ",yy," for Sensor ",sens_sel," in selected dates.")
      }
      
    }	# End Cycling on selected years
    
    bandsel <- bandsel_orig_choice  # reset bandsel to original user's choice
    
  } # End cycling on sensors
  
  #- ------------------------------------------------------------------------------- -#
  #  Create vrt files of time series - original and derived
  #- ------------------------------------------------------------------------------- -#
  
  
  if (sensor == "Both") {
    senslist = c("Terra","Aqua","Mixed")
  } # selected sensors
  
  for (sens_sel in senslist) {		# cycle on selected sensors
    
    for (band in which(bandsel == 1)) { # Create virtual files for original layers
      message("[",date(),"] Creating Virtual Files and rts time series for layer ",bandnames[band])
      MODIStsp_vrt_create(out_prod_folder = out_prod_folder, meta_band = bandnames[band],
                          file_prefixes = file_prefixes, sens_sel = sens_sel, ts_format = ts_format,  nodata_value = nodata_out[band], 
                          out_format = out_format, rts = rts)
    } #End Cycle on bandsel
    
    for (band in which(indexes_bandsel == 1)) {  # Create virtual files for QI layers
      message("[",date(),"] Creating Virtual Files and rts time series for layer ",indexes_bandnames[band])
      MODIStsp_vrt_create(out_prod_folder = out_prod_folder, meta_band = indexes_bandnames[band],
                          file_prefixes = file_prefixes, sens_sel = sens_sel, ts_format = ts_format, nodata_value = indexes_nodata_out[band], 
                          out_format = out_format, rts = rts)
    } #End Cycle on indexes_bandsel
    
    for (band in which(quality_bandsel == 1)) {	# Create virtual files for SI layers
      message("[",date(),"] Creating Virtual Files and rts time series for layer ",quality_bandnames[band])
      MODIStsp_vrt_create(out_prod_folder = out_prod_folder, meta_band = quality_bandnames[band]		,
                          file_prefixes = file_prefixes, sens_sel = sens_sel, ts_format = ts_format, nodata_value = quality_nodata_out[band], 
                          out_format = out_format, rts = rts)
    } #End Cycle on quality_bandsel
    
  }
  #- ------------------------------------------------------------------------------- -#
  # Close GUI and clean-up
  #- ------------------------------------------------------------------------------- -#
  gc()
  if (gui) {
    addHandlerUnrealize(mess_lab, handler = function(h,...) {
      return(FALSE)
    })		# Allow message lab to be closed since processing ended .
    dispose(mess_lab)
  }
  unlink(file.path(out_prod_folder,"Temp"),recursive = TRUE)
  return("DONE")
}
