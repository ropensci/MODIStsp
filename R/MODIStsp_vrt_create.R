#' @title Create MODIStsp virtual files
#' @description	Function used to create virtual files from time series of single-band
#'  files corresponding to different acquisition dates. The function takes as input
#'  the folder in which the single-band files are stored, and creates a ENVI Meta
#'  file and/or a GDAL vrt file that allows access to the full time series as if
#'  it was a single physical file.
#'  Created virtual files are stored in the "Time Series" subfolder of `out_prod_folder``
#' @param out_prod_folder `character` Main output folder.
#' @param file_prefixes `character array (2)` file_prefixes for TERRA and AQUA -
#'  used to identify the files corresponding to each sensor
#' @param ts_format `character ["ENVI" | "GDAL" | "Both"]` Required output format
#'  for virtual file.
#' @param out_format `character ["ENVI" | "GTiff"]` Format of images used as
#'  "input" for the vrt and contained in out_prod_folder/band folders.
#' @param rts `character ["Yes" | "No"]` If Yes, create "R" RasterStacks time series
#' @param verbose `logical` If FALSE, suppress processing messages, Default: TRUE
#' @inheritParams MODIStsp_process
#' @return NULL -
#'
#' @author Lorenzo Busetto, phD (2014-2017) \email{lbusett@@gmail.com}
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom raster setZ stack
#' @importFrom tools file_path_sans_ext
#' @importFrom stringr str_sub
#' @importFrom gdalUtils gdalbuildvrt
#'
MODIStsp_vrt_create <- function(
  sensor,
  out_prod_folder,
  bandnames, bandsel, nodata_out,
  indexes_bandnames, indexes_bandsel, indexes_nodata_out,
  quality_bandnames, quality_bandsel, quality_nodata_out,
  file_prefixes,
  ts_format, out_format, rts, 
  verbose) {
  
  if (length(sensor) == 2) {
    senslist <- c("Terra", "Aqua", "Mixed")
  } else {
    senslist <- sensor
  }
  
  for (sens_sel in senslist) {
    
    if (sens_sel == "Terra") {
      file_prefix <- file_prefixes[["Terra"]]
    }
    if (sens_sel == "Aqua")  {
      file_prefix <- file_prefixes[["Aqua"]]
    }
    if (sens_sel == "Mixed") {
      file_prefix <- paste(file_prefixes[["Terra"]], file_prefixes[["Aqua"]],
                           sep = "_")
    }
    
    if (sens_sel == "Combined") {
      file_prefix <- file_prefixes[["Terra"]]
    }
    
    meta_bands <- c(bandnames[which(bandsel == 1)],
                    indexes_bandnames[which(indexes_bandsel == 1)],
                    quality_bandnames[which(quality_bandsel == 1)])
    
    nodata_vals <- c(nodata_out[which(bandsel == 1)],
                     indexes_nodata_out[which(indexes_bandsel == 1)],
                     quality_nodata_out[which(quality_bandsel == 1)])
    
    for (mb in seq_along(meta_bands)) {
      
      meta_band    <- meta_bands[mb]
      nodata_value <- nodata_vals[mb]
      if (verbose) {
        message("[", date(), "] Creating Virtual Files and R time series for ",
                "layer ", meta_band)
      }
      
      #- --------------------------------------------------------#
      # retrieve files list of the time series (ENVI format) ####
      # (both .dat and .hdr)
      #
      if (out_format == "ENVI") {
        # get list of ENVI files
        out_meta_files <- list.files(file.path(out_prod_folder, meta_band),
                                     pattern = "\\.dat$", full.names = TRUE)
        if (sens_sel != "Mixed")  {
          out_meta_files <- out_meta_files[grep(file_prefix, out_meta_files)]
        }
        
        out_meta_files_hdr <- list.files(file.path(out_prod_folder, meta_band),
                                         pattern = "\\.hdr$", full.names = TRUE)
        if (sens_sel != "Mixed")  {
          out_meta_files_hdr <- out_meta_files_hdr[grep(file_prefix,
                                                        out_meta_files_hdr)]
        }
      }
      
      #- --------------------------------------------------------#
      # retrieve files list of the time series (GTiff format) ####
      #
      if (out_format == "GTiff") {
        # get list of TIFF files
        out_meta_files <- list.files(file.path(out_prod_folder, meta_band),
                                     pattern = "\\.tif$", full.names = TRUE)
        if (sens_sel != "Mixed")  {
          out_meta_files <- out_meta_files[grep(file_prefix, out_meta_files)]
        }
      }
      
      # Set a flag to 1 if "Mixed" time series are being processed but either no
      # AQUA or no TERRA files are available, so that in that case the creation
      # of META files for the mixed case is skipped !
      
      skip_flag <- 0
      if ( (sens_sel == "Mixed") &
           ( (length(grep(file_prefixes[["Aqua"]], out_meta_files)) == 0) |
             (length(grep(file_prefixes[["Terra"]], out_meta_files)) == 0))) {
        skip_flag <- 1
      }
      # If skip_flag == 1 (mixed TS, but data from TERRA or AQUA missing) do
      # nothing
      if (skip_flag != 1) {
        # If no files available, skip metadata creation
        if (length(out_meta_files) > 0) {
          
          #   __________________________________________________________________
          #   check/reset order of acquisition dates and files              ####
          #
          
          # retrieve the doys and years from filenames
          doys      <- stringr::str_sub(basename(out_meta_files), -7, -5)
          years     <- stringr::str_sub(basename(out_meta_files), -12, -9)
          # find the files order (by acq.date)
          acq_order <- order(as.numeric(paste0(years, doys)))
          
          # reorder doys and years
          doys           <- as.numeric(doys[acq_order])
          years          <- as.numeric(years[acq_order])
          #  Reorder Files  according to acquisition date (useful to have a META
          #  file with bands in the correct order
          out_meta_files <- out_meta_files[acq_order]
          temp_dates     <- as.Date(strptime(paste(years, doys),
                                             format = "%Y %j"))
          doy_min        <- min(doys[which(years == min(years))])
          year_min       <- min(years)
          doy_max        <- max(doys[which(years == max(years))])
          year_max       <- max(years)
          
          # ____________________________________________________________________
          # Write the ENVI meta file if needed                            ####
          #
          if (ts_format == "ENVI Meta Files" | ts_format == "ENVI and GDAL") {
            
            if (out_format == "ENVI") {
              
              # retrieve nsamp and nrow from first hdr file
              head_file   <- paste0(out_meta_files_hdr[1])
              fileConn_hd <- file(head_file)
              nsamp       <- strsplit(readLines(fileConn_hd)[4], "=")[[1]][2]
              nrow        <- strsplit(readLines(fileConn_hd)[5], "=")[[1]][2]
              close(fileConn_hd)
            }
            
            if (out_format == "GTiff") {
              # retrieve nsamp and nrow from first tif file
              nsamp <- raster::raster(out_meta_files[1])@ncols
              nrow  <- raster::raster(out_meta_files[1])@nrows
            }
            
            meta_dir <- file.path(out_prod_folder, "Time_Series", "ENVI_META",
                                  sens_sel, meta_band)
            dir.create(meta_dir, showWarnings = FALSE, recursive = TRUE)
            
            # define fileneame for meta file
            meta_filename <- file.path(meta_dir, paste(file_prefix, meta_band,
                                                       doy_min, year_min,
                                                       doy_max, year_max,
                                                       "META.dat", sep = "_"))
            fileConn_meta <- file(meta_filename, "w")
            # Write first line
            writeLines(c("ENVI META FILE"), fileConn_meta)
            # Write the lines of the META file corresponding to each input file
            for (ff in out_meta_files) {
              writeLines(c(paste0("File : ", ff),
                           paste0("Bands: 1"),
                           paste0("Dims: 1-", nsamp, " , 1-", nrow), ""),
                         fileConn_meta)
            }
            close(fileConn_meta)
            
            # Compute the "wavelengths" - DOYS elapsed from 01/01/2000
            #
            temp_dates <- as.Date(strptime(paste(years, doys),
                                           format = "%Y %j"))
            elapsed <- signif(difftime(
              temp_dates, strptime(paste(2000, 001), format = "%Y %j"),
              units = "days"), 5)
            
            # Write the hdr file for the meta file
            fileConn_meta_hdr <- file(
              paste0(tools::file_path_sans_ext(meta_filename), ".hdr"), "w"
            )
            # Write first line
            writeLines(c("ENVI"), fileConn_meta_hdr)
            writeLines(c("Description = {ENVI META FILE}"), fileConn_meta_hdr)
            writeLines(paste0("samples = ", nsamp), fileConn_meta_hdr)	#nsamp
            writeLines(paste0("lines = ", nrow), fileConn_meta_hdr)			#lines
            writeLines(paste0("bands = ", length(out_meta_files)),     #nbands
                       fileConn_meta_hdr)
            writeLines(paste("header offset = 0"), fileConn_meta_hdr)
            # File type - fundamental
            writeLines(c("file type = ENVI Meta File"), fileConn_meta_hdr)
            writeLines(c("read procedures = {envi_read_spatial_m, envi_read_spectral_m}"), #nolint
                       fileConn_meta_hdr)
            # Band names
            writeLines(c("band names = {",
                         paste(basename(out_meta_files), collapse = "," ), "}"),
                       fileConn_meta_hdr)
            writeLines(c("wavelength units = DOY"), fileConn_meta_hdr)
            # Wavelengths == DOY from 01/01/2000
            writeLines(c("wavelength = {",
                         paste(as.numeric(elapsed), collapse = ","), "}"),
                       fileConn_meta_hdr)
            # Data Ignore Value
            writeLines(c("data ignore value = ", nodata_value),
                       fileConn_meta_hdr, sep = " ")
            writeLines("", fileConn_meta_hdr)		# Dummy
            close(fileConn_meta_hdr)
            
          }
          #   __________________________________________________________________
          #   # Write the GDAL vrt file if needed                           ####
          #
          if (ts_format == "GDAL vrt files" | ts_format == "ENVI and GDAL") {
            
            meta_dir <- file.path(out_prod_folder, "Time_Series", "GDAL",
                                  sens_sel,
                                  meta_band)
            dir.create(meta_dir, showWarnings = FALSE, recursive = TRUE)
            
            meta_filename <- file.path(meta_dir, paste(file_prefix, meta_band,
                                                       doy_min, year_min,
                                                       doy_max, year_max,
                                                       "GDAL.vrt",
                                                       sep = "_"))
            
            gdalUtils::gdalbuildvrt(out_meta_files, meta_filename,
                                    separate = TRUE,
                                    srcnodata = nodata_value,
                                    vrtnodata = nodata_value)
            
          } # end If on necessity to build GDAL vrt files
          
          #   __________________________________________________________________
          #   Create RasterStacks if needed                                 ####
          #
          if (rts == "Yes") {
            
            meta_dir <- file.path(out_prod_folder, "Time_Series", "RData",
                                  sens_sel, meta_band)
            dir.create(meta_dir, showWarnings = FALSE, recursive = TRUE)
            
            # create stack. Use "quick" since already sure about alignment !
            raster_ts <- raster::stack(out_meta_files, quick = TRUE)
            # Add the "time" dimension to the rasterstack
            raster_ts <- raster::setZ(raster_ts, temp_dates, name = "time")
            RData_filename <- file.path(meta_dir, paste(file_prefix, meta_band,
                                                        doy_min, year_min,
                                                        doy_max, year_max,
                                                        "RData.RData",
                                                        sep = "_"))
            save(raster_ts, file = RData_filename)
            
          } # end If on necessity to build R Stack files
        }
      }
    }
  }
}
