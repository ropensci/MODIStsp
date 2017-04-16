#' MODIStsp_check_files
#' @description Accessory function used to see if all expected out files for the selected date are already present.
#' @details if all expected out files for the selected date are already present, check_files is set to TRUE, and date is skipped
#' in MODIStsp_process
#' @param out_prod_folder string output folder name
#' @param file_prefix string file prefix of the product (e.g., MOD13Q1)
#' @param bandnames string array Bandnames of the MODIS product
#' @param bandsel_orig_choice 0/1 array Indicates which original modis layers were selected to be processed
#' @param yy string year
#' @param DOY string doy
#' @param out_format string Gtiff or ENVI
#' @param indexes_bandnames string array Name of available spectral indexes forthe product
#' @param indexes_bandsel  0/1 array Indicates which ospectral indexes were selected to be processed
#' @param quality_bandnames string array Name of available Quality Indicators for the product
#' @param quality_bandsel 0/1 array Indicates which Quality Indicators were selected to be processed
#' @return check - logical = 1 if all expected output files are already existing
#'
#' @author Lorenzo Busetto, phD (2014-2015) \email{busetto.l@@irea.cnr.it}
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0

MODIStsp_check_files <- function(out_prod_folder, file_prefix, bandnames, 
                                 bandsel_orig_choice, yy, DOY, out_format,
                                 indexes_bandnames, indexes_bandsel, 
                                 quality_bandnames, quality_bandsel) {

  check <- T   # Initialize check to T --> changed if even only one file missing

  # check existence of all files related to Original HDF layers ----
  # cycle on selected bands
  for (band in which(bandsel_orig_choice == 1)) {
    # Create name for the HDF mosaic
    outfile     <- paste0(out_prod_folder, "/", bandnames[band], "_", yy, "_", DOY, ".hdf")    
    # Create name for the TIFF reprojected  mosaic
    outrep_file <- file.path(out_prod_folder, bandnames[band], 
                             paste0(file_prefix, "_",
                                    sub("[.][^.]*$", "", 
                                        basename(outfile), perl = TRUE)))
    if (out_format == "GTiff") {
      outder_file <- paste0(outrep_file, ".tif")
      }
    if (out_format == "ENVI") {
      outder_file <- paste0(outrep_file, ".dat")
      }
    if (file.exists(outder_file) == FALSE) {
      check <- F
      }
  }

  # check existence of all files related to spectral indexes ----
  # cycle on selected indexes
  for (band in which(indexes_bandsel == 1)) {
    # Create name for the HDF mosaic
    outfile     <- paste0(out_prod_folder, "/", indexes_bandnames[band], "_", yy, "_", DOY, ".hdf") 
    # Create name for the TIFF reprojected  mosaic
    outder_file <- file.path(out_prod_folder, indexes_bandnames[band], 
                             paste0(file_prefix, "_",
                                    sub("[.][^.]*$", "",
                                        basename(outfile), perl = TRUE)))
    if (out_format == "GTiff") {
      outder_file <- paste0(outder_file, ".tif")
      }
    if (out_format == "ENVI") {
      outder_file <- paste0(outder_file, ".dat")
      }
    if (file.exists(outder_file) == FALSE) {
      check <- FALSE
      }
  }

  # check existence of all files related to quality indicators ----
  # # cycle on selected quality
  for (band in which(quality_bandsel == 1)) {
    # Create name for the HDF mosaic
    outfile <- paste0(out_prod_folder, "/", quality_bandnames[band], "_", yy, "_", DOY, ".hdf")   
    # Create name for the TIFF reprojected  mosaic
    outder_file <- file.path(out_prod_folder, quality_bandnames[band], 
                             paste0(file_prefix, "_", 
                                    sub("[.][^.]*$", "", 
                                        basename(outfile), perl = TRUE)))  
    if (out_format == "GTiff") {
      outder_file <- paste0(outder_file, ".tif")
      }
    if (out_format == "ENVI") {
      outder_file <- paste0(outder_file, ".dat")
      }
    if (file.exists(outder_file) == FALSE) {
      check <- FALSE
      }
  }

  return(check)  # return FALSE if at least one file missing
}
