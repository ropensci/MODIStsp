#' @title Check if all files required for a given date already exist
#' @description Accessory function used to see if all expected out files for the
#'   selected date are already present in the output folder. If all expected out
#'   files are already present, check_files is set to TRUE, and the date is
#'   skipped in MODIStsp_process.
#' @param out_prod_folder `character` MODIStsp output folder
#' @param file_prefix `character` File prefix of the processed product
#'   (e.g., MOD13Q1)
#' @param yy `character` year
#' @param DOY `character` doy
#' @param bandnames `character array` Bandnames of the MODIS product
#' @param bandsel_orig_choice `numeric 0/1 array` Indicates which original MODIS
#'   layers were selected for processing (does not contain names of bands needed
#'   to compute SIs but not selected by the user!)
#' @param indexes_bandnames `character array` Names of available spectral
#'   indexes (standard + custom) available for the currently processed product
#' @param indexes_bandsel `numeric 0/1 array` Indicates which spectral indexes
#'   were selected for processing
#' @param quality_bandnames `character array` Name of available Quality
#'   Indicators for the currently processed product
#' @param quality_bandsel `numeric 0/1 array` Indicates which Quality Indicators
#'   were selected
#' @param out_format `character` GTiff or ENVI
#' @return check - logical = 1 if all expected output files are already existing
#' @author Lorenzo Busetto, phD (2014-2017) \email{lbusett@@gmail.com}
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0

check_files_existence <- function(out_prod_folder,
                                 file_prefix,
                                 yy,
                                 DOY,
                                 bandnames,
                                 bandsel_orig_choice,
                                 indexes_bandnames,
                                 indexes_bandsel,
                                 quality_bandnames,
                                 quality_bandsel,
                                 out_format) {

  # Initialize check to TRUE --> changed if even only one file missing
  check <- TRUE

  #  __________________________________________________________________________
  #  check existence of all files related to Original HDF layers           ####

  # cycle on selected bands
  for (band in which(bandsel_orig_choice == 1)) {
    # Create name for the TIFF/ENVI reprojected file to be checked
    outcheck_file <- file.path(
      out_prod_folder, bandnames[band],
      paste0(file_prefix, "_",
             bandnames[band], "_", yy, "_", DOY,
             ifelse(out_format == "GTiff", ".tif", ".dat"))
    )
    # check
    if (file.exists(outcheck_file) == FALSE) {
      check <- FALSE
    }
  }

  #  __________________________________________________________________________
  #  check existence of all files related to spectral indexes              ####

  # cycle on selected indexes
  for (band in which(indexes_bandsel == 1)) {
    # Create name for the TIFF/ENVI reprojected file to be checked
    outcheck_file <- file.path(
      out_prod_folder, indexes_bandnames[band],
      paste0(file_prefix, "_",
             indexes_bandnames[band], "_", yy, "_", DOY,
             ifelse(out_format == "GTiff", ".tif", ".dat"))
    )
    # check
    if (file.exists(outcheck_file) == FALSE) {
      check <- FALSE
    }
  }
  #  ___________________________________________________________________________
  # check existence of all files related to quality indicators              ####
  # cycle on selected quality indexes
  for (band in which(quality_bandsel == 1)) {
    # Create name for the TIFF/ENVI reprojected file to be checked
    outcheck_file <- file.path(
      out_prod_folder, quality_bandnames[band],
      paste0(file_prefix, "_",
             quality_bandnames[band], "_", yy, "_", DOY,
             ifelse(out_format == "GTiff", ".tif", ".dat"))
    )
    # check
    if (file.exists(outcheck_file) == FALSE) {
      check <- FALSE
    }
  }
  # return FALSE if at least one file was missing. Means that we cannot skip
  # the date completely, but have at least to look for missing layers and
  # process them
  return(check)
}
