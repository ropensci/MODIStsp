#' @title Identify the MODIS original bands needed for a given processing run
#' @description Helper function used in MODIStsp_process to identify which 
#'  MODIS hdf layers are required for the current process. The required layers
#'  include all MODIS original layers selected by the user, plus all those 
#'  required to compute the Spectral Indexes and Quality Indicators selected
#'  by the user
#' @param bands_indexes_matrix `matrix` built by `set_bandind_matrix`
#' @param out_prod_folder `character` Main folder where the MODIStsp processed 
#'  raster will be stored. Used to check if a given processed image already exists.
#' @param file_prefix File prefix corresponding to the MODIS product being 
#'  processed. Used to check if a given processed image already exists.
#' @param yy Year corresponding to the image being processed. Used to check if
#'  a given processed image already exists.
#' @param DOY DOY corresponding to the image being processed. Used to check if
#'  a given processed image already exists.. Used to check if a given processed image already exists.
#' @inheritParams MODIStsp_process
#' @return req_bands_indexes
#' @rdname get_reqbands
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
get_reqbands <- function(bands_indexes_matrix,
                         indexes_bandsel,
                         indexes_bandnames,
                         quality_bandsel,
                         quality_bandnames,
                         out_prod_folder,
                         file_prefix,
                         yy, DOY,
                         out_format,
                         reprocess) {
  # do a check to see if the product has at least one Quality indicator
  # or spectral index
  req_bands_indexes <- bands_indexes_matrix
  req_bands_indexes[, ] <- 0

  if (length(indexes_bandnames) != 0 |
      length(quality_bandnames) != 0 ) {
    # build matrix similar to band_indexes, but specific for this
    # year-doy process
    #

    for (band in which(indexes_bandsel == 1)) {
      indexes_band <- indexes_bandnames[band]
      out_filename <- file.path(
        out_prod_folder,
        indexes_band,
        paste0(file_prefix, "_", indexes_band, "_", yy, "_", DOY,
               ifelse(out_format == "GTiff", ".tif", ".dat"))
      )
      # if the index does not exists then find out the original
      # bands required to compute it
      if (file.exists(out_filename) == FALSE | reprocess == "Yes") {
        req_bands_indexes[, band] <- bands_indexes_matrix[, band]
      }
    }

    for (band in which(quality_bandsel == 1)) {
      quality_band <- quality_bandnames[band]
      out_filename <- file.path(
        out_prod_folder, quality_band,
        paste0(file_prefix, "_", quality_band, "_", yy, "_", DOY,
               ifelse(out_format == "GTiff", ".tif", ".dat"))
      )

      # if the QI does not exists then find out the original
      # bands required for it
      if (file.exists(out_filename) == FALSE | reprocess == "Yes") {
        req_bands_indexes[, band + length(indexes_bandsel)] <-
          bands_indexes_matrix[, band + length(indexes_bandsel)]
      }
    }
  }
  req_bands_indexes
}
