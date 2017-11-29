#' @title get_reqbands
#' @description FUNCTION_DESCRIPTION
#' @param bands_indexes_matrix PARAM_DESCRIPTION
#' @param out_prod_folder PARAM_DESCRIPTION
#' @param file_prefix PARAM_DESCRIPTION
#' @param yy PARAM_DESCRIPTION
#' @param DOY PARAM_DESCRIPTION
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
  # do a check to see if the product has at least one Quality Layer
  # or Possible Index
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
      if (file.exists(out_filename) == FALSE | reprocess == "Yes") {
        # if the index does not exists then find out the original
        # bands required to compute it
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
