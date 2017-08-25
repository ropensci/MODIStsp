#' @title bbox_from_file
#' @description Helper function used to retrieve the bounding box of a specified spatial file
#'   recognized by GDAL or OGG: the function reads the extent using gdalinfo or ogrinfo,
#'   converting it to a specified CRS.
#' @param file_path The path of the spatial file.
#' @param out_crs The output projection (string format).
#' @author Luigi Ranghetti, phD (2015-2017) \email{ranghetti.l@@irea.cnr.it}
#'
#' @importFrom gdalUtils gdalinfo gdalsrsinfo ogrinfo
#' @importFrom sp CRS
#' @importFrom rgeos show
#' @importFrom stats na.omit
#' @note License: GPL 3.0
#'
#' @importFrom sp CRS
#' @importFrom gdalUtils gdalsrsinfo gdalinfo
#' @importFrom rgdal GDALinfo ogrInfo
#' @importFrom utils head tail

bbox_from_file <- function(file_path, out_crs) {

  # Retrieve CRS using gdal: if fails, then the file is not a valid spatial file
  reference_gdalinfo <- suppressWarnings(try(GDALinfo(file_path),
                                             silent = TRUE))
  reference_ogrinfo  <- suppressWarnings(try(ogrInfo(file_path),
                                             silent = TRUE))
  
  if (is(reference_ogrinfo, "try-error") &
      is(reference_gdalinfo, "try-error")) {
    stop(paste("File format not recognised by GDAL or OGR.",
               if (class(reference_crs) == "try-error") {
                 paste("\n\nDetails:", reference_crs)
               }))
  }

  # If it does not fail, then retrieve the bounding box
  if (is(reference_gdalinfo, "try-error")) {
    reference_bbox <- matrix(
      reference_ogrinfo$extent,
      ncol=2, 
      dimnames=list(c("x","y"),c("min","max")))
    reference_crs <- gdalsrsinfo(file_path, as.CRS = TRUE)@projargs
  } else if (is(reference_ogrinfo, "try-error")) {
    reference_bbox <- gdalinfo(file_path, raw_output = FALSE)$bbox
    reference_crs <- attr(reference_gdalinfo, "projection")
  }
  
  # Convert the bounding box in the chosen projection (ensuring to full
  # include the original one)
  bbox_out <- reproj_bbox(reference_bbox,
                          reference_crs,
                          out_crs,
                          enlarge = TRUE)
  
  return(bbox_out)
}
