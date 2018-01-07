#' @title Retrieve bbox from a spatial file
#' @description Helper function used to retrieve the bounding box of a specified spatial file
#'   recognized by GDAL or OGG: the function reads the extent using gdalinfo or ogrinfo,
#'   converting it to a specified CRS.
#' @param file_path `string` path of a spatial file.
#' @param crs_out `string` proj4string of the desired output projection.
#' @author Luigi Ranghetti, phD (2015-2017) \email{ranghetti.l@@irea.cnr.it}
#' @importFrom gdalUtils gdalsrsinfo gdalinfo
#' @importFrom rgdal GDALinfo ogrInfo
#' @importFrom methods is
#' @note License: GPL 3.0

bbox_from_file <- function(file_path, crs_out) {

  # Retrieve CRS using gdal: if fails, then the file is not a valid spatial file
  in_gdalinfo <- suppressWarnings(try(rgdal::GDALinfo(file_path),
                                             silent = TRUE))
  in_ogrinfo  <- suppressWarnings(try(rgdal::ogrInfo(file_path),
                                             silent = TRUE))

  if (methods::is(in_ogrinfo, "try-error") &
      methods::is(in_gdalinfo, "try-error")) {
    stop(file_path, "is not recognised by GDAL or OGR as a valid spatial",
         "file. Please check your inputs. Aborting!")
  }

  # If it does not fail, then retrieve the bounding box
  if (methods::is(in_gdalinfo, "try-error")) {
    bbox_in <- matrix(in_ogrinfo$extent,
                             ncol = 2,
                             dimnames = list(c("x", "y"), c("min", "max")))
    crs_in <- gdalUtils::gdalsrsinfo(file_path, as.CRS = TRUE)@projargs
  } else if (methods::is(in_ogrinfo, "try-error")) {
    bbox_in <- gdalUtils::gdalinfo(file_path, raw_output = FALSE)$bbox
    crs_in  <- attr(in_gdalinfo, "projection")
  }

  # Convert the bounding box in the chosen projection (ensuring to include the
  # full area covered in the original bbox by using "enlarge == TRUE")
  bbox_out <- reproj_bbox(bbox_in,
                          crs_in,
                          crs_out,
                          enlarge = TRUE)

  return(bbox_out)
}
