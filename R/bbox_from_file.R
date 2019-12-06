#' @title Retrieve bbox from a spatial file
#' @description Helper function used to retrieve the bounding box of a specified spatial file
#'   recognized by GDAL or OGG: the function reads the extent using gdalinfo or ogrinfo,
#'   converting it to a specified CRS.
#' @param file_path `string` path of a spatial file.
#' @param crs_out `string` proj4string of the desired output projection.
#' @author Luigi Ranghetti, phD (2015-2017) \email{ranghetti.l@@irea.cnr.it}
#' @importFrom rgdal GDALinfo ogrInfo
#' @importFrom methods is
#' @note License: GPL 3.0

bbox_from_file <- function(file_path, crs_out) {

  # Sys.setenv(PROJ_LIB = "C:\\OSGeo4W64\\share\\proj")
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
# browser()
  # If it does not fail, then retrieve the bounding box
  if (methods::is(in_gdalinfo, "try-error")) {
    bbox_in <- matrix(in_ogrinfo$extent,
                      ncol = 2,
                      dimnames = list(c("x", "y"), c("min", "max")))
    # crs_in <- gdalUtils::gdalsrsinfo(file_path, as.CRS = TRUE)@projargs

    crs_in <- rgdal::ogrInfo(file_path)$wkt2
    if (is.null(crs_in)) {
      crs_in <- sp::CRS(in_ogrinfo$p4s)
    }

  } else if (methods::is(in_ogrinfo, "try-error")) {
    # ginfo <- rgdal::GDALinfo(file_path)
    bbox_in <- matrix(
      c(in_gdalinfo[["ll.x"]],
        in_gdalinfo[["ll.y"]],
        in_gdalinfo[["ll.x"]] + in_gdalinfo[["res.x"]] * in_gdalinfo[["columns"]],
        in_gdalinfo[["ll.y"]] + in_gdalinfo[["res.y"]] * in_gdalinfo[["rows"]]),
      nrow = 2)
    crs_in <- sp::CRS(attr(in_gdalinfo, "projection"))
  }

  # Convert the bounding box in the chosen projection (ensuring to include the
  # full area covered in the original bbox by using "enlarge == TRUE")
  bbox_out <- reproj_bbox(bbox_in,
                          crs_in,
                          crs_out,
                          enlarge = TRUE)

  return(bbox_out)
}
