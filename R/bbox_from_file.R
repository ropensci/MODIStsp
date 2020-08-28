#' @title Retrieve bbox from a spatial file
#' @description Helper function used to retrieve the bounding box of a specified spatial file
#'   recognized by  `sf` or `raster`: the function reads the extent using `sf::st_bbox()`
#' @param file_path `character` path of a spatial file.
#' @param crs_out  (`crs` | `character`) crs of the desired output projection,
#' or string coercible to it using `sf::st_crs()` (e.g., WKT or numeric
#' EPSG code)
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @author Luigi Ranghetti, phD (2017) <ranghetti.l@irea.cnr.it>
#' @importFrom sf st_crs st_read st_bbox
#' @importFrom raster raster
#' @note License: GPL 3.0

bbox_from_file <- function(file_path, crs_out) {

  #nocov start
  if (!file.exists(file_path)) {
    stop("Specified file path does not exist. Aborting!")
  }

  if(suppressWarnings(is.character(crs_out) && !is.na(as.numeric(crs_out)))) {
    crs_out <- as.numeric(crs_out)
  } else {
    if (crs_out == "MODIS Sinusoidal") {
      # crs_out <- sf::st_crs('PROJCS["MODIS Sinusoidal",GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Sinusoidal"],PARAMETER["false_easting",0.0],PARAMETER["false_northing",0.0],PARAMETER["central_meridian",0.0],PARAMETER["semi_major",6371007.181],PARAMETER["semi_minor",6371007.181],UNIT["m",1.0]]')
      crs_out <- sf::st_crs("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")
    }
  }

  if (!inherits(crs_out, "crs")) {
    crs_out <- try(sf::st_crs(crs_out))
    if (!inherits(crs_out, "crs")) {
      stop("`crs_out` is not an object of (or cohercible to) class `crs`.",
           " Aborting!")
    }
  }
  #nocov end

  # try opening as vector. If it does not fail, then retrieve the bounding box
  # of the vector and the wkt of the projection.
  if (!inherits(try(vectin <- sf::st_read(file_path, quiet = TRUE),
                    silent = TRUE), "try-error")) {

    crs_in  <- st_crs(vectin)
    bbox_in <- matrix(as.numeric(sf::st_bbox(vectin)),
                      ncol = 2,
                      dimnames = list(c("x", "y"), c("min", "max")))

    # try opening as raster If it does not fail, then retrieve the bounding box
    # of the raster and the wkt of the projection.
  } else if (!inherits(try(suppressWarnings(rastin <- raster::raster(file_path)),
                           silent = TRUE), "try-error")) {

    # Else retrieve the bounding box of the raster and the wkt of the projection
    crs_in  <- sf::st_crs(rastin)
    bbox_in <- matrix(as.numeric(sf::st_bbox(rastin)),
                      ncol = 2,
                      dimnames = list(c("x", "y"), c("min", "max")))
  } else {
    stop(file_path, "does not appear to be a valid spatial",
         "file. Please check your inputs. Aborting!")
  }

  # Convert the bounding box in the chosen projection (ensuring to include the
  # full area covered in the original bbox by using "enlarge == TRUE")
  bbox_out <- reproj_bbox(bbox_in,
                          crs_in,
                          crs_out,
                          enlarge = TRUE)

  return(bbox_out)
}
