#' @title Reproject a bounding box
#' @description
#'  Helper function used to reproject bounding boxes; setting the parameter
#'  'enlarge' allows to choose if the new one would be the one which completely
#'  includes the original extent in the output projection, or if is simply the
#'  one obtained by reprojecting the upper-left and the lower-right corners.
#' @param bbox The input bounding box (it can be a matrix obtained from `sp::bbox()`,
#'  or a numeric vector in the format (xmin, ymin, xmax, ymax)).
#' @param in_proj (`crs` | `character`) crs of the input projection,
#'  or string coercible to it using `sf::st_crs()` (e.g., WKT or numeric
#'  EPSG code)
#' @param out_proj `crs` `crs` of the output projection, or string coercible to
#'  it using `sf::st_crs()` (e.g., WKT or numeric EPSG code)
#' @param enlarge `logical`` if TRUE, the reprojected bounding box is the
#'  one which completely include the original one; if FALSE, it is simply the
#'  one obtained by reprojecting the upper-left and the lower-right corners.
#' @author Luigi Ranghetti, phD (2015)
#' @note License: GPL 3.0
#' @importFrom sf st_crs st_as_sf st_set_crs st_write st_as_text st_bbox st_read
#' @importFrom gdalUtilities ogr2ogr

reproj_bbox <- function(bbox, in_proj, out_proj, enlarge=TRUE) {

  # fix bbox if it is a list (#228)
  if (inherits(bbox, "list")) {bbox <- unlist(bbox)}

  if (!inherits(in_proj, "crs")) {
    if (suppressWarnings(!is.na(as.numeric(in_proj)))) {
      in_proj <- as.numeric(in_proj)
    } else {
      if (in_proj == "MODIS Sinusoidal") {
        # in_proj <- sf::st_crs('PROJCS["MODIS Sinusoidal",GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Sinusoidal"],PARAMETER["false_easting",0.0],PARAMETER["false_northing",0.0],PARAMETER["central_meridian",0.0],PARAMETER["semi_major",6371007.181],PARAMETER["semi_minor",6371007.181],UNIT["m",1.0]]') #nolint)
        in_proj <- sf::st_crs("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")
      }
    }
    in_proj <- try(sf::st_crs(in_proj))
    if (!inherits(in_proj, "crs")) {
      stop("`in_proj` is not of (or can be convereted to) class `crs`.",
           " Aborting!")
    }
  }

  if (!inherits(out_proj, "crs")) {
    if (suppressWarnings(!is.na(as.numeric(out_proj)))){
      out_proj <- as.numeric(out_proj)
    } else {
      if (out_proj == "MODIS Sinusoidal") {
        out_proj <- sf::st_crs("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")
        # out_proj <- sf::st_crs('PROJCS["MODIS Sinusoidal",GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Sinusoidal"],PARAMETER["false_easting",0.0],PARAMETER["false_northing",0.0],PARAMETER["central_meridian",0.0],PARAMETER["semi_major",6371007.181],PARAMETER["semi_minor",6371007.181],UNIT["m",1.0]]') #nolint)
      }}
    out_proj <- try(sf::st_crs(out_proj))
    if (!inherits(out_proj, "crs")) {
      stop("`out_proj` is not of (or can be convereted to) class `crs`.",
           " Aborting!")
    }
  }

  if (suppressWarnings(!any(is.na(bbox)))) {
    # densify the original bounding box
    N_dens <- ifelse(enlarge, 1000, 1)

    d_bbox_in <- data.frame(
      X = as.numeric(c(bbox[1] + (diff(bbox[c(1, 3)])) * (0:N_dens) / N_dens,
                       rep(bbox[3], N_dens - 1),
                       bbox[1] + (diff(bbox[c(1, 3)])) * (N_dens:0) / N_dens,
                       rep(bbox[1], N_dens - 1))),
      Y = as.numeric(c(rep(bbox[2], N_dens),
                       bbox[2] + (diff(bbox[c(2, 4)])) * (0:N_dens) / N_dens,
                       rep(bbox[4], N_dens - 1),
                       bbox[2] + (diff(bbox[c(2, 4)])) * (N_dens:1) / N_dens)),
      stringsAsFactors = FALSE
    )


    # convert to sf POLYGON

    pts  <- sf::st_as_sf(d_bbox_in, coords = c("X", "Y"), agr = "constant",
                         crs = in_proj)

    pts_convert <- sf::st_transform(pts, out_proj)

    bbox_out    <- sf::st_bbox(pts_convert)
    bbox_out <- matrix(bbox_out,
                       ncol = 2,
                       dimnames = list(c("x", "y"), c("min", "max")))

    # Legacy Ugly workaround to avoid gdal3 problems: save temporary files and use
    # gdalwarp for the conversion. This allows using WKT representation ----
    # d_bbox_in_file <- tempfile(fileext = ".geojson")
    # sf::st_write(pts, d_bbox_in_file, quiet = TRUE, delete_layer = TRUE)
    #
    # d_bbox_out_file <- tempfile(fileext = ".geojson")

    # gdalUtilities::ogr2ogr(d_bbox_in_file, d_bbox_out_file,
    #                        s_srs = sf::st_crs(in_proj),
    #                        t_srs = sf::st_crs(out_proj))
    #
    # bbox_out <- as.numeric(sf::st_bbox(sf::st_read(d_bbox_out_file, quiet = TRUE)))
    return(bbox_out)
  } else {
    return(bbox)
  }
}
