#' @title tiles_from_bbox
#' @description Helper function used to retrieve the tiles required to cover
#'  a given bounding box
#' @noRd
tiles_from_bbox <- function(bbox, outproj) {

  # mod_proj_str <- sf::st_crs(
  #   'PROJCS["MODIS Sinusoidal",GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Sinusoidal"],PARAMETER["false_easting",0.0],PARAMETER["false_northing",0.0],PARAMETER["central_meridian",0.0],
  #   PARAMETER["semi_major",6371007.181],PARAMETER["semi_minor",6371007.181],UNIT["m",1.0]]')
  #
  mod_proj_str <- sf::st_crs("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")

  bbox_mod  <- reproj_bbox(bbox,
                           outproj,
                           mod_proj_str,

                           enlarge = TRUE)

  modis_grid <- get(load(
    system.file(
      "ExtData/MODIS_Tiles.RData", package = "MODIStsp")))

  d_bbox_mod_tiled     <- suppressWarnings(sf::st_crop(modis_grid,
                                                       xmin = bbox_mod[1],
                                                       ymin = bbox_mod[2],
                                                       xmax = bbox_mod[3],
                                                       ymax = bbox_mod[4]))
  return(c(
    xmintile = min(d_bbox_mod_tiled$H),
    xmaxtile   = max(d_bbox_mod_tiled$H),
    ymintile = min(d_bbox_mod_tiled$V),
    ymaxtile   = max(d_bbox_mod_tiled$V)
  ))

}
