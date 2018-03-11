#' @title gh_tiles_from_bbox
#' @description Helper function used to retrieve the tiles required to cover
#'  a given bounding box
#' @noRd
#' @importFrom gWidgets svalue
#' @noRd
gh_tiles_from_bbox <- function(h, wids, mod_proj_str, modis_grid) {
  #nocov start
  bbox <- as.numeric(c(gWidgets::svalue(wids$output_xmin),
                       gWidgets::svalue(wids$output_ymin),
                       gWidgets::svalue(wids$output_xmax),
                       gWidgets::svalue(wids$output_ymax)))
  # Check if bbox is consistent
  
  n_bbox_compiled <- length(which(is.finite(bbox)))
  if (gWidgets::svalue(wids$output_ext) != "Select MODIS Tiles" &
      n_bbox_compiled == 0) {
    gmessage("Please specify an output bounding box!", title = "Warning")
  } else if (gWidgets::svalue(wids$proj_choice) == "User Defined" &
             nchar(gWidgets::svalue(wids$output_proj4)) == 0) {
    gmessage("Please specify an output projection", title = "Warning")
  } else if (n_bbox_compiled < 4) {
    gmessage("Error in Selected Output extent", title = "Warning")
  } else if (bbox[1] > bbox[3] | bbox[2] > bbox[4]) {
    gmessage("Error in Selected Output extent", title = "Warning")
  } else {
    # If all checks pass, retrieve the tiles and set the widget
    gui_update_tiles(bbox,
                     gWidgets::svalue(wids$output_proj4),
                     mod_proj_str,
                     modis_grid,
                     wids)
  }
  #nocov end
}