#' @title gh_load_extent
#' @description Handler used to perform requirted actions if "load extent
#'  from spatial file" is clicked.
#' @noRd
#' @importFrom gWidgets size svalue
#' @noRd
gh_load_extent <- function(h, wids, out_proj_list, mod_proj_str,
                           modis_grid) {
  #nocov start
  choice <- try(gfile(
    type = "open",
    text = "Select a vector or raster file",
    # TODO add formats to the lists!
    filter = list("Spatial files" = list(patterns = c("*.shp", "*.kml",
                                                      "*.tif", "*.dat")),
                  "Vector layers" = list(patterns = c("*.shp", "*.kml")),
                  "Raster layers" = list(patterns = c("*.tif", "*.dat")),
                  "All files"     = list(patterns = "*"))
  ), silent = TRUE)
  if (class(choice) != "try-error" & length(choice) != 0) {
    # Show window until the process finishes
    message("[", date(), "]", " Retrieving the Extent, please wait...")
    wait_window       <- gwindow(title = "Please wait",
                                 width = 400, height = 40)
    gWidgets::size(wait_window) <- c(100, 8)
    addHandlerUnrealize(wait_window,
                        handler = function(h, ...) return(TRUE))
    glabel(
      text      = paste("Retrieving Extent, please wait..."),
      editable  = FALSE,
      container = wait_window
    )
    Sys.sleep(0.05)
    # Convert bbox coordinates to output projection

    # curr_proj <-
    #   out_proj_crs <- ifelse(
    #     gWidgets::svalue(wids$proj_choice) != "User Defined",
    #     out_proj_list[[gWidgets::svalue(wids$proj_choice)]],
    #     gWidgets::svalue(wids$output_proj4))
    curr_proj <- out_proj_crs <- gWidgets::svalue(wids$output_proj4)

    print(gWidgets::svalue(wids$proj_choice))
    # Create the bounding box in the chosen projection retrieving it from
    # the specified file
    bbox_out <- try(bbox_from_file(file_path = choice,
                                   crs_out   = out_proj_crs),
                    silent = TRUE)
    if (class(bbox_out) == "try-error") {
      gmessage(bbox_out, title = "Error Detected!")
    } else {

      proj  <- gui_get_proj(CRS(curr_proj))
      units <- gui_get_units(CRS(curr_proj), proj)
      # re-set bbox in the GUI according coordinates retrieved from file
      gui_update_bboxlabels(bbox_out,
                            units,
                            wids)

      # Set tiles according with the bounding box
      gui_update_tiles(bbox_out,
                       out_proj_crs,
                       mod_proj_str,
                       modis_grid,
                       wids)
    }
    message("[", date(), "]", " Retrieving Extent, please wait... DONE!")
    dispose(wait_window)

  }
  #nocov end
}
