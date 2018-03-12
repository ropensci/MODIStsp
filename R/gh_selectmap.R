#' @title gh_selectmap
#' @description Handler used to perform actions required when the "Select from map"
#'  button is clicked.
#' @noRd
#' @importFrom data.table rbindlist
#' @importFrom gWidgets svalue
gh_selectmap <- function(h, ext_type, wids, mod_proj_str, modis_grid) {
  #nocov start
  if (requireNamespace("mapedit")) {
    if (ext_type == "Select MODIS Tiles") {

      # On MODIS tiles selection, use editFeatures to allow selection
      # from the Tiles Map
      tilemap <- get(load(system.file("ExtData" ,"MODIS_Tiles_latlon.RData",
                                      package = "MODIStsp")))
      sel <- mapedit::selectFeatures(
        tilemap,
        viewer = shiny::browserViewer(browser = getOption("browser"))
      )
      # On return, check the selection to see if it correspond to a rectangular
      # area. If not, message and abort.

      if (inherits(sel, "data.frame") & length(sel$h > 0)) {

        seltiles <- lapply(sel[["Name"]], FUN = function(x){
          h <- as.numeric(str_split_fixed(x, "[a-z]:", 3)[2])
          v <- as.numeric(str_split_fixed(x, "[a-z]:", 3)[3])
          data.frame(h = h, v = v)})
        seltiles <- data.table::rbindlist(seltiles)
        error_sel <- FALSE
        if (length(unique(sel[["h"]])) == 1) {
          min_h <- max_h <- sel[["h"]]
        } else {
          if (max(diff(sort(sel[["h"]]))) <= 1) {
            min_h <- min(sel[["h"]])
            max_h <- max(sel[["h"]])
          } else {
            error_sel <- TRUE
          }
        }

        if (length(unique(sel[["v"]])) == 1) {
          min_v <- max_v <- sel[["v"]]
        } else {
          if (max(diff(sort(sel[["v"]]))) <= 1) {
            min_v <- min(sel[["v"]])
            max_v <- max(sel[["v"]])
          } else {
            error_sel <- TRUE
          }
        }

        if (error_sel) {
          gmessage(strwrap(
            "Your selection contains non-contiguous tiles!\n
            MODIStsp only allows processing for contigous tiles selections!\n\n
            Please select again!"), icon = "warning")
        } else {
          # on proper selection, update the tiles sliders
          gWidgets::svalue(wids$start_x) <- min_h
          gWidgets::svalue(wids$end_x)   <- max_h
          gWidgets::svalue(wids$start_y) <- min_v
          gWidgets::svalue(wids$end_y)   <- max_v
        }
      }
    } else {

      # On Custom Area selection, use editMap to allow drawing a custom area
      tilemap <- get(load(system.file("ExtData/MODIS_Tiles_latlon.RData",
                                      package = "MODIStsp")))
      mm  <-  mapview::mapview(tilemap, alpha.regions = 0.1, color = "grey75")
      sel <- mapedit::editMap(
        mm,
        viewer = shiny::browserViewer(browser = getOption("browser")),
        title = "Select the output extent using the tools on the left")

      if (!is.null(sel[["finished"]])) {
        sel_bbox  <- sf::st_bbox(sel[["finished"]])
        curr_proj <- svalue(wids$output_proj4)

        #reproject the bbox to get coordinates in output projection. Use
        #enlarge = TRUE to be sure that all the area in the selected bbox
        #will be included in the extent in the target projection

        bbox_out <- reproj_bbox(sel_bbox,
                                "+init=epsg:4326",
                                curr_proj,
                                enlarge = TRUE)

        proj  <- gui_get_proj(CRS(curr_proj))
        units <- gui_get_units(CRS(curr_proj), proj)
        gWidgets::svalue(wids$pixsize2_lab) <- units

        # re-set bbox in the GUI according coordinates retrieved from file
        gui_update_bboxlabels(bbox_out,
                              units,
                              wids)

        # Set tiles according with the bounding box
        gui_update_tiles(bbox_out,
                         curr_proj,
                         mod_proj_str,
                         modis_grid,
                         wids)
      }
    }
  } else {
    gmessage(strwrap(
      "You need to install package `mapedit` to be able to
      use this functionality!\n\n
      You can install it using `install.packages(mapedit)`"),
      icon = "warning")
  }
  #nocov end
}
