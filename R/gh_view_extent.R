#' @title gh_view_extent
#' @description Handler used to perform actions required when the "View current
#'  extent" button is clicked.
#' @noRd
#' @importFrom gWidgets svalue
gh_view_extent <- function(ext_type, wids) {
  #nocov start
  v <- h <- NULL
  if (requireNamespace("mapedit")) {
    if (ext_type == "Select MODIS Tiles") {
      # leaflet <- leaflet::leaflet()
      
      min_h <- gWidgets::svalue(wids$start_x)
      max_h <- gWidgets::svalue(wids$end_x)
      min_v <- gWidgets::svalue(wids$start_y)
      max_v <- gWidgets::svalue(wids$end_y)
      
      tilemap <- get(load(system.file("ExtData/MODIS_Tiles_latlon.RData",
                                      package = "MODIStsp")))
      cursel <- subset(tilemap,
                       h >= min_h & h <= max_h & v >= min_v & v <= max_v)
      
      mm <- leaflet::leaflet(cursel)
      mm <- leaflet::addPolygons(mm)
      mm <- leaflet::addTiles(mm)
      mapedit::selectMap(
        mm,
        viewer = shiny::browserViewer(browser = getOption("browser"))
      )
    } else {
      
      bbox <- as.numeric(c(gWidgets::svalue(wids$output_xmin),
                           gWidgets::svalue(wids$output_ymin),
                           gWidgets::svalue(wids$output_xmax),
                           gWidgets::svalue(wids$output_ymax)))
      
      if (!(any(is.na(bbox)))) {
        bbox_out <- sf::st_bbox(
          c(xmin = bbox[1], ymin = bbox[2], xmax = bbox[3], ymax = bbox[4]),
          crs = gWidgets::svalue(wids$output_proj4))
        
        bbox_sf <- sf::st_as_sfc(bbox_out)
        bbox_sf <- sf::st_transform(bbox_sf, 4326)
        mm <- leaflet::leaflet(bbox_sf)
        mm <- leaflet::addPolygons(mm)
        mm <- leaflet::addTiles(mm)
        providers <- c("OpenStreetMap", "Esri.WorldImagery")
        mm <- leaflet::addProviderTiles(mm, "OpenStreetMap",
                                        group = "OpenStreetMap")
        mm <- leaflet::addProviderTiles(mm, "Esri.WorldImagery",
                                        group = "Esri.WorldImagery")
        mm <- leaflet::addLayersControl(
          mm,
          baseGroups = providers,
          options = leaflet::layersControlOptions(collapsed = FALSE))
        mapedit::selectMap(
          mm,
          viewer = shiny::browserViewer(browser = getOption("browser"))
        )
        
      } else {
        gmessage(strwrap(
          "Current Output extent is not valid!\n\n
           Please specify a bounding box or load it from a file!"))
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