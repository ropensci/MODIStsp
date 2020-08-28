shinyjs::disable("spafiletxt")

# observers used to deal with spatial file selection and corresponding map ----
shinyFiles::shinyFileChoose(input,
                            'spafile',
                            roots       = volumes,
                            defaultPath ='',
                            defaultRoot ='Home')

shiny::observe({
  req(input$spafile)
  # retrieve output path ----
  spafilepath  <- shinyFiles::parseFilePaths(roots=volumes,
                                             input$spafile)$datapath
  shiny::updateTextInput(session, "spafiletxt", "Spatial file",
                         file.path(spafilepath))
})

observe({

  spafilepath <-  req(input$spafiletxt)
  if (length(spafilepath) != 0) {
    invect <- try(sf::st_read(spafilepath, quiet = TRUE), silent = TRUE)
    if (!inherits(invect, "try-error")) {
      rv$spafilepath <- spafilepath
      rv$outbbox <- bbox_from_file(rv$spafilepath, input$outprojtxt)
      rv$invect  <- invect
      rv$inrast  <- NULL
      # shiny::updateTextInput(session, "spafiletxt", "Spatial file",
      #                        file.path(spafilepath))
    } else {
      inrast <- try(raster::raster(spafilepath))
      if (!inherits(inrast, "try-error")) {
        rv$spafilepath <- spafilepath
        rv$outbbox <- bbox_from_file(rv$spafilepath, input$outprojtxt)
        rv$inrast  <- inrast
        rv$invect  <- NULL
        # shiny::updateTextInput(session, "spafiletxt", "Spatial file",
        #                        file.path(spafilepath))
      } else {
        shinyalert::shinyalert(title = "", text = "Unable to read spatial file",
                               type = "error")
      }
    }
  }

  if (input$outprojtxt == "MODIS Sinusoidal") {
    # crs_out <- sf::st_crs('PROJCS["MODIS Sinusoidal",GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Sinusoidal"],PARAMETER["false_easting",0.0],PARAMETER["false_northing",0.0],PARAMETER["central_meridian",0.0],PARAMETER["semi_major",6371007.181],PARAMETER["semi_minor",6371007.181],UNIT["m",1.0]]')
    crs_out <- sf::st_crs("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")
  } else {
    crs_out <- check_projection(input$outprojtxt)
  }
  bbox <- st_bbox(c(xmin = rv$outbbox[1], xmax = rv$outbbox[3],
                    ymin = rv$outbbox[2], ymax = rv$outbbox[4]),
                  crs = crs_out)
  bbpoly <- sf::st_sf(geometry = sf::st_as_sfc(sf::st_bbox(bbox))) %>%
    sf::st_transform(4326)
  if (!is.null(rv$inrast)) {
    spafilemap <- leaflet::leaflet()  %>%
      leaflet::addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                        group = "Satellite") %>%
      leaflet::addTiles(group = "OpenStreetMap") %>%
      leaflet::addLayersControl(baseGroups = c("OpenStreetMap", "Satellite")) %>%
      leaflet::addRasterImage(rv$inrast) %>%
      leafem::addFeatures(bbpoly) %>%
      leaflet::addLegend(colors = c("red", "blue"),
                         labels = c("Loaded Raster", "Bounding Box (in output proj.)"))

    output$spafilemap <- leaflet::renderLeaflet(spafilemap)
  } else {
    if (!is.null(rv$invect)) {

      spafilemap <- leaflet::leaflet()  %>%
        leaflet::addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                          group = "Satellite") %>%
        leaflet::addTiles(group = "OpenStreetMap") %>%
        leaflet::addLayersControl(baseGroups = c("OpenStreetMap", "Satellite")) %>%
        leafem::addFeatures(sf::st_transform(rv$invect, 4326),
                            color = "red") %>%
        leafem::addFeatures(bbpoly) %>%
        leaflet::addLegend(colors = c("red", "blue"),
                           labels = c("Loaded Vector", "Bounding Box (in output proj.)"))

      output$spafilemap <- leaflet::renderLeaflet(spafilemap)
    }
  }

})
