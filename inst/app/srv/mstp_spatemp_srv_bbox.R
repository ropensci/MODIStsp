# observers used to deal with bbox manual selection ----
# (Manual or from map)

# Selection from tiles number (Manual) ----
observe({
  curminx  <- shiny::req(input$bboxxmin)
  curminy  <- shiny::req(input$bboxymin)
  curmaxx  <- shiny::req(input$bboxxmax)
  curmaxy  <- shiny::req(input$bboxymax)
  if(curminx > curmaxx) {
    shiny::updateNumericInput(session, "bboxxmax",
                              min = input$bboxxmin + 1, value = input$bboxxmin + 1,
                              step = 1)
  } else {
    shiny::updateNumericInput(session, "bboxxmax",
                              value = input$bboxxmax, step = 1)
  }
  if(curminy > curmaxy) {
    shiny::updateNumericInput(session, "bboxymax",
                              min   = input$bboxymin + 1,
                              value = input$bboxymin + 1,
                              step = 1)
  } else {
    shiny::updateNumericInput(session, "bboxymax",
                              value = input$bboxymax,
                              step = 1)
  }
})


observe({
  curminx  <- shiny::req(input$bboxxmin)
  curminy  <- shiny::req(input$bboxymin)
  curmaxx  <- shiny::req(input$bboxxmax)
  curmaxy  <- shiny::req(input$bboxymax)
  crs_out  <- shiny::req(input$outprojtxt)
  if (crs_out == "MODIS Sinusoidal") {
    # crs_out <- sf::st_crs('PROJCS["MODIS Sinusoidal",GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Sinusoidal"],PARAMETER["false_easting",0.0],PARAMETER["false_northing",0.0],PARAMETER["central_meridian",0.0],PARAMETER["semi_major",6371007.181],PARAMETER["semi_minor",6371007.181],UNIT["m",1.0]]')
    crs_out <- sf::st_crs("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")
  } else {
    crs_out <- check_projection(input$outprojtxt)
  }

  if(curmaxx >= curminx & curmaxy >= curminy) {
    bbox <- sf::st_bbox(c(xmin = curminx,
                          ymin = curminy,
                          xmax = curmaxx,
                          ymax = curmaxy),
                        crs = crs_out)
    polybbox <- sf::st_as_sf(sf::st_as_sfc(bbox))
    tmpfile <- tempfile(fileext = ".gpkg")
    sf::st_write(polybbox, tmpfile, quiet = TRUE)
    rv$outbbox <- bbox_from_file(tmpfile, input$outprojtxt)

    selbboxmap <- leaflet::leaflet() %>%
      leaflet::addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                        group = "Satellite"
      ) %>%
      leaflet::addTiles(group = "OpenStreetMap") %>%
      leaflet::addLayersControl(baseGroups = c("OpenStreetMap", "Satellite")) %>%
      leafem::addFeatures(sf::st_transform(polybbox, 4326))

    output$bboxmap <- leaflet::renderLeaflet(selbboxmap)
  }
})



