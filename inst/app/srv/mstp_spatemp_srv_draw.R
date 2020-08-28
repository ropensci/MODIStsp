# observers used to deal with extent drawing on map selection ----


# Selection from map ----
observeEvent(input$drawmap, ignoreNULL = FALSE, ignoreInit = TRUE,{

  extent_ns_namedd <- paste0("editor_",sample(1E9,1))
  extent_nsd <- NS(extent_ns_namedd)
  rv$extent_edits <- callModule(
    mapedit::editMod,
    extent_ns_namedd,
    leaflet::leaflet()  %>%
      leaflet::addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                        group = "Satellite"
      ) %>%
      leaflet::addTiles(group = "OpenStreetMap") %>%
      leaflet::addLayersControl(baseGroups = c("OpenStreetMap", "Satellite")),
    editor = "leafpm",
    editorOptions = list(
      toolbarOptions = leafpm::pmToolbarOptions(
        drawMarker = FALSE,
        drawPolyline = FALSE,
        drawCircle = FALSE
      )
    )
  )

  observe({
    showModal(modalDialog(
      title = "Draw the extent",
      size = "l",
      shiny::helpText(
        shiny::em("Click on the map to select the desired tiles.")),
      shiny::helpText(
        shiny::em("WARNING: If you select non contiguous tiles, all tiles creating a rectangle between minimum x/y and maximum x/y will be selected!")),
      mapedit::editModUI(extent_ns_namedd, height=500, width="100%"),
      easyClose = FALSE,
      footer = tagList(
        shiny::actionButton("save_extent_draw", strong("\u2000Ok"), icon=shiny::icon("check")),
        shiny::modalButton("\u2000Cancel", icon = shiny::icon("ban"))
      )
    )
    )}
  )
  # actionbutton to send back the selected tiles ----
  observeEvent(c(input$save_extent_draw,input$outprojtxt), ignoreInit = TRUE, ignoreNULL = TRUE,
               {
                 rv$drawn_extent <- rv$extent_edits()$finished
               })

})

observe({

  crs_out  <- shiny::req(input$outprojtxt)
  if (crs_out == "MODIS Sinusoidal") {
    # crs_out <- sf::st_crs('PROJCS["MODIS Sinusoidal",GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Sinusoidal"],PARAMETER["false_easting",0.0],PARAMETER["false_northing",0.0],PARAMETER["central_meridian",0.0],PARAMETER["semi_major",6371007.181],PARAMETER["semi_minor",6371007.181],UNIT["m",1.0]]')
    crs_out <- sf::st_crs("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")
  } else {
    crs_out <- check_projection(input$outprojtxt)
  }
  if (!is.null(rv$drawn_extent)) {

    tmpfile <- tempfile(fileext = ".gpkg")
    sf::st_write(rv$drawn_extent, tmpfile, quiet = TRUE)
    outbbox <- bbox_from_file(tmpfile, input$outprojtxt)
    bbox <- sf::st_bbox(c(xmin = outbbox[1],
                          xmax = outbbox[3],
                          ymin = outbbox[2],
                          ymax = outbbox[4]),
                        crs  = crs_out)
    rv$outbbox <- outbbox
    polybbox <- sf::st_as_sf(sf::st_as_sfc(bbox)) %>%
      sf::st_transform(4326)

    seldrawmap <- leaflet::leaflet()  %>%
      leaflet::addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                        group = "Satellite"
      ) %>%
      leaflet::addTiles(group = "OpenStreetMap") %>%
      leaflet::addLayersControl(baseGroups = c("OpenStreetMap", "Satellite")) %>%
      leafem::addFeatures(rv$drawn_extent, color = "red") %>%
      leafem::addFeatures(polybbox, color = "blue", fillOpacity = 0) %>%
      leaflet::addLegend(colors = c("red", "blue"),
                         labels = c("Drawn Extent", "Bounding Box (in output proj.)"))

    output$drawmap_interactive <- leaflet::renderLeaflet(seldrawmap)
    removeModal()
  }
})
