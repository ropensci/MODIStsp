# observers used to deal with tiles selection ----
# (Manual or from map)


rv$tilemap <- get(load(system.file("ExtData" ,"MODIS_Tiles_latlon.RData",
                                   package = "MODIStsp")))

# Selection from map ----
observeEvent(input$tilemap, ignoreNULL = FALSE, ignoreInit = TRUE,{

  rv_seltiles <- reactiveValues(intersect=NULL, selectgrid=NULL)

  extent_ns_name <- paste0("editor_",sample(1E9,1))
  extent_ns <- NS(extent_ns_name)
  g_sel <- callModule(
    mapedit::selectMod,
    extent_ns_name,
    leaflet::leaflet()  %>%
      leaflet::addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                        group = "Satellite") %>%
      leaflet::addTiles(group = "OpenStreetMap") %>%
      leaflet::addLayersControl(baseGroups = c("OpenStreetMap", "Satellite")) %>%
      leafem::addFeatures(rv$tilemap, layerId = ~seq_len(dim(rv$tilemap)[1]),
                          label = ~tile_id)
  )

  observe({
    # Find the intersection of selected grid with MODIS tiles
    gs <- g_sel()
    rv$selecttiles <- sf::st_sf(
      rv$tilemap[as.numeric(gs[which(gs$selected==TRUE),"id"]),]
    )

  })

  observe({
    showModal(modalDialog(
      title = "Draw the extent",
      size = "l",
      shiny::helpText(
        shiny::em("Click on the map to select the desired tiles.")),
      shiny::helpText(
        shiny::em("WARNING: If you select non contiguous tiles, all tiles creating a rectangle between minimum x/y and maximum x/y will be selected!")),
      mapedit::selectModUI(extent_ns_name, height=500, width="100%"),
      easyClose = FALSE,
      footer = tagList(
        shiny::actionButton("save_extent_tiles", strong("\u2000Ok"), icon=shiny::icon("check")),
        shiny::modalButton("\u2000Cancel", icon = shiny::icon("ban"))
      )
    )
    )}
  )
  # actionbutton to send back the selected tiles ----
  observeEvent(input$save_extent_tiles, ignoreInit = TRUE, ignoreNULL = TRUE,
               {
                 removeModal()
                 curtiles <- rv$selecttiles
                 seltiles <- data.frame(h = curtiles$h, v = curtiles$v)
                 selh <- seq(min(seltiles$h), max(seltiles$h), 1)
                 selv <- seq(min(seltiles$v), max(seltiles$v), 1)
                 whichtiles <- which((rv$tilemap$h %in% selh) & (rv$tilemap$v %in% selv))
                 rv$curtiles <- rv$tilemap[whichtiles,]
                 bbox <- as.numeric(sf::st_bbox(rv$curtiles))

                 seltilesmap <- leaflet::leaflet() %>%
                   leaflet::addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                                     group = "Satellite") %>%
                   leaflet::addTiles(group = "OpenStreetMap") %>%
                   leaflet::addLayersControl(baseGroups = c("OpenStreetMap", "Satellite")) %>%
                   leafem::addFeatures(rv$tilemap) %>%
                   leafem::addFeatures(rv$curtiles, color = "red", label = ~tile_id) %>%
                   leaflet::fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
                 output$tilesmap_interactive <- leaflet::renderLeaflet(seltilesmap)
               }
  )
})

# Selection from tiles number (Manual) ----
observe({
  curminh  <- shiny::req(input$xmintile)
  curminv <- shiny::req(input$ymintile)
  curmaxh  <- shiny::req(input$xmaxtile)
  curmaxv  <- shiny::req(input$ymaxtile)
  if(curminh > curmaxh) {
    shiny::updateNumericInput(session, "xmaxtile", "Maximum X tile", min = input$xmintile, max = 35, value = input$xmintile, step = 1)
  } else {
    shiny::updateNumericInput(session, "xmaxtile", "Maximum X tile", min = 0, max = 35, value = input$xmaxtile, step = 1)
  }
  if(curminv > curmaxv) {
    shiny::updateNumericInput(session, "ymaxtile", "Maximum Y tile", min = input$ymintile, max = 35, value = input$ymintile, step = 1)
  } else {
    shiny::updateNumericInput(session, "ymaxtile", "Maximum Y tile", min = 0, max = 17, value = input$ymaxtile, step = 1)
  }
})


observe({
  curminh  <- shiny::req(input$xmintile)
  curminv <- shiny::req(input$ymintile)
  curmaxh  <- shiny::req(input$xmaxtile)
  curmaxv  <- shiny::req(input$ymaxtile)
  if(curmaxh >= curminh & curmaxv >= curminv) {
    selh <- seq(min(req(input$xmintile)), max(req(input$xmaxtile)), 1)
    selv <- seq(min(req(input$ymintile)), max(req(input$ymaxtile)), 1)
    whichtiles <- which((rv$tilemap$h %in% selh) & (rv$tilemap$v %in% selv))
    rv$curtiles <- rv$tilemap[whichtiles,]
    bbox <- as.numeric(sf::st_bbox(rv$curtiles))

    seltilesmap <- leaflet::leaflet() %>%
      leaflet::addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                        group = "Satellite") %>%
      leaflet::addTiles(group = "OpenStreetMap") %>%
      leaflet::addLayersControl(baseGroups = c("OpenStreetMap", "Satellite")) %>%
      leafem::addFeatures(rv$tilemap) %>%
      leafem::addFeatures(rv$curtiles, color = "red", label = ~tile_id) %>%
      leaflet::fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
    output$tilesmap <- leaflet::renderLeaflet(seltilesmap)
  }
})



