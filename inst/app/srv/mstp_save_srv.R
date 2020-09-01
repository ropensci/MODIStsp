# observers to deal with saving options ----
shiny::observeEvent(input$save_opts, {
  # retrieve output path ----
  shinyFiles::shinyFileSave(input, 'save_opts', roots = volumes,
                            defaultRoot = "Home",
                            defaultPath = "")
  outpath <- shinyFiles::parseSavePath(roots=volumes, input$save_opts)$datapath
  if(length(outpath != 0)) {
    # perform checks on parameters ----
    # if (!all(is.null(c(input$sel_layers, input$sel_qual, input$sel_ind)))) {
    opts <- shinygui_saveopts(input, prod_opt_list, rv)

    if (is.character(opts)) {
      shinyalert::shinyalert(title = "Error",
                             text = opts,
                             type = "error")
    } else {
      jsonlite::write_json(opts, outpath, pretty = TRUE,
                           auto_unbox = TRUE)
      shinyalert::shinyalert(title = "Done",
                             text = paste0("Options file saved to ", outpath),
                             type = "success")
    }
    }
})




# observe({
#   showModal(modalDialog(
#     title = "Draw the extent",
#     size = "l",
#     shiny::helpText(em("Click on the map to select the desired tiles.")),
#     shiny::helpText(em("WARNING: If you select non contiguous tiles, all tiles creating a rectangle between minimum x/y and maximum x/y will be selected!")),
#     mapedit::selectModUI(extent_ns_name, height=500, width="100%"),
#     easyClose = FALSE,
#     footer = tagList(
#       shiny::actionButton("save_extent_draw", strong("\u2000Ok"), icon=icon("check")),
#       shiny::modalButton("\u2000Cancel", icon = icon("ban"))
#     )
#   )
#   )}
# )
# # actionbutton to send back the selected tiles ----
# observeEvent(input$save_extent_draw, ignoreInit = TRUE, ignoreNULL = TRUE,
#              {
#                removeModal()
#                curtiles <- rv$selecttiles
#                seltiles <- data.frame(h = curtiles$h, v = curtiles$v)
#                selh <- seq(min(seltiles$h), max(seltiles$h), 1)
#                selv <- seq(min(seltiles$v), max(seltiles$v), 1)
#                whichtiles <- which((rv$tilemap$h %in% selh) & (rv$tilemap$v %in% selv))
#                rv$curtiles <- rv$tilemap[whichtiles,]
#                bbox <- as.numeric(sf::st_bbox(rv$curtiles))
#
#                seltilesmap <- leaflet() %>% addTiles() %>%
#                  leafem::addFeatures(rv$tilemap) %>%
#                  leafem::addFeatures(rv$curtiles, color = "red", label = ~tile_id) %>%
#                  leaflet::fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
#                output$tilesmap_interactive <- leaflet::renderLeaflet(seltilesmap)
#              }
# )
# })
#
# observe({
#   curminh  <- shiny::req(input$xmintile)
#   curminv <- shiny::req(input$ymintile)
#   curmaxh  <- shiny::req(input$xmaxtile)
#   curmaxv  <- shiny::req(input$ymaxtile)
#   if(curminh > curmaxh) {
#     shiny::updateNumericInput(session, "xmaxtile", "Maximum X tile", min = input$xmintile, max = 35, value = input$xmintile, step = 1)
#   } else {
#     shiny::updateNumericInput(session, "xmaxtile", "Maximum X tile", min = 0, max = 35, value = input$xmaxtile, step = 1)
#   }
#   if(curminv > curmaxv) {
#     shiny::updateNumericInput(session, "ymaxtile", "Maximum Y tile", min = input$ymintile, max = 35, value = input$ymintile, step = 1)
#   } else {
#     shiny::updateNumericInput(session, "ymaxtile", "Maximum Y tile", min = 0, max = 17, value = input$ymaxtile, step = 1)
#   }
# })
#
#
# observe({
#   curminh  <- shiny::req(input$xmintile)
#   curminv <- shiny::req(input$ymintile)
#   curmaxh  <- shiny::req(input$xmaxtile)
#   curmaxv  <- shiny::req(input$ymaxtile)
#   if(curmaxh >= curminh & curmaxv >= curminv) {
#     selh <- seq(min(req(input$xmintile)), max(req(input$xmaxtile)), 1)
#     selv <- seq(min(req(input$ymintile)), max(req(input$ymaxtile)), 1)
#     whichtiles <- which((rv$tilemap$h %in% selh) & (rv$tilemap$v %in% selv))
#     rv$curtiles <- rv$tilemap[whichtiles,]
#     bbox <- as.numeric(sf::st_bbox(rv$curtiles))
#     seltilesmap <- leaflet() %>% addTiles() %>%
#       leafem::addFeatures(rv$tilemap) %>%
#       leafem::addFeatures(rv$curtiles, color = "red", label = ~tile_id) %>%
#       leaflet::fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
#     output$tilesmap <- leaflet::renderLeaflet(seltilesmap)
#   }
# })



