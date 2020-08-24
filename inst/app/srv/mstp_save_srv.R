# observers to deal with saving options ----

general_opts <- reactiveValues()

shiny::observeEvent(input$save_opts, {
  # retrieve output path ----
  shinyFiles::shinyFileSave(input, 'save_opts', roots = volumes,
                            defaultRoot = "Home",
                            defaultPath = "")
  outpath <- shinyFiles::parseSavePath(roots=volumes, input$save_opts)$datapath

  # retrieve inputs from shiny widgets ----

  mstp_opts$selprod    <- input$selprod
  # retrieve product options
  mstp_opts$prod_version <- "6"
  browser()
  mstp_opts$sensor  <- tolower(input$selplat)
  mstp_opts$bandsel <- input$sel_layers
  mstp_opts$quality_bandsel <- input$sel_qual
  mstp_opts$indexes_bandsel <- input$sel_ind
  # Retrieve download method and authentication ----
  mstp_opts$user            <- input$user
  mstp_opts$password        <- input$password
  mstp_opts$download_server <- input$down_meth
  mstp_opts$use_aria        <- input$use_aria
  mstp_opts$download_range  <- input$date_seas
  #
  # # Retrieve dates
  mstp_opts$start_date <- input$proc_dates[1]
  mstp_opts$end_date   <- input$proc_dates[2]
  #
  # # Retrieve Tiles options
  if (input$spatmeth == "Select Tiles") {
    mstp_opts$start_x <- input$xmintile
    mstp_opts$end_x   <- input$xmaxtile
    mstp_opts$start_y <- input$ymintile
    mstp_opts$end_y   <- input$ymaxtile
    mstp_opts$bbox    <- c(NA, NA, NA, NA)
  } else {
    tiles <- tiles_from_bbox(rv$outbbox, input$outprojtxt)
    mstp_opts$bbox <- rv$outbbox
    browser()
  }

    mstp_opts$projsel     <- input$projsel
    mstp_opts$output_proj <- input$outprojtxt
    mstp_opts$out_res_sel <- input$outressel
    mstp_opts$out_res     <- input$outres
    mstp_opts$resampling   <- input$outresamp

    # # Retrieve reprocess, delete and nodata
    mstp_opts$reprocess  <- gWidgets::svalue(wids$reprocess)
    mstp_opts$delete_hdf <- gWidgets::svalue(wids$delete)

    mstp_opts$nodata_change <- gWidgets::svalue(wids$nodata)
    mstp_opts$scale_val     <- gWidgets::svalue(wids$scale)


  # Retrieve format, virtual and compression

    mstp_opts$out_format <- gWidgets::svalue(wids$format)
    mstp_opts$ts_format  <- gWidgets::svalue(wids$timeseries)
    mstp_opts$compress   <- as.character(
      compress_dict[gWidgets::svalue(wids$compress)]
    )

  # Retrieve Folder options
    mstp_opts$out_folder     <- gWidgets::svalue(wids$outfold)
    mstp_opts$out_folder_mod <- gWidgets::svalue(wids$outfoldmod)

  #
  # # Retrieve Proj and extent options
  # general_opts$proj        <- gWidgets::svalue(wids$proj_choice)
  # general_opts$output_proj4  <- gWidgets::svalue(wids$output_proj4)
  # general_opts$out_res_sel <- gWidgets::svalue(wids$output_res_sel)
  # general_opts$out_res     <- gWidgets::svalue(wids$output_res)
  # general_opts$resampling  <- gWidgets::svalue(wids$output_resmeth)
  # general_opts$full_ext    <- ifelse(gWidgets::svalue(wids$output_ext) == "Select MODIS Tiles",
  #                                    TRUE,
  #                                    FALSE)
  # out_bbox <-  c(gWidgets::svalue(wids$output_xmin),
  #                gWidgets::svalue(wids$output_ymin),
  #                gWidgets::svalue(wids$output_xmax),
  #                gWidgets::svalue(wids$output_ymax))
  # if (any(out_bbox == "NULL")) out_bbox[] <- NA
  # if (any(is.na(out_bbox)))    out_bbox[] <- NA
  #
  # general_opts$bbox        <- c(gWidgets::svalue(wids$output_xmin),
  #                               gWidgets::svalue(wids$output_ymin),
  #                               gWidgets::svalue(wids$output_xmax),
  #                               gWidgets::svalue(wids$output_ymax))
  #
  # # Retrieve reprocess, delete and nodata
  # general_opts$reprocess  <- gWidgets::svalue(wids$reprocess)
  # general_opts$delete_hdf <- gWidgets::svalue(wids$delete)
  #
  # general_opts$nodata_change <- gWidgets::svalue(wids$nodata)
  # general_opts$scale_val     <- gWidgets::svalue(wids$scale)
  #
  #
  # # Retrieve format, virtual and compression
  #
  # general_opts$out_format <- gWidgets::svalue(wids$format)
  # general_opts$ts_format  <- gWidgets::svalue(wids$timeseries)
  # general_opts$compress   <- as.character(
  #   compress_dict[gWidgets::svalue(wids$compress)]
  # )
  #
  # # Retrieve Folder options
  # general_opts$out_folder     <- gWidgets::svalue(wids$outfold)
  # general_opts$out_folder_mod <- gWidgets::svalue(wids$outfoldmod)
  #
  # gui_env$check_save_opts <- TRUE
  # # Send warning if HDF deletion selected
  # if (general_opts$delete_hdf == "Yes") {
  #   gui_env$check_save_opts <- gWidgets::gconfirm(
  #     strwrap("Warning! HDF files in Original MODIS folder will be
  #             deleted at the end of processing! \n\n
  #             Are you sure? ", width = 80),
  #     title = "Warning", icon = "warning"
  #   )
  # }
  #
  # #- Perform checks on options consistency ---------------
  #
  # # Check if at least 1 layer selected
  #
  # if (max(general_opts$bandsel) +
  #     ifelse(length(general_opts$indexes_bandsel) > 0,
  #            max(general_opts$indexes_bandsel),
  #            0) + max(general_opts$quality_bandsel) == 0) {
  #   gWidgets::gmessage(
  #     message = "No Output bands or indexes selected - Please Correct!",
  #     title   = "Warning"
  #   )
  #   gui_env$check_save_opts <- FALSE
  # }
  #
  # # Check if dates, processing extent and tiles selection make sense
  # if (as.Date(general_opts$start_date) > as.Date(general_opts$end_date)) {
  #   gWidgets::gmessage(
  #     message = "Ending date earlier than starting date - Please correct!",
  #     title   = "Warning"
  #   )
  #   gui_env$check_save_opts <- FALSE
  # }
  #
  # if (
  #   inherits(try(as.Date(general_opts$start_date),
  #                silent = TRUE), "try-error") |
  #   inherits(try(as.Date(general_opts$end_date),
  #                silent = TRUE) ,"try-error")
  # ){
  #   gWidgets::gmessage(
  #     message = "One or both dates are in wrong format - Please correct!",
  #     title   = "Warning"
  #   )
  #   gui_env$check_save_opts <- FALSE
  # }
  #
  # if (general_opts$start_x > general_opts$end_x |
  #     general_opts$start_y > general_opts$end_y) {
  #   gWidgets::gmessage(message = "Error in Selected Tiles! Please correct!",
  #                      title   = "Warning")
  #   gui_env$check_save_opts <- FALSE
  # }
  #
  # # Check if bbox is consistent
  # suppressWarnings(general_opts$bbox <- as.numeric(general_opts$bbox))
  # general_opts$bbox <- as.numeric(general_opts$bbox)
  # n_bbox_compiled   <- length(which(is.finite(general_opts$bbox)))
  # if (general_opts$full_ext == FALSE) {
  #   if (n_bbox_compiled == 4) {
  #     if (general_opts$bbox[1] > general_opts$bbox[3] |
  #         general_opts$bbox[2] > general_opts$bbox[4]) {
  #       gWidgets::gmessage(message = "Error in Selected Output extent",
  #                          title   = "Warning")
  #       gui_env$check_save_opts <- FALSE
  #     }
  #   } else {
  #     if (n_bbox_compiled >= 0) {
  #       gWidgets::gmessage(
  #         message = "Error in Selected Output extent. Please correct!",
  #         title   = "Warning")
  #       gui_env$check_save_opts <- FALSE
  #     }
  #   }
  # }
  #
  # # Check if selected tiles are consistent with the bounding box
  # # (only if product is not tiled)
  # if (general_opts$full_ext == FALSE &
  #     prod_opt_list[[gWidgets::svalue(wids$prod)]][[gWidgets::svalue(wids$vers)]][["tiled"]] == 1 &
  #     gui_env$check_save_opts) {
  #   bbox_mod         <- reproj_bbox(
  #     general_opts$bbox,
  #     gWidgets::svalue(wids$output_proj4), mod_proj_str,
  #     enlarge = FALSE
  #   )
  #   d_bbox_mod_tiled <- suppressWarnings(sf::st_crop(modis_grid,
  #                                                    xmin = bbox_mod[1],
  #                                                    ymin = bbox_mod[2],
  #                                                    xmax = bbox_mod[3],
  #                                                    ymax = bbox_mod[4]))
  #   required_tiles   <- paste0(
  #     "H",
  #     apply(expand.grid("H" = min(d_bbox_mod_tiled$H):max(d_bbox_mod_tiled$H),
  #                       "V" = min(d_bbox_mod_tiled$V):max(d_bbox_mod_tiled$V)
  #     ), 1, paste, collapse = "_V")
  #   )
  #
  #   selected_tiles <- paste0(
  #     "H",
  #     apply(expand.grid(
  #       "H" = gWidgets::svalue(wids$start_x):gWidgets::svalue(wids$end_x),
  #       "V" = gWidgets::svalue(wids$start_y):gWidgets::svalue(wids$end_y)),
  #       1, paste, collapse = "_V")
  #   )
  #
  #   # If the bounding box does not intersect with the tiles, return a warning
  #   # asking to automatically retrieve from extent
  #   if (!any(required_tiles %in% selected_tiles)) {
  #     gui_env$check_save_opts <- gWidgets::gconfirm(
  #       strwrap("The selected tiles do not intersect the output bounding
  #               box. \n\n Do you want to discard your choice and retrieve
  #               automatically the required tiles from the bounding box?",
  #               width = 200),
  #       handler = function(h, ...) {
  #         selected_tiles       <<- required_tiles
  #         general_opts$start_x <<- min(d_bbox_mod_tiled$H)
  #         general_opts$end_x   <<- max(d_bbox_mod_tiled$H)
  #         general_opts$start_y <<- min(d_bbox_mod_tiled$V)
  #         general_opts$end_y   <<- max(d_bbox_mod_tiled$V)
  #       }
  #       , title = "Warning"
  #     )
  #   }
  #
  #   # If not all the required tiles are selected, ask to select them
  #   if (!all(required_tiles %in% selected_tiles) & gui_env$check_save_opts) {
  #     gWidgets::gconfirm(
  #       message = strwrap(paste(
  #         "The following tiles not currently selected are required to cover
  #         the output bounding box (",
  #         paste(required_tiles[!(required_tiles %in% selected_tiles)],
  #               collapse = ", "),
  #         "). \n\n Do you want to add them to the processing? Otherwise,
  #         nodata will be produced in the non-covered area.")),
  #       handler = function(h, ...) {
  #         selected_tiles       <<- required_tiles
  #         general_opts$start_x <<- min(d_bbox_mod_tiled$H)
  #         general_opts$end_x   <<- max(d_bbox_mod_tiled$H)
  #         general_opts$start_y <<- min(d_bbox_mod_tiled$V)
  #         general_opts$end_y   <<- max(d_bbox_mod_tiled$V)
  #       }
  #       , title = "question"
  #     )
  #   }
  #
  #   # If some selected tiles are not useful, ask to remove them
  #   if (!all(selected_tiles %in% required_tiles) & gui_env$check_save_opts) {
  #     gWidgets::gconfirm(
  #       message = strwrap(paste(
  #         "The following tiles are not required to cover the output
  #         bounding box (",
  #         paste(selected_tiles[!(selected_tiles %in% required_tiles)],
  #               collapse = ", "),
  #         "). \n\n Do you want to remove them from processing?")
  #       ),
  #       handler = function(h, ...) {
  #         selected_tiles       <<- required_tiles
  #         general_opts$start_x <<- min(d_bbox_mod_tiled$H)
  #         general_opts$end_x   <<- max(d_bbox_mod_tiled$H)
  #         general_opts$start_y <<- min(d_bbox_mod_tiled$V)
  #         general_opts$end_y   <<- max(d_bbox_mod_tiled$V)
  #       }
  #       , title = "Warning"
  #     )
  #   }
  # }
  #
  # # check if folders are defined
  # if (general_opts$out_folder == "" & gui_env$check_save_opts) {
  #   gWidgets::gmessage(
  #     message = "Please select an output folder MODIStsp outputs!",
  #     title   = "Warning"
  #   )
  #   gui_env$check_save_opts <- FALSE
  # }
  # if (general_opts$out_folder_mod == "" & gui_env$check_save_opts) {
  #   gWidgets::gmessage(
  #     message = "Please select an output folder for storage of original HDFs!",
  #     title   = "Warning")
  #   gui_env$check_save_opts <- FALSE
  # }
  #
  # # Issue Warning on Mode resamling
  # if (general_opts$resampling == "mode" & gui_env$check_save_opts) {
  #   check_mode <- gWidgets::gconfirm(
  #     message = strwrap(
  #       "Warning! You selected 'mode' resampling. Be aware that 'mode'
  #         resampling can give inconsistent results in areas affected by
  #         mixed high and low quality data, and fail in properly keeping
  #         track of quality indicators! \n\n Do you wish to continue?",
  #       width = 300),
  #     title   = "Warning"
  #   )
  #   if (check_mode == FALSE) {
  #     gui_env$check_save_opts <- FALSE
  #   }
  # }
  #
  # # check that user/password were provided in case of html download
  # if (general_opts$download_server == "http" &
  #     (general_opts$user == "" | general_opts$password == "") &
  #     gui_env$check_save_opts
  # ) {
  #   gWidgets::gmessage(
  #     message = strwrap("Username and password are mandatory in case of
  #                        `http` download!", width = 300),
  #     title   = "Warning")
  #   gui_env$check_save_opts <- FALSE
  # }
  #
  # # check that the select product is available on the selected server
  # #
  # http <- prod_opt_list[[general_opts$sel_prod]][[which(vapply(
  #   prod_opt_list[[general_opts$sel_prod]],
  #   function(x){
  #     x$v_number
  #   }
  #   , FUN.VALUE = "") == gWidgets::svalue(wids$vers))]]$http
  #
  # if (general_opts$sensor == "Both") {
  #   http <- c(http["Terra"][[1]], http["Aqua"][[1]])
  # } else {
  #   if (general_opts$sensor == "Combined") {
  #     http <- http[[1]]
  #   } else {
  #     http <- http[general_opts$sensor][[1]]
  #     if (general_opts$download_server == "http" & http == "Not Available"
  #         & general_opts$sensor == "Aqua") { #nolint
  #       gWidgets::gmessage(
  #         message = strwrap("The selected product/version is only available for
  #                       the Terra sensor.\n\n Please switch sensor!",
  #                           width = 300),
  #         title   = "Warning")
  #       gui_env$check_save_opts <- FALSE
  #     }
  #   }
  # }
  #
  # #   __________________________________________________________________________
  # #   # If all checks passed, save options file and return                  ####
  #
  # if (gui_env$check_save_opts) {
  #   jsonlite::write_json(general_opts, opts_jsfile, pretty = TRUE,
  #                        auto_unbox = TRUE)
  # }
  browser()
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



