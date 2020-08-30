# observers to load inputs from json and set shiny widgets accordingly ----

shinyFiles::shinyFileChoose(input, "load_opts",
                            defaultRoot = "Home",
                            roots = volumes, filetypes = "json")

shiny::observeEvent(input$load_opts, {

  inpath <- shinyFiles::parseFilePaths(volumes, input$load_opts)$datapath
  if (length(inpath) != 0) {

    opts   <- jsonlite::read_json(inpath)

    if (opts$MODIStspVersion >= 2.0) {

      # update category and product

      shiny::updateSelectInput(session, "selcat",
                               selected = opts$selcat)

      general_opts$selprod <- opts$selprod

      # shiny::updateSelectInput(session, "selprod",
      #                          selected = opts$sel_prod)

      curlayers <- prod_opt_list[[opts$selprod]][[1]]$bandnames
      curlabels <- prod_opt_list[[opts$selprod]][[1]]$band_fullnames
      # curlabels[which(curlayers %in% opts$bandsel)]
      general_opts$curlayers  <- unlist(opts$bandsel)
      general_opts$curqual    <- unlist(opts$quality_bandsel)
      general_opts$curindexes <- unlist(opts$indexes_bandsel)
      general_opts$selplat    <- opts$sensor
      shiny::updateSelectInput(session, "selplat",   selected = opts$sensor)
      shiny::updateSelectInput(session, "down_meth", selected = opts$download_server)
      shiny::updateTextInput(session,   "user", value = opts$user)
      shiny::updateTextInput(session,   "password",  value = opts$password)

      shiny::updateSelectInput(session, "downloader", selected = opts$downloader)
      shiny::updateSelectInput(session, "download_range",  selected = opts$date_seas)

      shiny::updateDateRangeInput(session, "proc_dates",
                                  start = as.Date(opts$start_date,format  = "%Y.%m.%d") ,
                                  end   = as.Date(opts$end_date,format  = "%Y.%m.%d"))

      shiny::updateSelectInput(session, "spatmeth", selected  = opts$spatmeth)

      shiny::updateNumericInput(session, "xmintile", value  = opts$start_x)
      shiny::updateNumericInput(session, "xmaxtile", value = opts$end_x)
      shiny::updateNumericInput(session, "ymintile", value  = opts$start_y)
      shiny::updateNumericInput(session, "ymaxtile", value  = opts$end_y)

      if (!is.null(opts$spafile)) {
        shiny::updateTextInput(session, "spafiletxt", value  = opts$spafile)
      }


      if (!all(all(is.null(unlist(opts$bbox))))) {
        shiny::updateNumericInput(session, "bboxxmin", value  = opts$bbox[1])
        shiny::updateNumericInput(session, "bboxxmax", value  = opts$bbox[3])
        shiny::updateNumericInput(session, "bboxymin", value  = opts$bbox[2])
        shiny::updateNumericInput(session, "bboxymax", value  = opts$bbox[4])
      }

      if (!is.null(opts$drawnext)) {
        rv$drawn_extent <- sf::st_read(opts$drawnext)
      }

      shiny::updateSelectInput(session, "out_projsel", selected  = opts$out_projsel)

      shiny::updateTextInput(session, "outprojtxt",   value  = opts$output_proj)

      shiny::updateSelectInput(session, "outressel", selected  = opts$out_res_sel)
      shiny::updateTextInput(session, "outres",      value  = opts$out_res)

      shiny::updateSelectInput(session, "resampmeth", selected  = opts$resampling)

      shiny::updateSelectInput(session, "reprocess", selected   =
                                 ifelse(opts$reprocess, "Yes" ,"No"))
      shiny::updateSelectInput(session, "delete_hdf", selected   =
                                 ifelse(opts$delete_hdf, "Yes" ,"No"))
      shiny::updateSelectInput(session, "nodata", selected   =
                                 ifelse(opts$nodata_change, "Yes" ,"No"))
      shiny::updateSelectInput(session, "scaleoff", selected   =
                                 ifelse(opts$scale_val, "Yes" ,"No"))

      shiny::updateSelectInput(session, "out_format", selected  = opts$out_format)
      shiny::updateSelectInput(session, "time_series", selected  = opts$ts_format)
      shiny::updateSelectInput(session, "compress", selected  = opts$compress)

      shiny::updateTextInput(session, "out_folder_txt",    value  = opts$out_folder)
      shiny::updateTextInput(session, "out_hdffolder_txt", value  = opts$out_folder_mod)
    } else {
      shinyalert::shinyalert("Warning", "Options Files created with
                           MODIStsp versions < 2.0 are no longer supported.
                           Please reinsert your options and save a new File!",
                           type = "error")
    }
  }
})
