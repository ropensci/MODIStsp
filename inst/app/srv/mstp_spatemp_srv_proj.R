

# observers to deal with projection selection ----
observe({
  curprod <- req(input$selprod)
  natres  <- prod_opt_list[[curprod]][[1]]$native_res
  # browser()
  if (natres != 5600) {
    natproj <- "MODIS Sinusoidal"

  } else {
    natproj <- "4008"
  }
  shiny::updateTextInput(session, "outprojtxt",natproj)
  shiny::updateNumericInput(session, "outres", "Output Resolution", value = natres)

})

observe({
  projchoice <- req(input$outprojsel)
  curprod <- req(input$selprod)
  natres  <- prod_opt_list[[curprod]][[1]]$native_res
  # browser()
  if (natres != 5600) {
    natproj <- "MODIS Sinusoidal"

  } else {
    natproj <- "4008"
  }
  if (input$outprojsel == "Native") {
    shiny::updateTextInput(session, "outprojtxt", "Projection Name/EPSG/Wkt", natproj)
  }
})

shiny::observeEvent(input$seloutproj, {
  shiny::showModal(shiny::modalDialog(
    title = "Select output projection",
    size = "l",
    shiny::helpText(em("Insert a valid EPSG code (e.g., 33632), or wkt2 string.")),
    shiny::textInput("projtextmodal", "Output projection"),
    easyClose = FALSE,
    footer = tagList(
      shiny::actionButton("save_textproj", strong("\u2000Ok"), icon=shiny::icon("check")),
      shiny::modalButton("\u2000Cancel", icon = shiny::icon("ban"))
    )
  ))
})

shiny::observeEvent(input$save_textproj, {
  chk_proj <- check_projection(input$projtextmodal)
  shiny::removeModal()
  if (!is.na(chk_proj)) {
    shiny::updateTextInput(session, "outprojtxt", "Projection Name/EPSG/Wkt", chk_proj)
  } else {
    shinyalert::shinyalert(
      "Error", type = "error", text = "Invalid EPSG code or WKT string. Try again!"

    )
  }
})

# Enable/disable widgets
#
shiny::observe({

  if (input$outprojsel == "User Defined"){
    shinyjs::enable("seloutproj")
  } else {
    shinyjs::disable("seloutproj")
  }

})
