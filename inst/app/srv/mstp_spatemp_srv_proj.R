

# observers to deal with projection selection ----
observe({
  curprod <- req(input$selprod)
  natres  <- prod_opt_list[[curprod]][[1]]$native_res
  # browser()
  if (natres != 5600) {
    natproj <- "MODIS Sinusoidal"

  } else {
    natproj <- "4326"
  }
  output$outprojtxt <- shiny::renderUI({
    shinyjs::disabled(shiny::textInput("outprojtxt", "Projection Name/EPSG/Wkt", natproj))})
  output$outres <- shiny::renderUI({
    shiny::numericInput("outres", "Ouput Resolution", natres)})

})

observe({
  projchoice <- req(input$outprojsel)
  curprod <- req(input$selprod)
  natres  <- prod_opt_list[[curprod]][[1]]$native_res
  # browser()
  if (natres != 5600) {
    natproj <- "MODIS Sinusoidal"

  } else {
    natproj <- "4326"
  }
  shiny::updateTextInput(session, "outprojtxt", "Projection Name/EPSG/Wkt", natproj)
})

shiny::observeEvent(input$seloutproj, {
  shiny::showModal(shiny::modalDialog(
    title = "Select output projection",
    size = "l",
    shiny::helpText(em("Insert a valid EPSG code (e.g., 33632), or wkt2 string.")),
    shiny::textInput("projtextmodal", "Output projection"),
    easyClose = FALSE,
    footer = tagList(
      shiny::actionButton("save_textproj", strong("\u2000Ok"), icon=icon("check")),
      shiny::modalButton("\u2000Cancel", icon = icon("ban"))
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
