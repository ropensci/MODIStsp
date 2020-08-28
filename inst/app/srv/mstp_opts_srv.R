# enable/disable panels ----

observe({
  req(input$down_meth)
  if(input$down_meth == "http") {
    shinyjs::enable("user")
    shinyjs::enable("password")
  } else {
    shinyjs::disable("user")
    shinyjs::disable("password")
  }
})


observe({
  req(input$out_format)
  if(input$out_format == "GTiff") {
    shinyjs::enable("compress")
  } else {
    shinyjs::disable("compress")
  }
})

observe({
  check_aria <- Sys.which("aria2c")
  if(check_aria != "") {
    shinyjs::enable("downloader")
  } else {
    shinyjs::disable("downloader")
  }
})

shinyjs::disable("out_folder_txt")
shinyFiles::shinyDirChoose(input, "out_folder", roots = volumes,
               defaultRoot = "Home",
               defaultPath = "")
shiny::observe({
  out_folder <- input$out_folder
  # if (!is.list(input$mainpath) & !is.null(RV_all$mainpath)) {
  #   mainpath_string <- RV_all$mainpath
  # } else {
  #   mainpath_string <- parseDirPath(volumes, mainfolder)
  # }
  outfold_string <- shinyFiles::parseDirPath(volumes, out_folder)
  updateTextInput(session, "out_folder_txt", value = as.character(outfold_string))
})

shinyjs::disable("out_hdffolder_txt")
shinyFiles::shinyDirChoose(input, "out_hdffolder", roots = volumes,
               defaultRoot = "Home",
               defaultPath = "")
shiny::observe({
  out_folder <- input$out_hdffolder
  # if (!is.list(input$mainpath) & !is.null(RV_all$mainpath)) {
  #   mainpath_string <- RV_all$mainpath
  # } else {
  #   mainpath_string <- parseDirPath(volumes, mainfolder)
  # }
  outfold_string <- shinyFiles::parseDirPath(volumes, out_folder)
  updateTextInput(session, "out_hdffolder_txt", value = as.character(outfold_string))
})
