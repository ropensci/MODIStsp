#initialize product and category selectors ----

output$selcats <-renderUI({
  curprod <- which(names(prod_opt_list) == general_opts$sel_prod)
  curcat  <- mod_prod_cat$cat[curprod]
  shiny::selectInput("selcat", "Product Category",
                     choices  = unique(mod_prod_cat$cat),
                     selected = curcat)
})

output$selprods <-renderUI({
  shiny::selectInput("selprod",
                     label = span("Product Name\u2000",
                                  actionLink("help_product",
                                             icon("question-circle"))),
                     choices = mod_prod_list[mod_prod_cat$cat == input$selcat],
                     selected = general_opts$selprod)
})


#Update products selector based on category ----
shiny::observe({
  req(input$selcats)
  shiny::updateSelectInput(session, "selprod", "Product Name",
                           choices = names(prod_opt_list)[mod_prod_cat$cat == input$selcat],
                           selected = 1)
})

#Update products selector based on category ----
observe({
  req(input$selprod)
  rv$newindex
  curprod   <- which(names(prod_opt_list) == input$selprod)

  curlayers <- prod_opt_list[[curprod]][[1]]$bandnames
  curlabels <- prod_opt_list[[curprod]][[1]]$band_fullnames
  shiny::updateCheckboxGroupInput(session, "sel_layers",
                                  choiceNames = curlabels,
                                  choiceValues = curlayers)

  qalayers <- prod_opt_list[[curprod]][[1]]$quality_bandnames
  qalabels <- prod_opt_list[[curprod]][[1]]$quality_fullnames
  shiny::updateCheckboxGroupInput(session, "sel_qual",
                                  choiceNames  = qalabels,
                                  choiceValues = qalayers)
  if (is.null(qalayers)) {
    qalayers <- NA
    qalabels <- "No Quality layers available"
  }

  indlayers <- prod_opt_list[[curprod]][[1]]$indexes_bandnames
  indlabels <- prod_opt_list[[curprod]][[1]]$indexes_fullnames
  indexes_file <- system.file("ExtData/Previous",
                              "MODIStsp_indexes.json",
                              package = "MODIStsp")

  if (file.exists(indexes_file)) {
    custom_indexes <- jsonlite::read_json(indexes_file)
  } else {
    custom_indexes <- NULL
  }
  if (length(custom_indexes) == 0) {
    custom_indexes <- NULL
  }

  custom_indexes <- custom_indexes[[curprod]][[1]]
  indlayers <- c(indlayers, custom_indexes$indexes_bandnames)
  indlabels <- c(indlabels, custom_indexes$indexes_fullnames)

  if (is.null(indlayers)) {
    indlayers <- NA
    indlabels <- "No Indexes available"
  }
  shiny::updateCheckboxGroupInput(session, "sel_ind",
                                  choiceNames  = indlabels,
                                  choiceValues = indlayers)
})

# Observers to create / deal with the modal used to add spectral indexes ----

shiny::observeEvent(input$addindex, {

  # Valid names for reflectance bands
  refbands_names <- c("b1_Red", "b2_NIR", "b3_Blue", "b4_Green", "b5_SWIR",
                      "b6_SWIR", "b7_SWIR")
  selprod <- MODIStsp_get_prodlayers(input$selprod)

  # check on bands available for a specific product
  avail_prodbands <- selprod$bandnames
  # reflectence bands available for selected product
  match_refbands <- avail_prodbands[match(refbands_names, avail_prodbands)]
  avail_refbands <- match_refbands[!is.na(match_refbands)]

  shiny::showModal(shiny::modalDialog(
    title = "Add a new Spectral Index output projection",
    size = "l",
    shiny::helpText(shiny::em("Insert a names and formula for the new index")),
    shiny::helpText(paste(c("Valid bandnames for this product are: \n", avail_refbands), collapse = " ")),
    shiny::textInput("indexfullname", "Spectral Index Full Name (e.g., Simple Ratio (b2_NIR/b1_Red))", width = "400px"),
    shiny::textInput("indexshortname", "Spectral Index Short Name (e.g., SR)"),
    shiny::textInput("indexformula", "Spectral Index Formula (e.g., (b2_NIR/b1_Red))"),
    easyClose = FALSE,
    footer = tagList(
      shiny::actionButton("save_index", strong("\u2000Ok"), icon=icon("check")),
      shiny::modalButton("\u2000Cancel", icon = icon("ban"))
    )
  ))
})

# Observers to check spectral index upon pressing ok ----
#
shiny::observeEvent(input$save_index, {


  indexfullname <- req(input$indexfullname)
  indexshortname <- req(input$indexshortname)
  indexformula <- req(input$indexformula)
  refbands_names <- c("b1_Red", "b2_NIR", "b3_Blue", "b4_Green", "b5_SWIR",
                      "b6_SWIR", "b7_SWIR")
  selprod <- MODIStsp_get_prodlayers(input$selprod)

  # check on bands available for a specific product
  avail_prodbands <- selprod$bandnames
  # reflectence bands available for selected product
  match_refbands <- avail_prodbands[match(refbands_names, avail_prodbands)]
  avail_refbands <- match_refbands[!is.na(match_refbands)]

  catch_err <- check_formula_errors(indexshortname,
                                    indexfullname,
                                    indexformula,
                                    prod_opt_list,
                                    refbands_names,
                                    avail_refbands)

  if (catch_err == 0) {
    save_formula(refbands_names,
                 req_bands = attr(catch_err, "req_bands"),
                 indexshortname,
                 indexfullname,
                 indexformula,
                 32767,
                 prod_opt_list)
    shinyalert::shinyalert(title = "You did it!", type = "success")
    shiny::removeModal()
    rv$newindex <- data.frame(indexshortname   = indexshortname,
                              indexfullname    = indexfullname,
                              indexformula     = indexformula,
                              stringsAsFactors = FALSE)
  } else {
    # Issue error warnings in the GUI if something went wrong!
    switch(
      as.character(catch_err),
      "1" = shinyalert::shinyalert(title =
                                     paste(
                                       "ERROR ! The Formula of the new Index is not computable.
                                       Please check it !\n Valid Band Names are: \n\n",
                                       paste(avail_refbands, collapse = ", "),
                                       "."), type = "error")
      ,
      "2" = shinyalert::shinyalert(
        title = "Index full or short name is already present.\n\n
        Please specify different ones.", type = "error"),
      "3" = shinyalert::shinyalert(
        title = "ERROR ! Please provide valid values for the Index Acronym,
             its fullname and the Formula.", type = "error")
    )
  }
})

# On creation of new index, update list of indexes ----


# enable/disable panels ----

shiny::observe({
  shiny::req(input$down_meth)
  if(input$down_meth == "http") {
    shinyjs::enable("user")
    shinyjs::enable("password")
  } else {
    shinyjs::disable("user")
    shinyjs::disable("password")
  }
})

shiny::observe({
  shiny::req(input$selprod)
  indexes <- MODIStsp_get_prodlayers(input$selprod)$indexes_bandnames
  if (is.null(indexes)) {
    shinyjs::disable("addindex")
  } else {
    shinyjs::enable("addindex")
  }
})


observeEvent(input$close_app, {
  shinyalert::shinyalert(title = "GoodBye!",
                         text = "Thanks for using MODIStsp",
                         type = "info")
  shinyjs::delay(3000, {shinyjs::js$closeWindow()
    stopApp()})

})
