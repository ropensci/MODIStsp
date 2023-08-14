#initialize product and category selectors ----

# general_opts <- reactiveValues()
rv           <- reactiveValues()

output$selcats <-renderUI({
  # curprod <- which(names(prod_opt_list) == general_opts$selprod)
  # curcat  <- mod_prod_cat$cat[curprod]
  shiny::selectInput(
    "selcat", "Product Category",
    choices  = unique(mod_prod_cat$cat),
    selected = unique(mod_prod_cat$cat)[1]
  )
})

output$selprods <-renderUI({
  shiny::selectInput(
    "selprod",
    label = shiny::span(
      "Product Name\u2000",
      shiny::actionLink("help_product", shiny::icon("question-circle"))
    ),
    choices = mod_prod_list[mod_prod_cat$cat == (input$selcat)],
    selected = mod_prod_list[mod_prod_cat$cat == (input$selcat)][1]
  )
})

output$selvers <-renderUI({
  req(input$selprod)
  shiny::selectInput(
    "selver",
    label = shiny::span(
      "Version\u2000",
      shiny::actionLink("help_version", shiny::icon("question-circle"))
    ),
    choices = names(prod_opt_list[[input$selprod]]),
    selected = rev(names(prod_opt_list[[input$selprod]]))[1]
  )
})

shiny::observeEvent(input$prodinfo, {
  curprod   <- which(names(prod_opt_list) == isolate(input$selprod))
  utils::browseURL(prod_opt_list[[curprod]][[input$selver]]$www)
})


#Update products selector based on category ----
observe({
  req(input$selprod)
  req(input$selver)
  rv$newindex
  curprod   <- which(names(prod_opt_list) == input$selprod)

  curlayers <- prod_opt_list[[curprod]][[input$selver]]$bandnames
  curlabels <- prod_opt_list[[curprod]][[input$selver]]$band_fullnames
  shiny::updateCheckboxGroupInput(
    session, "sel_layers",
    choiceNames = curlabels,
    choiceValues = curlayers,
    selected = NULL
  )

  # shiny::isolate(general_opts$curlayers) <- ""

  qalayers <- prod_opt_list[[curprod]][[input$selver]]$quality_bandnames
  qalabels <- prod_opt_list[[curprod]][[input$selver]]$quality_fullnames

  if (is.null(qalayers)) {
    qalayers <- NA
    qalabels <- "No Quality layers available"
  }

  shiny::updateCheckboxGroupInput(
    session, "sel_qual",
    choiceNames  = qalabels,
    choiceValues = qalayers,
    selected = NULL
  )

  # shiny::isolate(general_opts$curqual) <- ""

  indlayers <- prod_opt_list[[curprod]][[input$selver]]$indexes_bandnames
  indlabels <- prod_opt_list[[curprod]][[input$selver]]$indexes_fullnames
  indexes_file <- system.file("ExtData",
                              "MODIStsp_indexes.json",
                              package = "MODIStsp")

  if (file.exists(indexes_file)) {
    custom_indexes <- jsonlite::read_json(indexes_file)
  } else {
    custom_indexes <- NULL
  }
  if (length(custom_indexes) == 1) {
    custom_indexes <- NULL
  }

  custom_indexes <- custom_indexes[[curprod]][[input$selver]]
  indlayers <- c(indlayers, custom_indexes$indexes_bandnames)
  indlabels <- c(indlabels, custom_indexes$indexes_fullnames)

  if (is.null(indlayers)) {
    indlayers <- NA
    indlabels <- "No Indexes available"
  }
  shiny::updateCheckboxGroupInput(
    session, "sel_ind",
    choiceNames  = indlabels,
    choiceValues = indlayers,
    selected = NULL
  )

  # shiny::isolate(general_opts$curindexes) <- ""
})

# Observers to create / deal with the modal used to add spectral indexes ----

shiny::observeEvent(input$addindex, {

  # Valid names for reflectance bands
  refbands_names <- c("b1_Red", "b2_NIR", "b3_Blue", "b4_Green", "b5_SWIR",
                      "b6_SWIR", "b7_SWIR")
  selprod <- MODIStsp_get_prodlayers(input$selprod, input$selver)

  # check on bands available for a specific product
  avail_prodbands <- selprod$bandnames
  # reflectence bands available for selected product
  match_refbands <- avail_prodbands[match(refbands_names, avail_prodbands)]
  avail_refbands <- match_refbands[!is.na(match_refbands)]

  shiny::showModal(shiny::modalDialog(
    title = "Add a new Spectral Index",
    size = "l",
    shiny::helpText(shiny::em("Insert a names and formula for the new index")),
    shiny::helpText(paste(c("Valid bandnames for this product are: \n", avail_refbands), collapse = " ")),
    shiny::textInput("indexfullname", "Spectral Index Full Name (e.g., Simple Ratio (b2_NIR/b1_Red))", width = "400px"),
    shiny::textInput("indexshortname", "Spectral Index Short Name (e.g., SR)"),
    shiny::textInput("indexformula", "Spectral Index Formula (e.g., (b2_NIR/b1_Red))"),
    easyClose = FALSE,
    footer = tagList(
      shiny::actionButton("save_index", strong("\u2000Ok"), icon=shiny::icon("check")),
      shiny::modalButton("\u2000Cancel", icon = shiny::icon("ban"))
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
  selprod <- MODIStsp_get_prodlayers(input$selprod, input$selver)

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
    shinyalert::shinyalert(title = "Success!",
                           text  = "The new index had been added to the list." ,
                           type = "success")
    shiny::removeModal()
    rv$newindex <- data.frame(indexshortname   = indexshortname,
                              indexfullname    = indexfullname,
                              indexformula     = indexformula,
                              stringsAsFactors = FALSE)
  } else {
    # Issue error warnings in the GUI if something went wrong!
    switch(
      as.character(catch_err),
      "1" = shinyalert::shinyalert(title = "Error!",
                                   text = paste("The Formula of the new Index is not computable.
                                       Please check it !\n Valid Band Names are: \n\n",
                                                paste(avail_refbands, collapse = ", "),
                                                "."), type = "error")
      ,
      "2" = shinyalert::shinyalert(title = "Error",
                                   text = "Index full or short name is already present.\n\n
        Please specify different ones.", type = "error"),
      "3" = shinyalert::shinyalert(title = "Error!",
                                   text = "Please provide valid values for the Index Acronym,
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
  shiny::req(input$selver)
  shiny::req(input$selver %in% names(prod_opt_list[[input$selprod]]))
  indexes <- MODIStsp_get_prodlayers(input$selprod, input$selver)$indexes_bandnames
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
  shinyjs::delay(1000, {shinyjs::js$closeWindow()
    stopApp()})

})

observeEvent(input$run_app, {
  proc_opts <- shinygui_saveopts(input, prod_opt_list, rv)
  #retrieve the options from the widgets
  if (inherits(proc_opts, "character")) {
    # send error if neeeded ----
    shinyalert::shinyalert(title = "Error in input parameters!",
                           text = proc_opts,
                           type = "error")
  } else {
    shinyalert::shinyalert(title = "Starting Processing!",
                           text = "You can follow progress in the console!",
                           type = "info")
    shinyjs::delay(1000, {shinyjs::js$closeWindow()
      stopApp()
      MODIStsp_process(proc_opts, n_retries = 5)
    })
  }

})
