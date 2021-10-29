shinyalert::useShinyalert()
shinydashboard::tabItem(
  shinyjs::useShinyjs(),
  # shinyjs::extendShinyjs(text = jscode, functions = c("closeWindow")),
  tabName = "mstp_prds",
  shinydashboard::box(
    title = "Select MODIS Product",
    width = 12,
    shiny::div(
      style = "display:inline-block;width:100%",
      shiny::div(
        style = "display:inline-block;vertical-align:top;padding:1px;width:78%",
        shiny::uiOutput("selcats")
      ),
      shiny::div(
        style = "display:inline-block;vertical-align:top;padding:1px;width:18%",
        shiny::selectInput(
          "selplat",
          label = shiny::span("Platform\u2000",
                              shiny::actionLink("help_platform",
                                                shiny::icon("question-circle"))),
          choices = c("Terra", "Aqua", "Both"))
      )
    ),
    shiny::div(
      style = "display:inline-block;width:100%",
      shiny::div(
        style = "display:inline-block;vertical-align:top;padding:1px;width:70%",
        shiny::uiOutput("selprods")
      ),
      shiny::div(
        style = "display:inline-block;vertical-align:top;padding:1px;width:5%;margin-top:25px;margin-right:10px",
        shiny::actionButton("prodinfo", "", icon = shiny::icon("atlas"),
                            title = "Open Documentation webpage")
      ),
      shiny::div(
        style = "display:inline-block;vertical-align:top;padding:1px;width:20%",
        shiny::uiOutput("selvers")
      )
    )
  ),
  shinydashboard::box(
    title = "Select Layers to be processed",
    width = 12,
    solidHeader = TRUE,
    collapsible = TRUE,
    status = "primary",
    shiny::div(
      style = "display:inline-block;padding:1px",
      shiny::div(
        style = "display:inline-block;vertical-align:top;padding:1px",
        shiny::div(
          style = "display:inline-block;vertical-align:top;padding:1px;margin-right:15px",
          shiny::checkboxGroupInput(
            "sel_layers",
            label = shiny::span("MODIS Layers\u2000",
                                shiny::actionLink("help_layers",
                                                  shiny::icon("question-circle"))),
            c(""))
        ),
        shiny::div(
          style = "display:inline-block;vertical-align:top;padding:1px;margin-right:15px",
          shiny::checkboxGroupInput(
            "sel_qual",
            label = shiny::span("Quality Indicators\u2000",
                                shiny::actionLink("help_quality",
                                                  shiny::icon("question-circle"))),
            c(""))
        )
      ),
      shiny::div(style = "display:inline-block;vertical-align:top;padding:1px",
                 shiny::checkboxGroupInput(
                   "sel_ind",
                   label = shiny::span("Spectral Indexes\u2000",
                                       shiny::actionLink("help_indexes",
                                                         shiny::icon("question-circle"))),
                   c(""))
      )
    )
  ),
  shiny::actionButton("addindex", "Add New Spectral Index", width = "100%")
)
