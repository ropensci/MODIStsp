shinydashboard::tabItem(
  shinyjs::useShinyjs(),
  shinyalert::useShinyalert(),
  # shinyjs::extendShinyjs(text = jscode, functions = c("closeWindow")),
  tabName = "mstp_spatemp",
  # Temporal selectors -----
  shinydashboard::box(
    title = "Temporal Extent",
    width = NULL,
    solidHeader = TRUE,
    collapsible = TRUE,
    status = "primary",
    shiny::div(
      style = "display:inline-block;padding:1px",
      shiny::div(
        style = "display:inline-block;vertical-align:top;padding:1px",
        shiny::div(style = "display:inline-block;vertical-align:top;padding:1px;margin-right:15px",
                   shiny::dateRangeInput("proc_dates", "Temporal Range",
                                         start = as.Date("2002-01-01"),
                                         end   = as.Date(Sys.Date())
                   )
        ),
        shiny::div(style = "display:inline-block;vertical-align:top;padding:1px;margin-right:15px",
                   shiny::selectInput("date_seas", "Date Range Type",
                                      c("Full" , "Seasonal")
                   )
        )

      )
    )
  ),
  # Projection selectors -----
  shinydashboard::box(
    title = "Output Projection",
    width = NULL,
    solidHeader = TRUE,
    collapsible = TRUE,
    status = "primary",
    shiny::div(
      # Native vs. manual projection ----
      shiny::div(
        style = "display:inline-block;padding:1px;vertical-align:top",
        shiny::div(
          style = "display:inline-block;vertical-align:top;padding:1px;margin-right:10px",
          shiny::selectInput("outprojsel", "Output Projection",
                             c("Native",
                               "User Defined"),
                             width = "120px")

        )
      ),

      # Conditional wids to deal with projection change ----
      shiny::div(
        style = "display:inline-block;padding:1px;vertical-align:top",
        # shiny::conditionalPanel(
        #   condition = "input.outprojsel == 'Manual'",
        shiny::div(
          style = "display:inline-block;vertical-align:top;padding:1px;margin-right:5px",
          shiny::uiOutput("outprojtxt")
        ),
        shiny::div(
          style = "display:inline-block;vertical-align:top;padding:1px;margin-right:5px; margin-top:25px",
          shiny::actionButton("seloutproj", "Change", title = "Click to select output projection")
          # )
        )
      ),
      # Native vs. manual resolution ----
      shiny::div(
        style = "display:inline-block;padding:1px;vertical-align:top",
        shiny::div(
          style = "display:inline-block;vertical-align:top;padding:1px;margin-right:10px",
          shiny::selectInput("outressel", "Output Resolution",
                             c("Native",
                               "Resampled"),
                             width = "120px")

        )
      ),

      # Conditional wids to deal with projection change ----
      shiny::div(
        style = "display:inline-block;padding:1px;vertical-align:top",
        shiny::conditionalPanel(
          condition = "input.outressel == 'Resampled'",
          shiny::div(
            style = "display:inline-block;vertical-align:top;padding:1px;margin-right:5px",
            shiny::uiOutput("outres")
          )
        )
      ),
      shiny::div(
        style = "display:inline-block;padding:1px;vertical-align:top",
        shiny::conditionalPanel(
          condition = "input.outressel == 'Resampled' | input.outprojsel == 'User Defined'",
          shiny::div(
            style = "display:inline-block;vertical-align:top;padding:1px;margin-right:5px",
            shiny::selectInput("resampmeth", "Resampling Method",
                               c("near",
                                 "bilinear",
                                 "cubic",
                                 "cubicspline",
                                 "average",
                                 "mode",
                                 "max",
                                 "min",
                                 "med",
                                 "q1",
                                 "q3",
                                 "sum"))
          )
        )
      )
    )
  ),

  # Spatial selectors -----
  #
  shinydashboard::box(
    title = "Spatial Extent",
    width = NULL,
    solidHeader = TRUE,
    collapsible = TRUE,
    status = "primary",
    shiny::div(
      # style = "display:inline-block;vertical-align:top;padding:1px",
      shiny::div(
        style = "display:inline-block;vertical-align:top;padding:1px;margin-right:5px",
        shiny::selectInput("spatmeth", "Selection Method",
                           c("Select Tiles",
                             "Select Bounding Box",
                             "Load From Spatial File",
                             "Draw On Map"),
                           width = "200px")
      )

    ),


    # From Tiles ----
    shiny::div(
      shiny::conditionalPanel(
        condition = "input.spatmeth == 'Select Tiles'",
        style = "display:inline-block;padding:1px;width:98%",
        shiny::selectInput("selecttile", "Select Method",
                           c("Manual", "From Map"),
                           selected = "Manual", width = "98%"),
        shiny::conditionalPanel(
          "input.selecttile == 'Manual'",
          shiny::div(
            style = "display:inline-block;vertical-align:top;padding:1px",
            shiny::div(style = "display:inline-block;vertical-align:top;padding:1px;margin-right:15px",
                       shiny::numericInput("xmintile", "Minimum X tile",
                                           min = 0, max = 35, value = 18, step = 1)
            ),
            shiny::div(style = "display:inline-block;vertical-align:top;padding:1px;margin-right:15px",
                       shiny::numericInput("xmaxtile", "Maximum X tile",
                                           min = 0, max = 35, value = 18, step = 1)
            )
          ),
          shiny::div(
            style = "display:inline-block;vertical-align:top;padding:1px",
            shiny::div(style = "display:inline-block;vertical-align:top;padding:1px;margin-right:15px",
                       shiny::numericInput("ymintile", "Minimum Y tile",
                                           min = 0, max = 17, value = 8, step = 1)
            ),
            shiny::div(style = "display:inline-block;vertical-align:top;padding:1px;margin-right:15px",
                       shiny::numericInput("ymaxtile", "Maximum Y tile",
                                           min = 0, max = 17, value = 8, step = 1)
            )
          )
        ),
        shiny::conditionalPanel(
          "input.selecttile == 'Manual'",
          shiny::div(
            leaflet::leafletOutput("tilesmap")
          )
        ),
        shiny::conditionalPanel(
          "input.selecttile == 'From Map'",
          style = "display:inline-block;vertical-align:top;padding:1px;width:98%",
          shiny::div(
            style = "display:inline-block;vertical-align:top;padding:1px;width:200px",
            shiny::actionButton("tilemap", "Click to change selection")
          ),
          shiny::div(
            leaflet::leafletOutput("tilesmap_interactive")
          )
        )
      )
    ),

    # From Bounding Box ----
    shiny::conditionalPanel(
      condition = "input.spatmeth == 'Select Bounding Box'",
      style = "display:inline-block;padding:1px; width:100%",
      shiny::div(
        style = "display:inline-block;vertical-align:top;padding:1px",
        shiny::div(style = "display:inline-block;vertical-align:top;padding:1px;margin-right:15px",
                   shiny::numericInput("bboxxmin", "Bounding Box Minimum X ",
                                       value = 500000, step = 1)
        ),
        shiny::div(style = "display:inline-block;vertical-align:top;padding:1px;margin-right:15px",
                   shiny::numericInput("bboxxmax", "Bounding Box Maximum X ",
                                       value = 1600000, step = 1)
        )
      ),

      shiny::div(
        style = "display:inline-block;vertical-align:top;padding:1px",
        shiny::div(style = "display:inline-block;vertical-align:top;padding:1px;margin-right:15px",
                   shiny::numericInput("bboxymin", "Bounding Box Minimum Y",
                                       value = 4000000, step = 1)
        ),
        shiny::div(style = "display:inline-block;vertical-align:top;padding:1px;margin-right:15px",
                   shiny::numericInput("bboxymax", "Bounding Box Maximum Y",
                                       value = 5250000, step = 1)
        )
      ),
      shiny::div(
        leaflet::leafletOutput("bboxmap")
      )
    ),

    # From Spatial File ----
    shiny::conditionalPanel(
      condition = "input.spatmeth == 'Load From Spatial File'",
      style = "display:inline-block;padding:1px;width:100%",
      shiny::div(
        style = "display:inline-block;vertical-align:centre;padding:1px;width:100%",
        shiny::div(style = "display:inline-block;vertical-align:centre;padding:1px;margin-right:5px;
                   width:80%",
                   shiny::textInput("spafiletxt", "Spatial file","")
        ),
        shiny::div(style = "display:inline-block;vertical-align:centre;padding:1px;margin-right:2px;width:10%",
                   shinyFiles::shinyFilesButton(
                     'spafile', 'Browse', icon = shiny::icon("upload"),
                     title = "Select a vector or raster file",
                     'Please select a raster or vector spatial file', FALSE)
        )
      ),
      shiny::div(
        leaflet::leafletOutput("spafilemap")
      )
    ),
    # From Map ----
    shiny::conditionalPanel(
      condition = "input.spatmeth == 'Draw On Map'",
      # style = "display:inline-block;padding:1px",
      shiny::div(
        style = "display:inline-block;vertical-align:top;padding:1px;width:200px",
        shiny::actionButton("drawmap", "Draw extent", icon = shiny::icon("draw-polygon"))
      ),
      shiny::div(
        leaflet::leafletOutput("drawmap_interactive")
      )
    )
  )
)

