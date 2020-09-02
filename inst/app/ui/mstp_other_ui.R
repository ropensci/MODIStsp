shinyalert::useShinyalert()
shinydashboard::tabItem(
  shinyjs::useShinyjs(),
  # shinyjs::extendShinyjs(text = jscode, functions = c("closeWindow")),
  tabName = "mstp_opts",
  shinyjs::useShinyjs(),
  shinydashboard::box(
    # download method box ----
    title = "Download Method",
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
          shiny::selectInput(
            "down_meth",
            label = shiny::span(
              "Download Method\u2000",
              shiny::actionLink("help_downmeth", shiny::icon("question-circle"))
            ),
            c("http", "offline"))
        ),
        shiny::div(
          style = "display:inline-block;vertical-align:top;padding:1px;margin-right:15px",
          shiny::textInput(
            "user",
            label = shiny::span(
              "User Name\u2000",
              shiny::actionLink("help_user", shiny::icon("question-circle"))
            ), "")
        )
      ),
      shiny::div(
        style = "display:inline-block;vertical-align:top;padding:1px;margin-right:15px",
        shiny::textInput(
          "password",
          label = shiny::span(
            "Password\u2000",
            shiny::actionLink("help_password", shiny::icon("question-circle"))
          ), "")
      ),
      shiny::div(
        style = "display:inline-block;vertical-align:top;padding:1px;margin-right:15px",
        shiny::selectInput(
          "downloader",
          label = shiny::span(
            "Downloader\u2000",
            shiny::actionLink("help_downloader", shiny::icon("question-circle"))
          ), c("http", "aria2")
        )
      )
    )
  ),
  shinydashboard::box(
    # Output options box ----
    title = "Output Options",
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
          shiny::selectInput(
            "out_format",
            label = shiny::span(
              "Output Format\u2000",
              shiny::actionLink("help_format", shiny::icon("question-circle"))
            ),
            c("GTiff", "ENVI"))
        ),
        shiny::div(
          style = "display:inline-block;vertical-align:top;padding:1px;margin-right:15px",
          shiny::selectInput(
            "compress",
            label = shiny::span(
              "Compression\u2000",
              shiny::actionLink("help_compression", shiny::icon("question-circle"))
            ),
            c("None", "PACKBITS", "LZW", "DEFLATE" )
            , selected = "LZW")
        )
      ),
      shiny::div(
        style = "display:inline-block;vertical-align:top;padding:1px;margin-right:15px",
        shiny::checkboxGroupInput(
          "time_series",
          label = shiny::span(
            "Save Time Series As\u2000",
            shiny::actionLink("help_time_series", shiny::icon("question-circle"))
          ),
          c("R RasterStack", "GDAL VRT", "ENVI Meta Files"))
      ),
      shiny::div(
        style = "display:inline-block;vertical-align:top;padding:1px;margin-right:15px",
        shiny::selectInput(
          "scaleoff",
          label = shiny::span(
            "Apply Scale/Offset\u2000",
            shiny::actionLink("help_scaleoff", shiny::icon("question-circle"))
          ),
          c("Yes", "No"), selected = "No")
      ),
      shiny::div(
        style = "display:inline-block;vertical-align:top;padding:1px;margin-right:15px",
        shiny::selectInput(
          "nodata",
          label = shiny::span(
            "Modify NoData Values\u2000",
            shiny::actionLink("help_nodata", shiny::icon("question-circle"))
          ),
          c("Yes", "No"), selected = "No")
      )
    )
  ),
  shinydashboard::box(
    # Output folders box ----
    title = "Output Folders",
    width = 12,
    solidHeader = TRUE,
    collapsible = TRUE,
    status = "primary",
    shiny::div(
      style = "display:inline-block;width:100%",
      shiny::div(
        style = "display:inline-block;width:90%;vertical-align:top",
        shiny::div(
          style = "display:inline-block;padding:1px;margin-right:5px;width:70%;vertical-align:top",
          shiny::textInput(
            "out_folder_txt",
            label = shiny::span(
              "Main Output Folder\u2000",
              shiny::actionLink("help_outfolder", shiny::icon("question-circle"))
            ),
            "")
        ),
        shiny::div(
          style = "display:inline-block;margin-right:25px;width:8%;vertical-align:top;margin-top:25px",
          shinyFiles::shinyDirButton(
            "out_folder",
            "Browse",
            "Main Output Folder")
        ),
        shiny::div(
          style = "display:inline-block;margin-right:5px;width:15%;vertical-align:top",
          shiny::selectInput(
            "reprocess",
            label = shiny::span("Reprocess\u2000"#,
                         # shiny::actionLink("help_reprocess", shiny::icon("question-circle"))
            ),
            c("Yes", "No"), selected = "No"
          )
        )
      )
    ),
    shiny::div(
      style = "display:inline-block;width:100%",
      shiny::div(
        style = "display:inline-block;width:90%;vertical-align:top",
        shiny::div(
          style = "display:inline-block;padding:1px;margin-right:5px;width:70%;vertical-align:top",
          shiny::textInput(
            "out_hdffolder_txt",
            label = shiny::span(
              "Output Folder for storage of original MODIS HDF\u2000",
              shiny::actionLink("help_outfolderhdf", shiny::icon("question-circle"))
            ),
            "")
        ),
        shiny::div(
          style = "display:inline-block;margin-right:25px;width:8%;vertical-align:centre;margin-top:25px",
          shinyFiles::shinyDirButton(
            "out_hdffolder",
            "Browse", "Output Folder for storage of original MODIS HDF")
        ),
        shiny::div(
          style = "display:inline-block;margin-right:5px;width:15%;vertical-align:top",
          shiny::selectInput(
            "delete_hdf",
            label = shiny::span("Delete HDF\u2000"#,
                         # shiny::actionLink("help_delete", shiny::icon("question-circle"))
            ),
            c("Yes", "No"), selected = "Yes"
          )
        )
      )
    )
  )
)
