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
            label = span(
              "Download Method\u2000",
              actionLink("help_downmeth", icon("question-circle"))
            ),
            c("http", "offline"))
        ),
        shiny::div(
          style = "display:inline-block;vertical-align:top;padding:1px;margin-right:15px",
          shiny::textInput(
            "user",
            label = span(
              "User Name\u2000",
              actionLink("help_user", icon("question-circle"))
            ), "")
        )
      ),
      shiny::div(
        style = "display:inline-block;vertical-align:top;padding:1px;margin-right:15px",
        shiny::textInput(
          "password",
          label = span(
            "Password\u2000",
            actionLink("help_password", icon("question-circle"))
          ), "")
      ),
      shiny::div(
        style = "display:inline-block;vertical-align:top;padding:1px;margin-right:15px",
        shiny::selectInput(
          "downloader",
          label = span(
            "Downloader\u2000",
            actionLink("help_downloader", icon("question-circle"))
          ), c("https", "aria2")
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
            label = span(
              "Output Format\u2000",
              actionLink("help_format", icon("question-circle"))
            ),
            c("TIFF", "ENVI"))
        ),
        shiny::div(
          style = "display:inline-block;vertical-align:top;padding:1px;margin-right:15px",
          shiny::selectInput(
            "compress",
            label = span(
              "Compression\u2000",
              actionLink("help_compression", icon("question-circle"))
            ),
            c("None", "PACKBITS", "LZW", "DEFLATE" )
            , selected = "LZW")
        )
      ),
      shiny::div(
        style = "display:inline-block;vertical-align:top;padding:1px;margin-right:15px",
        shiny::checkboxGroupInput(
          "time_series",
          label = span(
            "Save Time Series As\u2000",
            actionLink("help_time_series", icon("question-circle"))
          ),
          c("R RasterStack", "GDAL VRT", "ENVI Meta Files"))
      ),
      shiny::div(
        style = "display:inline-block;vertical-align:top;padding:1px;margin-right:15px",
        shiny::selectInput(
          "scaleoff",
          label = span(
            "Apply Scale/Offset\u2000",
            actionLink("help_scaleoff", icon("question-circle"))
          ),
          c("Yes", "No"), selected = "No")
      ),
      shiny::div(
        style = "display:inline-block;vertical-align:top;padding:1px;margin-right:15px",
        shiny::selectInput(
          "nodata",
          label = span(
            "Modify NoData Values\u2000",
            actionLink("help_nodata", icon("question-circle"))
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
            label = span(
              "Main Output Folder\u2000",
              actionLink("help_outfolder", icon("question-circle"))
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
            label = span("Reprocess\u2000",
                         actionLink("help_reprocess", icon("question-circle"))
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
            label = span(
              "Ouput Folder for storage of original MODIS HDF\u2000",
              actionLink("help_outfolderhdf", icon("question-circle"))
            ),
            "")
        ),
        shiny::div(
          style = "display:inline-block;margin-right:25px;width:8%;vertical-align:centre;margin-top:25px",
          shinyFiles::shinyDirButton(
            "out_hdffolder",
            "Browse", "Ouput Folder for storage of original MODIS HDF")
        ),
        shiny::div(
          style = "display:inline-block;margin-right:5px;width:15%;vertical-align:top",
          shiny::selectInput(
            "delete_hdf",
            label = span("Delete HDF\u2000",
                         actionLink("help_delete", icon("question-circle"))
            ),
            c("Yes", "No"), selected = "Yes"
          )
        )
      )
    )
  )
)
