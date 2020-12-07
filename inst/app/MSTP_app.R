#   ____________________________________________________________________________
#   This script Initializes the main UI of the clustering app                 ####
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

# add path for images
shiny::addResourcePath( 'www', system.file('app/www', package = 'MODIStsp') )

MSTP_ui <- shinydashboard::dashboardPage(
  #   ____________________________________________________________________________
  #   Initialize top bar and logos                                            ####

  skin = "green",
  header = shinydashboard::dashboardHeader(
    title = "Menu",
    shiny::tags$li(class = "dropdown", shiny::tags$h3(
      style = "color:white;margin:0;padding-top:15px;padding-bottom:15px;padding-left:10px;padding-right:20px;
      font-style:italic",
      paste0("MODIStsp v.", packageVersion("MODIStsp"))
    )),
    shiny::tags$li(class ="dropdown", shiny::tags$a(
      href="https://docs.ropensci.org/MODIStsp/",
      shiny::icon("book"),
      style="margin:0;padding-top:11px;padding-bottom:11px;padding-left:10px;padding-right:10px;font-size:30px;",
      target="_blank"
    )),
    shiny::tags$li(class ="dropdown", shiny::tags$a(
      href="https://github.com/ropensci/MODIStsp/",
      shiny::icon("github"),
      style="margin:0;padding-top:11px;padding-bottom:11px;padding-left:10px;padding-right:10px;font-size:30px;",
      target="_blank"
    ))
    ,
    shiny::tags$li(class ="dropdown", shiny::tags$a(
      href="http://www.irea.cnr.it",
      shiny::tags$img(src="www/irea_logo.png"),
      style="margin:0;padding-top:2px;padding-bottom:2px;padding-left:10px;padding-right:10px;",
      target="_blank"
    ))

  ),

  #   ____________________________________________________________________________
  #   Initialize sidebar                                            ####

  shinydashboard::dashboardSidebar(
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = jscode, functions = c("closeWindow")),
    id = "",
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Product and Layers", tabName = "mstp_prds",
                               icon = shiny::icon("layer-group"))
      ,
      shinydashboard::menuItem("Spatial/Temporal options", tabName = "mstp_spatemp",
                               icon = shiny::icon("filter"))
      ,
      shinydashboard::menuItem("Output Format and Folders", tabName = "mstp_opts",
                               icon = shiny::icon("cogs")),
      shiny::div(style = "width:100%",
                 shiny::actionButton("run_app", "Run MODIStsp",
                                     icon = shiny::icon("play"),
                                     style = "width:90%")
      ),
      shiny::div(
        shinyFiles::shinySaveButton('save_opts', '  Save Options',
                                    'Save as json ...', filetype = "json",
                                    icon = shiny::icon("save"),
                                    style = "width:90%")
      ),
      shiny::div(style = "width:100%",
                 shinyFiles::shinyFilesButton("load_opts", "  Load Options",
                                              "Load from json ...", FALSE,
                                              icon = shiny::icon("upload"),
                                              style = "width:90%")
      ),
      shiny::div(style = "width:100%",
                 shiny::actionButton("close_app", "Quit MODIStsp",
                                     icon = shiny::icon("sign-out-alt"),
                                     style = "width:90%")
      )
    )
  ),
  #   ____________________________________________________________________________
  #   Initialize Body - soure UIs of the different panels                     ####

  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    # tags$script(HTML("$('body').addClass('sidebar-mini');")),
    shinydashboard::tabItems(

      source(system.file("app/ui/mstp_prds_ui.R", package = "MODIStsp"),
             local = TRUE)$value,
      source(system.file("app/ui/mstp_spatemp_ui.R", package = "MODIStsp"),
             local = TRUE)$value,
      source(system.file("app/ui/mstp_other_ui.R", package = "MODIStsp"),
             local = TRUE)$value
      #
      # source("ui/cl_s2_ui.R", local = TRUE)$value,
      #
      # source("ui/cl_vivar_ui.R", local = TRUE)$value,
      #
      # source("ui/cl_clust_ui.R", local = TRUE)$value
    )
  )
)
#   ____________________________________________________________________________
#   Initialize server functions                                             ####

MSTP_server <- shiny::shinyServer(function(input, output, session) {

  #   ____________________________________________________________________________
  #   Source all main server scripts                                          ####

  source(system.file("app/srv/mstp_prds_srv.R", package = "MODIStsp"),
         local = TRUE)$value
  source(system.file("app/srv/mstp_opts_srv.R", package = "MODIStsp"),
         local = TRUE)$value
  source(system.file("app/srv/mstp_spatemp_srv_tiles.R", package = "MODIStsp"),
         local = TRUE)$value
  source(system.file("app/srv/mstp_spatemp_srv_bbox.R", package = "MODIStsp"),
         local = TRUE)$value
  source(system.file("app/srv/mstp_spatemp_srv_file.R", package = "MODIStsp"),
         local = TRUE)$value
  source(system.file("app/srv/mstp_spatemp_srv_draw.R", package = "MODIStsp"),
         local = TRUE)$value
  source(system.file("app/srv/mstp_spatemp_srv_proj.R", package = "MODIStsp"),
         local = TRUE)$value
  source(system.file("app/srv/mstp_save_srv.R", package = "MODIStsp"),
         local = TRUE)$value
  source(system.file("app/srv/mstp_load_srv.R", package = "MODIStsp"),
         local = TRUE)$value
  source(system.file("app/srv/mstp_helpmess_srv.R", package = "MODIStsp"),
         local = TRUE)$value


  # Event close_app ----
  # Close app from the sidebar menu

  # observeEvent(input$close_app, {
  #   mstp_exit_gui()
  # })

  session$onSessionEnded(stopApp)

})

# shiny::shinyApp(MSTP_ui, MSTP_server)
