#' @title Build and manage the MODIStsp GUI
#' @description
#'	Function used to generate and handle the GUI used to allow selection of
#'	MODIStsp processing parameters.
#' @return the function is called for its side effects - opening
#'  the GUI and allowing to set, save, load options and eventually
#'  launch the processing.
#' @author Lorenzo Busetto, phD (2014-2017) \email{lbusett@@gmail.com}
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom utils packageVersion browseURL
#'
MODIStsp_GUI <- function() {

  #nocov start
  file_path <- system.file("app/MSTP_app.R", package = "MODIStsp")
  if (!nzchar(file_path)) stop("Shiny app not found")
  MSTP_ui <- MSTP_server <- NULL # avoid NOTE about undefined globals
  source(file_path, local = TRUE)

  server_env <- environment(MSTP_server)

  # Initialise pipe
  if (requireNamespace("magrittr", quietly = TRUE)) {
    `%>%` <- magrittr::`%>%`
  }

  # ____________________________________________________________________________
  # Files/Folder Initialization and set-up of default parameters            ####

  # Load the products options from "/ExtData/MODIStsp_ProdOpts.RData"
  prod_opt_list <- load_prodopts()
  # Here you add any variables that your server can find
  #
  server_env$prod_opt_list <- prod_opt_list
  mod_prod_cat <- as.data.frame(
    t(vapply(prod_opt_list, function(x){
      c(x[[1]]$cat01, x[[1]]$cat02)
    }
    , FUN.VALUE = character(2)))
  )
  names(mod_prod_cat) <- c("cat01", "cat02")
  server_env$mod_prod_cat$cat <- apply(mod_prod_cat, 1, paste, collapse = " - ")
  server_env$mod_prod_list    <- names(prod_opt_list)
  server_env$volumes <- c(shinyFiles::getVolumes()(),
               "Home" = dirname(dirname(rappdirs::user_data_dir())))
  app <- shiny::shinyApp(MSTP_ui, MSTP_server)
  shiny::runApp(app, launch.browser = TRUE)
  #nocov end
}
