#' @title gh_help
#' @description Helper function used to create an "help button" within the GUI
#' @noRd
#' @importFrom utils browseURL
#' @noRd
gh_help <- function(h, sel_help, help_messages, info_addr = NULL) {
  #nocov start
  if (!all(requireNamespace(c("gWidgets", "gWidgetsRGtk2")))) {
    stop("You need to install package gWidgets to use MODIStsp GUI. Please install it with:
                install.packages(c('gWidgets', 'gWidgetsRGtk2')")
  } else {
    requireNamespace("gWidgets")
    requireNamespace("gWidgetsRGtk2")
  }


  which_help <- NULL
  help_box <- gWidgets::gbasicdialog(title      = "Help",
                                     parent     = NULL,
                                     do.buttons = FALSE,
                                     horizontal = FALSE,
                                     width      = 10,
                                     height     = 10)

  helptext <- subset(help_messages, which_help == sel_help)[["text"]]

  help_mess_lab <- gWidgets::glabel(
    text = strwrap(helptext, 80),
    editable  = FALSE,
    container = help_box,
    markup = TRUE
  )
  if (!is.null(info_addr)) {
    moreinfo <- gWidgets::gbutton(
      text = paste0("More Info"),
      container = help_box,
      handler   = function(h, ...) {
        utils::browseURL(info_addr)
      }
    )
  }
  gWidgets::visible(help_box) <- TRUE
  #nocov end
}
