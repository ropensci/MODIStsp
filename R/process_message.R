#' @title Spawn processing update messages
#' @description helper function to provide processing messages
#' @param mess_text `character` text to be shown in the processing windows
#'   and/or the console
#' @param gui `logical` indicating if the message should be passed to the 
#'   status window or only to the console. 
#' @param mess_lab pointer to the gwindow used to shoe messages when gui == TRUE
#' @param verbose `logical` If FALSE, suppress processing messages, Default: TRUE
#' @return The function is called for its side effects
#' @rdname process_message
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom gWidgets svalue
process_message <- function(mess_text, gui, mess_lab, verbose = TRUE) {
  if (gui) {
    #nocov start
    if (verbose) gWidgets::svalue(mess_lab) <- paste("---", mess_text, "---")
    Sys.sleep(0.05)
    if (verbose) message("[", date(), "] ", mess_text)
    #nocov end
  } else {
    if (verbose) message("[", date(), "] ", mess_text)
  }
}