#' @title process_message
#' @description helper function to provide processing messages
#' @param mess_text `character` text to be shown in the processing windows
#'   and/or the console
#' @param gui `logical` indicating if the message should be passed to the 
#'   status window or only to the console. 
#' @param mess_lab pointer to the gwindow used to shoe messages when gui == TRUE
#' @return The function is called for its side effects
#' @rdname process_message
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom gWidgets svalue
process_message <- function(mess_text, gui, mess_lab) {
  if (gui) {
    #nocov start
    gWidgets::svalue(mess_lab) <- paste("---", mess_text, "---")
    Sys.sleep(0.05)
    message("[", date(), "] ", mess_text)
    #nocov end
  } else {
    message("[", date(), "] ", mess_text)
  }
}