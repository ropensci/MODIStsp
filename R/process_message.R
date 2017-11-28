#' @title process_message
#' @description helper function to provide processing messages
#' @param mess_text PARAM_DESCRIPTION
#' @param gui PARAM_DESCRIPTION
#' @param mess_lab PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @rdname process_message
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom gWidgets svalue
process_message <- function(mess_text, gui, mess_lab) {
  if (gui) {
    gWidgets::svalue(mess_lab) <- paste("---", mess_text, "---")
    Sys.sleep(0.05)
    message("[", date(), "] ", mess_text)
  } else {
    message("[", date(), "] ", mess_text)
  }
}