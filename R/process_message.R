#' @title Spawn processing update messages
#' @description helper function to provide processing messages
#' @param mess_text `character` text to be shown in the processing windows
#'   and/or the console
#' @param verbose `logical` If FALSE, suppress processing messages, Default: TRUE
#' @return The function is called for its side effects
#' @rdname process_message
#' @author Lorenzo Busetto, phD (2017)

process_message <- function(mess_text, verbose = TRUE) {
    if (verbose) message("[", date(), "] ", mess_text)
}
