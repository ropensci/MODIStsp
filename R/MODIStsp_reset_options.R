#' @title MODIStsp_reset_options
#' @description Helper function used to reset MODIStsp options to default
#'  by removing the MODIStsp_Previous.json file.
#'  May be useful to get back to a "working" state if the GUI gets somehow
#'  corrupted due to an invalid MODIStsp_Previous.json file used.
#' @return The function is called for its side effects
#' @rdname MODIStsp_reset_options
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#'
MODIStsp_reset_options <- function() {
  # no coverage because only viable in Interactive
  #nocov start
  prevopts_file <- system.file("ExtData/Previous/MODIStsp_Previous.json",
                               package = "MODIStsp")
  strwrap(message(
    "This will remove your current MODIStsp options and file and reset them to ", #nolint
    "default next time you open the GUI. Do you wish to continue?\n\n",
    "1: Yes\n\n",
    "2: No"))
  choice <- ""
  while (!choice %in% c("1", "2")) {
    choice <- readline(prompt = "Choice (1/2): ")
  }
  if (choice == "1") {
    file.remove(prevopts_file)
  }
  #nocov start
}
