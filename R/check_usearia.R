#' @title check_usearia
#' @description FUNCTION_DESCRIPTION
#' @inheritParams MODIStsp_process
#' @param mess pointer to the message window used to show processing messages
#'  when gui = TRUE
#' @return use_aria `logical` Indicating if aria2c should be used to accelerate
#'  download
#' @rdname check_usearia
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom gWidgets gmessage dispose
#' 
check_usearia <- function(use_aria, gui, mess) {
  # Check if "aria2c" requested. If so, verify that the executable is found
  # on the path. If NOT revert to "standard" download using httr::GET

  if (use_aria) {
    test_aria <- Sys.which("aria2c")
    if (test_aria == "") {
      if (gui) {
        noaria <- gconfirm(
          strwrap("aria2c was not found! It is either not installed or not
                    found on your path! \n Do you want to proceed with standard
                    download?", width = 77)
        )
        if (noaria) {
          use_aria <- 0
        } else {
          gWidgets::gmessage(
            strwrap("Please ensure that aria2c is installed and in your path! \n
                      See http://aria2.github.io", width = 70))
          gWidgets::dispose(mess)
          stop("aria2c was not found! Ensure that aria2c is installed and in ",
               "your path!\n (See http://aria2.github.io).\n Aborting!")
        }
      } else {
        message("aria2c was not found! It is either not installed or not on ",
                "your path! \nContinuing with normal download... ")
      }
    }
  }
  use_aria
}