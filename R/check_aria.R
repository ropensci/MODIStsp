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

process_message <- function(mess_text, gui, mess_lab) {
  if (gui) {
    gWidgets::svalue(mess_lab) <- paste("---", mess_text, "---")
    Sys.sleep(0.05)
    message("[", date(), "] ", mess_text)
  } else {
    message("[", date(), "] ", mess_text)
  }
}
