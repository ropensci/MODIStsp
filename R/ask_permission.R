#' @title ask_permission
#' @description Ask users for permission to write the previous options file.
#' @return 'logical' if TRUE, the user authorized saving in ExtData/Previous
#' @rdname ask_permission

ask_permission <- function() {
  #nocov start
  #   __________________________________________________________________________
  #   NOTE: The function is excluded from coverage reports since it must be ####
  #   used interactively.
  #
  if (!file.exists(file.path(
    system.file("ExtData/Previous", package = "MODIStsp"),
    "MODIStsp_Previous.json"))) {

    strwrap(message(
      "MODIStsp would like to save a \"MODIStsp_Previous.json\" file \n",
      "containing information about its last successfull run in the folder \n", #nolint
      "`.../your-r-library/MODIStsp/ExtData/.` \n\n ",
      "Do you authorize this? \n\n",
      "1: Yes - ",
      "\"MODIStsp_Previous.json\" will be saved permanently and updated after\n",#nolint
      "after each successfull run of the tool. You will not see this message anymore.\n\n", #nolint

      "2: No - ",
      "Previous options will be written to `tempdir` and will be lost\ when you\n", #nolint
      "exit `R`. You will see this message at each `MODIStsp` execution.\n"
    ))
    choice <- ""
    while (!choice %in% c("1", "2")) {
      choice <- readline(prompt = "Choice (1/2): ")
    }
  } else {
    choice <- "1"
  }
  ifelse(choice == "1", TRUE, FALSE)
  #nocov end
}
