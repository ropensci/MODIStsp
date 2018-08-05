#' @title gh_changeproj
#' @description Handler for the actions to be taken when the projection is
#'  changed
#' @importFrom gWidgets svalue enabled ginput gmessage
#' @importFrom sp CRS
#' @noRd
#'
gh_changeproj <- function(h, wids, out_proj_list, bbox_out) {

  #nocov start
  # check if the change was triggered by a load_opts call
  is_loadopts <- deparse(sys.calls()[[sys.nframe()-10]])[1] ==
    "gui_load_options(choice, wids, prod_opt_list, compress_dict)"

  if (gWidgets::svalue(wids$proj_choice) != "User Defined") {
    if (is_loadopts){
      choice <- TRUE
    } else {
      choice <- warn_projmess2()
    }
    if (choice) {
      gWidgets::enabled(wids$change_proj_but)  <- FALSE
      old_proj  <- gWidgets::svalue(wids$output_proj4)
      newproj   <- out_proj_list[[gWidgets::svalue(wids$proj_choice)]]
      gWidgets::svalue(wids$output_proj4)  <- newproj

      # Get the units and kind of proj
      proj  <- gui_get_proj(sp::CRS(newproj))
      units <- gui_get_units(sp::CRS(newproj), proj)
      gWidgets::svalue(wids$pixsize2_lab) <- units
      gui_update_bboxlabels(bbox_out,
                            units,
                            wids,
                            reset = TRUE)
    }
  } else {
    # If user chooses "user defined" projection, open a GUI for inputting
    # a proj4 string and reset the bbox labels
    old_proj  <- gWidgets::svalue(wids$output_proj4)
    gWidgets::enabled(wids$change_proj_but)  <- TRUE
    # gWidgets::svalue(wids$output_proj4) <- ""
    new_proj <- NA

    if (
      deparse(sys.calls()[[sys.nframe() - 10]])[1] ==
      "gui_load_options(choice, wids, prod_opt_list, compress_dict)" ) {
      choice <- TRUE
    } else {
      choice <- warn_projmess1()
    }
    if (choice) {

      if (!is_loadopts) {
      selproj <- gWidgets::ginput(
        paste("Please insert a valid proj4string,",
              "an EPSG code or an UTM grid zone (e.g. 32N):"),
        parent     = NULL,
        do.buttons = TRUE,
        size       = 800,
        horizontal = TRUE
      )
      } else {
        selproj <- svalue(wids$output_proj4)
      }

      # verify the inputted string. Revert to previous on error, or modify
      # projstring and update the bounding box by converting coordinates to
      # new out proj

      if (length(selproj) != 0 && selproj != "" && !is.na(selproj)) {
        new_proj <- check_proj4string(selproj, abort = FALSE, verbose = FALSE)
        # On error, send out a message and reset wids$proj_choice and proj4
        # wid to previous values
        if (is.na(new_proj)) {
          gWidgets::gmessage(
            message = strwrap(paste(
              "The projection is not recognized,",
              "so the previous projection will be kept."
            )),
            title   = strwrap("Invalid projection")
          )
          gWidgets::svalue(wids$output_proj4) <- old_proj
          gWidgets::svalue(wids$proj_choice)  <- "Native"
        } else {

          gWidgets::svalue(wids$output_proj4) <- new_proj

          # If valid proj4string, and output is a bounding box, recompute
          # the bounding box in output proj coordinates

          if (!gWidgets::svalue(wids$output_ext) == "Select MODIS Tiles") {

            # Get the units and kind of proj

            proj  <- gui_get_proj(sp::CRS(new_proj))
            units <- gui_get_units(sp::CRS(new_proj), proj)
            gWidgets::svalue(wids$pixsize2_lab) <- units
            gui_update_bboxlabels(bbox_out,
                                  units,
                                  wids,
                                  reset = TRUE)

          } else {
            proj  <- gui_get_proj(sp::CRS(new_proj))
            units <- gui_get_units(sp::CRS(new_proj), proj)
            gWidgets::svalue(wids$pixsize2_lab) <- units
          }
        }
      } else {

        # on error, reset to previous values
        gWidgets::svalue(wids$output_proj4) <- old_proj
        gWidgets::svalue(wids$proj_choice)  <- old_proj
      }
    }
  }
  #nocov end
}
