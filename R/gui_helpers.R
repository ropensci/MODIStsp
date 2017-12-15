#### HELPER FUNCTIONS FOR MODIStsp_GUI TO AVOID REPETITIONS AND FACILITATE  ####
#### READING

#' @title gui_update_tiles
#' @description Helper function to update the labels of the gui showing the
#'  bounding box coordinates when a spatial file is selected or a projection
#'  change is issued.
#' @importFrom raster crop extent
#' @importFrom gWidgets svalue
#' @noRd
#'
gui_update_bboxlabels <- function(bbox_out,
                                  units,
                                  output_ul_east_wid,
                                  output_ul_north_wid,
                                  output_lr_east_wid,
                                  output_lr_north_wid) {
  #nocov start

  digits <- ifelse(units == "dec. degrees", 4, 1)
  gWidgets::svalue(output_ul_east_wid)  <- formatC(bbox_out[1, 1],
                                                   digits = digits,
                                                   format = "f")
  gWidgets::svalue(output_ul_north_wid) <- formatC(bbox_out[2, 2],
                                                   digits = digits,
                                                   format = "f")
  gWidgets::svalue(output_lr_east_wid)  <- formatC(bbox_out[1, 2],
                                                   digits = digits,
                                                   format = "f")
  gWidgets::svalue(output_lr_north_wid) <- formatC(bbox_out[2, 1],
                                                   digits = digits,
                                                   format = "f")
  #nocov end
}

# Helper function to update the selected tiles with the intersection with ####
# the bounding box

#' @title gui_update_tiles
#' @description Helper function to update the selected tiles with the
#' intersection with the bounding box
#' @importFrom raster crop extent
#' @importFrom gWidgets svalue
#' @noRd
#'
gui_update_tiles <- function(bbox_out,
                             output_proj4_wid,
                             mod_proj_str,
                             modis_grid,
                             start_x_wid,
                             end_x_wid,
                             start_y_wid,
                             end_y_wid) {
  #nocov start

  bbox_mod  <- reproj_bbox(bbox_out,
                           gWidgets::svalue(output_proj4_wid),
                           mod_proj_str,
                           enlarge = TRUE)

  d_bbox_mod_tiled    <- raster::crop(modis_grid, raster::extent(bbox_mod))
  gWidgets::svalue(start_x_wid) <- min(d_bbox_mod_tiled$H)
  gWidgets::svalue(end_x_wid)   <- max(d_bbox_mod_tiled$H)
  gWidgets::svalue(start_y_wid) <- min(d_bbox_mod_tiled$V)
  gWidgets::svalue(end_y_wid)   <- max(d_bbox_mod_tiled$V)
  #nocov end
}

#' @title gui_get_proj
#' @description GUI Helper functions to get currently selected projection
#' @importFrom utils head tail
#' @noRd
#'
gui_get_proj <- function(sel_output_proj) {
  #nocov start
  utils::head(strsplit(utils::tail(
    strsplit(sel_output_proj@projargs, "+proj=")[[1]], 1), " +")[[1]], 1)
  #nocov end
}

#' @title gui_get_units
#' @description GUI Helper functions to get measure units of currently selected
#'  projection
#' @importFrom utils head tail
#' @noRd
#'
gui_get_units <- function(sel_output_proj, proj) {
  #nocov start
  if (proj == "longlat") {
    units <- "dec.degrees"
  } else {
    units <- ifelse(
      length(strsplit(sel_output_proj@projargs, "+units=")[[1]]) > 1,
      utils::head(strsplit(utils::tail(strsplit(
        sel_output_proj@projargs, "+units=")[[1]], 1), " +")[[1]], 1),
      "Unknown"
    )
  }
  units
  #nocov end
}

#' @title gui_load_options
#' @description GUI Helper function used to load options from JSON and set
#'  values of the GUI accordingly
#' @importFrom gWidgets svalue gmessage
#' @importFrom jsonlite fromJSON
#' @importFrom raster crop extent
#' @noRd
#'
gui_load_options <- function(js_file) {

  #nocov start

  # load file and reset all widgets to values found in the loaded file
  general_opts     <- jsonlite::fromJSON(js_file)
  svalue(cat_wid)  <- with(
    prod_opt_list[[general_opts$sel_prod]][[general_opts$prod_version]],
    paste(cat01, cat02, sep = " - ")
  )
  svalue(prod_wid) <- general_opts$sel_prod
  svalue(vers_wid) <- general_opts$prod_version
  svalue(sens_wid) <- general_opts$sensor

  # set dummy variables holding the initial values of selected bands
  gui_env$temp_wid_bands         <- general_opts$bandsel
  gui_env$temp_wid_bands_indexes <- general_opts$indexes_bandsel
  gui_env$temp_wid_bands_quality <- general_opts$quality_bandsel

  svalue(server_wid)   <- general_opts$download_server
  svalue(user_wid)     <- general_opts$user
  svalue(password_wid) <- general_opts$password
  svalue(aria_wid)     <- general_opts$use_aria
  svalue(seas_wid)     <- general_opts$download_range

  # Dates options
  svalue(start_date_wid) <- general_opts$start_date
  svalue(end_date_wid)   <- general_opts$end_date

  # Tiles options
  svalue(start_x_wid) <- general_opts$start_x
  svalue(end_x_wid)   <- general_opts$end_x
  svalue(start_y_wid) <- general_opts$start_y
  svalue(end_y_wid)   <- general_opts$end_y

  # Proj and extent options
  svalue(proj_wid)            <- general_opts$proj
  svalue(output_proj4_wid)    <- general_opts$user_proj4
  svalue(output_res_sel_wid)  <- general_opts$out_res_sel
  svalue(output_res_wid)      <- general_opts$out_res
  svalue(output_resmeth_wid)  <- general_opts$resampling
  svalue(output_ext_wid)      <- general_opts$full_ext
  svalue(output_ul_east_wid)  <- general_opts$bbox[1]
  svalue(output_lr_east_wid)  <- general_opts$bbox[3]
  svalue(output_lr_north_wid) <- general_opts$bbox[2]
  svalue(output_ul_north_wid) <- general_opts$bbox[4]
  svalue(reprocess_wid)       <- general_opts$reprocess
  svalue(delete_wid)          <- general_opts$delete_hdf
  svalue(nodata_wid)          <- general_opts$nodata_change
  svalue(scale_wid)           <- general_opts$scale_val

  svalue(format_wid)     <- general_opts$out_format
  svalue(timeseries_wid) <- general_opts$ts_format

  svalue(compress_wid)   <- names(general_opts$compress)

  # Folder options
  svalue(outfold_wid)    <- general_opts$out_folder
  svalue(outfoldmod_wid) <- general_opts$out_folder_mod
  #nocov end
}


#' @title gui_prepare_to_save_options
#' @description Helper function to check consistency of the selected processing
#'  options before saving to a json file or starting MODIStsp processing
#' @noRd
#' @importFrom gWidgets gconfirm gmessage svalue
#' @importFrom jsonlite fromJSON
#' @importFrom raster crop extent
#' @noRd
gui_prepare_to_save_options <- function(general_opts,
                                        gui_env,
                                        ...) {
  #nocov start
  svalue <- gWidgets::svalue
  # workaround to retrieve custom index, since it was already saved to the
  # JSON but it is not available in current variables

  general_opts$custom_indexes <-
    jsonlite::fromJSON(opt_jsfile)$custom_indexes
  # retrieve product options
  general_opts$sel_prod <- mod_prod_list[
    which(mod_prod_list == svalue(prod_wid))
    ]
  general_opts$prod_version <-
    prod_opt_list[[general_opts$sel_prod]][[which(vapply(
      prod_opt_list[[general_opts$sel_prod]],
      function(x){
        x$v_number
      }
      , FUN.VALUE = "") == svalue(vers_wid))]]$v_number
  general_opts$sensor <- svalue(sens_wid)
  #retrieve selected bands
  if (exists("temp_wid_bands", where = gui_env)) {
    general_opts$bandsel <- gui_env$temp_wid_bands
  }
  #retrieve selected spectral indexes
  if (exists("temp_wid_bands_indexes", where = gui_env)) {
    general_opts$indexes_bandsel <- gui_env$temp_wid_bands_indexes
  }
  #retrieve selected quality indicators
  if (exists("temp_wid_bands_indexes", where = gui_env)) {
    general_opts$quality_bandsel <- gui_env$temp_wid_bands_quality
  }

  # Retrieve download method and authentication
  general_opts$user            <- svalue(user_wid)
  general_opts$password        <- svalue(password_wid)
  general_opts$download_server <- svalue(server_wid)
  general_opts$use_aria        <- svalue(aria_wid)
  general_opts$download_range  <- svalue(seas_wid)

  # Retrieve dates
  general_opts$start_date <- svalue(start_date_wid)
  general_opts$end_date   <- svalue(end_date_wid)

  # Retrieve Tiles options
  general_opts$start_x <- svalue(start_x_wid)
  general_opts$end_x   <- svalue(end_x_wid)
  general_opts$start_y <- svalue(start_y_wid)
  general_opts$end_y   <- svalue(end_y_wid)

  # Retrieve Proj and extent options
  general_opts$proj        <- svalue(proj_wid)
  general_opts$user_proj4  <- svalue(output_proj4_wid)
  general_opts$out_res_sel <- svalue(output_res_sel_wid)
  general_opts$out_res     <- svalue(output_res_wid)
  general_opts$resampling  <- svalue(output_resmeth_wid)
  general_opts$full_ext    <- svalue(output_ext_wid)
  general_opts$bbox        <- c(svalue(output_ul_east_wid),
                                svalue(output_lr_north_wid),
                                svalue(output_lr_east_wid),
                                svalue(output_ul_north_wid))

  # Retrieve reprocess, delete and nodata
  general_opts$reprocess  <- svalue(reprocess_wid)
  general_opts$delete_hdf <- svalue(delete_wid)

  general_opts$nodata_change <- svalue(nodata_wid)
  general_opts$scale_val     <- svalue(scale_wid)
  general_opts$rts           <- svalue(rts_wid)

  # Retrieve format, virtual and compression
  general_opts$out_format <- svalue(format_wid)
  general_opts$ts_format  <- svalue(timeseries_wid)
  general_opts$compress   <- compress_dict[svalue(compress_wid)]

  # Retrieve Folder options
  general_opts$out_folder     <- svalue(outfold_wid)
  general_opts$out_folder_mod <- svalue(outfoldmod_wid)

  gui_env$check_save_opts <- TRUE
  # Send warning if HDF deletion selected
  if (general_opts$delete_hdf == "Yes") {
    gui_env$check_save_opts <- gWidgets::gconfirm(
      strwrap("Warning! HDF files in Original MODIS folder will be
              deleted at the end of processing! \n\n
              Are you sure? ", width = 80),
      title = "Warning", icon = "warning"
    )
  }

  #- Perform checks on options consistency ---------------

  # Check if at least 1 layer selected

  if (max(general_opts$bandsel) +
      ifelse(length(general_opts$indexes_bandsel) > 0,
             max(general_opts$indexes_bandsel),
             0) + max(general_opts$quality_bandsel) == 0) {
    gWidgets::gmessage(
      message = "No Output bands or indexes selected - Please Correct!",
      title   = "Warning"
    )
    gui_env$check_save_opts <- FALSE
  }

  # Check if dates, processing extent and tiles selection make sense
  if (as.Date(general_opts$start_date) > as.Date(general_opts$end_date)) {
    gWidgets::gmessage(
      message = "Ending date earlier than starting date - Please correct!",
      title   = "Warning"
    )
    gui_env$check_save_opts <- FALSE
  }

  if (
    class(try(as.Date(general_opts$start_date),
              silent = TRUE)) == "try-error" |
    class(try(as.Date(general_opts$end_date),
              silent = TRUE)) == "try-error"
  ) {
    gWidgets::gmessage(
      message = "One or both dates are in wrong format - Please correct!",
      title   = "Warning"
    )
    gui_env$check_save_opts <- FALSE
  }

  if (general_opts$start_x > general_opts$end_x |
      general_opts$start_y > general_opts$end_y) {
    gWidgets::gmessage(message = "Error in Selected Tiles! Please correct!",
                       title   = "Warning")
    gui_env$check_save_opts <- FALSE
  }

  # Check if bbox is consistent
  suppressWarnings(general_opts$bbox <- as.numeric(general_opts$bbox))
  general_opts$bbox <- as.numeric(general_opts$bbox)
  n_bbox_compiled   <- length(which(is.finite(general_opts$bbox)))
  if (general_opts$full_ext == "Resized") {
    if (n_bbox_compiled == 4) {
      if (general_opts$bbox[1] > general_opts$bbox[3] |
          general_opts$bbox[2] > general_opts$bbox[4]) {
        gWidgets::gmessage(message = "Error in Selected Output extent",
                           title   = "Warning")
        gui_env$check_save_opts <- FALSE
      }
    } else {
      if (n_bbox_compiled >= 0) {
        gWidgets::gmessage(
          message = "Error in Selected Output extent. Please correct!",
          title   = "Warning")
        gui_env$check_save_opts <- FALSE
      }
    }
  }
  # Check if selected tiles are consistent with the bounding box
  if (general_opts$full_ext == "Resized" & gui_env$check_save_opts) {
    bbox_mod         <- reproj_bbox(
      general_opts$bbox,
      svalue(output_proj4_wid), mod_proj_str,
      enlarge = TRUE
    )
    d_bbox_mod_tiled <- raster::crop(modis_grid, raster::extent(bbox_mod))
    required_tiles   <- paste0(
      "H",
      apply(expand.grid("H" = min(d_bbox_mod_tiled$H):max(d_bbox_mod_tiled$H),
                        "V" = min(d_bbox_mod_tiled$V):max(d_bbox_mod_tiled$V)
      ), 1, paste, collapse = "_V")
    )

    selected_tiles <- paste0(
      "H",
      apply(expand.grid(
        "H" = svalue(start_x_wid):svalue(end_x_wid),
        "V" = svalue(start_y_wid):svalue(end_y_wid)),
        1, paste, collapse = "_V")
    )

    # If the bounding box does not intersect with the tiles, return a warning
    # asking to automatically retrieve from extent
    if (!any(required_tiles %in% selected_tiles)) {
      gui_env$check_save_opts <- gWidgets::gconfirm(
        strwrap("The selected tiles do not intersect the output bounding
                box. \n\n Do you want to discard your choice and retrieve
                automatically the required tiles from the bounding box?",
                width = 200),
        handler = function(h, ...) {
          selected_tiles       <<- required_tiles
          general_opts$start_x <<- min(d_bbox_mod_tiled$H)
          general_opts$end_x   <<- max(d_bbox_mod_tiled$H)
          general_opts$start_y <<- min(d_bbox_mod_tiled$V)
          general_opts$end_y   <<- max(d_bbox_mod_tiled$V)
        }
        , title = "Warning"
      )
    }

    # If not all the required tiles are selected, ask to select them
    if (!all(required_tiles %in% selected_tiles) & gui_env$check_save_opts) {
      gWidgets::gconfirm(
        message = strwrap(paste(
          "The following tiles not currently selected are required to cover
          the output bounding box (",
          paste(required_tiles[!(required_tiles %in% selected_tiles)],
                collapse = ", "),
          "). \n\n Do you want to add them to the processing? Otherwise,
          nodata will be produced in the non-covered area.")),
        handler = function(h, ...) {
          selected_tiles       <<- required_tiles
          general_opts$start_x <<- min(d_bbox_mod_tiled$H)
          general_opts$end_x   <<- max(d_bbox_mod_tiled$H)
          general_opts$start_y <<- min(d_bbox_mod_tiled$V)
          general_opts$end_y   <<- max(d_bbox_mod_tiled$V)
        }
        , title = "question"
      )
    }

    # If some selected tiles are not useful, ask to remove them
    if (!all(selected_tiles %in% required_tiles) & gui_env$check_save_opts) {
      gWidgets::gconfirm(
        message = strwrap(paste(
          "The following tiles are not required to cover the output
          bounding box (",
          paste(selected_tiles[!(selected_tiles %in% required_tiles)],
                collapse = ", "),
          "). \n\n Do you want to remove them from processing?")
        ),
        handler = function(h, ...) {
          selected_tiles       <<- required_tiles
          general_opts$start_x <<- min(d_bbox_mod_tiled$H)
          general_opts$end_x   <<- max(d_bbox_mod_tiled$H)
          general_opts$start_y <<- min(d_bbox_mod_tiled$V)
          general_opts$end_y   <<- max(d_bbox_mod_tiled$V)
        }
        , title = "Warning"
      )
    }
  }

  # check if folders are defined
  if (general_opts$out_folder == "" & gui_env$check_save_opts) {
    gWidgets::gmessage(
      message = "Please Select an output folder MODIStsp outputs!",
      title   = "Warning"
    )
    gui_env$check_save_opts <- FALSE
  }
  if (general_opts$out_folder_mod == "" & gui_env$check_save_opts) {
    gWidgets::gmessage(
      message = "Select an output folder for storage of original HDFs!",
      title   = "Warning")
    gui_env$check_save_opts <- FALSE
  }

  # Issue Warning on Mode resamling
  if (general_opts$resampling == "mode" & gui_env$check_save_opts) {
    check_mode <- gWidgets::gconfirm(
      message = strwrap(
        "Warning! You selected 'mode' resampling. Be aware that 'mode'
          resampling can give inconsistent results in areas affected by
          mixed high and low quality data, and fail in properly keeping
          track of quality indicators! \n\n Do you wish to continue?",
        width = 300),
      title   = "Warning"
    )
    if (check_mode == FALSE) {
      gui_env$check_save_opts <- FALSE
    }
  }

  # check that user/password were provided in case of html download
  if (general_opts$download_server == "http" &
      (general_opts$user == "" | general_opts$password == "") &
      gui_env$check_save_opts
  ) {
    gWidgets::gmessage(
      message = strwrap("Username and password are mandatory in case of
                         `http` download! \n\n Please provide them or
                          choose 'ftp' download.", width = 300),
      title   = "Warning")
    gui_env$check_save_opts <- FALSE
  }

  return(general_opts)
  #nocov end
}
# END save options function
