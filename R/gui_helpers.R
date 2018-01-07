#### HELPER FUNCTIONS FOR MODIStsp_GUI TO AVOID REPETITIONS AND FACILITATE  ####
#### MAINTENANCE

#' @title gui_update_bboxlabels
#' @description Helper function to update the labels of the gui showing the
#'  bounding box coordinates when a spatial file is selected or a projection
#'  change is issued.
#' @importFrom raster crop extent
#' @importFrom gWidgets svalue
#' @noRd
#'
gui_update_bboxlabels <- function(bbox_out,
                                  units,
                                  wids) {
  #nocov start
  
  digits <- ifelse(units == "dec. degrees", 4, 1)
  gWidgets::svalue(wids$output_ul_east)  <- formatC(bbox_out[1, 1],
                                                    digits = digits,
                                                    format = "f")
  gWidgets::svalue(wids$output_ul_north) <- formatC(bbox_out[2, 2],
                                                    digits = digits,
                                                    format = "f")
  gWidgets::svalue(wids$output_lr_east)  <- formatC(bbox_out[1, 2],
                                                    digits = digits,
                                                    format = "f")
  gWidgets::svalue(wids$output_lr_north) <- formatC(bbox_out[2, 1],
                                                    digits = digits,
                                                    format = "f")
  #nocov end
}

#' @title gui_update_tiles
#' @description Helper function to update the selected tiles with the
#' intersection with the bounding box
#' @importFrom raster crop extent
#' @importFrom gWidgets svalue
#' @noRd
#'
gui_update_tiles <- function(bbox_out,
                             mod_proj_str,
                             modis_grid,
                             wids) {
  #nocov start
  
  bbox_mod  <- reproj_bbox(bbox_out,
                           gWidgets::svalue(wids$output_proj4),
                           mod_proj_str,
                           enlarge = TRUE)
  
  d_bbox_mod_tiled    <- raster::crop(modis_grid, raster::extent(bbox_mod))
  gWidgets::svalue(wids$start_x) <- min(d_bbox_mod_tiled$H)
  gWidgets::svalue(wids$end_x)   <- max(d_bbox_mod_tiled$H)
  gWidgets::svalue(wids$start_y) <- min(d_bbox_mod_tiled$V)
  gWidgets::svalue(wids$end_y)   <- max(d_bbox_mod_tiled$V)
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
gui_get_units <- function(sel_output_proj,
                          proj) {
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
gui_load_options <- function(opts_jsfile,
                             wids,
                             prod_opt_list) {
  #nocov start
  
  # load file and reset all widgets to values found in the loaded file
  general_opts <- try(jsonlite::fromJSON(opts_jsfile), silent = TRUE)
 
  # stop on error
  if (class(general_opts) == "try-error") {
    stop("Unable to read the provided JSON options file. Please check your ", 
         "inputs!")
  }
  
  gWidgets::svalue(wids$cat) <- paste(
    prod_opt_list[[general_opts$sel_prod]][[general_opts$prod_version]]$cat01,
    prod_opt_list[[general_opts$sel_prod]][[general_opts$prod_version]]$cat02,
    sep = " - "
  )
  gWidgets::svalue(wids$prod) <- general_opts$sel_prod
  gWidgets::svalue(wids$vers) <- general_opts$prod_version
  gWidgets::svalue(wids$sens) <- general_opts$sensor
  
  # set dummy variables holding the initial values of selected bands
  gui_env$temp_wid_bands         <- general_opts$bandsel
  gui_env$temp_wid_bands_indexes <- general_opts$indexes_bandsel
  gui_env$temp_wid_bands_quality <- general_opts$quality_bandsel
  
  gWidgets::svalue(wids$server)   <- general_opts$download_server
  gWidgets::svalue(wids$user)     <- general_opts$user
  gWidgets::svalue(wids$password) <- general_opts$password
  gWidgets::svalue(wids$aria)     <- general_opts$use_aria
  gWidgets::svalue(wids$seas)     <- general_opts$download_range
  
  # Dates options
  gWidgets::svalue(wids$start_date) <- general_opts$start_date
  gWidgets::svalue(wids$end_date)   <- general_opts$end_date
  
  # Tiles options
  gWidgets::svalue(wids$start_x) <- general_opts$start_x
  gWidgets::svalue(wids$end_x)   <- general_opts$end_x
  gWidgets::svalue(wids$start_y) <- general_opts$start_y
  gWidgets::svalue(wids$end_y)   <- general_opts$end_y
  
  # Proj and extent options
  gWidgets::svalue(wids$proj)            <- general_opts$proj
  gWidgets::svalue(wids$output_proj4)    <- general_opts$user_proj4
  gWidgets::svalue(wids$output_res_sel)  <- general_opts$out_res_sel
  gWidgets::svalue(wids$output_res)      <- general_opts$out_res
  gWidgets::svalue(wids$output_resmeth)  <- general_opts$resampling
  gWidgets::svalue(wids$output_ext)      <- general_opts$full_ext
  gWidgets::svalue(wids$output_ul_east)  <- general_opts$bbox[1]
  gWidgets::svalue(wids$output_lr_east)  <- general_opts$bbox[3]
  gWidgets::svalue(wids$output_lr_north) <- general_opts$bbox[2]
  gWidgets::svalue(wids$output_ul_north) <- general_opts$bbox[4]
  gWidgets::svalue(wids$reprocess)       <- general_opts$reprocess
  gWidgets::svalue(wids$delete)          <- general_opts$delete_hdf
  gWidgets::svalue(wids$nodata)          <- general_opts$nodata_change
  gWidgets::svalue(wids$scale)           <- general_opts$scale_val
  
  gWidgets::svalue(wids$format)     <- general_opts$out_format
  gWidgets::svalue(wids$timeseries) <- general_opts$ts_format
  
  gWidgets::svalue(wids$compress)   <- names(general_opts$compress)
  
  # Folder options
  gWidgets::svalue(wids$outfold)    <- general_opts$out_folder
  gWidgets::svalue(wids$outfoldmod) <- general_opts$out_folder_mod
  #nocov end
}


#' @title gui_save_options
#' @description Helper function to check consistency of the selected processing
#'  options before saving to a json file or starting MODIStsp processing
#' @noRd
#' @importFrom gWidgets gconfirm gmessage svalue
#' @importFrom jsonlite fromJSON
#' @importFrom raster crop extent
#' @noRd
gui_save_options <- function(general_opts,
                             gui_env,
                             opts_jsfile,
                             mod_prod_list,
                             mod_proj_str,
                             modis_grid,
                             prod_opt_list,
                             compress_dict,
                             wids) {
  #nocov start
  
  # workaround to retrieve custom index, since it was already saved to the
  # JSON but it is not available in current variables
  
  general_opts$custom_indexes <-
    jsonlite::fromJSON(opts_jsfile)$custom_indexes
  # retrieve product options
  general_opts$sel_prod <- mod_prod_list[
    which(mod_prod_list == gWidgets::svalue(wids$prod))
    ]
  general_opts$prod_version <-
    prod_opt_list[[general_opts$sel_prod]][[which(vapply(
      prod_opt_list[[general_opts$sel_prod]],
      function(x){
        x$v_number
      }
      , FUN.VALUE = "") == gWidgets::svalue(wids$vers))]]$v_number
  

  general_opts$sensor <- gWidgets::svalue(wids$sens)
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
  general_opts$user            <- gWidgets::svalue(wids$user)
  general_opts$password        <- gWidgets::svalue(wids$password)
  general_opts$download_server <- gWidgets::svalue(wids$server)
  general_opts$use_aria        <- gWidgets::svalue(wids$aria)
  general_opts$download_range  <- gWidgets::svalue(wids$seas)
  
  # Retrieve dates
  general_opts$start_date <- gWidgets::svalue(wids$start_date)
  general_opts$end_date   <- gWidgets::svalue(wids$end_date)
  
  # Retrieve Tiles options
  general_opts$start_x <- gWidgets::svalue(wids$start_x)
  general_opts$end_x   <- gWidgets::svalue(wids$end_x)
  general_opts$start_y <- gWidgets::svalue(wids$start_y)
  general_opts$end_y   <- gWidgets::svalue(wids$end_y)
  
  # Retrieve Proj and extent options
  general_opts$proj        <- gWidgets::svalue(wids$proj)
  general_opts$user_proj4  <- gWidgets::svalue(wids$output_proj4)
  general_opts$out_res_sel <- gWidgets::svalue(wids$output_res_sel)
  general_opts$out_res     <- gWidgets::svalue(wids$output_res)
  general_opts$resampling  <- gWidgets::svalue(wids$output_resmeth)
  general_opts$full_ext    <- gWidgets::svalue(wids$output_ext)
  general_opts$bbox        <- c(gWidgets::svalue(wids$output_ul_east),
                                gWidgets::svalue(wids$output_lr_north),
                                gWidgets::svalue(wids$output_lr_east),
                                gWidgets::svalue(wids$output_ul_north))
  
  # Retrieve reprocess, delete and nodata
  general_opts$reprocess  <- gWidgets::svalue(wids$reprocess)
  general_opts$delete_hdf <- gWidgets::svalue(wids$delete)
  
  general_opts$nodata_change <- gWidgets::svalue(wids$nodata)
  general_opts$scale_val     <- gWidgets::svalue(wids$scale)
  general_opts$rts           <- gWidgets::svalue(wids$rts)
  
  # Retrieve format, virtual and compression
  general_opts$out_format <- gWidgets::svalue(wids$format)
  general_opts$ts_format  <- gWidgets::svalue(wids$timeseries)
  general_opts$compress   <- compress_dict[gWidgets::svalue(wids$compress)]
  
  # Retrieve Folder options
  general_opts$out_folder     <- gWidgets::svalue(wids$outfold)
  general_opts$out_folder_mod <- gWidgets::svalue(wids$outfoldmod)
  
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
      gWidgets::svalue(wids$output_proj4), mod_proj_str,
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
        "H" = gWidgets::svalue(wids$start_x):gWidgets::svalue(wids$end_x),
        "V" = gWidgets::svalue(wids$start_y):gWidgets::svalue(wids$end_y)),
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
      message = "Please select an output folder MODIStsp outputs!",
      title   = "Warning"
    )
    gui_env$check_save_opts <- FALSE
  }
  if (general_opts$out_folder_mod == "" & gui_env$check_save_opts) {
    gWidgets::gmessage(
      message = "Please select an output folder for storage of original HDFs!",
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
  
  # check that the select product is available on the selected server
  # 
  http <- prod_opt_list[[general_opts$sel_prod]][[which(vapply(
    prod_opt_list[[general_opts$sel_prod]],
    function(x){
      x$v_number
    }
    , FUN.VALUE = "") == gWidgets::svalue(wids$vers))]]$http
  
  ftp <- prod_opt_list[[general_opts$sel_prod]][[which(vapply(
    prod_opt_list[[general_opts$sel_prod]],
    function(x){
      x$v_number
    }
    , FUN.VALUE = "") == gWidgets::svalue(wids$vers))]]$ftp
  
  if (general_opts$sensor == "Both") {
    http <- c(http["Terra"][[1]], http["Aqua"][[1]])
    ftp  <- c(ftp["Terra"][[1]], ftp["Aqua"][[1]])
  } else {
    http <- http[general_opts$sensor][[1]]
    ftp  <- ftp[general_opts$sensor][[1]]
  }
  
  if (general_opts$download_server == "ftp" & unique(ftp) == "Not Available") {
    gWidgets::gmessage(
      message = strwrap("The selected product/version is not available over 
         ftp.\n\n Please try switching to http download!", width = 300),
      title   = "Warning")
    gui_env$check_save_opts <- FALSE
  }
  
  if (general_opts$download_server == "http" & unique(http) == "Not Available") { #nolint
    gWidgets::gmessage(
      message = strwrap("The selected product/version is only available for the 
                        Terra sensor.\n\n Please switch sensor!", width = 300),
      title   = "Warning")
    gui_env$check_save_opts <- FALSE
  }
  
  #   __________________________________________________________________________
  #   # If all checks passed, save options file and return                  ####
  
  if (gui_env$check_save_opts) {
    jsonlite::write_json(general_opts, opts_jsfile, pretty = TRUE,
                         auto_unbox = TRUE)
  }
  
  return(general_opts)
  #nocov end
}
# END save options function
