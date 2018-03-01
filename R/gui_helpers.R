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

  sel_prod      <- general_opts$sel_prod
  sel_prodopts  <- prod_opt_list[[sel_prod]]
  cur_prodopts <- sel_prodopts[[general_opts$prod_version]]

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


  curr_sel_layers <- paste(
    cur_prodopts[["band_fullnames"]][which(general_opts[["bandsel"]] == 1)],
    collapse = "; ")
  gWidgets::svalue(wids$sel_layers) <- curr_sel_layers

  curr_sel_qual <- paste(
    cur_prodopts[["quality_fullnames"]][which(general_opts[["quality_bandsel"]] == 1)],
    collapse = "; ")
  gWidgets::svalue(wids$sel_qi) <- curr_sel_qual

  check_names_indexes <- c(
    cur_prodopts[["indexes_fullnames"]],
    as.list(general_opts$custom_indexes[[sel_prod]]
            [[gWidgets::svalue(wids$vers)]])$indexes_fullnames
  )

  curr_sel_si <- paste(
    check_names_indexes[which(general_opts[["indexes_bandsel"]] == 1)],
    collapse = "; ")
  gWidgets::svalue(wids$sel_si) <- curr_sel_si

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

  gWidgets::svalue(wids$output_proj4)    <- general_opts$user_proj4
  gWidgets::svalue(wids$proj)            <- general_opts$proj
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

  gWidgets::svalue(wids$compress)   <- names(
    compress_dict[which(compress_dict == general_opts$compress)]
  )

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

  if (file.exists(opts_jsfile)) {
    general_opts$custom_indexes <-
      jsonlite::fromJSON(opts_jsfile)$custom_indexes
  }
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
  if (general_opts$full_ext == "Define Custom Area") {
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
  if (general_opts$full_ext == "Define Custom Area" & gui_env$check_save_opts) {
    bbox_mod         <- reproj_bbox(
      general_opts$bbox,
      gWidgets::svalue(wids$output_proj4), mod_proj_str,
      enlarge = FALSE
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

# GUI handlers ----

# gh_childs ----
gh_childs <- function(h, ...) {

  prod_opt_list <- get(load(prodopts_file))
  general_opts  <- jsonlite::fromJSON(opts_jsfile)
  curr_prod     <- gWidgets::svalue(wids$prod)
  curr_vers     <- gWidgets::svalue(wids$vers)
  curr_opts     <- prod_opt_list[[curr_prod]]
  # retrieve band names available for sel. product
  check_names   <- curr_opts[[curr_vers]]$band_fullnames
  # retrieve currently selected original layers
  wids$check     <- gui_env$temp_wid_bands
  selgroup      <-  gWidgets::gbasicdialog(
    title      = paste0("Select Processing Layers -  ",
                        curr_prod,
                        " vers. ", curr_vers
    ),
    parent     = NULL,
    do.buttons = TRUE,
    horizontal = FALSE,
    # this handler saves the current values of selected layers, so that:
    #   - upon realizing the widget, currently selected layers are ticked;
    #   - if user cancels operation after changing something, we go back to
    #     previous selection
    handler    = function(h, ...) {
      # onfind which layers selected and store in gui_env$temp_wid_bands
      wids$pos      <- which(check_names %in% gWidgets::svalue(wids$bands))
      tmp_arr_bands <- array(data = 0, dim = length(check_names))
      tmp_arr_bands[wids$pos] <- 1
      gui_env$temp_wid_bands <- tmp_arr_bands
      # update the selected layers widget lable

      cur_prodopts <- curr_opts[[gWidgets::svalue(wids$vers)]]
      curr_sel_layers <- paste(
        cur_prodopts[["band_fullnames"]][which(tmp_arr_bands != 0)],
        collapse = "; ")

      gWidgets::svalue(wids$sel_layers) <- ifelse(curr_sel_layers == "",
                                                  " - None Selected - ",
                                                  curr_sel_layers)

      # Find which indexes selected and store in
      # gui_env$temp_wid_bands_indexes
      if (length(which(check_names_indexes != "") > 0)) {
        wids$pos <- which(
          check_names_indexes %in% gWidgets::svalue(wids$bands_indexes)
        )
        tmp_arr_ind <- array(data = 0, dim = length(check_names_indexes))
        tmp_arr_ind[wids$pos] <- 1
        gui_env$temp_wid_bands_indexes <- tmp_arr_ind

        # update the selected layers widget lable
        curr_sel_si <- paste(
          check_names_indexes[which(tmp_arr_ind != 0)],
          collapse = "; ")

        gWidgets::svalue(wids$sel_si) <- ifelse(curr_sel_si == "",
                                                " - None Selected - ",
                                                curr_sel_si)
      }

      # Find which QI selected and store in gui_env$temp_wid_bands_quality
      if (length(which(check_names_quality != "") > 0)) {
        wids$pos <- which(
          check_names_quality %in% gWidgets::svalue(wids$bands_quality)
        )
        tmp_arr_qual <- array(data = 0, dim = length(check_names_quality))
        tmp_arr_qual[wids$pos] <- 1
        gui_env$temp_wid_bands_quality <- tmp_arr_qual
        # update the selected layers widget lable
        curr_sel_qual <- paste(
          cur_prodopts[["quality_fullnames"]][which(tmp_arr_qual != 0)],
          collapse = "; ")
        gWidgets::svalue(wids$sel_qi) <- ifelse(curr_sel_qual == "",
                                                " - None Selected - ",
                                                curr_sel_qual)
      }
    }
  )

  # child widgets for original layers selection ----
  cbox_main  <- gWidgets::ggroup(container = selgroup, horizontal = FALSE)
  cbox_total <- gWidgets::ggroup(container = cbox_main, horizontal = TRUE)
  cbox       <- gWidgets::gframe(
    text       = strwrap("<span foreground='red' size='large'>
                         Original MODIS Layers </span>"),
    markup     = TRUE,
    container  = cbox_total,
    horizontal = FALSE
  )
  wids$bands  <-  gWidgets::gcheckboxgroup(items     = check_names,
                                           checked   = as.logical(wids$check),
                                           container = cbox,
                                           use.table = FALSE)
  gWidgets::addSpring(cbox)
  layers_help <-  gWidgets::gbutton(
    text    = " ? ", handler = function(h, ...) {
      gh_help(h, "layers_help", help_messages, NULL, ...)
    },
    container = cbox,
    expand    = FALSE)

  # child widgets for Quailty Indicators selection ----
  # retrieve quality band names (if existing for sel. product)
  check_names_quality <- curr_opts[[curr_vers]]$quality_fullnames
  if (!is.null(check_names_quality)) {
    check_wid_quality <- gui_env$temp_wid_bands_quality
    cbox_quality <- gWidgets::gframe(
      text       = strwrap("<span foreground='red' size='large'>
                           Quality Indicators </span>"),
      markup     = TRUE,
      container  = cbox_total,
      horizontal = FALSE
    )
    gWidgets::addSpring(cbox_quality)
    wids$bands_quality <- gWidgets::gcheckboxgroup(
      items     = check_names_quality,
      checked   = as.logical(check_wid_quality),
      container = cbox_quality,
      use.table = FALSE
    )
    qi_help <- gWidgets::gbutton(
      text    = " ? ", handler = function(h, ...) {
        gh_help(h, "qi_help", help_messages, NULL, ...)
      },
      container = cbox_quality,
      expand    = FALSE)
  }

  # child widgets for spectral indexes selection  ----
  # retrieve indexes  names (if existing for sel. product)
  check_names_indexes <- c(
    curr_opts[[curr_vers]]$indexes_fullnames,
    as.list(general_opts$custom_indexes[[curr_prod]]
            [[curr_vers]])$indexes_fullnames
  )
  if (!is.null(check_names_indexes)) {
    # retrieve currently selected indexes layers
    check_wid_indexes <- gui_env$temp_wid_bands_indexes
    cbox_indexes      <- gWidgets::gframe(
      text       = strwrap("<span foreground='red' size='large'>
                           Additional Spectral Indexes</span>"),
      markup     = TRUE,
      container  = cbox_total,
      horizontal = FALSE
    )
    wids$bands_indexes <- gWidgets::gcheckboxgroup(
      items     = check_names_indexes,
      checked   = as.logical(check_wid_indexes),
      container = cbox_indexes,
      use.table = FALSE
    )
    glabel(text = "", container = cbox_indexes)

    ##  .................................................................. #
    ##  Here we create the sub child widget for creation of custom      ####
    ##  indexes. The `MODIStsp_addindex` function is used to spawn a modal
    ##  widget for indexes creation

    wids$band_newindex  <- gWidgets::gbutton(
      text    = "Add New Indices",
      handler = function(h, ...) {
        # Run addindex() function ----
        addind <- MODIStsp_addindex(opts_jsfile = opts_jsfile,
                                    prodopts_file = prodopts_file,
                                    selprod       = curr_prod,
                                    selvers       = curr_vers)

        # since upon return the widget for layers selection is automatically
        # disposed to allow addition of the index, here we check and save
        # which layers and indexes are currently selected
        general_opts  <- jsonlite::fromJSON(opts_jsfile)
        wids$pos <- which(check_names %in% gWidgets::svalue(wids$bands))
        tmp_arr_bands <- array(data = 0, dim = length(check_names))
        tmp_arr_bands[wids$pos] <- 1
        gui_env$temp_wid_bands <- tmp_arr_bands
        # store in selected indexes gui_env$temp_wid_bands_indexes array
        if (length(which(check_names_indexes != "") > 0)) {
          wids$pos <- which(
            check_names_indexes %in% gWidgets::svalue(wids$bands_indexes)
          )
          tmp_arr_ind <- array(data = 0, dim = length(check_names_indexes))
          tmp_arr_ind[wids$pos] <- 1
          gui_env$temp_wid_bands_indexes <- tmp_arr_ind
        }
        # store selected QIs in gui_env$temp_wid_bands_quality array
        if (length(which(check_names_quality != "") > 0)) {
          wids$pos <- which(
            check_names_quality %in% gWidgets::svalue(wids$bands_quality)
          )
          tmp_arr_qual <- array(data = 0, dim = length(check_names_quality))
          tmp_arr_qual[wids$pos] <- 1
          gui_env$temp_wid_bands_quality <- tmp_arr_qual
        }
        dispose(selgroup)
      },
      container = cbox_indexes,
      expand = FALSE
    )
    gWidgets::addSpring(cbox_indexes)
    si_help <- gWidgets::gbutton(text    = " ? ", handler = function(h, ...) {
      gh_help(h, "si_help_addindex", help_messages, NULL, ...)
    },
    container = cbox_indexes,
    expand    = FALSE)
  }

  # Start/Cancel buttons for layers selection child widget ----
  bands_group <- ggroup(container = cbox_main, horizontal = FALSE)

  # Widget for "www" button for layers selection child widget ----
  gWidgets::addSpring(bands_group)
  www_but <- gWidgets::gbutton(
    text = paste0("Product Info - www (",
                  curr_prod,
                  " vers. ", curr_vers, ")"),
    container = bands_group,
    handler   = function(h, ...) {
      utils::browseURL(curr_opts[[curr_vers]]$www)
    }
  )
  gWidgets::font(www_but) <- list(family = "sans", weight = "bold",
                                  color = "red")

  gWidgets::visible(selgroup, set = TRUE)
}

# Help messages ----

gh_help <- function(h, sel_help, help_messages, info_addr = NULL, ...) {
  help_box <- gWidgets::gbasicdialog(title      = "Help",
                                     parent     = NULL,
                                     do.buttons = FALSE,
                                     horizontal = FALSE,
                                     width      = 10,
                                     height     = 10)

  helptext <- subset(help_messages, which_help == sel_help)[["text"]]

  help_mess_lab <- gWidgets::glabel(
    text = strwrap(helptext, 80),
    editable  = FALSE,
    container = help_box,
    markup = TRUE
  )
  if (!is.null(info_addr)) {
    moreinfo <- gWidgets::gbutton(
      text = paste0("More Info"),
      container = help_box,
      handler   = function(h, ...) {
        utils::browseURL(info_addr)
      }
    )
  }
  gWidgets::visible(help_box) <- TRUE
  # size(help_mess_lab) <- list(width = 80)
}

# Handler for buttons for interactive extent selection ----
gh_selectmap <- function(h, ext_type, ...) {

  if (requireNamespace("mapedit")) {
    if (ext_type == "Select MODIS Tiles") {
      # On MODIS tiles selection, use editFeatures to allow selection
      # from the Tiles Map

      tilemap <- get(load(system.file("ExtData" ,"MODIS_Tiles_latlon.RData",
                                      package = "MODIStsp")))
      sel <- mapedit::selectFeatures(
        tilemap,
        viewer = shiny::browserViewer(browser = getOption("browser"))
      )
      if (!is.null(sel[["finished"]])) {

        seltiles <- lapply(sel[["Name"]], FUN = function(x){
          h <- as.numeric(str_split_fixed(x, "[a-z]:", 3)[2])
          v <- as.numeric(str_split_fixed(x, "[a-z]:", 3)[3])
          data.frame(h = h, v = v)})
        seltiles <- data.table::rbindlist(seltiles)
        error_sel <- FALSE
        if (length(unique(sel[["h"]])) == 1) {
          min_h <- max_h <- sel[["h"]]
        } else {
          if (max(diff(sort(sel[["h"]]))) <= 1) {
            min_h <- min(sel[["h"]])
            max_h <- max(sel[["h"]])
          } else {
            error_sel <- TRUE
          }
        }

        if (length(unique(sel[["v"]])) == 1) {
          min_v <- max_v <- sel[["v"]]
        } else {
          if (max(diff(sort(sel[["v"]]))) <= 1) {
            min_v <- min(sel[["v"]])
            max_v <- max(sel[["v"]])
          } else {
            error_sel <- TRUE
          }
        }

        if (error_sel) {
          gmessage(strwrap(
            "Your selection contains non-contiguous tiles!\n
      MODIStsp only allows processing for contigous tiles selections!\n\n
      Please select again!"), icon = "warning")
        } else {
          # on proper selection, update the tiles sliders
          gWidgets::svalue(wids$start_x) <- min_h
          gWidgets::svalue(wids$end_x)   <- max_h
          gWidgets::svalue(wids$start_y) <- min_v
          gWidgets::svalue(wids$end_y)   <- max_v
        }
      }
    } else {

      # On Custom Area selection, use editMap to allow drawing a custom area
      tilemap <- get(load(system.file("ExtData/MODIS_Tiles_latlon.RData",
                                      package = "MODIStsp")))
      mm <-  mapview::mapview(tilemap, alpha.regions = 0.1, color = "grey75")
      sel <- mapedit::editMap(
        mm,
        viewer = shiny::browserViewer(browser = getOption("browser")))

      if (!is.null(sel[["finished"]])) {
        sel_bbox <- sf::st_bbox(sel[["finished"]])

        crs_out <- ifelse(gWidgets::svalue(wids$proj) != "User Defined",
                          out_proj_list[[gWidgets::svalue(wids$proj)]],
                          general_opts$user_proj4)

        bbox_out <- reproj_bbox(sel_bbox,
                                CRS("+init=epsg:4326"),
                                crs_out,
                                enlarge = TRUE)

        # re-set bbox in the GUI according coordinates retrieved from file
        gui_update_bboxlabels(bbox_out,
                              units,
                              wids)

        # Set tiles according with the bounding box
        gui_update_tiles(bbox_out,
                         mod_proj_str,
                         modis_grid,
                         wids)
      }
    }
  } else {
    gmessage(strwrap(
      "You need to install package `mapedit` to be able to
    use this functionality!\n\n
    You can install it using `install.packages(mapedit)`"),
      icon = "warning")
  }
}

# Handler for load extent from file button

gh_load_extent <- function(h, ...) {

  choice <- try(gfile(
    type = "open",
    text = "Select a vector or raster file",
    # TODO add formats to the lists!
    filter = list("Spatial files" = list(patterns = c("*.shp", "*.kml",
                                                      "*.tif", "*.dat")),
                  "Vector layers" = list(patterns = c("*.shp", "*.kml")),
                  "Raster layers" = list(patterns = c("*.tif", "*.dat")),
                  "All files"     = list(patterns = "*"))
  ), silent = TRUE)
  if (class(choice) != "try-error" & length(choice) != 0) {
    # Show window until the process finishes
    message("[", date(), "]", " Retrieving the Extent, please wait...")
    wait_window       <- gwindow(title = "Please wait",
                                 width = 400, height = 40)
    gWidgets::size(wait_window) <- c(100, 8)
    addHandlerUnrealize(wait_window,
                        handler = function(h, ...) return(TRUE))
    wait_window_lab <- glabel(
      text      = paste("Retrieving Extent, please wait..."),
      editable  = FALSE,
      container = wait_window
    )
    Sys.sleep(0.05)
    # Convert bbox coordinates to output projection
    out_proj_crs <- ifelse(gWidgets::svalue(wids$proj) != "User Defined",
                           out_proj_list[[gWidgets::svalue(wids$proj)]],
                           general_opts$user_proj4)
    # Create the bounding box in the chosen projection retrieving it from
    # the specified file
    bbox_out <- try(bbox_from_file(file_path = choice,
                                   crs_out   = out_proj_crs),
                    silent = TRUE)
    if (class(bbox_out) == "try-error") {
      gmessage(bbox_out, title = "Error Detected!")
    } else {
      # re-set bbox in the GUI according coordinates retrieved from file
      gui_update_bboxlabels(bbox_out,
                            units,
                            wids)

      # Set tiles according with the bounding box
      gui_update_tiles(bbox_out,
                       mod_proj_str,
                       modis_grid,
                       wids)
    }
    message("[", date(), "]", " Retrieving Extent, please wait... DONE!")
    dispose(wait_window)

  }
}

# gh_view_extent ----
gh_view_extent <- function(h, ext_type, ...) {

  if (requireNamespace("mapedit")) {
    if (ext_type == "Select MODIS Tiles") {
      min_h <- gWidgets::svalue(wids$start_x)
      max_h <- gWidgets::svalue(wids$end_x)
      min_v <- gWidgets::svalue(wids$start_y)
      max_v <- gWidgets::svalue(wids$end_y)

      tilemap <- get(load(system.file("ExtData/MODIS_Tiles_latlon.RData",
                                      package = "MODIStsp")))
      cursel <- subset(tilemap,
                       h >= min_h & h <= max_h & v >= min_v & v <= max_v)
      mm <- leaflet::leaflet(cursel)
      mm <- leaflet::addPolygons(mm)
      mm <- leaflet::addTiles(mm)
      mapedit::selectMap(
        mm,
        viewer = shiny::browserViewer(browser = getOption("browser"))
      )
    } else {

      bbox <- as.numeric(c(gWidgets::svalue(wids$output_ul_east),
                           gWidgets::svalue(wids$output_lr_north),
                           gWidgets::svalue(wids$output_lr_east),
                           gWidgets::svalue(wids$output_ul_north)))
      bbox_sf <- sf::st_as_sfc(
        sf::st_bbox(
          c(xmin = bbox[1], xmax = bbox[3], ymax = bbox[4], ymin = bbox[2]),
          crs = sf::st_crs(mod_proj_str))
      )
      bbox_sf  <- sf::st_transform(bbox_sf, 4326)
      mm <- leaflet::leaflet(bbox_sf)
      mm <- leaflet::addPolygons(mm)
      mm <- leaflet::addTiles(mm)
      mapedit::selectMap(
        mm,
        viewer = shiny::browserViewer(browser = getOption("browser"))
      )

    }

  } else {
    gmessage(strwrap(
      "You need to install package `mapedit` to be able to
    use this functionality!\n\n
    You can install it using `install.packages(mapedit)`"),
      icon = "warning")
  }
}

# gh_view_extent ----
gh_tiles_from_bbox <- function(h, ...) {
  bbox <- as.numeric(c(gWidgets::svalue(wids$output_ul_east),
                       gWidgets::svalue(wids$output_lr_north),
                       gWidgets::svalue(wids$output_lr_east),
                       gWidgets::svalue(wids$output_ul_north)))
  # Check if bbox is consistent

  n_bbox_compiled <- length(which(is.finite(bbox)))
  if (gWidgets::svalue(wids$output_ext) != "Select MODIS Tiles" &
      n_bbox_compiled == 0) {
    gmessage("Please specify an output bounding box!", title = "Warning")
  } else if (gWidgets::svalue(wids$proj) == "User Defined" &
             nchar(gWidgets::svalue(wids$output_proj4)) == 0) {
    gmessage("Please specify an output projection", title = "Warning")
  } else if (n_bbox_compiled < 4) {
    gmessage("Error in Selected Output extent", title = "Warning")
  } else if (bbox[1] > bbox[3] | bbox[2] > bbox[4]) {
    gmessage("Error in Selected Output extent", title = "Warning")
  } else {
    # If all checks pass, retrieve the tiles and set the widget
    gui_update_tiles(bbox,
                     mod_proj_str,
                     modis_grid,
                     wids)
  }
}

# gh_selcat ----
gh_selcat <- function(h, ...) {
  # Identify only products of this category
  sel_prod    <- mod_prod_list[mod_prod_cat$cat == gWidgets::svalue(wids$cat)][1] #nolint
  wids$prod[] <- mod_prod_list[mod_prod_cat$cat == gWidgets::svalue(wids$cat)] #nolint
  gWidgets::svalue(wids$prod) <- sel_prod
  sel_prodopts <- prod_opt_list[[sel_prod]]
  # Select the last version (it assumes that versions in xml file are in
  # increasing order)

  wids$vers[] <- names(sel_prodopts)
  gWidgets::svalue(wids$vers) <- sel_prodopts[[length(sel_prodopts)]]$v_number #nolint
  # Disable sensor choice for combined datasets
  if (sel_prodopts[[gWidgets::svalue(wids$vers)]]$combined == 1) {
    gWidgets::enabled(wids$sens) <- FALSE
    wids$sens[] <- "Combined"
    gWidgets::svalue(wids$sens)  <- "Combined"
  } else {
    gWidgets::enabled(wids$sens) <- TRUE
    wids$sens[] <- c("Terra", "Aqua", "Both")
    gWidgets::svalue(wids$sens)  <- general_opts$sensor
  }
  # On product change, automatically modify the default projection - latlon
  # for tiled, Sinu for nontiled
  if (sel_prodopts[[gWidgets::svalue(wids$vers)]]$tiled == 0) {
    gWidgets::enabled(tiles_group) <- FALSE
    gWidgets::svalue(wids$proj)    <- "Native"
    gWidgets::svalue(wids$output_proj4) <-
      "+init=epsg:4008 +proj=longlat +ellps=clrk66 +no_defs"
  } else {
    gWidgets::enabled(tiles_group) <- TRUE
    gWidgets::svalue(wids$proj)    <- "Native"
    gWidgets::svalue(wids$output_proj4) <-
      "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs" #nolint
  }

  # reset dummy variables for band selection to 0 on product change
  gui_env$temp_wid_bands         <- 0
  gui_env$temp_wid_bands_indexes <- 0
  gui_env$temp_wid_bands_quality <- 0
}

# gh_selprod ----

gh_selprod <- function(h, ...) {
  sel_prod   <- ifelse(!is.null(gWidgets::svalue(wids$prod)),
                       gWidgets::svalue(wids$prod),
                       sel_prod)

  sel_prodopts <- prod_opt_list[[sel_prod]]
  # Select the last version (it assumes that versions in xml file are in
  # increasing order)
  wids$vers[]       <- names(sel_prodopts)
  gWidgets::svalue(wids$vers) <- sel_prodopts[[length(sel_prodopts)]]$v_number #nolint
  # Disable sensor choice for combined datasets
  if (sel_prodopts[[gWidgets::svalue(wids$vers)]]$combined == 1) {
    gWidgets::enabled(wids$sens) <- FALSE
    wids$sens[]        <- "Combined"
    gWidgets::svalue(wids$sens)  <- "Combined"
  } else {
    gWidgets::enabled(wids$sens) <- TRUE
    wids$sens[]        <- c("Terra", "Aqua", "Both")
    gWidgets::svalue(wids$sens)  <- general_opts$sensor
  }
  # On product change, automatically modify the default projection - latlon
  # for tiled, Sinu for nontiled

  if (sel_prodopts[[gWidgets::svalue(wids$vers)]]$tiled == 0) {
    gWidgets::enabled(tiles_group) <- FALSE
    gWidgets::svalue(wids$proj)    <- "Native"
    gWidgets::svalue(wids$output_proj4) <-
      "+init=epsg:4008 +proj=longlat +ellps=clrk66 +no_defs"
  } else {
    gWidgets::enabled(tiles_group) <- TRUE
    gWidgets::svalue(wids$proj)    <- "Native"
    gWidgets::svalue(wids$output_proj4) <-
      "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs" #nolint
  }
  # reset dummy variables for band selection to 0 on product change
  gui_env$temp_wid_bands         <- 0
  gui_env$temp_wid_bands_indexes <- 0
  gui_env$temp_wid_bands_quality <- 0

  cur_prodopts <- sel_prodopts[[svalue(wids$vers)]]

  # Original Bands ----

  svalue(wids$sel_layers) <- "- None selected -"
  svalue(wids$sel_qi)     <- ifelse(
    length(cur_prodopts[["quality_bandnames"]]) == 0,
    "- No Quality Indicators are available for this product - ",
    " - None selected - "
  )
  svalue(wids$sel_si)     <- ifelse(
    length(cur_prodopts[["indexes_bandnames"]]) == 0,
    "- Spectral Indexes can not be computed on this product - ",
    " - None selected - "
  )
}
