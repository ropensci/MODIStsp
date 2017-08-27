#' @title build and manage the MODIStsp GUI
#' @description
#'	Function used to generate and handle the GUI used to allow selection of
#'	MODIStsp processing parameters. If the "previous options" file
#'	(MODIStsp_Previous.json) already exists, it is loaded and used to reinstate
#'	the GUI to its last state. Otherwise, the previous options file is created
#'	by launching the MODIStsp_read_xml fucntion
#' @param general_opts data.frame containing general processing options passed
#'   by MODIStsp
#' @param prod_opt_list List of MODIS products specifications (read from
#'   MODIStsp_ProdOpts.xml file)
#' @param scroll_window logical parameter passed by MODIStsp main function.
#' @param MODIStsp_dir main folder of the package
#' @param previous_jsfile json parameters file containgn data of the last
#'   execution
#' @param prodopts_file rdata file containing info about MODIS products
#' @return quit - Logical - tells the main if running processing or exiting
#'  (also, Processing options are saved in "previous" file and (if
#'   "Save options" is pressed) in user's selected file)
#' @author Lorenzo Busetto, phD (2014-2015) \email{busetto.l@@irea.cnr.it}
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom hash hash keys values
#' @importFrom raster crop extent raster plot
#' @importFrom RJSONIO fromJSON toJSON
#' @importFrom sp CRS
#' @importFrom grDevices dev.new
#' @importFrom utils packageVersion browseURL
#' @importFrom glue glue
#' @importFrom gWidgets svalue gconfirm gmessage gbasicdialog ggroup
#'  getToolkitWidget gframe gdroplist enabled size addSpring glabel
#'  gcombobox addSpace gbutton gcheckboxgroup dispose visible gradio
#'  gedit

MODIStsp_GUI <- function(general_opts,
                         prod_opt_list,
                         MODIStsp_dir,
                         previous_jsfile,
                         prodopts_file,
                         scroll_window){

  #### HELPER FUNCTIONS TO AVOID REPETITIONS AND FACILITATE READING  ####
  #### Putting these here allows to avoid to explicitly pass a lot of
  #### arguments!!!
  ## Update the selected tiles with the intersection with the bounding box

  update_tiles <- function(bbox_out,
                           output_proj4_wid,
                           mod_proj_str,
                           modis_grid,
                           start_x_wid,
                           end_x_wid,
                           start_y_wid,
                           end_y_wid) {

    bbox_mod  <- reproj_bbox(bbox_out,
                             svalue(output_proj4_wid),
                             mod_proj_str,
                             enlarge = TRUE)

    d_bbox_mod_tiled    <- raster::crop(modis_grid, raster::extent(bbox_mod))
    svalue(start_x_wid) <- min(d_bbox_mod_tiled$H)
    svalue(end_x_wid)   <- max(d_bbox_mod_tiled$H)
    svalue(start_y_wid) <- min(d_bbox_mod_tiled$V)
    svalue(end_y_wid)   <- max(d_bbox_mod_tiled$V)
  }

  ## Update the labels in the bbox group
  update_bboxlabels <- function(bbox_out,
                                units,
                                output_ul_east_wid,
                                output_ul_north_wid,
                                output_lr_east_wid,
                                output_lr_north_wid) {

    digits <- ifelse(units == "dec. degrees", 4, 1)
    svalue(output_ul_east_wid)  <- formatC(bbox_out[1, 1], digits = digits,
                                           format = "f")
    svalue(output_ul_north_wid) <- formatC(bbox_out[2, 2], digits = digits,
                                           format = "f")
    svalue(output_lr_east_wid)  <- formatC(bbox_out[1, 2], digits = digits,
                                           format = "f")
    svalue(output_lr_north_wid) <- formatC(bbox_out[2, 1], digits = digits,
                                           format = "f")
  }

  ### Get currently selected projection and its measure units
  get_proj <- function(sel_output_proj) {
    head(strsplit(tail(
      strsplit(sel_output_proj@projargs, "+proj=")[[1]], 1), " +")[[1]], 1)
  }

  get_units <- function(sel_output_proj, proj) {
    if (proj == "longlat") {
      units <- "dec.degrees"
    } else {
      units <- ifelse(
        length(strsplit(sel_output_proj@projargs, "+units=")[[1]]) > 1,
        head(strsplit(tail(strsplit(sel_output_proj@projargs,
                                    "+units=")[[1]], 1), " +")[[1]], 1),
        "Unknown"
      )
    }
    units
  }

  ### Save the options to a JSON file
  prepare_to_save_options <- function(general_opts,
                                      gui_env,
                                      ...) {
    # workaround to retrieve custom index, since it was already saved to the
    # JSON but it is not available in current variables

    general_opts$custom_indexes <-
      RJSONIO::fromJSON(previous_jsfile)$custom_indexes
    # retrieve product options
    general_opts$sel_prod <- mod_prod_list[
      which(mod_prod_list == svalue(prod_wid))
      ]
    general_opts$prod_version <-
      prod_opt_list[[general_opts$sel_prod]][[which(vapply(
        prod_opt_list[[general_opts$sel_prod]],
        function(x){
          x$v_number
        }, FUN.VALUE = "") == svalue(vers_wid))]]$v_number
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
      gui_env$check_save_opts <- gconfirm(
        glue::glue("Warning! HDF files in Original MODIS folder will be ", 
        "deleted at the end of processing! \n\nAre you sure? "),
        title = "Warning", icon = "warning"
      )
    }

    #- Perform checks on options consistency ---------------

    # Check if at least 1 layer selected

    if (max(general_opts$bandsel) +
        ifelse(length(general_opts$indexes_bandsel) > 0,
               max(general_opts$indexes_bandsel),
               0) + max(general_opts$quality_bandsel) == 0) {
      gmessage(
        message = "No Output bands or indexes selected - Please Correct!",
        title   = "Warning"
      )
      gui_env$check_save_opts <- FALSE
    }

    # Check if dates, processing extent and tiles selection make sense
    if (as.Date(general_opts$start_date) > as.Date(general_opts$end_date)) {
      gmessage(
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
      gmessage(
        message = "One or both dates are in wrong format - Please correct!",
        title   = "Warning"
      )
      gui_env$check_save_opts <- FALSE
    }

    if (general_opts$start_x > general_opts$end_x |
        general_opts$start_y > general_opts$end_y) {
      gmessage(message = "Error in Selected Tiles! Please correct!",
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
          gmessage(message = "Error in Selected Output extent",
                   title   = "Warning")
          gui_env$check_save_opts <- FALSE
        }
      } else {
        if (n_bbox_compiled >= 0) {
          gmessage(message = "Error in Selected Output extent. Please correct!",
                   title   = "Warning")
          gui_env$check_save_opts <- FALSE
        }
      }
    }
    # Check if selected tiles are consistent with the bounding box
    if (general_opts$full_ext == "Resized" & gui_env$check_save_opts == TRUE) {
      bbox_mod         <- reproj_bbox(general_opts$bbox,
                                      svalue(output_proj4_wid), mod_proj_str,
                                      enlarge = TRUE)
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
        gui_env$check_save_opts <- gconfirm(
          glue::glue("The selected tiles do not intersect the output bounding ", 
                "box. \n\n Do you want to discard your choice and retrieve ",
                "automatically the required tiles from the bounding box? "),
          handler = function(h, ...) {
            selected_tiles       <<- required_tiles
            general_opts$start_x <<- min(d_bbox_mod_tiled$H)
            general_opts$end_x   <<- max(d_bbox_mod_tiled$H)
            general_opts$start_y <<- min(d_bbox_mod_tiled$V)
            general_opts$end_y   <<- max(d_bbox_mod_tiled$V)
          }, title = "Warning"
        )
      }

      # If not all the required tiles are selected, ask to select them
      if (!all(required_tiles %in% selected_tiles) & gui_env$check_save_opts) {
        gconfirm(
          message = glue::glue(
            "The following tiles not currently selected are required to cover ", 
            "the output bounding box (",
              paste(required_tiles[!(required_tiles %in% selected_tiles)],
                    collapse = ", "),
              "). \n\n Do you want to add them to the processing? Otherwise, ", 
              " nodata will be produced in the non-covered area."),
          handler = function(h, ...) {
            selected_tiles       <<- required_tiles
            general_opts$start_x <<- min(d_bbox_mod_tiled$H)
            general_opts$end_x   <<- max(d_bbox_mod_tiled$H)
            general_opts$start_y <<- min(d_bbox_mod_tiled$V)
            general_opts$end_y   <<- max(d_bbox_mod_tiled$V)
          }, title = "question"
        )
      }

      # If some selected tiles are not useful, ask to remove them
      if (!all(selected_tiles %in% required_tiles) & gui_env$check_save_opts) {
        gconfirm(
          message = glue::glue(
            "The following tiles are not required to cover the output ", 
            "bounding box (",
            paste(selected_tiles[!(selected_tiles %in% required_tiles)],
                collapse = ", "),
          "). \n\n Do you want to remove them from processing?"
        ),
        handler = function(h, ...) {
          selected_tiles       <<- required_tiles
          general_opts$start_x <<- min(d_bbox_mod_tiled$H)
          general_opts$end_x   <<- max(d_bbox_mod_tiled$H)
          general_opts$start_y <<- min(d_bbox_mod_tiled$V)
          general_opts$end_y   <<- max(d_bbox_mod_tiled$V)
        }, title = "Warning"
        )
      }
    }

    # check if folders are defined
    if (general_opts$out_folder == "" & gui_env$check_save_opts) {
      gmessage(message = "Please Select an output folder MODIStsp outputs!",
               title   = "Warning")
      gui_env$check_save_opts <- FALSE
    }
    if (general_opts$out_folder_mod == "" & gui_env$check_save_opts) {
      gmessage(
        message = "Select an output folder for storage of original HDFs!",
        title   = "Warning")
      gui_env$check_save_opts <- FALSE
    }

    # Issue Warning on Mode resamling
    if (general_opts$resampling == "mode" & gui_env$check_save_opts) {
      check_mode <- gconfirm(
        message = glue::glue(
          "Warning! You selected 'mode' resampling. Be aware that 'mode' ",
          "resampling can give inconsistent results in areas affected by ", 
          "mixed high and low quality data, and fail in properly keeping ", 
          "track of quality indicators! \n\n Do you wish to continue?"
        ),
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
      gmessage(
        message = glue::glue("Username and password are mandatory in case of ", 
                             "`http` download! \n\n Please provide them or ",
                             "choose 'ftp' download."
        ),
        title   = "Warning")
      gui_env$check_save_opts <- FALSE
    }

    return(general_opts)
  } # END save options function

  ## load options from JSON and set values of the GUI accordingly
  load_options <- function(choice) {
    # load file and reset all widgets to values found in the loaded file
    general_opts     <- RJSONIO::fromJSON(choice)
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
  } # end load options

  # create a new env to facilitate values-passing between widgets
  gui_env      <- new.env()
  gui_env$quit <- TRUE

  # -------------------------------------------------------------------------- #
  ####                      MAIN FUNCTION STARTS HERE                       ####
  # -------------------------------------------------------------------------- #

  #   __________________________________________________________________________
  #   Start Building the GUI                                                ####

  main_win <- gbasicdialog(
    title      = paste0("MODIStsp - v. ", utils::packageVersion("MODIStsp")),
    parent     = NULL,
    do.buttons = FALSE
  )

  # frame1 and 2 with expand=FALSE grant that widgets are not "too much
  # expanded", neither horizontally nor vertically

  main_frame1 <- ggroup(container  = main_win,
                        horizontal = TRUE,
                        expand     = FALSE,
                        use.scroll_window = scroll_window)

  main_frame2 <- ggroup(container  = main_frame1,
                        horizontal = FALSE,
                        expand     = FALSE)

  #   __________________________________________________________________________
  #   Initialize Main container: holds all widgets                          ####

  main_group  <- ggroup(container  = main_frame2,
                        horizontal = FALSE,
                        expand     = FALSE)

  if (scroll_window  == TRUE) {
    getToolkitWidget(main_win)$maximize()
  }

  mod_prod_cat <- as.data.frame(
    t(vapply(prod_opt_list, function(x){
      c(x[[1]]$cat01, x[[1]]$cat02)
    }, FUN.VALUE = character(2)))
  )
  names(mod_prod_cat) <- c("cat01", "cat02")
  mod_prod_cat$cat    <- apply(mod_prod_cat, 1, paste, collapse = " - ")

  # get the product name selected in the previous options file and find the
  # corresponding category
  mod_prod_list <- names(prod_opt_list)
  sel_prod      <- general_opts$sel_prod
  sel_cat       <- mod_prod_cat$cat[match(sel_prod, mod_prod_list)]
  sel_prodopts  <- prod_opt_list[[sel_prod]]
  # set defaults for projection names and corresponding proj4strings
  out_proj_names <- c("Sinusoidal", "UTM 32N", "Latlon WGS84", "Latlon MODIS",
                      "User Defined")
  out_proj_list  <- hash::hash(
    "Sinusoidal"   = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs",
    "UTM 32N"      = "+init=epsg:32632 +proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
    "Latlon WGS84" = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
    "Latlon MODIS" = "+init=epsg:4008 +proj=longlat +ellps=clrk66 +no_defs",
    "User Defined" = ""
  )
  mod_proj_str <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

  #   __________________________________________________________________________
  #   Initialize Widgets for product selection and bands selection          ####

  satprod_frame <- gframe(
    text       = glue::glue("<span foreground='red' size='x-large'>", 
                         "MODIS Product, Platform and Layers selection", 
                         "</span>"),
    markup     = TRUE,
    horizontal = FALSE,
    container  = main_group,
    spacing    = 5
  )

  # set dummy global variables holding the initial values of selected bands
  gui_env$temp_wid_bands         <- general_opts$bandsel
  gui_env$temp_wid_bands_indexes <- general_opts$indexes_bandsel
  gui_env$temp_wid_bands_quality <- general_opts$quality_bandsel

  #   __________________________________________________________________________
  #   Initialize Widgets for category selection                             ####

  satprod1_group <- ggroup(horizontal = TRUE, container = satprod_frame)
  cat_label      <- glabel(text      = "Category:",
                           container = satprod1_group,
                           editable  = FALSE)
  size(cat_label) <- list(width = 100)

  cat_wid <- gdroplist(
    items      = unique(mod_prod_cat$cat),
    container  = satprod1_group,
    horizontal = TRUE,
    selected   = match(sel_cat, unique(mod_prod_cat$cat)),
    handler    = function(h, ...) {
      # Identify only products of this category
      sel_prod   <- mod_prod_list[mod_prod_cat$cat == svalue(cat_wid)][1]
      prod_wid[] <- mod_prod_list[mod_prod_cat$cat == svalue(cat_wid)]
      svalue(prod_wid) <- sel_prod
      sel_prodopts <- prod_opt_list[[sel_prod]]
      # Select the last version (it assumes that versions in xml file are in
      # increasing order)

      vers_wid[]       <- names(sel_prodopts)
      svalue(vers_wid) <- sel_prodopts[[length(sel_prodopts)]]$v_number
      # Disable sensor choice for combined datasets
      if (sel_prodopts[[svalue(vers_wid)]]$combined == 1) {
        enabled(sens_wid) <- FALSE
        sens_wid[]        <- "Combined"
        svalue(sens_wid)  <- "Combined"
      } else {
        enabled(sens_wid) <- TRUE
        sens_wid[]        <- c("Terra", "Aqua", "Both")
        svalue(sens_wid)  <- general_opts$sensor
      }
      # On product change, automatically modify the default projection - latlon
      # for tiled, Sinu for nontiled
      if (sel_prodopts[[svalue(vers_wid)]]$tiled == 0) {
        enabled(tiles_group) <- FALSE
        svalue(proj_wid)     <- "Latlon MODIS"
      } else {
        enabled(tiles_group) <- TRUE
        svalue(proj_wid)     <- "Sinusoidal"
      }

      # reset dummy variables for band selection to 0 on product change
      gui_env$temp_wid_bands         <- 0
      gui_env$temp_wid_bands_indexes <- 0
      gui_env$temp_wid_bands_quality <- 0
    }
  )
  size(cat_wid) <- list(width = 335, height = 30)

  #   __________________________________________________________________________
  #   Initialize Widgets for Product selection                              ####

  addSpring(satprod1_group)
  prod_label <- glabel(text = "Product:", container = satprod1_group)

  prod_wid <- gdroplist(
    items     = mod_prod_list[mod_prod_cat$cat == svalue(cat_wid)],
    container = satprod1_group, horizontal = TRUE,
    selected  = match(sel_prod,
                      mod_prod_list[mod_prod_cat$cat == svalue(cat_wid)]),
    handler   = function(h, ...) {
      sel_prod   <- if (!is.null(svalue(prod_wid))) {
        svalue(prod_wid)
      } else {
        sel_prod
      }
      sel_prodopts <- prod_opt_list[[sel_prod]]
      # Select the last version (it assumes that versions in xml file are in
      # increasing order)
      vers_wid[]       <- names(sel_prodopts)
      svalue(vers_wid) <- sel_prodopts[[length(sel_prodopts)]]$v_number
      # Disable sensor choice for combined datasets
      if (sel_prodopts[[svalue(vers_wid)]]$combined == 1) {
        enabled(sens_wid) <- FALSE
        sens_wid[]        <- "Combined"
        svalue(sens_wid)  <- "Combined"
      } else {
        enabled(sens_wid) <- TRUE
        sens_wid[]        <- c("Terra", "Aqua", "Both")
        svalue(sens_wid)  <- general_opts$sensor
      }
      # On product change, automatically modify the default projection - latlon
      # for tiled, Sinu for nontiled
      if (sel_prodopts[[svalue(vers_wid)]]$tiled == 0) {
        enabled(tiles_group) <- FALSE
        svalue(proj_wid)     <- "Latlon MODIS"
      } else {
        enabled(tiles_group) <- TRUE
        svalue(proj_wid)     <- "Sinusoidal"
      }
      # reset dummy variables for band selection to 0 on product change
      gui_env$temp_wid_bands         <- 0
      gui_env$temp_wid_bands_indexes <- 0
      gui_env$temp_wid_bands_quality <- 0
    }
  )
  size(prod_wid)   <- list(width = 325, height = 30)
  font(prod_label) <- font(cat_label) <- list(family = "sans", weight = "bold")

  #   __________________________________________________________________________
  #   Initialize Widgets for Sensor selection                               ####

  satprod2_group <- ggroup(horizontal = TRUE, container = satprod_frame)

  sens_label       <- glabel(text = " Platform:", container = satprod2_group)
  size(sens_label) <- list(width = 100)
  sens_wid         <- gcombobox(items     = c("Terra"),
                                container = satprod2_group,
                                text      = "Select Platform", selected = 1)
  if (sel_prodopts[[general_opts$prod_version]]$combined == 1) {
    enabled(sens_wid) <- FALSE
  } else {
    sens_wid[]       <- c("Terra", "Aqua", "Both")
    svalue(sens_wid) <- general_opts$sensor
  }
  size(sens_wid) <- list(width = 150)
  addSpace(satprod2_group, 5)

  #   __________________________________________________________________________
  #   Initialize widgets for Version selection                              ####

  vers_label <- glabel(text = " Version:", container = satprod2_group)
  vers_wid   <- gcombobox(
    items     = vapply(sel_prodopts, function(x){
      x[["v_number"]]
    }, FUN.VALUE = ""),
    container = satprod2_group, text = "Select Version",
    selected  = match(general_opts$prod_version, names(sel_prodopts)),
    handler   = function(h, ...) {
      # reset dummy variables for band selection to 0 on product change
      gui_env$temp_wid_bands         <- 0
      gui_env$temp_wid_bands_indexes <- 0
      gui_env$temp_wid_bands_quality <- 0
    }
  )
  size(vers_wid) <- list(width = 100)
  addSpace(satprod2_group, 1)
  addSpring(satprod2_group)

  #   __________________________________________________________________________
  #   Initialize Widgets for Processing Layers selection                    ####

  band_label <- glabel(text = "Processing Layers:", container = satprod2_group)

  ##  ..........................................................................
  ##  Upon click on the button we create a Child widget for selection of
  ##  processing layers (could try to separate a function for this, but it would
  ##  be a hassle) and deal with all events

  band_wid   <- gbutton(
    text    = "   Click To Select   ",
    handler = function(h, ...) {

      prod_opt_list <- get(load(prodopts_file))
      general_opts  <- RJSONIO::fromJSON(previous_jsfile)
      curr_prod     <- svalue(prod_wid)
      curr_vers     <- svalue(vers_wid)
      curr_opts     <- prod_opt_list[[curr_prod]]
      # retrieve band names available for sel. product
      check_names   <- curr_opts[[curr_vers]]$band_fullnames
      # retrieve currently selected original layers
      check_wid     <- gui_env$temp_wid_bands
      selgroup      <- gbasicdialog(
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
          pos_wid       <- which(check_names %in% svalue(bands_wid))
          tmp_arr_bands <- array(data = 0, dim = length(check_names))
          tmp_arr_bands[pos_wid] <- 1
          gui_env$temp_wid_bands <- tmp_arr_bands
          # Find which indexes selected and store in
          # gui_env$temp_wid_bands_indexes
          if (length(which(check_names_indexes != "") > 0)) {
            pos_wid <- which(check_names_indexes %in% svalue(bands_wid_indexes))
            tmp_arr_ind <- array(data = 0, dim = length(check_names_indexes))
            tmp_arr_ind[pos_wid] <- 1
            gui_env$temp_wid_bands_indexes <- tmp_arr_ind
          }
          # Find which QI selected and store in gui_env$temp_wid_bands_quality
          if (length(which(check_names_quality != "") > 0)) {
            pos_wid <- which(check_names_quality %in% svalue(bands_wid_quality))
            tmp_arr_qual <- array(data = 0, dim = length(check_names_quality))
            tmp_arr_qual[pos_wid] <- 1
            gui_env$temp_wid_bands_quality <- tmp_arr_qual
          }
        }
      )

      # child widgets for original layers selection ----
      cbox_main  <- ggroup(container = selgroup, horizontal = FALSE)
      cbox_total <- ggroup(container = cbox_main, horizontal = TRUE)
      cbox       <- gframe(
        text       = glue::glue("<span foreground='red' size='large'>", 
                                "Original MODIS Layers", 
                                "</span>"),
        markup     = TRUE,
        container  = cbox_total,
        horizontal = FALSE
      )
      bands_wid  <- gcheckboxgroup(items     = check_names,
                                   checked   = as.logical(check_wid),
                                   container = cbox,
                                   use.table = FALSE)

      # child widgets for Quailty Indicators selection ----
      # retrieve quality band names (if existing for sel. product)
      check_names_quality <- curr_opts[[curr_vers]]$quality_fullnames
      if (!is.null(check_names_quality)) {
        check_wid_quality <- gui_env$temp_wid_bands_quality
        cbox_quality <- gframe(
          text       = glue::glue("<span foreground='red' size='large'>", 
                               "Quality Indicators", 
                               "</span>"),
          markup     = TRUE,
          container  = cbox_total,
          horizontal = FALSE
        )

        bands_wid_quality <- gcheckboxgroup(
          items     = check_names_quality,
          checked   = as.logical(check_wid_quality),
          container = cbox_quality,
          use.table = FALSE
        )
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
        cbox_indexes      <- gframe(
          text       = glue::glue("<span foreground='red' size='large'>",
                                  "Additional Spectral Indexes",
                                  "</span>"),
          markup     = TRUE,
          container  = cbox_total,
          horizontal = FALSE
        )
        bands_wid_indexes <- gcheckboxgroup(
          items     = check_names_indexes,
          checked   = as.logical(check_wid_indexes),
          container = cbox_indexes,
          use.table = FALSE
        )
        band_indexes_space <- glabel(text = "", container = cbox_indexes)

        ##  ....................................................................
        ##  Here we create the sub-child widget for creation of custom      ####
        ##  indexes. The `MODIStsp_addindex` function is used to spawn a modal
        ##  widget for indexes creation

        band_wid_newindex  <- gbutton(
          text    = "Add custom indexes",
          handler = function(h, ...) {
            # Run addindex() function ----
            addind <- MODIStsp_addindex(option_jsfile = previous_jsfile,
                                        prodopts_file = prodopts_file,
                                        selprod       = curr_prod,
                                        selvers       = curr_vers)

            # since upon return the widget for layers selection is automatically
            # disposed to allow addition of the index, here we check and save
            # which layers and indexes are currently selected
            general_opts  <- RJSONIO::fromJSON(previous_jsfile)
            pos_wid       <- which(check_names %in% svalue(bands_wid))
            tmp_arr_bands <- array(data = 0, dim = length(check_names))
            tmp_arr_bands[pos_wid] <- 1
            gui_env$temp_wid_bands <- tmp_arr_bands
            # store in selected indexes gui_env$temp_wid_bands_indexes array
            if (length(which(check_names_indexes != "") > 0)) {
              pos_wid <-
                which(check_names_indexes %in% svalue(bands_wid_indexes))
              tmp_arr_ind <- array(data = 0, dim = length(check_names_indexes))
              tmp_arr_ind[pos_wid] <- 1
              gui_env$temp_wid_bands_indexes <- tmp_arr_ind
            }
            # store selected QIs in gui_env$temp_wid_bands_quality array
            if (length(which(check_names_quality != "") > 0)) {
              pos_wid <-
                which(check_names_quality %in% svalue(bands_wid_quality))
              tmp_arr_qual <- array(data = 0, dim = length(check_names_quality))
              tmp_arr_qual[pos_wid] <- 1
              gui_env$temp_wid_bands_quality <- tmp_arr_qual
            }
            dispose(selgroup)
          },
          container = cbox_indexes,
          expand = FALSE
        )
      }

      # Start/Cancel buttons for layers selection child widget ----
      bands_group <- ggroup(container = cbox_main, horizontal = FALSE)

      # Widget for "www" button for layers selection child widget ----
      addSpring(bands_group)
      www_but <- gbutton(
        text = paste0("Product Details  (",
                      curr_prod,
                      " vers. ", curr_vers, ")"),
        container = bands_group,
        handler   = function(button, ...) {
          utils::browseURL(curr_opts[[curr_vers]]$www)
        }
      )
      visible(selgroup, set = TRUE)    # visualize band selection widgets
    },
    # here finishes the initialization of the layer selection widget group
    container = satprod2_group,
    expand    = FALSE
  )

  font(vers_label) <- list(family = "sans", weight = "bold")
  font(sens_label) <- font(band_label) <- list(family = "sans", weight = "bold")
  size(band_wid)   <- list(width = 270)

  #   __________________________________________________________________________
  #   Initialize Widgets for download mode selection and authentication     ####

  download_frame <- gframe(
    text       = "<span foreground='red' size='x-large'>Download Method</span>",
    markup     = TRUE,
    container  = main_group,
    horizontal = TRUE,
    expand     = TRUE
  )

  methods_group  <- ggroup(container  = download_frame,
                           horizontal = TRUE,
                           expand     = TRUE)

  method_lab <- glabel(text = " Download Server:", container = methods_group)

  server_wid <- gcombobox(
    items     = c("http", "ftp", "offline"),
    text      = "Select",
    container = methods_group,
    selected  = match(general_opts$download_server,
                      c("http", "ftp", "offline")),
    handler   = function(h, ...) {
      current_sel <- svalue(server_wid)
      if (current_sel != "http") {
        enabled(authenticate_group) <- FALSE
      } else {
        enabled(authenticate_group) <- TRUE
      }
      if (current_sel != "offline") {
        enabled(aria_wid) <- enabled(aria_help) <- TRUE
      } else {
        enabled(aria_wid) <- enabled(aria_help) <- FALSE
      }
    }
  )
  size(server_wid) <- list(width = 100)

  addSpace(methods_group, 20)
  authenticate_group <- ggroup(container = methods_group)

  user_lab <- glabel(text      = " User Name:",
                     container = authenticate_group)

  user_wid <- gedit(text      = general_opts$user,
                    container = authenticate_group,
                    width     = 15)

  addSpace(authenticate_group, 20)
  password_lab <- glabel(text      = " Password:",
                         container = authenticate_group)

  password_wid <- gedit(text      = general_opts$password,
                        container = authenticate_group,
                        width     = 15)
  visible(password_wid) <- FALSE

  if (svalue(server_wid) != "http") {
    enabled(authenticate_group) <- FALSE
  } else {
    enabled(authenticate_group) <- TRUE
  }

  addSpring(methods_group)
  aria_wid <- gcheckbox("Use 'aria2c'",
                        checked   = general_opts$use_aria,
                        container = methods_group)
  addSpace(methods_group, 2)
  aria_help <- gbutton(
    text    = " ? ",
    handler = function(h, ...) {
      help_box <- gbasicdialog(title      = "Help",
                               parent     = NULL,
                               do.buttons = FALSE,
                               horizontal = FALSE,
                               width      = 400,
                               height     = 40)

      help_mess_lab <- glabel(
        text = glue::glue(
          "If selected, use aria2c to accelerate download. \n\n",
          "It is necessary that aria2c is installed and that the binary \n",
          "executable is in the user system PATH.\n",
          "(See https://aria2.github.io)"
          ), 
        editable  = FALSE,
        container = help_box
      )

      visible(help_box) <- TRUE
    },
    container = methods_group,
    expand = FALSE
  )

  font(method_lab) <- list(family = "sans", weight = "bold")
  font(user_lab)   <- font(password_lab) <- list(family = "sans",
                                                 weight = "bold")

  #   __________________________________________________________________________
  #   Initialize Widgets for Dates selection                                ####

  dates_frame <- gframe(
    text       = glue::glue("<span foreground='red' size='x-large'>",
                            "Processing Period",
                            "</span>"),
    markup     = TRUE,
    container  = main_group,
    horizontal = TRUE,
    expand     = TRUE
  )

  dates_group <- ggroup(container  = dates_frame,
                        horizontal = TRUE,
                        expand     = TRUE)

  start_date_lab <- glabel(text      = " Starting Date (yyyy-mm-dd):  ",
                           container = dates_group)

  start_date_wid <- gedit(text      = general_opts$start_date,
                          container = dates_group,
                          width     = 15)
  addSpace(dates_group, 9)

  end_date_lab <- glabel(text      = " Ending Date (yyyy-mm-dd):  ",
                         container = dates_group)

  end_date_wid <- gedit(text      = general_opts$end_date,
                        container = dates_group,
                        width     = 15)

  font(start_date_lab) <- font(end_date_lab) <- list(family = "sans",
                                                     weight = "bold")
  addSpring(dates_group)

  seas_lab       <- glabel(text = "Period: ", container = dates_group)
  font(seas_lab) <- list(family = "sans", weight = "bold")

  seas_array  <- c("full", "seasonal")
  seas_wid    <-  gcombobox(
    items     = seas_array,
    container = dates_group,
    selected  = match(general_opts$download_range, seas_array)
  )
  size(seas_wid) <- list(width = 120)

  seas_help <- gbutton(
    text = " ? ",
    handler = function(h, ...) {
      help_box <- gbasicdialog(title      = "Help",
                               parent     = NULL,
                               do.buttons = FALSE,
                               horizontal = FALSE,
                               width      = 400,
                               height     = 40)
      help_mess_lab <- glabel(
        text = glue::glue(
          "- `Full`: all the available images between the starting and the \n",
          "ending dates will be downloaded;\n\n",
          "- `Seasonal`: only the images included in the season will be \n",
          "downloaded (e.g: if the starting date is 2005-12-01 and the \n", 
          "date is 2010-02-31, the images of December, January and February \n",
          "from 2005 to 2010 will be processed (excluding 2005-01, 2005-02 \n",
          "and 2010-12 - )."
        ),
        editable  = FALSE,
        container = help_box
      )
      visible(help_box) <- TRUE
    },
    container = dates_group,
    expand = FALSE
  )

  #   __________________________________________________________________________
  #   Initialize Widgets for Tiles selection                                ####

  spatial_frame <- gframe(
    text       = glue::glue("<span foreground='red' size='x-large'>",
                            "Spatial Extent",
                            "</span>"),
    markup     = TRUE,
    container  = main_group,
    horizontal = FALSE,
    expand     = TRUE
  )

  output_ext_group <- ggroup(container = spatial_frame, horizontal = TRUE)

  output_ext_lab   <- glabel(text      = " Output Extent:",
                             container = output_ext_group)

  font(output_ext_lab) <- list(family = "sans", weight = "bold")

  output_ext_wid <- gcombobox(
    items     = c("Full Tiles Extent", "Resized"),
    container = output_ext_group,
    selected = match(general_opts$full_ext, c("Full Tiles Extent", "Resized")),
    handler  = function(h, ...) {
      current_sel <- svalue(output_ext_wid)
      # On "Full Tiles Extent" selection, disable the bbox fields
      if (current_sel == "Full Tiles Extent") {
        enabled(bbox_group)      <- FALSE
        enabled(tiles_from_bbox) <- FALSE
        enabled(bbox_from_file)  <- FALSE
      } else {
        # On "Resized" selection, enable the bbox fields
        enabled(bbox_group)      <- TRUE
        enabled(tiles_from_bbox) <- TRUE
        enabled(bbox_from_file)  <- TRUE
      }
    }
  )
  size(output_ext_wid) <- list(width = 150)
  addSpring(output_ext_group)

  # button to retrieve tiles from bounding box ----

  if (!exists("modis_grid")) {
    modis_grid <- get(load(file.path(MODIStsp_dir,
                                     "ExtData/MODIS_Tiles.RData")))
  }  # Laod MODIS grid ancillary file
  tiles_from_bbox <- gbutton(
    text    = "Retrieve Tiles from bounding box",
    handler = function(h, ...) {
      bbox <- as.numeric(c(svalue(output_ul_east_wid),
                           svalue(output_lr_north_wid),
                           svalue(output_lr_east_wid),
                           svalue(output_ul_north_wid)))
      # Check if bbox is consistent

      n_bbox_compiled <- length(which(is.finite(bbox)))
      if (svalue(output_ext_wid) != "Full Tiles Extent" &
          n_bbox_compiled == 0) {
        gmessage("Please specify an output bounding box!", title = "Warning")
      } else if (svalue(proj_wid) == "User Defined" &
                 nchar(svalue(output_proj4_wid)) == 0) {
        gmessage("Please specify an output projection", title = "Warning")
      } else if (n_bbox_compiled < 4) {
        gmessage("Error in Selected Output extent", title = "Warning")
      } else if (bbox[1] > bbox[3] | bbox[2] > bbox[4]) {
        gmessage("Error in Selected Output extent", title = "Warning")
      } else {
        # If all checks pass, retrieve the tiles and set the widget
        update_tiles(bbox,
                     output_proj4_wid, mod_proj_str,
                     modis_grid,
                     start_x_wid, end_x_wid,
                     start_y_wid, end_y_wid)
      }
    },
    container = output_ext_group
  )

  if (svalue(output_ext_wid) != "Full Tiles Extent") {
    enabled(tiles_from_bbox) <- TRUE
  } else {
    enabled(tiles_from_bbox) <- FALSE
  }

  # Button to load extent from SHP or KML file ----

  bbox_from_file <- gbutton(
    text = "Load Extent from a spatial file",
    handler = function(h, ...) {
      # File selection widget
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
        size(wait_window) <- c(100, 8)
        addHandlerUnrealize(wait_window,
                            handler = function(h, ...) return(TRUE))
        wait_window_lab <- glabel(
          text      = paste("Retrieving Extent, please wait..."),
          editable  = FALSE,
          container = wait_window
        )
        Sys.sleep(0.05)

        # Convert bbox coordinates to output projection
        out_proj_crs <- if (svalue(proj_wid) != "User Defined") {
          out_proj_list[[svalue(proj_wid)]]
        } else {
          general_opts$user_proj4
        }

        # Create the bounding box in the chosen projection retrieving it from
        # the specified file
        bbox_out <- try(bbox_from_file(file_path = choice,
                                       out_crs   = out_proj_crs),
                        silent = TRUE)
        if (class(bbox_out) == "try-error") {
          gmessage(bbox_out, title = "Error Detected!")
        } else {
          # re-set bbox in the GUI according coordinates retrieved from file
          update_bboxlabels(bbox_out,
                            units,
                            output_ul_east_wid,
                            output_ul_north_wid,
                            output_lr_east_wid,
                            output_lr_east_wid)

          # Set tiles according with the bounding box
          update_tiles(bbox_out,
                       output_proj4_wid, mod_proj_str,
                       modis_grid,
                       start_x_wid, end_x_wid,
                       start_y_wid, end_y_wid)
        }
        message("[", date(), "]", " Retrieving Extent, please wait... DONE!")
        dispose(wait_window)
      }
    }, container = output_ext_group
  )

  if (svalue(output_ext_wid) != "Full Tiles Extent") {
    enabled(bbox_from_file) <- TRUE
  } else {
    enabled(bbox_from_file) <- FALSE
  }

  fake_group     <- ggroup(container = spatial_frame)
  fake_lab       <- glabel(text = " ", container = fake_group)
  size(fake_lab) <- list(height = 15)

  #   __________________________________________________________________________
  #   Initialize Widgets for tiles selection                                ####

  spatial_group  <- ggroup(container = spatial_frame, horizontal = TRUE)

  tiles_group   <- gframe(text       = "<b><i> Required MODIS Tiles </i></b>",
                          markup     = TRUE,
                          container  = spatial_group,
                          horizontal = FALSE,
                          expand     = FALSE,
                          pos        = 0.5)

  # horizontal ----
  x_group       <- ggroup(container = tiles_group, horizontal = TRUE,
                          spacing = 5)

  start_x_lab   <- glabel(text = " Horizontal:", container = x_group)
  start_x_start <- glabel(text = "Start", container = x_group)
  start_x_wid   <- gspinbutton(0, 35, text = "Select",
                               container   = x_group,
                               value       = general_opts$start_x)

  end_x_lab     <- glabel(text = "End", container = x_group)
  end_x_wid     <- gspinbutton(0, 35, text = "Select",
                               container   = x_group,
                               value       = general_opts$end_x)

  # show map button ----
  show_map <- gbutton(
    text    = "Show Tiles Map",
    handler = function(h, ...) {
      grDevices::dev.new(width = 8, height = 4.8, noRStudioGD = TRUE)
      raster::plot(raster::raster(file.path(MODIStsp_dir,
                                            "/ExtData/MODIS_Tiles.gif")))
    },
    container = x_group
  )

  # vertical ----
  y_group       <- ggroup(container  = tiles_group,
                          horizontal = TRUE,
                          spacing    = 5)

  start_y_lab   <- glabel(text = " Vertical:    ", container = y_group)
  start_y_start <- glabel(text = "Start", container = y_group)
  start_y_wid   <- gspinbutton(0, 17, text = "Select",
                               container   = y_group,
                               value       = general_opts$start_y)

  end_y_lab     <- glabel(text = "End", container = y_group)
  end_y_wid     <- gspinbutton(0, 17, text = "Select",
                               container   = y_group,
                               value       = general_opts$end_y)

  font(start_x_lab) <- font(start_y_lab) <- list(family = "sans",
                                                 weight = "bold")

  # If a non-tiled product is selected, grey-out the tiles selection groups
  if (prod_opt_list[[general_opts$sel_prod]]
      [[general_opts$prod_version]]$tiled == 0) {
    enabled(tiles_group) <- FALSE
  } else {
    enabled(tiles_group) <- TRUE
  }

  # Text labels showing Extent ----
  addSpace(tiles_group, 1)
  addSpring(spatial_group)
  bounding_group <- gframe(
    text       = "<b><i> Output Bounding Box (in output projection!) </i></b>",
    markup     = TRUE,
    container  = spatial_group,
    horizontal = FALSE,
    expand     = FALSE,
    pos        = 0.5
  )
  font(bounding_group) <- list(family = "sans", weight = "bold")
  addSpace(bounding_group, 1)

  # bounding box ----
  bbox_group <- ggroup(horizontal = TRUE, container = bounding_group)

  min_group  <- ggroup(horizontal = FALSE, container = bbox_group)
  lat_w_group <- ggroup(horizontal = TRUE,  container = min_group)
  addSpring(lat_w_group)

  output_ul_east_lab <- glabel("Left East. (xmin)", container = lat_w_group)
  output_ul_east_wid <- gedit(text      = general_opts$bbox[1],
                              container = lat_w_group,
                              width     = 10)
  long_s_group <- ggroup(horizontal = TRUE, container = min_group)
  addSpring(long_s_group)

  output_lr_north_lab <- glabel("Lower North. (ymin)", container = long_s_group)
  output_lr_north_wid <- gedit(text      = general_opts$bbox[2],
                               container = long_s_group,
                               width     = 10)
  addSpace(bbox_group, 10)

  max_group  <- ggroup(horizontal = FALSE, container = bbox_group)
  lat_e_group <- ggroup(horizontal = TRUE,  container = max_group)
  addSpring(lat_e_group)

  output_lr_east_lab <- glabel("Right East. (xmax)", container = lat_e_group)
  output_lr_east_wid <- gedit(text      = general_opts$bbox[3],
                              container = lat_e_group,
                              width     = 10)
  long_n_group <- ggroup(horizontal = TRUE, container = max_group)
  addSpring(long_n_group)

  output_ul_north_lab <- glabel("Upper North. (ymax)", container = long_n_group)
  output_ul_north_wid <- gedit(text      = general_opts$bbox[4],
                               container = long_n_group,
                               width     = 10)

  font(output_ul_east_lab) <- font(output_ul_north_lab) <- list(family = "sans")
  font(output_lr_east_lab) <- font(output_lr_north_lab) <- list(family = "sans")

  # disable corner labels if "Full Extent" requested ----
  if (general_opts$full_ext == "Full Tiles Extent") {
    enabled(bbox_group) <- FALSE
  } else {
    enabled(bbox_group) <- TRUE
  }

  #   __________________________________________________________________________
  #   Initialize Widgets for Projection, resolution and bbox selection      ####

  output_proj_frame <- gframe(
    text       = glue::glue("<span foreground='red' size='x-large'>",
                            "Reprojection and Resize Options",
                            "</span>"),
    markup     = T,
    container  = main_group,
    horizontal = FALSE,
    expand     = TRUE
  )

  output_proj_group <- ggroup(container = output_proj_frame, horizontal = TRUE)

  # Projection ----
  output_proj_lab       <- glabel(text      = "Output Projection:",
                                  container = output_proj_group)
  font(output_proj_lab) <- list(family = "sans", weight = "bold")
  size(output_proj_lab) <- list(width = 150)

  proj_wid <- gcombobox(
    items     = out_proj_names,
    container = output_proj_group,
    selected  = match(general_opts$proj, out_proj_names),
    handler   = function(h, ...) {
      current_sel       <- svalue(proj_wid)
      gui_env$old_proj4 <- svalue(output_proj4_wid)

      if (current_sel != "User Defined") {
        enabled(output_proj4_wid) <- FALSE
        enabled(change_proj_but)  <- FALSE
        svalue(output_proj4_wid)  <- out_proj_list[[svalue(proj_wid)]]
        sel_output_proj           <- sp::CRS(svalue(output_proj4_wid))
        # Get the units and kind of proj
        proj  <- get_proj(sel_output_proj)
        units <- get_units(sel_output_proj, proj)
        svalue(pixsize2_lab) <- units
        # If valid proj4string, and output is a bounding box, recompute bounding
        # box to output proj coordinates, then update values in the text labels
        if (svalue(output_ext_wid) != "Full Tiles Extent") {
          bbox_in  <- as.numeric(c(svalue(output_ul_east_wid),
                                   svalue(output_lr_north_wid),
                                   svalue(output_lr_east_wid),
                                   svalue(output_ul_north_wid)))
          bbox_out <- reproj_bbox(bbox_in,
                                  gui_env$old_proj4,
                                  sel_output_proj@projargs,
                                  enlarge = FALSE)
          update_bboxlabels(bbox_out,
                            units,
                            output_ul_east_wid,
                            output_ul_north_wid,
                            output_lr_east_wid,
                            output_lr_north_wid)
        }
      } else {
        # If user chooses "user defined" projection, open a GUI for inputting
        # a proj4 string
        old_sel_projwid <- hash::keys(out_proj_list)[which(
          hash::values(out_proj_list) == gui_env$old_proj4)]
        enabled(output_proj4_wid) <- FALSE
        enabled(change_proj_but)  <- TRUE
        selproj <- ginput("Please Insert a valid Proj4 string	!			",
                          parent     = NULL,
                          do.buttons = TRUE,
                          size       = 800,
                          horizontal = TRUE)

        # verify the inputted string. Revert to previous on error, or modify
        # projstring and update the bounding box by converting coordinates to
        # new out proj
        if (length(selproj) != 0 & selproj != "") {
          sel_output_proj <- try(sp::CRS(selproj), silent = TRUE)
          # On error, send out a message and reset proj_wid and proj4 wid to
          # previous values
          if (class(sel_output_proj) == "try-error") {
            gmessage(
              message = sel_output_proj,
              title   = glue::glue(
                "Proj4 String Not Recognized - Keeping the old output ",
                "projection")
            )
          } else {
            svalue(output_proj4_wid) <- sel_output_proj

            # If valid proj4string, and output is a bounding box, recompute
            # the bounding box in output proj coordinates

            bbox_in  <- as.numeric(c(svalue(output_ul_east_wid),
                                     svalue(output_lr_north_wid),
                                     svalue(output_lr_east_wid),
                                     svalue(output_ul_north_wid)))

            bbox_out <- reproj_bbox(bbox_in,
                                    gui_env$old_proj4,
                                    sel_output_proj@projargs,
                                    enlarge = FALSE)

            # Get the units and kind of proj

            proj  <- get_proj(sel_output_proj)
            units <- get_units(sel_output_proj, proj)
            svalue(pixsize2_lab) <- units
            update_bboxlabels(bbox_out,
                              units,
                              output_ul_east_wid,
                              output_ul_north_wid,
                              output_lr_east_wid,
                              output_lr_north_wid)
          }
        } else {
          # on error, reset to previous values
          svalue(output_proj4_wid) <- gui_env$old_proj4
          svalue(proj_wid)         <- old_sel_projwid
        }
      }
    }
  )

  # Text widget showing the current output proj4string ----
  outproj_user_lab       <- glabel(text      = "  PROJ4 String:",
                                   container = output_proj_group)
  font(outproj_user_lab) <- list(family = "sans", weight = "bold")

  output_proj4_wid       <- gtext(text      = general_opts$proj,
                                  container = output_proj_group,
                                  width     = 250,
                                  height    = 30,
                                  editable  = FALSE,
                                  expand    = TRUE)
  svalue(output_proj4_wid) <- out_proj_list[[svalue(proj_wid)]]

  # Button to change the user defined projection ----
  change_proj_but <- gbutton(
    text      = "Change",
    container = output_proj_group,
    handler   = function(h, ...) {
      selproj <- ginput(
        message    = "Please Insert a valid Proj4 string				",
        parent     = NULL,
        do.buttons = TRUE,
        size       = 800,
        horizontal = TRUE
      )
      if (length(selproj) != 0 & selproj != "")  {
        sel_output_proj <- try(sp::CRS(selproj), silent = TRUE)
        # Check if proj4string is valid
        if (class(sel_output_proj) == "try-error") {
          gmessage(sel_output_proj, title = "Proj4 String Not Recognized")
          svalue(output_proj4_wid) <- ""
        } else {
          svalue(output_proj4_wid) <- sel_output_proj
          proj  <- get_proj(sel_output_proj)
          units <- get_units(sel_output_proj, proj)
          svalue(pixsize2_lab) <- units
          # If valid proj4string, and output is a bounding box, recompute
          # bounding box in output proj coordinates
          if (svalue(output_ext_wid) != "Full Tiles Extent") {

            bbox_in  <- as.numeric(c(svalue(output_ul_east_wid),
                                     svalue(output_lr_north_wid),
                                     svalue(output_lr_east_wid),
                                     svalue(output_ul_north_wid)))

            bbox_out <- reproj_bbox(bbox_in,
                                    gui_env$old_proj4,
                                    sel_output_proj@projargs,
                                    enlarge = FALSE)

            update_bboxlabels(bbox_out,
                              units,
                              output_ul_east_wid,
                              output_ul_north_wid,
                              output_lr_east_wid,
                              output_lr_north_wid)
          }
        }
      }
    }
  )

  size(change_proj_but) <- c(58, 30)

  # Grey-out proj change widgets if !User Defined proj
  if (general_opts$proj == "User Defined") {
    enabled(output_proj4_wid) <- TRUE
    enabled(change_proj_but)  <- TRUE
  } else {
    enabled(change_proj_but)  <- FALSE
    enabled(output_proj4_wid) <- FALSE
  }

  output_res_group <- ggroup(container = output_proj_frame, horizontal = TRUE)
  output_res_lab   <- glabel(text      = "Output Resolution:",
                             container = output_res_group)
  font(output_res_lab) <- list(family = "sans", weight = "bold")
  size(output_res_lab) <- list(width = 150, height = 30)

  # Dropdown Native vs. Resampled resolution ----

  output_res_sel_wid  <- gcombobox(
    items     = c("Native", "Resampled"),
    container = output_res_group,
    selected  = match(general_opts$out_res_sel, c("Native", "Resampled")),
    handler   = function(h, ...) {
      current_sel <- svalue(output_res_sel_wid)
      if (current_sel == "Native") {
        enabled(output_res_wid) <- FALSE
        svalue(output_res_wid)  <- paste("native")
      } else {
        enabled(output_res_wid) <- TRUE
        svalue(output_res_wid)  <- ""
      }
    }
  )

  size(output_res_sel_wid) <- size(proj_wid) <- list(width = 120)

  # input field to define/see output resolution ----
  pixsize_lab       <- glabel(text      = "  Pixel Size:",
                              container = output_res_group)
  font(pixsize_lab) <- list(family = "sans", weight = "bold")

  output_res_wid <- gedit(text      = general_opts$out_res,
                          container = output_res_group,
                          width     = 10)

  if (svalue(output_res_sel_wid) == "Native") {
    svalue(output_res_wid)  <- paste("native")
    enabled(output_res_wid) <- FALSE
  } else {
    svalue(output_res_wid)  <- general_opts$out_res
    enabled(output_res_wid) <- TRUE
  }

  # Initial set-up of the output projection on the basis of current
  # values in widgets
  sel_output_proj <- sp::CRS(if (svalue(proj_wid) == "User Defined") {
    svalue(output_proj4_wid)
  } else {
    out_proj_list[[svalue(proj_wid)]]
  })
  proj  <- get_proj(sel_output_proj)
  units <- get_units(sel_output_proj, proj)
  pixsize2_lab <- glabel(text = units, container = output_res_group)

  # Dropdown menu to select Resampling Method ----

  addSpring(output_res_group)
  resmeth_lab       <- glabel(text      = "    Resampling Method:  ",
                              container = output_res_group)
  font(resmeth_lab) <- list(family = "sans", weight = "bold")
  size(resmeth_lab) <- list(width = 200)

  resamp_array       <- c("near", "mode")
  output_resmeth_wid <- gcombobox(items     = resamp_array,
                                  container = output_res_group,
                                  selected  = match(general_opts$resampling,
                                                    resamp_array))
  size(output_resmeth_wid) <- list(width = 120)

  #   __________________________________________________________________________
  #   Initialize Widgets for Format and reprocess options                   ####

  options_frame <- gframe(
    text       = glue::glue("<span foreground='red' size='x-large'>",
                            "Processing Options",
                            "</span>"),
    markup     = TRUE,
    container  = main_group,
    expand     = TRUE,
    horizontal = FALSE
  )

  opt_group     <- ggroup(container  = options_frame,
                          horizontal = TRUE,
                          expand     = TRUE)

  # Out format ----
  format_lab       <- glabel(text      = "Output Files Format:",
                             container = opt_group)
  font(format_lab) <- list(family = "sans", weight = "bold")
  size(format_lab) <- list(width = 180)

  format_wid <- gcombobox(
    items    = c("ENVI", "GTiff"), text = "Select", container = opt_group,
    selected = match(general_opts$out_format, c("ENVI", "GTiff")),
    handler  = function(h, ...) {
      current_sel <- svalue(format_wid)
      if (current_sel != "GTiff") {
        enabled(compress_group) <- FALSE
      } else {
        enabled(compress_group) <- TRUE
      }
    }
  )
  size(format_wid) <- c(100, 30)

  # Compression ----
  compress_group       <- ggroup(container = opt_group, horizontal = TRUE)
  compress_dict        <- c("None", "PACKBITS", "LZW", "DEFLATE")
  names(compress_dict) <- c("None", "Low (PACKBITS)", "Medium (LZW)",
                            "High (DEFLATE)")
  compress_lab         <- glabel(text      = "Compression: ",
                                 container = compress_group)
  size(compress_lab)   <- list(width = 130)
  font(compress_lab)   <- list(family = "sans", weight = "bold")

  compress_wid <- gcombobox(items     = names(compress_dict),
                            container = compress_group,
                            selected  = match(general_opts$compress,
                                              names(compress_dict)))

  # grey out compression if ENVI output
  if (general_opts$out_format == "GTiff") {
    enabled(compress_group) <- TRUE
  } else {
    enabled(compress_group) <- FALSE
  }
  size(compress_wid) <- c(150, 30)
  addSpring(opt_group)

  # Virtual raster creation menu ----
  timeseries_lab       <- glabel(text      = "Create Virtual Rasters:",
                                 container = opt_group)
  font(timeseries_lab) <- list(family = "sans", weight = "bold")

  timeseries_wid <- gcombobox(
    items     = c("None", "ENVI Meta Files", "GDAL vrt Files", "ENVI and GDAL"),
    container = opt_group,
    selected  = match(general_opts$ts_format,
                      c("None", "ENVI Meta Files", "GDAL vrt Files",
                        "ENVI and GDAL"))
  )
  size(timeseries_lab) <- c(190, 30)
  size(timeseries_wid) <- c(140, 30)

  # RasterStacks and NODATA Yes/No ----
  other_group <- ggroup(container = options_frame, horizontal = TRUE)
  rts_lab     <- glabel(text = "Create RasterStacks: ", container = other_group)
  rts_wid     <- gradio(items      = c("Yes", "No"),
                        text       = "Select",
                        container  = other_group,
                        selected   = match(general_opts$rts, c("Yes", "No")),
                        horizontal = TRUE)
  font(rts_lab) <- list(family = "sans", weight = "bold")
  addSpace(other_group, 15)

  addSpring(other_group)
  nodata_lab <- glabel(text = "Change NODATA values: ", container = other_group)
  nodata_wid <- gradio(items      = c("Yes", "No"),
                       text       = "Select",
                       container  = other_group,
                       selected   = match(general_opts$nodata_change,
                                          c("Yes", "No")),
                       horizontal = TRUE)
  font(nodata_lab) <- list(family = "sans", weight = "bold")

  nodata_help <- gbutton(
    text = " ? ",
    handler = function(h, ...) {
      help_box <- gbasicdialog(title = "Help",
                               parent     = NULL,
                               do.buttons = FALSE,
                               horizontal = FALSE,
                               width      = 400,
                               height     = 40)

      help_mess_lab <- glabel(
        text = glue::glue(
          "`No`: original MODIS nodata values are maintained; \n\n",
          "`Yes`: nodata values are replaced with default values equal \n",
          "to the maximum possible value of the data type of the \n",
          "output (e.g. 255 for unsigned 8-bit integer, 32767 for signed \n ",
          "16-bit integer)."
        ),
        editable  = FALSE,
        container = help_box
      )
      visible(help_box) <- TRUE
    },
    container = other_group,
    expand    = FALSE
  )

  # Apply scale/offset ----
  addSpace(other_group, 15)
  scale_lab <- glabel(text = "Scale output values: ", container = other_group)

  # default value in case of use of old json settings files - consider removing!
  if (with(general_opts, !exists("scale_val"))) {
    general_opts$scale_val <- "No"
  }

  scale_wid <- gradio(items      = c("Yes", "No"),
                      text       = "Select",
                      container  = other_group,
                      selected   = match(general_opts$scale_val,
                                         c("Yes", "No")),
                      horizontal = TRUE)
  font(scale_lab) <- list(family = "sans", weight = "bold")

  scale_help <- gbutton(
    text = " ? ",
    handler = function(h, ...) {
      help_box <- gbasicdialog(title = "Help",
                               parent = NULL,
                               do.buttons = FALSE,
                               horizontal = FALSE,
                               width = 400,
                               height = 40)
      help_mess_lab <- glabel(
        text = glue::glue(
          "NASA provides outputs as integer values, indicating a potential \n",
          "scale factor and/or an offset to apply in order to obtain values \n",
          "in the indicated measure units. \n\n", 
          "If `No`: output files are left as provided by NASA, and \n",
          "spectral indices are produced as integer values with a 10000 \n",
          "scale factor.\n\n",
          "If `Yes`, scale factors and offsets are applied, and \n",
          "spectral indices are computed as floating point values. \n",
          "(Notice that in this case the size of output products is \n", 
          "generally larger.)"
        ),
        editable = FALSE,
        container = help_box
      )
      visible(help_box) <- TRUE
    },
    container = other_group,
    expand    = FALSE
  )

  #   __________________________________________________________________________
  #   Initialize Widgets for output folders selection                       ####

  # HDF output folder ----

  outfoldmod_frame <- gframe(
    text      = glue::glue("<span foreground='red' size='x-large'>",
                           "Folder for storing original MODIS HDF files",
                           "</span>"),
    markup    = TRUE,
    container = main_group,
    expand    = TRUE,
    fill      = TRUE
  )

  outfoldmod_group <- ggroup(horizontal = TRUE,
                             container  = outfoldmod_frame,
                             expand     = TRUE,
                             fill       = TRUE)

  # Label for selected folder
  outfoldmod_wid   <- gedit(
    text      = format(general_opts$out_folder_mod, justify = "right"),
    container = outfoldmod_group,
    width     = 46,
    expand    = TRUE
  )
  fold_choose <- gbutton(
    text    = "Browse",
    handler = function(h, ...) {
      choice <- try(gfile(
        type = "selectdir",
        text = "Select the Output Folder for storage of original HDFs..."
      ), silent = TRUE)
      if (class(choice) != "try-error") {
        if (length(choice) != 0) {
          ## On selection, set value of the label widget
          svalue(outfoldmod_wid) <- choice
          ## On selection,  Set value of the selected variable
          general_opts$out_folder_mod <- format(choice, justify = "left")
        }
      }
    },
    container = outfoldmod_group
  )

  # HDF delete option checkbox ----
  addSpace(outfoldmod_group, 5)
  delete_lab       <- glabel(text      = "Delete original HDF files: ",
                             container = outfoldmod_group)
  font(delete_lab) <- list(family = "sans", weight = "bold")

  delete_wid       <- gradio(items      = c("Yes", "No"),
                             text       = "Select",
                             container  = outfoldmod_group,
                             selected   = 2,
                             horizontal = T)

  # Main output folder ----
  outfold_frame <- gframe(
    text = glue::glue("<span foreground='red' size='x-large'>", 
                      "Folder for storing MODIStsp processed Time Series",
                      "</span>"),
    markup    = TRUE,
    container = main_group,
    expand    = TRUE,
    fill      = TRUE
  )

  outfold_group <- ggroup(horizontal = TRUE,
                          container  = outfold_frame,
                          expand     = TRUE,
                          fill       = TRUE)

  outfold_wid   <- gedit(text      = format(general_opts$out_folder,
                                            justify = "right"),
                         container = outfold_group,
                         width     = 46,
                         expand    = TRUE)

  fold_choose   <- gbutton(
    text    = "Browse",
    handler = function(h, ...) {
      choice <- try(gfile(
        type = "selectdir", text = "Select the Output Folder for MODIS data..."
      ), silent = TRUE)
      if (class(choice) != "try-error")  {
        if (length(choice) != 0) {
          svalue(outfold_wid) <- choice
          # 	On new selection,  Set value of the selected variable
          general_opts$out_folder <- format(choice, justify = "left")
        }}
    },
    container = outfold_group
  )

  # Reprocessing options checkbox ----

  addSpace(outfold_group, 5)
  reprocess_lab       <- glabel(text = "ReProcess Existing Data: ",
                                container = outfold_group)
  font(reprocess_lab) <- list(family = "sans", weight = "bold")
  reprocess_wid       <- gradio(items      = c("Yes", "No"),
                                text       = "Select",
                                container  = outfold_group,
                                selected   = match(general_opts$reprocess,
                                                   c("Yes", "No")),
                                horizontal = TRUE)

  #   __________________________________________________________________________
  #   Initialize Start/quit/Save/Load buttons                               ####

  but_group <- ggroup(container = main_group, horizontal = TRUE)

  # If "Start" pressed, retrieve selected values and save in previous file
  start_but <- gbutton(
    text      = "Start Processing",
    container = but_group,
    handler   = function(h, ...) {
      general_opts <- prepare_to_save_options(general_opts, gui_env)
      # If check passed, save previous file and return
      if (gui_env$check_save_opts) {
        write(RJSONIO::toJSON(general_opts), previous_jsfile)
        gui_env$quit <- FALSE
        dispose(main_win)
      }
    }
  )

  # If "quit", set "quit to T and exit
  quit_but <- gbutton(
    text       = "Quit Program",
    container  = but_group,
    handler    = function(h, ...) {
      gui_env$quit <- TRUE
      dispose(main_win)
    }
  )
  addSpring(but_group)

  # On "Load", ask for a old options file and load it --------
  load_but <- gbutton(
    text      = "Load Options",
    container = but_group,
    handler   = function(h, ...){
      choice <- try(gfile(
        type    = "open",
        text    = "Select file for loading processing options...",
        initial.filename = getwd(),
        filter  = list("JSON files" = list(patterns   = c("*.json")),
                       "text files" = list(mime.types = c("text/plain")),
                       "All files"  = list(patterns   = c("*")))
      ), silent = TRUE)

      if (class(choice) != "try-error") {
        if (length(choice) == 0) {
          continue_load <- FALSE
        } else {
          if (length(grep("\\.json$", choice)) == 0) {
            continue_load <- gconfirm(
              message = glue::glue("The selected file does not seem to be a ",
                                   "JSON file. \n\n",
                                   "Do you want to continue?"),
              title = "Warning",
              icon  = "warning"
            )
          } else {
            continue_load <- TRUE
          }
        }
      } else {
        continue_load <- FALSE
      }
      if (continue_load) {
        load_options(choice)
      } else {
        gmessage(message = "Loading failed - Please select a different file!",
                 title   = "Warning")
      }
    }
  )

  # On "Save", ask for a file name and save options (must be a JSON file!) ----
  save_but <- gbutton(
    text      = "Save Options",
    container = but_group,
    handler   = function(h, ...) {

      choice <- (gfile(type = "save",
                       text = "Select file for saving processing options...",
                       "text files" = list(mime.types = c("text/plain")),
                       "All files" = list(patterns = c("*")), container = aa)
      )

      if (class(choice) == "try-error" | length(choice) == 0) {
        continue_save <- FALSE
      } else {
        if (length(grep("\\.json$", choice)) == 0) {
          continue_save <- gconfirm(
            message = glue::glue("The selected file does not seem to be a ",
                                 "JSON file. \n\n",
                                 "Do you want to continue?"),
            title = "Warning",
            icon  = "warning"
          )
        } else {
          continue_save <- TRUE
        }
      }
      if (continue_save == TRUE) {
        # add file extension if missing
        choice <- paste0(gsub("\\.json$", "", choice), ".json")
        if (choice != ".json") {
          general_opts <- prepare_to_save_options(general_opts,
                                                  gui_env,
                                                  previous_jsfile,
                                                  mod_prod_list)
          # If check passed, save previous file and return
          if (gui_env$check_save_opts) {
            write(RJSONIO::toJSON(general_opts), choice)
          }
        }
      }
      message(continue_save)
    }
  )

  ## show the selection GUI
  visible(main_win, set = TRUE)
  return(gui_env$quit)

}  # END OF MAIN FUNCTION
