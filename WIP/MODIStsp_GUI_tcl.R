#' @title build and manage the MODIStsp GUI
#' @description
#'	Function used to generate and handle the GUI used to allow selection of
#'	MODIStsp processing parameters. If the "previous options" file
#'	(MODIStsp_Previous.json) already exists, it is loaded and used to reinstate
#'	the GUI to its last state. Otherwise, the previous options file is created
#'	by launching the MODIStsp_read_xml function
#' @param general_opts data.frame containing general processing options passed
#'   by MODIStsp
#' @param prod_opt_list List of MODIS products specifications (read from
#'   MODIStsp_ProdOpts.xml file)
#' @param scroll_window logical parameter passed by MODIStsp main function.
#' @param MODIStsp_dir main folder of the package
#' @param opt_jsfile json parameters file containing data of the last
#'   execution, or the ones contained in the `options_file` eventually passed to
#'   `MODIStsp`
#' @param prodopts_file rdata file containing info about MODIS products
#' @return start - Logical - tells the main if running processing or exiting
#'  (also, Processing options are saved in "previous" file and (if
#'   "Save options" is pressed) in user's selected file)
#' @author Lorenzo Busetto, phD (2014-2015) \email{busetto.l@@irea.cnr.it}
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom hash hash keys values
#' @importFrom raster crop extent raster plot
#' @importFrom jsonlite fromJSON write_json
#' @importFrom sp CRS
#' @importFrom grDevices dev.new
#' @importFrom utils packageVersion browseURL head tail
#' @importFrom gWidgets svalue gconfirm gmessage gbasicdialog ggroup
#'  getToolkitWidget gframe gcombobox enabled size addSpring glabel
#'  gcombobox addSpace gbutton gcheckboxgroup dispose visible gradio
#'  gedit gcheckbox gfile gspinbutton ginput gtext

MODIStsp_GUI_tcl <- function(general_opts,
                         prod_opt_list,
                         MODIStsp_dir,
                         opt_jsfile,
                         prodopts_file,
                         scroll_window){

  #   __________________________________________________________________________
  #   NOTE: The function is excluded from coverage reports since it must be ####
  #   used interactively

  # nocov start

  # create a new env to facilitate values-passing between widgets + set the
  # environment of helpers to that of the function to avoid having to create
  # globals
  gui_env       <- new.env()
  gui_env$start <- FALSE
  environment(gui_load_options) <- environment()
  environment(gui_prepare_to_save_options) <- environment()

  #   __________________________________________________________________________
  #   Start Building the GUI                                                ####

  # workaround to avoid NOTE
  aria_wid <- compress_dict <- compress_wid <- delete_wid <- end_x_wid <-
    end_y_wid <- format_wid <- mod_proj_str <- modis_grid <- nodata_wid <-
    outfold_wid <- outfoldmod_wid <- output_ext_wid <- output_lr_east_wid  <-
    output_lr_north_wid <- output_proj4_wid <- output_res_sel_wid <-
    output_res_wid <- output_resmeth_wid <- output_ul_east_wid <-
    output_ul_north_wid <- password_wid <-
    prod_wid <- proj_wid <- reprocess_wid <- rts_wid <- scale_wid <- seas_wid <-
    sens_wid <- server_wid <- start_date_wid <- start_x_wid <- start_y_wid <-
    timeseries_wid <- user_wid <- vers_wid <- NULL

  main_win <- gbasicdialog(
    title      = paste0("MODIStsp - v. ", utils::packageVersion("MODIStsp")),
    parent     = NULL,
    do.buttons = FALSE
  )
  gWidgets::size(main_win) <- c(850, 750)

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

  if (scroll_window) {
    getToolkitWidget(main_win)$maximize()
  }

  mod_prod_cat <- as.data.frame(
    t(vapply(prod_opt_list, function(x){
      c(x[[1]]$cat01, x[[1]]$cat02)
    }
    , FUN.VALUE = character(2)))
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
  out_proj_list  <- list(
    "Sinusoidal"   = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs", #nolint
    "UTM 32N"      = "+init=epsg:32632 +proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", #nolint
    "Latlon WGS84" = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", #nolint
    "Latlon MODIS" = "+init=epsg:4008 +proj=longlat +ellps=clrk66 +no_defs",
    "User Defined" = ""
  )
  mod_proj_str <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs" #nolint

  #   __________________________________________________________________________
  #   Initialize Widgets for product selection and bands selection          ####

  satprod_frame <- gframe(
    text       = strwrap("MODIS Product, Platform and Layers selection"),
    markup     = TRUE,
    horizontal = FALSE,
    container  = main_group
  )
  # gWidgets::size(satprod_frame) <- c(100, 20 )
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
  addSpace(satprod1_group, 5)
  # gWidgets::size(cat_label) <- list(width = 10)

  cat_wid <- gcombobox(
    items      = unique(mod_prod_cat$cat),
    container  = satprod1_group,
    horizontal = TRUE,
    selected   = match(sel_cat, unique(mod_prod_cat$cat)),
    handler    = function(h, ...) {
      # Identify only products of this category
      sel_prod   <- mod_prod_list[mod_prod_cat$cat == gWidgets::svalue(cat_wid)][1] #nolint
      prod_wid[] <- mod_prod_list[mod_prod_cat$cat == gWidgets::svalue(cat_wid)]
      gWidgets::svalue(prod_wid) <- sel_prod
      sel_prodopts <- prod_opt_list[[sel_prod]]
      # Select the last version (it assumes that versions in xml file are in
      # increasing order)

      vers_wid[]       <- names(sel_prodopts)
      gWidgets::svalue(vers_wid) <- sel_prodopts[[length(sel_prodopts)]]$v_number #nolint
      # Disable sensor choice for combined datasets
      if (sel_prodopts[[gWidgets::svalue(vers_wid)]]$combined == 1) {
        gWidgets::enabled(sens_wid) <- FALSE
        sens_wid[]        <- "Combined"
        gWidgets::svalue(sens_wid)  <- "Combined"
      } else {
        gWidgets::enabled(sens_wid) <- TRUE
        sens_wid[]        <- c("Terra", "Aqua", "Both")
        gWidgets::svalue(sens_wid)  <- general_opts$sensor
      }
      # On product change, automatically modify the default projection - latlon
      # for tiled, Sinu for nontiled
      if (sel_prodopts[[gWidgets::svalue(vers_wid)]]$tiled == 0) {
        gWidgets::enabled(tiles_group) <- FALSE
        gWidgets::svalue(proj_wid)     <- "Latlon MODIS"
      } else {
        gWidgets::enabled(tiles_group) <- TRUE
        gWidgets::svalue(proj_wid)     <- "Sinusoidal"
      }

      # reset dummy variables for band selection to 0 on product change
      gui_env$temp_wid_bands         <- 0
      gui_env$temp_wid_bands_indexes <- 0
      gui_env$temp_wid_bands_quality <- 0
    }
  )
  size(cat_wid) <- 330
  addSpace(satprod1_group, 5)
  # gWidgets::size(cat_wid) <- list(width = 335, height = 30)

  #   __________________________________________________________________________
  #   Initialize Widgets for Product selection                              ####

  # addSpring(satprod1_group)
  prod_label <- glabel(text = "Product:", container = satprod1_group)
  addSpace(satprod1_group, 5)
  prod_wid <- gcombobox(
    items     = mod_prod_list[mod_prod_cat$cat == gWidgets::svalue(cat_wid)],
    container = satprod1_group, horizontal = TRUE,
    selected  = match(
      sel_prod,
      mod_prod_list[mod_prod_cat$cat == gWidgets::svalue(cat_wid)]),
    handler   = function(h, ...) {
      sel_prod   <- ifelse(!is.null(gWidgets::svalue(prod_wid)),
                           gWidgets::svalue(prod_wid),
                           sel_prod)

      sel_prodopts <- prod_opt_list[[sel_prod]]
      # Select the last version (it assumes that versions in xml file are in
      # increasing order)
      vers_wid <- names(sel_prodopts)
      gWidgets::svalue(vers_wid) <- sel_prodopts[[length(sel_prodopts)]]$v_number #nolint
      # Disable sensor choice for combined datasets
      if (sel_prodopts[[gWidgets::svalue(vers_wid)]]$combined == 1) {
        gWidgets::enabled(sens_wid) <- FALSE
        sens_wid <- "Combined"
        gWidgets::svalue(sens_wid)  <- "Combined"
      } else {
        gWidgets::enabled(sens_wid) <- TRUE
        sens_wid <- c("Terra", "Aqua", "Both")
        gWidgets::svalue(sens_wid)  <- general_opts$sensor
      }
      # On product change, automatically modify the default projection - latlon
      # for tiled, Sinu for nontiled
      if (sel_prodopts[[gWidgets::svalue(vers_wid)]]$tiled == 0) {
        gWidgets::enabled(tiles_group) <- FALSE
        gWidgets::svalue(proj_wid)     <- "Latlon MODIS"
      } else {
        gWidgets::enabled(tiles_group) <- TRUE
        gWidgets::svalue(proj_wid)     <- "Sinusoidal"
      }
      # reset dummy variables for band selection to 0 on product change
      gui_env$temp_wid_bands         <- 0
      gui_env$temp_wid_bands_indexes <- 0
      gui_env$temp_wid_bands_quality <- 0
    }
  )
  # gWidgets::size(prod_wid)   <- list(width = 325, height = 30)
  gWidgets::font(prod_label) <- gWidgets::font(cat_label) <- list(
    family = "serif", weight = "bold", size = 11)
  size(prod_wid) <- 330
  #   __________________________________________________________________________
  #   Initialize Widgets for Sensor selection                               ####

  satprod2_group <- ggroup(horizontal = TRUE, container = satprod_frame)

  sens_label       <- glabel(text = "Platform:", container = satprod2_group)
  addSpace(satprod2_group, 5)
  # gWidgets::size(sens_label) <- list(width = 100)
  sens_wid         <- gcombobox(items     = c("Terra"),
                                container = satprod2_group,
                                text      = "Select Platform", selected = 1,
                                size = c(200,1))
  addSpace(satprod2_group, 5)
  if (sel_prodopts[[general_opts$prod_version]]$combined == 1) {
    gWidgets::enabled(sens_wid) <- FALSE
  } else {
    sens_wid[]       <- c("Terra", "Aqua", "Both")
    gWidgets::svalue(sens_wid) <- general_opts$sensor
  }
  # gWidgets::size(sens_wid) <- list(width = 150)
  # addSpace(satprod2_group, 5)

  #   __________________________________________________________________________
  #   Initialize widgets for Version selection                              ####

  vers_label <- glabel(text = " Version:", container = satprod2_group)
  vers_wid   <- gcombobox(
    items     = vapply(sel_prodopts, function(x){
      x[["v_number"]]
    }
    , FUN.VALUE = ""),
    container = satprod2_group, text = "Select Version",
    selected  = match(general_opts$prod_version, names(sel_prodopts)),
    handler   = function(h, ...) {
      # reset dummy variables for band selection to 0 on product change
      gui_env$temp_wid_bands         <- 0
      gui_env$temp_wid_bands_indexes <- 0
      gui_env$temp_wid_bands_quality <- 0
    }
  )
  # gWidgets::size(vers_wid) <- list(width = 100)
  # addSpace(satprod2_group, 1)
  # addSpring(satprod2_group)

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
      general_opts  <- jsonlite::fromJSON(opt_jsfile)
      curr_prod     <- gWidgets::svalue(prod_wid)
      curr_vers     <- gWidgets::svalue(vers_wid)
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
        do.buttons = TRUE,
        horizontal = FALSE,
        # this handler saves the current values of selected layers, so that:
        #   - upon realizing the widget, currently selected layers are ticked;
        #   - if user cancels operation after changing something, we go back to
        #     previous selection
        handler    = function(h, ...) {
          # onfind which layers selected and store in gui_env$temp_wid_bands
          pos_wid       <- which(check_names %in% gWidgets::svalue(bands_wid))
          tmp_arr_bands <- array(data = 0, dim = length(check_names))
          tmp_arr_bands[pos_wid] <- 1
          gui_env$temp_wid_bands <- tmp_arr_bands
          # Find which indexes selected and store in
          # gui_env$temp_wid_bands_indexes
          if (length(which(check_names_indexes != "") > 0)) {
            pos_wid <- which(
              check_names_indexes %in% gWidgets::svalue(bands_wid_indexes)
            )
            tmp_arr_ind <- array(data = 0, dim = length(check_names_indexes))
            tmp_arr_ind[pos_wid] <- 1
            gui_env$temp_wid_bands_indexes <- tmp_arr_ind
          }
          # Find which QI selected and store in gui_env$temp_wid_bands_quality
          if (length(which(check_names_quality != "") > 0)) {
            pos_wid <- which(
              check_names_quality %in% gWidgets::svalue(bands_wid_quality)
            )
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
        text       = strwrap("Original MODIS Layers"),
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
          text       = strwrap("Quality Indicators"),
          markup     = TRUE,
          container  = cbox_total,
          horizontal = FALSE,
          expand = TRUE
          )

        bands_wid_quality <- gcheckboxgroup(
          items     = check_names_quality,
          checked   = as.logical(check_wid_quality),
          container = cbox_quality,
          use.table = FALSE,
          expand = TRUE
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
          text       = strwrap("Additional Spectral Indexes"),
          markup     = TRUE,
          container  = cbox_total,
          horizontal = FALSE,
          expand = TRUE
          )
        bands_wid_indexes <- gcheckboxgroup(
          items     = check_names_indexes,
          checked   = as.logical(check_wid_indexes),
          container = cbox_indexes,
          expand = TRUE
        )
        glabel(text = "", container = cbox_indexes)

        ##  .................................................................. #
        ##  Here we create the sub child widget for creation of custom      ####
        ##  indexes. The `MODIStsp_addindex` function is used to spawn a modal
        ##  widget for indexes creation

        band_wid_newindex  <- gbutton(
          text    = "Add custom indexes",
          handler = function(h, ...) {
            # Run addindex() function ----
            addind <- MODIStsp_addindex(option_jsfile = opt_jsfile,
                                        prodopts_file = prodopts_file,
                                        selprod       = curr_prod,
                                        selvers       = curr_vers)

            # since upon return the widget for layers selection is automatically
            # disposed to allow addition of the index, here we check and save
            # which layers and indexes are currently selected
            general_opts  <- jsonlite::fromJSON(opt_jsfile)
            pos_wid       <- which(check_names %in% gWidgets::svalue(bands_wid))
            tmp_arr_bands <- array(data = 0, dim = length(check_names))
            tmp_arr_bands[pos_wid] <- 1
            gui_env$temp_wid_bands <- tmp_arr_bands
            # store in selected indexes gui_env$temp_wid_bands_indexes array
            if (length(which(check_names_indexes != "") > 0)) {
              pos_wid <- which(
                check_names_indexes %in% gWidgets::svalue(bands_wid_indexes)
              )
              tmp_arr_ind <- array(data = 0, dim = length(check_names_indexes))
              tmp_arr_ind[pos_wid] <- 1
              gui_env$temp_wid_bands_indexes <- tmp_arr_ind
            }
            # store selected QIs in gui_env$temp_wid_bands_quality array
            if (length(which(check_names_quality != "") > 0)) {
              pos_wid <- which(
                check_names_quality %in% gWidgets::svalue(bands_wid_quality)
              )
              tmp_arr_qual <- array(data = 0, dim = length(check_names_quality))
              tmp_arr_qual[pos_wid] <- 1
              gui_env$temp_wid_bands_quality <- tmp_arr_qual
            }
            gWidgets::dispose(selgroup)
          },
          container = cbox_indexes,
          expand = FALSE
        )
      }

      # Start/Cancel buttons for layers selection child widget ----
      bands_group <- ggroup(container = cbox_main, horizontal = FALSE)

      # Widget for "www" button for layers selection child widget ----
      # addSpring(bands_group)
      www_but <- gbutton(
        text = paste0("Product Details  (",
                      curr_prod,
                      " vers. ", curr_vers, ")"),
        container = bands_group,
        handler   = function(button, ...) {
          utils::browseURL(curr_opts[[curr_vers]]$www)
        }
      )

      gWidgets::visible(selgroup, set = TRUE)
    },
    # here finishes the initialization of the layer selection widget group
    container = satprod2_group,
    expand    = FALSE
  )

  gWidgets::font(vers_label) <- list(family = "serif", weight = "bold",
                                     size = 11)
  gWidgets::font(sens_label) <- gWidgets::font(band_label) <-
  list(family = "serif", weight = "bold", size = 11)
  # gWidgets::size(band_wid)   <- list(width = 270)

  #   __________________________________________________________________________
  #   Initialize Widgets for download mode selection and authentication     ####

  download_frame <- gframe(
    text       = "Download Method",
    markup     = TRUE,
    container  = main_group,
    horizontal = TRUE,
    expand     = FALSE,
    size = 14
  )

  methods_group  <- ggroup(container  = download_frame,
                           horizontal = TRUE,
                           expand     = FALSE)

  method_lab <- glabel(text = " Download Server:", container = methods_group)

  server_wid <- gcombobox(
    items     = c("http", "ftp", "offline"),
    text      = "Select",
    container = methods_group,
    selected  = match(general_opts$download_server,
                      c("http", "ftp", "offline")),
    handler   = function(h, ...) {
      current_sel <- gWidgets::svalue(server_wid)
      gWidgets::enabled(authenticate_group) <- ifelse(
        current_sel != "http", FALSE, TRUE
      )
      gWidgets::enabled(aria_wid)  <- ifelse(
        current_sel != "offline", TRUE, FALSE
      )
    }
  )

  # gWidgets::size(server_wid) <- list(width = 100)

  # addSpace(methods_group, 20)
  authenticate_group <- ggroup(container = methods_group)

  user_lab <- glabel(text      = " User Name:",
                     container = authenticate_group)

  user_wid <- gedit(text      = general_opts$user,
                    container = authenticate_group,
                    width     = 15)

  # addSpace(authenticate_group, 20)
  password_lab <- glabel(text      = " Password:",
                         container = authenticate_group)

  password_wid <- gedit(text      = general_opts$password,
                        container = authenticate_group,
                        width     = 15)
  gWidgets::visible(password_wid) <- FALSE

  current_sel <- gWidgets::svalue(server_wid)
  gWidgets::enabled(authenticate_group) <- ifelse(
    current_sel != "http", FALSE, TRUE
  )

  # addSpring(methods_group)
  aria_wid <- gcheckbox("Use 'aria2c'",
                        checked   = general_opts$use_aria,
                        container = methods_group)
  # addSpace(methods_group, 2)
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
        text = strwrap(
          "Select this option to use `aria2c` to accelerate download. \n\n
          Note that `aria2c` must be installed and that the binary
          executable is in your PATH, otherwise the selector will not be
          available.\n\n (See https://aria2.github.io for installation info)",
          width = 80
        ),
        editable  = FALSE,
        container = help_box
      )

      gWidgets::visible(help_box) <- TRUE
    },
    container = methods_group,
    expand = FALSE
      )
  check_aria <- Sys.which("aria2c")
  if (!check_aria == "") {
    gWidgets::enabled(aria_wid) <- ifelse(current_sel != "offline", TRUE, FALSE)
    gWidgets::svalue(aria_wid)  <- FALSE
  } else {
    gWidgets::enabled(aria_wid)  <- FALSE
  }

  gWidgets::font(method_lab) <-list(family = "serif", weight = "bold", size = 11)
  gWidgets::font(user_lab)   <- gWidgets::font(password_lab) <-
   list(family = "serif", weight = "bold", size = 11)

  #   __________________________________________________________________________
  #   Initialize Widgets for Dates selection                                ####

  dates_frame <- gframe("ppppo",
    markup     = TRUE,
    container  = main_group,
    horizontal = TRUE,
    expand     = FALSE
    )
  gWidgets::font(dates_frame) <- list(family = "serif", weight = "bold", size = 40)

  dates_group <- ggroup(container  = dates_frame,
                        horizontal = TRUE,
                        expand     = FALSE)

  start_date_lab <- glabel(text      = " Starting Date (yyyy-mm-dd):  ",
                           container = dates_group)

  start_date_wid <- gedit(text      = general_opts$start_date,
                          container = dates_group,
                          width     = 15)
  # addSpace(dates_group, 9)

  end_date_lab <- glabel(text      = " Ending Date (yyyy-mm-dd):  ",
                         container = dates_group)

  end_date_wid <- gedit(text      = general_opts$end_date,
                        container = dates_group,
                        width     = 15)

  gWidgets::font(start_date_lab) <- gWidgets::font(end_date_lab) <-
   list(family = "serif", weight = "bold", size = 11)
  # addSpring(dates_group)

  seas_lab       <- glabel(text = "Period: ", container = dates_group)
  gWidgets::font(seas_lab) <-list(family = "serif", weight = "bold", size = 11)

  seas_array  <- c("full", "seasonal")
  seas_wid    <-  gcombobox(
    items     = seas_array,
    container = dates_group,
    selected  = match(general_opts$download_range, seas_array)
  )
  # gWidgets::size(seas_wid) <- list(width = 120)

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
        text = strwrap(
          "- `Full`: all the available images between the starting and the
          ending dates will be downloaded;\n\n
          - `Seasonal`: only the images included in the season will be
          downloaded (e.g: if the starting date is 2005-12-01 and the
          date is 2010-02-31, the images of December, January and February
          from 2005 to 2010 will be processed (excluding 2005-01, 2005-02
          and 2010-12 - ).", width = 80
          ),
        editable  = FALSE,
        container = help_box
          )
      gWidgets::visible(help_box) <- TRUE
    },
    container = dates_group,
    expand = FALSE
        )

  #   __________________________________________________________________________
  #   Initialize Widgets for Tiles selection                                ####

  spatial_frame <- gframe(
    text       = strwrap("Spatial Extent"),
    markup     = TRUE,
    container  = main_group,
    horizontal = FALSE,
    expand     = FALSE
    )

  output_ext_group <- ggroup(container = spatial_frame, horizontal = TRUE)

  output_ext_lab   <- glabel(text      = "Output Extent:",
                             container = output_ext_group)

  gWidgets::font(output_ext_lab) <- list(family = "serif", weight = "bold", size = 11)

  output_ext_wid <- gcombobox(
    items     = c("Full Tiles Extent", "Resized"),
    container = output_ext_group,
    selected = match(general_opts$full_ext, c("Full Tiles Extent", "Resized")),
    handler  = function(h, ...) {
      current_sel <- gWidgets::svalue(output_ext_wid)
      # On "Full Tiles Extent" selection, disable the bbox fields
      if (current_sel == "Full Tiles Extent") {
        gWidgets::enabled(bbox_group)      <- FALSE
        gWidgets::enabled(tiles_from_bbox) <- FALSE
        gWidgets::enabled(bbox_from_file)  <- FALSE
      } else {
        # On "Resized" selection, enable the bbox fields
        gWidgets::enabled(bbox_group)      <- TRUE
        gWidgets::enabled(tiles_from_bbox) <- TRUE
        gWidgets::enabled(bbox_from_file)  <- TRUE
      }
    }
  )
  # gWidgets::size(output_ext_wid) <- list(width = 150)
  # addSpring(output_ext_group)

  # button to retrieve tiles from bounding box ----

  if (!exists("modis_grid")) {
    modis_grid <- get(load(file.path(MODIStsp_dir,
                                     "ExtData/MODIS_Tiles.RData")))
  }  # Laod MODIS grid ancillary file
  tiles_from_bbox <- gbutton(
    text    = "Retrieve Tiles from bounding box",
    handler = function(h, ...) {
      bbox <- as.numeric(c(gWidgets::svalue(output_ul_east_wid),
                           gWidgets::svalue(output_lr_north_wid),
                           gWidgets::svalue(output_lr_east_wid),
                           gWidgets::svalue(output_ul_north_wid)))
      # Check if bbox is consistent

      n_bbox_compiled <- length(which(is.finite(bbox)))
      if (gWidgets::svalue(output_ext_wid) != "Full Tiles Extent" &
          n_bbox_compiled == 0) {
        gmessage("Please specify an output bounding box!", title = "Warning")
      } else if (gWidgets::svalue(proj_wid) == "User Defined" &
                 nchar(gWidgets::svalue(output_proj4_wid)) == 0) {
        gmessage("Please specify an output projection", title = "Warning")
      } else if (n_bbox_compiled < 4) {
        gmessage("Error in Selected Output extent", title = "Warning")
      } else if (bbox[1] > bbox[3] | bbox[2] > bbox[4]) {
        gmessage("Error in Selected Output extent", title = "Warning")
      } else {
        # If all checks pass, retrieve the tiles and set the widget
        gui_update_tiles(bbox,
                         output_proj4_wid, mod_proj_str,
                         modis_grid,
                         start_x_wid, end_x_wid,
                         start_y_wid, end_y_wid)
      }
    },
    container = output_ext_group
  )

  gWidgets::enabled(tiles_from_bbox) <- ifelse(
    gWidgets::svalue(output_ext_wid) != "Full Tiles Extent",
    TRUE,
    FALSE
  )

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
        # gWidgets::size(wait_window) <- c(100, 8)
        addHandlerUnrealize(wait_window,
                            handler = function(h, ...) return(TRUE))
        wait_window_lab <- glabel(
          text      = paste("Retrieving Extent, please wait..."),
          editable  = FALSE,
          container = wait_window
        )
        Sys.sleep(0.05)
        # Convert bbox coordinates to output projection
        out_proj_crs <- ifelse(gWidgets::svalue(proj_wid) != "User Defined",
                               out_proj_list[[gWidgets::svalue(proj_wid)]],
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
                                output_ul_east_wid,
                                output_ul_north_wid,
                                output_lr_east_wid,
                                output_lr_north_wid)

          # Set tiles according with the bounding box
          gui_update_tiles(bbox_out,
                           output_proj4_wid, mod_proj_str,
                           modis_grid,
                           start_x_wid, end_x_wid,
                           start_y_wid, end_y_wid)
        }
        message("[", date(), "]", " Retrieving Extent, please wait... DONE!")
        gWidgets::dispose(wait_window)
      }
    }
    , container = output_ext_group
  )

  # enable/disable the bbox_from_file button
  gWidgets::enabled(bbox_from_file) <- ifelse(
    gWidgets::svalue(output_ext_wid) != "Full Tiles Extent",
    TRUE,
    FALSE
  )

  # fake_group     <- ggroup(container = spatial_frame)
  # fake_lab       <- glabel(text = " ", container = fake_group)
  # gWidgets::size(fake_lab) <- list(height = 15)

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
  x_group       <- ggroup(container = tiles_group, horizontal = TRUE, spacing = 5)

  start_x_lab   <- glabel(text = " Horizontal:", container = x_group)
  start_x_start <- glabel(text = "Start", container = x_group)
  start_x_wid   <- gspinbutton(0, 35, 1, text = "Select",
                               container   = x_group,
                               value       = general_opts$start_x,
                               digits = 1, width = 30,
                               size = 30)

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
                                            "ExtData/MODIS_Tiles.gif")))
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

  gWidgets::font(start_x_lab) <- gWidgets::font(start_y_lab) <-
   list(family = "serif", weight = "bold", size = 11)
  size(start_x_wid) <- 30
  size(start_y_wid) <- 30
  size(end_x_wid) <- 30
  size(end_y_wid) <- 30
  # If a non-tiled product is selected, grey-out the tiles selection groups
  gWidgets::enabled(tiles_group) <- ifelse(
    prod_opt_list[[general_opts$sel_prod]][[general_opts$prod_version]]$tiled == 0, #nolint
    FALSE,
    TRUE)

  # Text labels showing Extent ----
  # addSpace(tiles_group, 1)
  # addSpring(spatial_group)
  bounding_group <- gframe(
    text       = "<b><i> Output Bounding Box (in output projection!) </i></b>",
    markup     = TRUE,
    container  = spatial_group,
    horizontal = FALSE,
    expand     = FALSE,
    pos        = 0.5
  )
  gWidgets::font(bounding_group) <-list(family = "serif", weight = "bold", size = 11)
  # addSpace(bounding_group, 1)

  # bounding box ----
  bbox_group <- ggroup(horizontal = TRUE, container = bounding_group)

  min_group  <- ggroup(horizontal = FALSE, container = bbox_group)
  lat_w_group <- ggroup(horizontal = TRUE,  container = min_group)
  # addSpring(lat_w_group)

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
  # addSpace(bbox_group, 10)

  max_group   <- ggroup(horizontal = FALSE, container = bbox_group)
  lat_e_group <- ggroup(horizontal = TRUE,  container = max_group)
  # addSpring(lat_e_group)

  output_lr_east_lab <- glabel("Right East. (xmax)", container = lat_e_group)
  output_lr_east_wid <- gedit(text      = general_opts$bbox[3],
                              container = lat_e_group,
                              width     = 10)
  long_n_group <- ggroup(horizontal = TRUE, container = max_group)
  # addSpring(long_n_group)

  output_ul_north_lab <- glabel("Upper North. (ymax)", container = long_n_group)
  output_ul_north_wid <- gedit(text      = general_opts$bbox[4],
                               container = long_n_group,
                               width     = 10)

  gWidgets::font(output_ul_east_lab) <- gWidgets::font(output_ul_north_lab) <-
    list(family = "normal")
  gWidgets::font(output_lr_east_lab) <- gWidgets::font(output_lr_north_lab) <-
    list(family = "normal")

  # disable corner labels if "Full Extent" requested ----
  gWidgets::enabled(bbox_group) <- ifelse(
    general_opts$full_ext == "Full Tiles Extent", FALSE, TRUE
  )

  #   __________________________________________________________________________
  #   Initialize Widgets for Projection, resolution and bbox selection      ####

  output_proj_frame <- gframe(
    text       = strwrap("Reprojection and Resize Options"),
    markup     = TRUE,
    container  = main_group,
    horizontal = FALSE,
    expand     = FALSE
    )

  output_proj_group <- ggroup(container = output_proj_frame, horizontal = TRUE)

  # Projection ----
  output_proj_lab       <- glabel(text      = "Output Projection:",
                                  container = output_proj_group)
  gWidgets::font(output_proj_lab) <-list(family = "serif", weight = "bold", size = 11)
  # gWidgets::size(output_proj_lab) <- list(width = 150)

  proj_wid <- gcombobox(
    items     = out_proj_names,
    container = output_proj_group,
    selected  = match(general_opts$proj, out_proj_names),
    handler   = function(h, ...) {
      current_sel       <- gWidgets::svalue(proj_wid)
      gui_env$old_proj4 <- gWidgets::svalue(output_proj4_wid)
      if (current_sel != "User Defined") {
        gWidgets::enabled(output_proj4_wid) <- FALSE
        gWidgets::enabled(change_proj_but) <- FALSE
        gWidgets::svalue(output_proj4_wid) <- out_proj_list[[gWidgets::svalue(proj_wid)]] #nolint
        sel_output_proj           <- sp::CRS(gWidgets::svalue(output_proj4_wid))
        # Get the units and kind of proj
        proj  <- gui_get_proj(sel_output_proj)
        units <- gui_get_units(sel_output_proj, proj)
        gWidgets::svalue(pixsize2_lab) <- units
        # If valid proj4string, and output is a bounding box, recompute bounding
        # box to output proj coordinates, then update values in the text labels
        if (gWidgets::svalue(output_ext_wid) != "Full Tiles Extent") {
          bbox_in  <- as.numeric(c(gWidgets::svalue(output_ul_east_wid),
                                   gWidgets::svalue(output_lr_north_wid),
                                   gWidgets::svalue(output_lr_east_wid),
                                   gWidgets::svalue(output_ul_north_wid)))
          bbox_out <- reproj_bbox(bbox_in,
                                  gui_env$old_proj4,
                                  sel_output_proj@projargs,
                                  enlarge = FALSE)
          gui_update_bboxlabels(bbox_out,
                                units,
                                output_ul_east_wid,
                                output_ul_north_wid,
                                output_lr_east_wid,
                                output_lr_north_wid)
        }
      } else {
        # If user chooses "user defined" projection, open a GUI for inputting
        # a proj4 string
        old_sel_projwid <- out_proj_list[gui_env$old_proj4]
        gWidgets::enabled(output_proj4_wid) <- FALSE
        gWidgets::enabled(change_proj_but)  <- TRUE
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
              title   = strwrap("Proj4 String Not Recognized - Keeping the old
                                output projection")
              )
          } else {
            gWidgets::svalue(output_proj4_wid) <- sel_output_proj

            # If valid proj4string, and output is a bounding box, recompute
            # the bounding box in output proj coordinates

            bbox_in  <- as.numeric(c(gWidgets::svalue(output_ul_east_wid),
                                     gWidgets::svalue(output_lr_north_wid),
                                     gWidgets::svalue(output_lr_east_wid),
                                     gWidgets::svalue(output_ul_north_wid)))

            bbox_out <- reproj_bbox(bbox_in,
                                    gui_env$old_proj4,
                                    sel_output_proj@projargs,
                                    enlarge = FALSE)

            # Get the units and kind of proj

            proj  <- gui_get_proj(sel_output_proj)
            units <- gui_get_units(sel_output_proj, proj)
            gWidgets::svalue(pixsize2_lab) <- units
            gui_update_bboxlabels(bbox_out,
                                  units,
                                  output_ul_east_wid,
                                  output_ul_north_wid,
                                  output_lr_east_wid,
                                  output_lr_north_wid)
          }
        } else {
          # on error, reset to previous values
          gWidgets::svalue(output_proj4_wid) <- gui_env$old_proj4
          gWidgets::svalue(proj_wid)         <- old_sel_projwid
        }
      }
    }
  )

  # Text widget showing the current output proj4string ----
  outproj_user_lab       <- glabel(text      = "  PROJ4 String:",
                                   container = output_proj_group)
  gWidgets::font(outproj_user_lab) <-list(family = "serif", weight = "bold", size = 11)

  output_proj4_wid       <- gtext(text      = general_opts$proj,
                                  container = output_proj_group,
                                  width     = 250,
                                  height    = 30,
                                  editable  = FALSE,
                                  expand    = FALSE)
  gWidgets::svalue(output_proj4_wid) <- out_proj_list[[gWidgets::svalue(proj_wid)]] #nolint

  # Button to change the user defined projection ----
  change_proj_but <- gbutton(
    text      = "Change",
    container = output_proj_group,
    handler   = function(h, ...) {
      selproj <- ginput(
        message    = "Please Insert a valid Proj4 string				",
        parent     = NULL,
        do.buttons = TRUE,
        # size       = 800,
        horizontal = TRUE
      )
      if (length(selproj) != 0 & selproj != "")  {
        sel_output_proj <- try(sp::CRS(selproj), silent = TRUE)
        # Check if proj4string is valid
        if (class(sel_output_proj) == "try-error") {
          gmessage(sel_output_proj, title = "Proj4 String Not Recognized")
          gWidgets::svalue(output_proj4_wid) <- ""
        } else {
          gWidgets::svalue(output_proj4_wid) <- sel_output_proj
          proj  <- gui_get_proj(sel_output_proj)
          units <- gui_get_units(sel_output_proj, proj)
          gWidgets::svalue(pixsize2_lab) <- units
          # If valid proj4string, and output is a bounding box, recompute
          # bounding box in output proj coordinates
          if (gWidgets::svalue(output_ext_wid) != "Full Tiles Extent") {

            bbox_in  <- as.numeric(c(gWidgets::svalue(output_ul_east_wid),
                                     gWidgets::svalue(output_lr_north_wid),
                                     gWidgets::svalue(output_lr_east_wid),
                                     gWidgets::svalue(output_ul_north_wid)))

            bbox_out <- reproj_bbox(bbox_in,
                                    gui_env$old_proj4,
                                    sel_output_proj@projargs,
                                    enlarge = FALSE)

            gui_update_bboxlabels(bbox_out,
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

  # gWidgets::size(change_proj_but) <- c(58, 30)

  # Grey-out proj change widgets if !User Defined proj
  if (general_opts$proj == "User Defined") {
    gWidgets::enabled(output_proj4_wid) <- TRUE
    gWidgets::enabled(change_proj_but)  <- TRUE
  } else {
    gWidgets::enabled(change_proj_but)  <- FALSE
    gWidgets::enabled(output_proj4_wid) <- FALSE
  }

  output_res_group <- ggroup(container = output_proj_frame, horizontal = TRUE)
  output_res_lab   <- glabel(text      = "Output Resolution:",
                             container = output_res_group)
  gWidgets::font(output_res_lab) <-list(family = "serif", weight = "bold", size = 11)
  # gWidgets::size(output_res_lab) <- list(width = 150, height = 30)

  # Dropdown Native vs. Resampled resolution ----

  output_res_sel_wid  <- gcombobox(
    items     = c("Native", "Resampled"),
    container = output_res_group,
    selected  = match(general_opts$out_res_sel, c("Native", "Resampled")),
    handler   = function(h, ...) {
      current_sel <- gWidgets::svalue(output_res_sel_wid)
      if (current_sel == "Native") {
        gWidgets::enabled(output_res_wid) <- FALSE
        gWidgets::svalue(output_res_wid)  <- paste("native")
      } else {
        gWidgets::enabled(output_res_wid) <- TRUE
        gWidgets::svalue(output_res_wid)  <- ""
      }
    }
  )

  # gWidgets::size(output_res_sel_wid) <- gWidgets::size(proj_wid) <-
  #   list(width = 120)

  # input field to define/see output resolution ----
  pixsize_lab       <- glabel(text      = "  Pixel Size:",
                              container = output_res_group)
  gWidgets::font(pixsize_lab) <-list(family = "serif", weight = "bold", size = 11)

  output_res_wid <- gedit(text      = general_opts$out_res,
                          container = output_res_group,
                          width     = 10)

  if (gWidgets::svalue(output_res_sel_wid) == "Native") {
    gWidgets::svalue(output_res_wid)  <- paste("native")
    gWidgets::enabled(output_res_wid) <- FALSE
  } else {
    gWidgets::svalue(output_res_wid)  <- general_opts$out_res
    gWidgets::enabled(output_res_wid) <- TRUE
  }

  # Initial set-up of the output projection on the basis of current
  # values in widgets
  sel_output_proj <- sp::CRS(if (gWidgets::svalue(proj_wid) == "User Defined") {
    gWidgets::svalue(output_proj4_wid)
  } else {
    out_proj_list[[gWidgets::svalue(proj_wid)]]
  })
  proj  <- gui_get_proj(sel_output_proj)
  units <- gui_get_units(sel_output_proj, proj)
  pixsize2_lab <- glabel(text = units, container = output_res_group)

  # Dropdown menu to select Resampling Method ----

  # addSpring(output_res_group)
  resmeth_lab       <- glabel(text      = "    Resampling Method:  ",
                              container = output_res_group)
  gWidgets::font(resmeth_lab) <-list(family = "serif", weight = "bold", size = 11)
  # gWidgets::size(resmeth_lab) <- list(width = 200)

  resamp_array       <- c("near", "mode")
  output_resmeth_wid <- gcombobox(items     = resamp_array,
                                  container = output_res_group,
                                  selected  = match(general_opts$resampling,
                                                    resamp_array))
  # gWidgets::size(output_resmeth_wid) <- list(width = 120)

  #   __________________________________________________________________________
  #   Initialize Widgets for Format and reprocess options                   ####

  options_frame <- gframe(
    text       = strwrap("Processing Options"),
    markup     = TRUE,
    container  = main_group,
    expand     = FALSE,
    horizontal = FALSE
    )

  opt_group     <- ggroup(container  = options_frame,
                          horizontal = TRUE,
                          expand     = FALSE)

  # Out format ----
  format_lab       <- glabel(text      = "Output Files Format:",
                             container = opt_group)
  gWidgets::font(format_lab) <-list(family = "serif", weight = "bold", size = 11)
  # gWidgets::size(format_lab) <- list(width = 180)

  format_wid <- gcombobox(
    items    = c("ENVI", "GTiff"), text = "Select", container = opt_group,
    selected = match(general_opts$out_format, c("ENVI", "GTiff")),
    handler  = function(h, ...) {
      current_sel <- gWidgets::svalue(format_wid)
      gWidgets::enabled(compress_group) <- ifelse(
        current_sel != "GTiff", FALSE, TRUE
      )
    }
  )
  # gWidgets::size(format_wid) <- c(100, 30)

  # Compression ----
  compress_group       <- ggroup(container = opt_group, horizontal = TRUE)
  compress_dict        <- c("None", "PACKBITS", "LZW", "DEFLATE")
  names(compress_dict) <- c("None", "Low (PACKBITS)", "Medium (LZW)",
                            "High (DEFLATE)")
  compress_lab         <- glabel(text      = "Compression: ",
                                 container = compress_group)
  # gWidgets::size(compress_lab)   <- list(width = 130)
  gWidgets::font(compress_lab)   <-list(family = "serif", weight = "bold", size = 11)

  compress_wid <- gcombobox(items     = names(compress_dict),
                            container = compress_group,
                            selected  = match(general_opts$compress,
                                              names(compress_dict)))

  # grey out compression if ENVI output
  gWidgets::enabled(compress_group) <- ifelse(
    current_sel != "GTiff", FALSE, TRUE
  )

  # gWidgets::size(compress_wid) <- c(150, 30)
  # addSpring(opt_group)

  # Virtual raster creation menu ----
  timeseries_lab       <- glabel(text      = "Create Virtual Rasters:",
                                 container = opt_group)
  gWidgets::font(timeseries_lab) <-list(family = "serif", weight = "bold", size = 11)

  timeseries_wid <- gcombobox(
    items     = c("None", "ENVI Meta Files", "GDAL vrt Files", "ENVI and GDAL"),
    container = opt_group,
    selected  = match(general_opts$ts_format,
                      c("None", "ENVI Meta Files", "GDAL vrt Files",
                        "ENVI and GDAL"))
  )
  # gWidgets::size(timeseries_lab) <- c(190, 30)
  # gWidgets::size(timeseries_wid) <- c(140, 30)

  # RasterStacks and NoData Yes/No ----
  other_group <- ggroup(container = options_frame, horizontal = TRUE)
  rts_lab     <- glabel(text = "Create RasterStacks: ", container = other_group)
  rts_wid     <- gradio(items      = c("Yes", "No"),
                        text       = "Select",
                        container  = other_group,
                        selected   = match(general_opts$rts, c("Yes", "No")),
                        horizontal = TRUE)
  gWidgets::font(rts_lab) <-list(family = "serif", weight = "bold", size = 11)
  # addSpace(other_group, 15)

  # addSpring(other_group)
  nodata_lab <- glabel(text = "Change NoData values: ", container = other_group)
  nodata_wid <- gradio(items      = c("Yes", "No"),
                       text       = "Select",
                       container  = other_group,
                       selected   = match(general_opts$nodata_change,
                                          c("Yes", "No")),
                       horizontal = TRUE)
  gWidgets::font(nodata_lab) <-list(family = "serif", weight = "bold", size = 11)

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
        text = strwrap(
          "`No`: original MODIS nodata values are maintained; \n\n
          `Yes`: nodata values are replaced with default values equal
          to the maximum possible value of the data type of the
          output (e.g. 255 for unsigned 8-bit integer, 32767 for signed
          16-bit integer).", width = 80
        ),
        editable  = FALSE,
        container = help_box
        )
      gWidgets::visible(help_box) <- TRUE
    },
    container = other_group,
    expand    = FALSE
  )

  # Apply scale/offset ----
  # addSpace(other_group, 15)
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
  gWidgets::font(scale_lab) <-list(family = "serif", weight = "bold", size = 11)

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
        text = strwrap(
          "NASA provides outputs as integer values, indicating a potential
          scale factor and/or an offset to apply in order to obtain values
          in the indicated measure units. \n\n
          If `No`: output files are left as provided by NASA, and
          spectral indices are produced as integer values with a 10000
          scale factor.\n\n
          If `Yes`, scale factors and offsets are applied, and
          spectral indices are computed as floating point values.
          (Notice that in this case the size of output products is
          generally larger.)", width = 80
        ),
        editable = FALSE,
        container = help_box
        )
      gWidgets::visible(help_box) <- TRUE
    },
    container = other_group,
    expand    = FALSE
      )

  #   __________________________________________________________________________
  #   Initialize Widgets for output folders selection                       ####

  # HDF output folder ----

  outfoldmod_frame <- gframe(
    text      = strwrap("Folder for storing original MODIS HDF files"),
    markup    = TRUE,
    container = main_group,
    expand    = FALSE,
    fill      = FALSE
    )

  outfoldmod_group <- ggroup(horizontal = TRUE,
                             container  = outfoldmod_frame,
                             expand     = FALSE,
                             fill       = FALSE)

  # Label for selected folder
  outfoldmod_wid   <- gedit(
    text      = format(general_opts$out_folder_mod, justify = "right"),
    container = outfoldmod_group,
    width     = 46,
    expand    = FALSE
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
          gWidgets::svalue(outfoldmod_wid) <- choice
          ## On selection,  Set value of the selected variable
          general_opts$out_folder_mod <- format(choice, justify = "left")
        }
      }
    },
    container = outfoldmod_group
  )

  # HDF delete option checkbox ----
  # addSpace(outfoldmod_group, 5)
  delete_lab       <- glabel(text      = "Delete original HDF files: ",
                             container = outfoldmod_group)
  gWidgets::font(delete_lab) <-list(family = "serif", weight = "bold", size = 11)

  delete_wid       <- gradio(items      = c("Yes", "No"),
                             text       = "Select",
                             container  = outfoldmod_group,
                             selected   = 2,
                             horizontal = TRUE)

  # Main output folder ----
  outfold_frame <- gframe(
    text = strwrap("Folder for storing MODIStsp processed Time Series"),
    markup    = TRUE,
    container = main_group,
    expand    = FALSE,
    fill      = TRUE
    )

  outfold_group <- ggroup(horizontal = TRUE,
                          container  = outfold_frame,
                          expand     = FALSE,
                          fill       = TRUE)

  outfold_wid   <- gedit(text      = format(general_opts$out_folder,
                                            justify = "right"),
                         container = outfold_group,
                         width     = 46,
                         expand    = FALSE)

  fold_choose   <- gbutton(
    text    = "Browse",
    handler = function(h, ...) {
      choice <- try(gfile(
        type = "selectdir", text = "Select the Output Folder for MODIS data..."
      ), silent = TRUE)
      if (class(choice) != "try-error")  {
        if (length(choice) != 0) {
          gWidgets::svalue(outfold_wid) <- choice
          # 	On new selection,  Set value of the selected variable
          general_opts$out_folder <- format(choice, justify = "left")
        }}
    },
    container = outfold_group
  )

  # Reprocessing options checkbox ----

  # addSpace(outfold_group, 5)
  reprocess_lab       <- glabel(text = "ReProcess Existing Data: ",
                                container = outfold_group)
  gWidgets::font(reprocess_lab) <- list(family = "serif", weight = "bold",
                                        size = 11)
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
      general_opts <- gui_prepare_to_save_options(general_opts, gui_env)
      # If check passed, save previous file and return
      if (gui_env$check_save_opts) {
        jsonlite::write_json(general_opts, opt_jsfile, pretty = TRUE,
                             auto_unbox = TRUE)
        gui_env$start <- TRUE
        gWidgets::dispose(main_win)
      }
    }
  )

  # If "quit", set "quit to T and exit
  quit_but <- gbutton(
    text       = "Quit Program",
    container  = but_group,
    handler    = function(h, ...) {
      gui_env$start <- FALSE
      gWidgets::dispose(main_win)

    }
  )
  # addSpring(but_group)

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
              message = strwrap("The selected file does not seem to be a
                                JSON file. \n\n
                                Do you want to continue?"),
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
        gui_load_options(choice)
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
                       "All files" = list(patterns = c("*")), container = "aa")
      )

      if (class(choice) == "try-error" | length(choice) == 0) {
        continue_save <- FALSE
      } else {
        if (length(grep("\\.json$", choice)) == 0) {
          continue_save <- gconfirm(
            message = strwrap("The selected file does not seem to be a
                              JSON file. \n\n
                              Do you want to continue?"),
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
          general_opts <- gui_prepare_to_save_options(general_opts,
                                                      gui_env,
                                                      opt_jsfile,
                                                      mod_prod_list)
          # If check passed, save previous file and return
          if (gui_env$check_save_opts) {
            jsonlite::write_json(general_opts, choice, pretty = TRUE,
                                 auto_unbox = TRUE)
          }
        }
      }
      message(continue_save)
    }
          )

  ## show the selection GUI
  gWidgets::visible(main_win, set = TRUE)

  return(gui_env$start)

  # nocov end

    }  # END OF MAIN FUNCTION
