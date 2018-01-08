#' @title Build and manage the MODIStsp GUI
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
#' @param opts_jsfile json parameters file containing data of the last
#'   execution, or the ones contained in the `options_file` eventually passed to
#'   `MODIStsp`
#' @param prodopts_file rdata file containing info about MODIS products
#' @return start - Logical - tells the main if running processing or exiting
#'  (also, Processing options are saved in "previous" file and (if
#'   "Save options" is pressed) in user's selected file)
#' @author Lorenzo Busetto, phD (2014-2017) \email{lbusett@@gmail.com}
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom raster crop extent raster plot
#' @importFrom jsonlite fromJSON write_json
#' @importFrom sp CRS
#' @importFrom grDevices dev.new
#' @importFrom utils packageVersion browseURL head tail
#' @importFrom gWidgets svalue gconfirm gmessage gbasicdialog ggroup
#'  getToolkitWidget gframe gdroplist enabled size addSpring glabel
#'  gcombobox addSpace gbutton gcheckboxgroup dispose visible gradio
#'  gedit gcheckbox gfile gspinbutton ginput gtext

MODIStsp_GUI <- function(general_opts,
                         prod_opt_list,
                         MODIStsp_dir,
                         opts_jsfile,
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
  environment(gui_save_options) <- environment()
  bbox_out      <- NULL
  #   __________________________________________________________________________
  #   Start Building the GUI                                                ####
  wids <- list()
  # workaround to avoid NOTE
  compress_dict <- mod_proj_str <- NULL
  
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
                        expand     = TRUE, 
                        use.scroll_window = scroll_window)
  
  #   __________________________________________________________________________
  #   Initialize Main container: holds all widgets                          ####
  
  main_group  <- ggroup(container  = main_frame2,
                        horizontal = FALSE,
                        expand     = TRUE)
  
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
    text       = strwrap("<span foreground='red' size='x-large'>
                         MODIS Product, Platform and Layers selection
                         </span>"),
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
  
  wids$cat <- gdroplist(
    items      = unique(mod_prod_cat$cat),
    container  = satprod1_group,
    horizontal = TRUE,
    selected   = match(sel_cat, unique(mod_prod_cat$cat)),
    handler    = function(h, ...) {
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
        gWidgets::svalue(wids$proj)     <- "Latlon MODIS"
      } else {
        gWidgets::enabled(tiles_group) <- TRUE
        gWidgets::svalue(wids$proj)     <- "Sinusoidal"
      }
      
      # reset dummy variables for band selection to 0 on product change
      gui_env$temp_wid_bands         <- 0
      gui_env$temp_wid_bands_indexes <- 0
      gui_env$temp_wid_bands_quality <- 0
    }
  )
  
  #   __________________________________________________________________________
  #   Initialize Widgets for Product selection                              ####
  
  addSpring(satprod1_group)
  prod_label <- glabel(text = "Product:", container = satprod1_group)
  
  wids$prod <- gdroplist(
    items     = mod_prod_list[mod_prod_cat$cat == gWidgets::svalue(wids$cat)],
    container = satprod1_group, horizontal = TRUE,
    selected  = match(
      sel_prod,
      mod_prod_list[mod_prod_cat$cat == gWidgets::svalue(wids$cat)]),
    handler   = function(h, ...) {
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
        gWidgets::svalue(wids$proj)     <- "Latlon MODIS"
      } else {
        gWidgets::enabled(tiles_group) <- TRUE
        gWidgets::svalue(wids$proj)     <- "Sinusoidal"
      }
      # reset dummy variables for band selection to 0 on product change
      gui_env$temp_wid_bands         <- 0
      gui_env$temp_wid_bands_indexes <- 0
      gui_env$temp_wid_bands_quality <- 0
    }
  )
  
  gWidgets::font(prod_label) <- gWidgets::font(cat_label) <- list(
    family = "sans", weight = "bold")
  
  #   __________________________________________________________________________
  #   Initialize Widgets for Sensor selection                               ####
  
  satprod2_group <- ggroup(horizontal = TRUE, container = satprod_frame)
  
  sens_label       <- glabel(text = " Platform:", container = satprod2_group)
  
  wids$sens         <- gcombobox(items     = c("Terra"),
                                 container = satprod2_group,
                                 text      = "Select Platform", selected = 1)
  
  if (sel_prodopts[[general_opts$prod_version]]$combined == 1) {
    gWidgets::enabled(wids$sens) <- FALSE
  } else {
    wids$sens[]       <- c("Terra", "Aqua", "Both")
    gWidgets::svalue(wids$sens) <- general_opts$sensor
  }
  
  addSpace(satprod2_group, 5)
  
  #   __________________________________________________________________________
  #   Initialize widgets for Version selection                              ####
  
  vers_label <- glabel(text = " Version:", container = satprod2_group)
  wids$vers   <- gcombobox(
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
  
  addSpace(satprod2_group, 1)
  addSpring(satprod2_group)
  
  #   __________________________________________________________________________
  #   Initialize Widgets for Processing Layers selection                    ####
  
  band_label <- glabel(text = "Processing Layers:", container = satprod2_group)
  
  ##  ..........................................................................
  ##  Upon click on the button we create a Child widget for selection of
  ##  processing layers (could try to separate a function for this, but it would
  ##  be a hassle) and deal with all events
  
  wids$band   <- gbutton(
    text    = "   Click To Select   ",
    handler = function(h, ...) {
      
      prod_opt_list <- get(load(prodopts_file))
      general_opts  <- jsonlite::fromJSON(opts_jsfile)
      curr_prod     <- gWidgets::svalue(wids$prod)
      curr_vers     <- gWidgets::svalue(wids$vers)
      curr_opts     <- prod_opt_list[[curr_prod]]
      # retrieve band names available for sel. product
      check_names   <- curr_opts[[curr_vers]]$band_fullnames
      # retrieve currently selected original layers
      wids$check     <- gui_env$temp_wid_bands
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
          wids$pos       <- which(check_names %in% gWidgets::svalue(wids$bands))
          tmp_arr_bands <- array(data = 0, dim = length(check_names))
          tmp_arr_bands[wids$pos] <- 1
          gui_env$temp_wid_bands <- tmp_arr_bands
          # Find which indexes selected and store in
          # gui_env$temp_wid_bands_indexes
          if (length(which(check_names_indexes != "") > 0)) {
            wids$pos <- which(
              check_names_indexes %in% gWidgets::svalue(wids$bands_indexes)
            )
            tmp_arr_ind <- array(data = 0, dim = length(check_names_indexes))
            tmp_arr_ind[wids$pos] <- 1
            gui_env$temp_wid_bands_indexes <- tmp_arr_ind
          }
          # Find which QI selected and store in gui_env$temp_wid_bands_quality
          if (length(which(check_names_quality != "") > 0)) {
            wids$pos <- which(
              check_names_quality %in% gWidgets::svalue(wids$bands_quality)
            )
            tmp_arr_qual <- array(data = 0, dim = length(check_names_quality))
            tmp_arr_qual[wids$pos] <- 1
            gui_env$temp_wid_bands_quality <- tmp_arr_qual
          }
        }
      )
      
      # child widgets for original layers selection ----
      cbox_main  <- ggroup(container = selgroup, horizontal = FALSE)
      cbox_total <- ggroup(container = cbox_main, horizontal = TRUE)
      cbox       <- gframe(
        text       = strwrap("<span foreground='red' size='large'>
                              Original MODIS Layers </span>"),
        markup     = TRUE,
        container  = cbox_total,
        horizontal = FALSE
      )
      wids$bands  <- gcheckboxgroup(items     = check_names,
                                    checked   = as.logical(wids$check),
                                    container = cbox,
                                    use.table = FALSE)
      
      # child widgets for Quailty Indicators selection ----
      # retrieve quality band names (if existing for sel. product)
      check_names_quality <- curr_opts[[curr_vers]]$quality_fullnames
      if (!is.null(check_names_quality)) {
        check_wid_quality <- gui_env$temp_wid_bands_quality
        cbox_quality <- gframe(
          text       = strwrap("<span foreground='red' size='large'>
                               Quality Indicators </span>"),
          markup     = TRUE,
          container  = cbox_total,
          horizontal = FALSE
        )
        
        wids$bands_quality <- gcheckboxgroup(
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
          text       = strwrap("<span foreground='red' size='large'>
                                Additional Spectral Indexes</span>"),
          markup     = TRUE,
          container  = cbox_total,
          horizontal = FALSE
        )
        wids$bands_indexes <- gcheckboxgroup(
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
        
        wids$band_newindex  <- gbutton(
          text    = "Add custom indexes",
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
        handler   = function(h, ...) {
          utils::browseURL(curr_opts[[curr_vers]]$www)
        }
      )
      
      gWidgets::visible(selgroup, set = TRUE)
    },
    # here finishes the initialization of the layer selection widget group
    container = satprod2_group,
    expand    = FALSE
  )
  
  gWidgets::font(vers_label) <- list(family = "sans", weight = "bold")
  gWidgets::font(sens_label) <- gWidgets::font(band_label) <- list(
    family = "sans", weight = "bold")
  
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
  
  wids$server <- gcombobox(
    items     = c("http", "ftp", "offline"),
    text      = "Select",
    container = methods_group,
    selected  = match(general_opts$download_server,
                      c("http", "ftp", "offline")),
    handler   = function(h, ...) {
      current_sel <- gWidgets::svalue(wids$server)
      gWidgets::enabled(authenticate_group) <- ifelse(
        current_sel != "http", FALSE, TRUE
      )
      gWidgets::enabled(wids$aria)  <- ifelse(
        current_sel != "offline", TRUE, FALSE
      )
    }
  )
  
  addSpace(methods_group, 20)
  authenticate_group <- ggroup(container = methods_group)
  
  user_lab <- glabel(text      = " User Name:",
                     container = authenticate_group)
  
  wids$user <- gedit(text      = general_opts$user,
                     container = authenticate_group,
                     width     = 15)
  
  addSpace(authenticate_group, 20)
  password_lab <- glabel(text      = " Password:",
                         container = authenticate_group)
  
  wids$password <- gedit(text      = general_opts$password,
                         container = authenticate_group,
                         width     = 15)
  gWidgets::visible(wids$password) <- FALSE
  
  current_sel <- gWidgets::svalue(wids$server)
  gWidgets::enabled(authenticate_group) <- ifelse(
    current_sel != "http", FALSE, TRUE
  )
  
  addSpring(methods_group)
  wids$aria <- gcheckbox("Use 'aria2c'",
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
    gWidgets::enabled(wids$aria) <- ifelse(current_sel != "offline",
                                           TRUE,
                                           FALSE)
    gWidgets::svalue(wids$aria)  <- FALSE
  } else {
    gWidgets::enabled(wids$aria)  <- FALSE
  }
  
  gWidgets::font(method_lab) <- list(family = "sans", weight = "bold")
  gWidgets::font(user_lab)   <- gWidgets::font(password_lab) <-
    list(family = "sans", weight = "bold")
  
  #   __________________________________________________________________________
  #   Initialize Widgets for Dates selection                                ####
  
  dates_frame <- gframe(
    text       = strwrap("<span foreground='red' size='x-large'>
                         Processing Period </span>"),
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
  
  wids$start_date <- gedit(text      = general_opts$start_date,
                           container = dates_group,
                           width     = 15)
  addSpace(dates_group, 9)
  
  end_date_lab <- glabel(text      = " Ending Date (yyyy-mm-dd):  ",
                         container = dates_group)
  
  wids$end_date <- gedit(text      = general_opts$end_date,
                         container = dates_group,
                         width     = 15)
  
  gWidgets::font(start_date_lab) <- gWidgets::font(end_date_lab) <-
    list(family = "sans", weight = "bold")
  addSpring(dates_group)
  
  seas_lab       <- glabel(text = "Period: ", container = dates_group)
  gWidgets::font(seas_lab) <- list(family = "sans", weight = "bold")
  
  seas_array  <- c("full", "seasonal")
  wids$seas    <-  gcombobox(
    items     = seas_array,
    container = dates_group,
    selected  = match(general_opts$download_range, seas_array)
  )
  
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
    text       = strwrap("<span foreground='red' size='x-large'>
                          Spatial Extent </span>"),
    markup     = TRUE,
    container  = main_group,
    horizontal = FALSE,
    expand     = TRUE
  )
  
  output_ext_group <- ggroup(container = spatial_frame, horizontal = TRUE)
  
  output_ext_lab   <- glabel(text      = " Output Extent:",
                             container = output_ext_group)
  
  gWidgets::font(output_ext_lab) <- list(family = "sans", weight = "bold")
  wids$output_ext <- gdroplist(
    items     = c("Full Tiles Extent", "Resized"),
    container = output_ext_group,
    selected = match(general_opts$full_ext, c("Full Tiles Extent", "Resized")),
    handler  = function(h, ...) {
      current_sel <- gWidgets::svalue(wids$output_ext)
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
  
  addSpring(output_ext_group)
  
  # button to retrieve tiles from bounding box ----
  
  if (!exists("modis_grid")) {
    modis_grid <- get(load(file.path(MODIStsp_dir,
                                     "ExtData/MODIS_Tiles.RData")))
  }  # Laod MODIS grid ancillary file
  tiles_from_bbox <- gbutton(
    text    = "Retrieve Tiles from bounding box",
    handler = function(h, ...) {
      bbox <- as.numeric(c(gWidgets::svalue(wids$output_ul_east),
                           gWidgets::svalue(wids$output_lr_north),
                           gWidgets::svalue(wids$output_lr_east),
                           gWidgets::svalue(wids$output_ul_north)))
      # Check if bbox is consistent
      
      n_bbox_compiled <- length(which(is.finite(bbox)))
      if (gWidgets::svalue(wids$output_ext) != "Full Tiles Extent" &
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
        gui_update_tiles(bbox_out,
                         mod_proj_str,
                         modis_grid,
                         wids)
      }
    },
    container = output_ext_group
  )
  
  gWidgets::enabled(tiles_from_bbox) <- ifelse(
    gWidgets::svalue(wids$output_ext) != "Full Tiles Extent",
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
    , container = output_ext_group
  )
  
  # enable/disable the bbox_from_file button
  gWidgets::enabled(bbox_from_file) <- ifelse(
    gWidgets::svalue(wids$output_ext) != "Full Tiles Extent",
    TRUE,
    FALSE
  )
  
  fake_group     <- ggroup(container = spatial_frame)
  fake_lab       <- glabel(text = " ", container = fake_group)
  
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
  wids$start_x   <- gspinbutton(0, 35, text = "Select",
                                container   = x_group,
                                value       = general_opts$start_x)
  
  end_x_lab     <- glabel(text = "End", container = x_group)
  wids$end_x     <- gspinbutton(0, 35, text = "Select",
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
  wids$start_y   <- gspinbutton(0, 17, text = "Select",
                                container   = y_group,
                                value       = general_opts$start_y)
  
  end_y_lab     <- glabel(text = "End", container = y_group)
  wids$end_y     <- gspinbutton(0, 17, text = "Select",
                                container   = y_group,
                                value       = general_opts$end_y)
  
  gWidgets::font(start_x_lab) <- gWidgets::font(start_y_lab) <-
    list(family = "sans", weight = "bold")
  
  # If a non-tiled product is selected, grey-out the tiles selection groups
  gWidgets::enabled(tiles_group) <- ifelse(
    prod_opt_list[[general_opts$sel_prod]][[general_opts$prod_version]]$tiled == 0, #nolint
    FALSE,
    TRUE
  )
  
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
  gWidgets::font(bounding_group) <- list(family = "sans", weight = "bold")
  addSpace(bounding_group, 1)
  
  # bounding box ----
  bbox_group <- ggroup(horizontal = TRUE, container = bounding_group)
  
  min_group  <- ggroup(horizontal = FALSE, container = bbox_group)
  lat_w_group <- ggroup(horizontal = TRUE,  container = min_group)
  addSpring(lat_w_group)
  
  output_ul_east_lab <- glabel("Left East. (xmin)", container = lat_w_group)
  wids$output_ul_east <- gedit(text      = general_opts$bbox[1],
                               container = lat_w_group,
                               width     = 10)
  long_s_group <- ggroup(horizontal = TRUE, container = min_group)
  addSpring(long_s_group)
  
  output_lr_north_lab <- glabel("Lower North. (ymin)", container = long_s_group)
  wids$output_lr_north <- gedit(text      = general_opts$bbox[2],
                                container = long_s_group,
                                width     = 10)
  addSpace(bbox_group, 10)
  
  max_group   <- ggroup(horizontal = FALSE, container = bbox_group)
  lat_e_group <- ggroup(horizontal = TRUE,  container = max_group)
  addSpring(lat_e_group)
  
  output_lr_east_lab <- glabel("Right East. (xmax)", container = lat_e_group)
  wids$output_lr_east <- gedit(text      = general_opts$bbox[3],
                               container = lat_e_group,
                               width     = 10)
  long_n_group <- ggroup(horizontal = TRUE, container = max_group)
  addSpring(long_n_group)
  
  output_ul_north_lab <- glabel("Upper North. (ymax)", container = long_n_group)
  wids$output_ul_north <- gedit(text      = general_opts$bbox[4],
                                container = long_n_group,
                                width     = 10)
  
  gWidgets::font(output_ul_east_lab) <- gWidgets::font(output_ul_north_lab) <-
    list(family = "sans")
  gWidgets::font(output_lr_east_lab) <- gWidgets::font(output_lr_north_lab) <-
    list(family = "sans")
  
  # disable corner labels if "Full Extent" requested ----
  gWidgets::enabled(bbox_group) <- ifelse(
    general_opts$full_ext == "Full Tiles Extent", FALSE, TRUE
  )
  
  #   __________________________________________________________________________
  #   Initialize Widgets for Projection, resolution and bbox selection      ####
  
  output_proj_frame <- gframe(
    text       = strwrap("<span foreground='red' size='x-large'>
                         Reprojection and Resize Options </span>"),
    markup     = TRUE,
    container  = main_group,
    horizontal = FALSE,
    expand     = TRUE
  )
  
  output_proj_group <- ggroup(container = output_proj_frame, horizontal = TRUE)
  
  # Projection ----
  output_proj_lab       <- glabel(text      = "Output Projection:",
                                  container = output_proj_group)
  gWidgets::font(output_proj_lab) <- list(family = "sans", weight = "bold")

  wids$proj <- gcombobox(
    items     = out_proj_names,
    container = output_proj_group,
    selected  = match(general_opts$proj, out_proj_names),
    handler   = function(h, ...) {
      current_sel       <- gWidgets::svalue(wids$proj)
      gui_env$old_proj4 <- gWidgets::svalue(wids$output_proj4)
      if (current_sel != "User Defined") {
        gWidgets::enabled(wids$output_proj4) <- FALSE
        gWidgets::enabled(change_proj_but)   <- FALSE
        gWidgets::svalue(wids$output_proj4)  <- out_proj_list[[gWidgets::svalue(wids$proj)]] #nolint
        sel_output_proj <- sp::CRS(gWidgets::svalue(wids$output_proj4))
        # Get the units and kind of proj
        proj  <- gui_get_proj(sel_output_proj)
        units <- gui_get_units(sel_output_proj, proj)
        gWidgets::svalue(pixsize2_lab) <- units
        # If valid proj4string, and output is a bounding box, recompute bounding
        # box to output proj coordinates, then update values in the text labels
        if (gWidgets::svalue(wids$output_ext) != "Full Tiles Extent") {
          bbox_in  <- as.numeric(c(gWidgets::svalue(wids$output_ul_east),
                                   gWidgets::svalue(wids$output_lr_north),
                                   gWidgets::svalue(wids$output_lr_east),
                                   gWidgets::svalue(wids$output_ul_north)))
          bbox_out <- reproj_bbox(bbox_in,
                                  gui_env$old_proj4,
                                  sel_output_proj@projargs,
                                  enlarge = FALSE)
          gui_update_bboxlabels(bbox_out,
                                units,
                                wids)
        }
      } else {
        # If user chooses "user defined" projection, open a GUI for inputting
        # a proj4 string
        old_sel_projwid <- out_proj_list[gui_env$old_proj4]
        gWidgets::enabled(wids$output_proj4) <- FALSE
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
          # On error, send out a message and reset wids$proj and proj4 wid to
          # previous values
          if (class(sel_output_proj) == "try-error") {
            gmessage(
              message = sel_output_proj,
              title   = strwrap("Proj4 String Not Recognized - Keeping the old
                                output projection")
            )
          } else {
            gWidgets::svalue(wids$output_proj4) <- sel_output_proj
            
            # If valid proj4string, and output is a bounding box, recompute
            # the bounding box in output proj coordinates
            
            bbox_in  <- as.numeric(c(gWidgets::svalue(wids$output_ul_east),
                                     gWidgets::svalue(wids$output_lr_north),
                                     gWidgets::svalue(wids$output_lr_east),
                                     gWidgets::svalue(wids$output_ul_north)))
            
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
                                  wids)
          }
        } else {
          # on error, reset to previous values
          gWidgets::svalue(wids$output_proj4) <- gui_env$old_proj4
          gWidgets::svalue(wids$proj)         <- old_sel_projwid
        }
      }
    }
  )
  
  # Text widget showing the current output proj4string ----
  outproj_user_lab       <- glabel(text      = "  PROJ4 String:",
                                   container = output_proj_group)
  gWidgets::font(outproj_user_lab) <- list(family = "sans", weight = "bold")
  
  wids$output_proj4       <- gtext(text      = general_opts$proj,
                                   container = output_proj_group,
                                   width     = 250,
                                   height    = 30,
                                   editable  = FALSE,
                                   expand    = TRUE)
  
  gWidgets::svalue(wids$output_proj4) <- out_proj_list[[gWidgets::svalue(wids$proj)]] #nolint
  
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
          gWidgets::svalue(wids$output_proj4) <- ""
        } else {
          gWidgets::svalue(wids$output_proj4) <- sel_output_proj
          proj  <- gui_get_proj(sel_output_proj)
          units <- gui_get_units(sel_output_proj, proj)
          gWidgets::svalue(pixsize2_lab) <- units
          # If valid proj4string, and output is a bounding box, recompute
          # bounding box in output proj coordinates
          if (gWidgets::svalue(wids$output_ext) != "Full Tiles Extent") {
            
            bbox_in  <- as.numeric(c(gWidgets::svalue(wids$output_ul_east),
                                     gWidgets::svalue(wids$output_lr_north),
                                     gWidgets::svalue(wids$output_lr_east),
                                     gWidgets::svalue(wids$output_ul_north)))
            
            bbox_out <- reproj_bbox(bbox_in,
                                    gui_env$old_proj4,
                                    sel_output_proj@projargs,
                                    enlarge = FALSE)
            
            gui_update_bboxlabels(bbox_out,
                                  units,
                                  wids)
          }
        }
      }
    }
  )
  
  gWidgets::size(change_proj_but) <- c(58, 30)
  
  # Grey-out proj change widgets if !User Defined proj
  if (general_opts$proj == "User Defined") {
    gWidgets::enabled(wids$output_proj4) <- TRUE
    gWidgets::enabled(change_proj_but)  <- TRUE
  } else {
    gWidgets::enabled(change_proj_but)  <- FALSE
    gWidgets::enabled(wids$output_proj4) <- FALSE
  }
  
  output_res_group <- ggroup(container = output_proj_frame, horizontal = TRUE)
  output_res_lab   <- glabel(text      = "Output Resolution:",
                             container = output_res_group)
  gWidgets::font(output_res_lab) <- list(family = "sans", weight = "bold")

  # Dropdown Native vs. Resampled resolution ----
  
  wids$output_res_sel  <- gcombobox(
    items     = c("Native", "Resampled"),
    container = output_res_group,
    selected  = match(general_opts$out_res_sel, c("Native", "Resampled")),
    handler   = function(h, ...) {
      current_sel <- gWidgets::svalue(wids$output_res_sel)
      if (current_sel == "Native") {
        gWidgets::enabled(wids$output_res) <- FALSE
        gWidgets::svalue(wids$output_res)  <- paste("native")
      } else {
        gWidgets::enabled(wids$output_res) <- TRUE
        gWidgets::svalue(wids$output_res)  <- ""
      }
    }
  )

  # input field to define/see output resolution ----
  pixsize_lab       <- glabel(text      = "  Pixel Size:",
                              container = output_res_group)
  gWidgets::font(pixsize_lab) <- list(family = "sans", weight = "bold")
  
  wids$output_res <- gedit(text      = general_opts$out_res,
                           container = output_res_group,
                           width     = 10)
  
  if (gWidgets::svalue(wids$output_res_sel) == "Native") {
    gWidgets::svalue(wids$output_res)  <- paste("native")
    gWidgets::enabled(wids$output_res) <- FALSE
  } else {
    gWidgets::svalue(wids$output_res)  <- general_opts$out_res
    gWidgets::enabled(wids$output_res) <- TRUE
  }
  
  # Initial set-up of the output projection on the basis of current
  # values in widgets
  sel_output_proj <- sp::CRS(
    if (gWidgets::svalue(wids$proj) == "User Defined") {
      gWidgets::svalue(wids$output_proj4)
    } else {
      out_proj_list[[gWidgets::svalue(wids$proj)]]
    }
  )
  proj  <- gui_get_proj(sel_output_proj)
  units <- gui_get_units(sel_output_proj, proj)
  pixsize2_lab <- glabel(text = units, container = output_res_group)
  
  # Dropdown menu to select Resampling Method ----
  
  addSpring(output_res_group)
  resmeth_lab       <- glabel(text      = "    Resampling Method:  ",
                              container = output_res_group)
  gWidgets::font(resmeth_lab) <- list(family = "sans", weight = "bold")
  # gWidgets::size(resmeth_lab) <- list(width = 200)
  
  resamp_array       <- c("near", "mode")
  wids$output_resmeth <- gcombobox(items     = resamp_array,
                                   container = output_res_group,
                                   selected  = match(general_opts$resampling,
                                                     resamp_array))
  
  #   __________________________________________________________________________
  #   Initialize Widgets for Format and reprocess options                   ####
  
  options_frame <- gframe(
    text       = strwrap("<span foreground='red' size='x-large'>
                         Processing Options </span>"),
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
  gWidgets::font(format_lab) <- list(family = "sans", weight = "bold")
  
  wids$format <- gcombobox(
    items    = c("ENVI", "GTiff"), text = "Select", container = opt_group,
    selected = match(general_opts$out_format, c("ENVI", "GTiff")),
    handler  = function(h, ...) {
      current_sel <- gWidgets::svalue(wids$format)
      gWidgets::enabled(compress_group) <- ifelse(
        current_sel != "GTiff", FALSE, TRUE
      )
    }
  )
  gWidgets::size(wids$format) <- c(100, 30)
  
  # Compression ----
  compress_group       <- ggroup(container = opt_group, horizontal = TRUE)
  compress_dict        <- c("None", "PACKBITS", "LZW", "DEFLATE")
  names(compress_dict) <- c("None", "Low (PACKBITS)", "Medium (LZW)",
                            "High (DEFLATE)")
  compress_lab         <- glabel(text      = "Compression: ",
                                 container = compress_group)
  gWidgets::font(compress_lab)  <- list(family = "sans", weight = "bold")
  
  wids$compress <- gcombobox(items     = names(compress_dict),
                             container = compress_group,
                             selected  = match(general_opts$compress,
                                               names(compress_dict)))
  
  # grey out compression if ENVI output
  gWidgets::enabled(compress_group) <- ifelse(
    current_sel != "GTiff", FALSE, TRUE
  )
  
  gWidgets::size(wids$compress) <- c(150, 30)
  addSpring(opt_group)
  
  # Virtual raster creation menu ----
  timeseries_lab       <- glabel(text      = "Create Virtual Rasters:",
                                 container = opt_group)
  gWidgets::font(timeseries_lab) <- list(family = "sans", weight = "bold")
  
  wids$timeseries <- gcombobox(
    items     = c("None", "ENVI Meta Files", "GDAL vrt Files", "ENVI and GDAL"),
    container = opt_group,
    selected  = match(general_opts$ts_format,
                      c("None", "ENVI Meta Files", "GDAL vrt Files",
                        "ENVI and GDAL"))
  )
  
  # RasterStacks and NoData Yes/No ----
  other_group <- ggroup(container = options_frame, horizontal = TRUE)
  rts_lab     <- glabel(text = "Create RasterStacks: ", container = other_group)
  wids$rts     <- gradio(items      = c("Yes", "No"),
                         text       = "Select",
                         container  = other_group,
                         selected   = match(general_opts$rts, c("Yes", "No")),
                         horizontal = TRUE)
  gWidgets::font(rts_lab) <- list(family = "sans", weight = "bold")
  addSpace(other_group, 15)
  
  addSpring(other_group)
  nodata_lab <- glabel(text = "Change NoData values: ", container = other_group)
  wids$nodata <- gradio(items      = c("Yes", "No"),
                        text       = "Select",
                        container  = other_group,
                        selected   = match(general_opts$nodata_change,
                                           c("Yes", "No")),
                        horizontal = TRUE)
  gWidgets::font(nodata_lab) <- list(family = "sans", weight = "bold")
  
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
  addSpace(other_group, 15)
  scale_lab <- glabel(text = "Scale output values: ", container = other_group)
  
  # default value in case of use of old json settings files - consider removing!
  if (!exists(general_opts$scale_val)) {
    general_opts$scale_val <- "No"
  }
  
  wids$scale <- gradio(items      = c("Yes", "No"),
                       text       = "Select",
                       container  = other_group,
                       selected   = match(general_opts$scale_val,
                                          c("Yes", "No")),
                       horizontal = TRUE)
  gWidgets::font(scale_lab) <- list(family = "sans", weight = "bold")
  
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
    text      = strwrap("<span foreground='red' size='x-large'>
                        Folder for storing original MODIS HDF files </span>"),
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
  wids$outfoldmod   <- gedit(
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
          gWidgets::svalue(wids$outfoldmod) <- choice
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
  gWidgets::font(delete_lab) <- list(family = "sans", weight = "bold")
  
  wids$delete       <- gradio(items      = c("Yes", "No"),
                              text       = "Select",
                              container  = outfoldmod_group,
                              selected   = 2,
                              horizontal = TRUE)
  
  # Main output folder ----
  outfold_frame <- gframe(
    text = strwrap("<span foreground='red' size='x-large'>
                   Folder for storing MODIStsp processed Time Series </span>"),
    markup    = TRUE,
    container = main_group,
    expand    = TRUE,
    fill      = TRUE
  )
  
  outfold_group <- ggroup(horizontal = TRUE,
                          container  = outfold_frame,
                          expand     = TRUE,
                          fill       = TRUE)
  
  wids$outfold   <- gedit(text      = format(general_opts$out_folder,
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
          gWidgets::svalue(wids$outfold) <- choice
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
  gWidgets::font(reprocess_lab) <- list(family = "sans", weight = "bold")
  wids$reprocess       <- gradio(items      = c("Yes", "No"),
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
      general_opts <- gui_save_options(general_opts,
                                       gui_env,
                                       opts_jsfile,
                                       mod_prod_list,
                                       mod_proj_str,
                                       modis_grid,
                                       prod_opt_list,
                                       compress_dict,
                                       wids)
      
      # If check passed, save previous file and return
      if (gui_env$check_save_opts) {
        
        gui_env$start <- TRUE
        dispose(main_win)
        return(gui_env$start)
      }
    }
  )
  
  # If "quit", set "start" to FALSE and exit
  quit_but <- gbutton(
    text       = "Quit Program",
    container  = but_group,
    handler    = function(h, ...) {
      gui_env$start <- FALSE
      dispose(main_win)
      return(gui_env$start)
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
        gui_load_options(choice, wids, prod_opt_list)
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
          general_opts <- gui_save_options(general_opts,
                                           gui_env,
                                           opts_jsfile = choice,
                                           mod_prod_list,
                                           mod_proj_str,
                                           modis_grid,
                                           prod_opt_list,
                                           compress_dict,
                                           wids)
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
