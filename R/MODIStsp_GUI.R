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
#' @importFrom data.table rbindlist
#' @importFrom utils packageVersion browseURL head tail
#' @importFrom gWidgets svalue gconfirm gmessage gbasicdialog ggroup
#'  getToolkitWidget gframe gdroplist enabled size addSpring glabel
#'  gcombobox addSpace gbutton gcheckboxgroup dispose visible gradio
#'  gedit gcheckbox gfile gspinbutton ginput gtext
#' @import gWidgetsRGtk2

MODIStsp_GUI <- function(general_opts,
                         prod_opt_list,
                         MODIStsp_dir,
                         opts_jsfile,
                         prodopts_file,
                         scroll_window){
  #nocov start
  #   __________________________________________________________________________
  #   NOTE: The function is excluded from coverage reports since it must be ####
  #   used interactively. The same goes for all helper function and event
  #   handlers!

  compress_dict <- mod_proj_str <- NULL
  bbox_out  <- NULL
  help_messages <- create_help_messages()

  # create a new env to facilitate values-passing between widgets + set the
  # environment of helpers to that of the function to avoid having to create
  # globals TODO consider remeoving and passing everything explicitly!

  gui_env       <- new.env()
  gui_env$start <- FALSE
  environment(gui_load_options) <- environment()
  environment(gui_save_options) <- environment()
  environment(gh_sellayers)        <- environment()
  environment(gh_load_extent)   <- environment()
  environment(gh_selectmap)     <- environment()
  environment(gh_view_extent)   <- environment()
  environment(gh_selcat)        <- environment()
  environment(gh_selprod)       <- environment()
  environment(gh_changeproj)    <- environment()
  environment(gh_tiles_from_bbox) <- environment()

  #   __________________________________________________________________________
  #   Start Building the GUI                                                ####
  wids <- list()
  # workaround to avoid NOTE


  main_win <- gbasicdialog(
    title      = paste0("MODIStsp - v. ", utils::packageVersion("MODIStsp")),
    parent     = NULL,
    do.buttons = FALSE)
  gWidgets::size(main_win) <- list(width = 900)
  # frame1 and 2 with expand=FALSE grant that widgets are not "too much
  # expanded", neither horizontally nor vertically

  main_frame1 <- ggroup(container  = main_win,
                        horizontal = TRUE,
                        expand     = TRUE,
                        use.scrollwindow = scroll_window)

  main_frame2 <- ggroup(container  = main_frame1,
                        horizontal = FALSE,
                        expand     = TRUE,
                        use.scrollwindow = scroll_window)

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
  out_proj_names <- c("Native",
                      "User Defined")
  out_proj_list  <- list(
    "Native"   = ifelse(
      prod_opt_list[[sel_prod]][[general_opts$prod_version]][["tiled"]] == "1",
      "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs", #nolint
      "+init=epsg:4008 +proj=longlat +ellps=clrk66 +no_defs"),
    "User Defined" = ""
  )
  mod_proj_str <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs" #nolint

  #   __________________________________________________________________________
  #   Initialize Widgets for product selection and bands selection          ####

  satprod_frame <- gframe(
    text       = strwrap("<span foreground='red' size='x-large'>
                         MODIS Product and Platform </span>"),
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

  satprod0_group <- ggroup(horizontal = TRUE, container = satprod_frame)
  cat_label      <- glabel(text      = "Category:",
                           container = satprod0_group,
                           editable  = FALSE)

  wids$cat <- gdroplist(
    items      = unique(mod_prod_cat$cat),
    container  = satprod0_group,
    horizontal = TRUE,
    selected   = match(sel_cat, unique(mod_prod_cat$cat)),
    handler    = function(h, ...) {
      gh_selcat(h, wids,
                mod_prod_list, mod_prod_cat,
                prod_opt_list, general_opts)
    }
  )
  gWidgets::size(wids$cat) <- list(width = 442)


  # addSpring(satprod0_group)

  www_but <- gbutton(
    text = paste0("Product Info (www)"),
    container = satprod0_group,
    handler   = function(h, ...) {
      utils::browseURL(sel_prodopts[[svalue(wids$vers)]][["www"]])
    }
  )
  gWidgets::font(www_but) <- list(family = "serif", weight = "bold",
                                  color = "red")
  # addSpace(satprod0_group, 1)
  # addSpring(satprod0_group)

  #   __________________________________________________________________________
  #   Initialize Widgets for Product selection                              ####

  # addSpring(satprod1_group)
  satprod1_group <- ggroup(horizontal = TRUE, container = satprod_frame)
  prod_label <- glabel(text = "Product:", container = satprod1_group)
  addSpace(satprod1_group, 3)
  wids$prod <- gdroplist(
    items     = mod_prod_list[mod_prod_cat$cat == gWidgets::svalue(wids$cat)],
    container = satprod1_group, horizontal = TRUE,
    selected  = match(
      sel_prod,
      mod_prod_list[mod_prod_cat$cat == gWidgets::svalue(wids$cat)]),
    handler   = function(h, ...) {
      gh_selprod(h, wids, prod_opt_list, general_opts)
    }
  )
  gWidgets::size(wids$prod) <- list(width = 442)
  gWidgets::font(prod_label) <- gWidgets::font(cat_label) <- list(
    family = "sans", weight = "bold")

  #   __________________________________________________________________________
  #   Initialize widgets for Version selection                              ####

  vers_label <- glabel(text = " Version:", container = satprod1_group)
  wids$vers   <- gcombobox(
    items     = vapply(sel_prodopts, function(x){
      x[["v_number"]]
    }
    , FUN.VALUE = ""),
    container = satprod1_group, text = "Select Version",
    selected  = match(general_opts$prod_version, names(sel_prodopts)),
    handler   = function(h, ...) {
      # reset dummy variables for band selection to 0 on product change
      gui_env$temp_wid_bands         <- 0
      gui_env$temp_wid_bands_indexes <- 0
      gui_env$temp_wid_bands_quality <- 0
    }
  )

  #   __________________________________________________________________________
  #   Initialize Widgets for Sensor selection                               ####

  satprod2_group <- ggroup(horizontal = TRUE, container = satprod_frame)

  sens_label     <- glabel(text = " Platform:", container = satprod1_group)

  wids$sens      <- gcombobox(items     = c("Terra", "Aqua", "Both"),
                              container = satprod1_group,
                              text      = "Select Platform", selected = 1)

  if (sel_prodopts[[general_opts$prod_version]]$combined == 1) {
    gWidgets::enabled(wids$sens) <- FALSE
    wids$sens[] <- "Combined"
    gWidgets::svalue(wids$sens)  <- "Combined"
  } else {
    wids$sens[]       <- c("Terra", "Aqua", "Both")
    gWidgets::svalue(wids$sens) <- general_opts$sensor
  }

  #   __________________________________________________________________________
  #   Initialize Widgets for Processing Layers selection                    ####

  layers_frame <- gframe(
    text       = "<span foreground='red' size='x-large'>Layers to be processed</span>", #nolint
    markup     = TRUE,
    container  = main_group,
    horizontal = TRUE,
    expand     = TRUE
  )

  sel_labs_group <- ggroup(horizontal = FALSE, container = layers_frame)
  layer_group    <- ggroup(horizontal = TRUE, container = sel_labs_group)
  band_label     <- glabel(text = "Original MODIS layer:",
                           container = layer_group)
  cur_prodopts <- sel_prodopts[[svalue(wids$vers)]]

  # Original Bands ----
  curr_sel_layers <- paste(
    cur_prodopts[["band_fullnames"]][which(general_opts[["bandsel"]] == 1)],
    collapse = "; ")
  wids$sel_layers <- gtext(text = ifelse(curr_sel_layers == "",
                                         "- None Selected - ", curr_sel_layers),
                           container = layer_group,
                           width     = 600,
                           height    = 15,
                           editable  = FALSE,
                           expand    = TRUE)
  gWidgets::enabled(wids$sel_layers) <- FALSE

  layers_help <- gbutton(text    = " ? ", handler = function(h, ...) {
    gh_help(h, "layers_help", help_messages, NULL)
  },
  container = layer_group,
  expand = FALSE
  )
  # Quality Indicators ----

  if (length(cur_prodopts[["quality_fullnames"]]) == 0) {
    curr_sel_qual <- "- No Quality Indicators are available for this product - "
  } else {
    curr_sel_qual <- paste(
      cur_prodopts[["quality_fullnames"]][which(general_opts[["quality_bandsel"]] == 1)],#nolint
      collapse = "; ")
  }

  qual_group      <- ggroup(horizontal = TRUE, container = sel_labs_group)
  qa_label        <- glabel(text = "Quality Indicators:",
                            container = qual_group)
  addSpace(qual_group, 11)
  wids$sel_qi   <- gtext(text = ifelse(curr_sel_qual == "",
                                       "- None Selected - ", curr_sel_qual),
                         container = qual_group,
                         width     = 600,
                         height    = 15,
                         editable  = FALSE,
                         expand    = TRUE)
  gWidgets::enabled(wids$sel_qi) <- FALSE
  qi_help <- gbutton(text = " ? ", handler = function(h, ...) {
    gh_help(h, "qi_help", help_messages,
            info_addr = "https://goo.gl/MrVyfF")
  },
  container = qual_group,
  expand = FALSE
  )

  # Spectral Indexes ----
  if (length(cur_prodopts[["indexes_fullnames"]]) == 0) {
    curr_sel_si <- "- Spectral Indexes can not be computed on this product - "
  } else {
    check_names_indexes <- c(
      cur_prodopts[["indexes_fullnames"]],
      as.list(general_opts$custom_indexes[[sel_prod]]
              [[svalue(wids$vers)]])$indexes_fullnames
    )
    curr_sel_si <- paste(
      check_names_indexes[which(general_opts[["indexes_bandsel"]] == 1)],
      collapse = "; ")
  }

  si_group    <- ggroup(horizontal = TRUE, container = sel_labs_group)

  si_label    <- glabel(text = "Spectral Indexes:",
                        container = si_group)
  addSpace(si_group, 16)
  wids$sel_si  <- gtext(text = ifelse(curr_sel_si == "",
                                      "- None Selected - ", curr_sel_si),
                        container = si_group,
                        width     = 600,
                        height    = 15,
                        editable  = FALSE,
                        expand    = TRUE)
  gWidgets::enabled(wids$sel_si) <- FALSE
  si_help <- gbutton(text = " ? ", handler = function(h, ...) {
    gh_help(h, "si_help", help_messages, NULL)
  },
  container = si_group,
  expand = FALSE
  )

  # Childs to select layers ----
  #  ..........................................................................
  #  Upon clicking on the button we create a Child widget for selection of
  #  processing layers (could try to separate a function for this, but it would
  #  be a hassle) and deal with all events
  # addSpring(layers_frame)
  wids$band   <- gbutton(
    text    = " Change\nSelection ",
    handler = function(h, ...) {
      gh_sellayers(prodopts_file,
                   opts_jsfile,
                   gui_env,
                   out_proj_list,
                   help_messages)
    },
    # here finishes the initialization of the layer selection widget group
    container = layers_frame,
    expand    = TRUE,
    fill      = c("y")
  )

  gWidgets::size(wids$band) <- list(width = 30)

  gWidgets::font(vers_label) <- list(family = "sans", weight = "bold")
  gWidgets::font(sens_label) <- gWidgets::font(band_label) <-
    gWidgets::font(qa_label) <-  gWidgets::font(si_label) <- list(
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
    items     = c("http", "offline"),
    text      = "Select",
    container = methods_group,
    selected  = match(general_opts$download_server,
                      c("http", "offline")),
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
  server_help <- gbutton(text = " ? ",
                         handler = function(h, ...) {
                           gh_help(h, "server_help", help_messages)
                         },
                         container = methods_group, expand = FALSE
  )

  addSpace(methods_group, 10)
  authenticate_group <- ggroup(container = methods_group)

  user_lab <- glabel(text      = " User Name:",
                     container = authenticate_group)

  wids$user <- gedit(text      = general_opts$user,
                     container = authenticate_group,
                     width     = 15)

  addSpace(authenticate_group, 10)
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

  aria_help <- gbutton(text = " ? ",
                       handler = function(h, ...) {
                         gh_help(h, "aria_help", help_messages,
                                 info_addr = "https://aria2.github.io/")
                       },
                       container = methods_group, expand = FALSE
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

  season_help <- gbutton(text = " ? ",
                         handler = function(h, ...) {
                           gh_help(h, "season_help", help_messages, NULL)
                         },
                         container = dates_group, expand = FALSE)

  #   __________________________________________________________________________
  #   Initialize Widgets for Tiles selection                                ####

  spatial_frame <- gframe(
    text       = strwrap("<span foreground='red' size='x-large'>
                         Output Spatial Extent </span>"),
    markup     = TRUE,
    container  = main_group,
    horizontal = FALSE,
    expand     = TRUE
  )
  sel_group <- ggroup(container = spatial_frame, horizontal = TRUE)
  addSpace(sel_group, 100)
  output_ext_group <- ggroup(container = spatial_frame, horizontal = TRUE)
  wids$output_ext <- gradio(
    items      = c("Select MODIS Tiles", "Define Custom Area"),
    expand     = TRUE, fill = "x",
    horizontal = TRUE,
    container  = sel_group,
    selected   = match(general_opts$full_ext,
                       c("Select MODIS Tiles", "Define Custom Area")),
    handler  = function(h, ...) {
      current_sel <- gWidgets::svalue(wids$output_ext)
      sel_prod   <- ifelse(!is.null(gWidgets::svalue(wids$prod)),
                           gWidgets::svalue(wids$prod),
                           sel_prod)
      sel_prodopts <- prod_opt_list[[sel_prod]]
      if (sel_prodopts[[gWidgets::svalue(wids$vers)]]$tiled == 1) {
        # On "Select MODIS Tiles" selection, disable the bbox fields
        if (current_sel == "Select MODIS Tiles") {
          gWidgets::enabled(bbox_group)      <- FALSE
          gWidgets::enabled(tiles_from_bbox) <- FALSE
          gWidgets::enabled(bbox_from_file)  <- FALSE
          gWidgets::enabled(tiles_group)     <- TRUE
        } else {
          # On "Custom  Area" selection, enable the bbox fields
          gWidgets::enabled(bbox_group)      <- TRUE
          gWidgets::enabled(tiles_from_bbox) <- TRUE
          gWidgets::enabled(bbox_from_file)  <- TRUE
          gWidgets::enabled(tiles_group)     <- FALSE
        }
      } else {
        if (current_sel == "Select MODIS Tiles") {
          gWidgets::svalue(wids$output_ext)  <- "Define Custom Area"
          gWidgets::enabled(bbox_group)      <- TRUE
          gWidgets::enabled(tiles_from_bbox) <- TRUE
          gWidgets::enabled(bbox_from_file)  <- TRUE
          gWidgets::enabled(tiles_group)     <- FALSE
        }
      }
    }
  )

  view_button <- gbutton(text = " View current extent ",
                         handler = function(h, ...) {
                           gh_view_extent(ext_type = svalue(wids$output_ext),
                                          wids)
                         },
                         container = sel_group, expand = FALSE)

  extent_help <- gbutton(text = " ? ",
                         handler = function(h, ...) {
                           gh_help(h, "extent_help", help_messages, NULL)
                         },
                         container = sel_group, expand = FALSE)

  #   __________________________________________________________________________
  #   Initialize Widgets for tiles selection                                ####

  spatial_group  <- ggroup(container = spatial_frame, horizontal = TRUE)

  tiles_group   <- gframe(text       = "<b><i> Required MODIS Tiles </i></b>",
                          markup     = TRUE,
                          container  = spatial_group,
                          horizontal = FALSE,
                          expand     = TRUE,
                          pos        = 0.5)

  # horizontal ----
  x_group       <- ggroup(container = tiles_group, horizontal = TRUE,
                          spacing = 2, expand = TRUE)

  start_x_lab   <- glabel(text = " Horizontal:", container = x_group)
  gWidgets::addSpace(x_group, 1)
  start_x_start <- glabel(text = "Start", container = x_group)
  wids$start_x  <- gspinbutton(0, 35, text = "Select",
                               container   = x_group,
                               value       = general_opts$start_x,
                               expand     = TRUE)

  end_x_lab     <- glabel(text = "End", container = x_group)
  wids$end_x    <- gspinbutton(0, 35, text = "Select",
                               container   = x_group,
                               value       = general_opts$end_x,
                               expand     = TRUE)

  # vertical ----
  y_group       <- ggroup(container  = tiles_group,
                          horizontal = TRUE,
                          spacing    = 2,
                          expand     = TRUE)

  start_y_lab   <- glabel(text = " Vertical:     ", container = y_group)
  start_y_start <- glabel(text = "Start", container = y_group)
  wids$start_y  <- gspinbutton(0, 17, text = "Select",
                               container   = y_group,
                               value       = general_opts$start_y,
                               expand     = TRUE)

  end_y_lab     <- glabel(text = "End", container = y_group)
  wids$end_y    <- gspinbutton(0, 17, text = "Select",
                               container   = y_group,
                               value       = general_opts$end_y,
                               expand     = TRUE)

  gWidgets::font(start_x_lab) <- gWidgets::font(start_y_lab) <-
    list(family = "sans", weight = "bold")

  # If a non-tiled product is selected, grey-out the tiles selection groups
  gWidgets::enabled(tiles_group) <- ifelse(
    prod_opt_list[[general_opts$sel_prod]][[general_opts$prod_version]]$tiled == 0, #nolint
    FALSE,
    TRUE
  )

  # draw on map map button ----

  # Extent buttons -----
  spatialbut_group  <- ggroup(container = spatial_group, horizontal = FALSE,
                              expand = FALSE)
  # draw on map map button ----
  select_map_tiles <- gbutton(
    text    = "Select On Map",
    handler = function(h, ...) {
      gh_selectmap(h, svalue(wids$output_ext), wids,
                   mod_proj_str, modis_grid)

    }, container = spatialbut_group, expand = TRUE)

  # Text labels showing Extent ----
  addSpace(tiles_group, 1)
  # addSpring(spatial_group)
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
  bbox_group  <- ggroup(horizontal = TRUE, container = bounding_group)

  min_group   <- ggroup(horizontal = FALSE, container = bbox_group)
  lat_w_group <- ggroup(horizontal = TRUE,  container = min_group)
  addSpring(lat_w_group)

  output_xmin_lab  <- glabel("x-min", container = lat_w_group, expand = TRUE)
  wids$output_xmin <- gedit(text = general_opts$bbox[1],
                            container = lat_w_group,
                            width     = 9)
  long_s_group <- ggroup(horizontal = TRUE, container = min_group)
  addSpring(long_s_group)

  output_ymin_lab  <- glabel("y-min", container = long_s_group)
  wids$output_ymin <- gedit(text      = general_opts$bbox[2],
                            container = long_s_group,
                            width     = 9)
  addSpace(bbox_group, 2)

  max_group   <- ggroup(horizontal = FALSE, container = bbox_group)
  lat_e_group <- ggroup(horizontal = TRUE,  container = max_group)
  addSpring(lat_e_group)

  output_xmax_lab  <- glabel("x-max", container = lat_e_group)
  wids$output_xmax <- gedit(text = general_opts$bbox[3],
                            container = lat_e_group,
                            width     = 9)
  long_n_group <- ggroup(horizontal = TRUE, container = max_group)
  addSpring(long_n_group)

  output_ymax_lab  <- glabel("y-max", container = long_n_group)
  wids$output_ymax <- gedit(text = general_opts$bbox[4],
                            container = long_n_group,
                            width     = 9)

  gWidgets::font(output_xmin_lab) <- gWidgets::font(output_ymax_lab) <-
    list(family = "sans", weight = "bold")
  gWidgets::font(output_xmax_lab) <- gWidgets::font(output_ymin_lab) <-
    list(family = "sans", weight = "bold")

  # Button to load extent from SHP or KML file ----

  bbox_from_file <- gbutton(
    text = "Load Extent from a spatial file",
    handler = function(h, ...) {
      # File selection widget
      gh_load_extent(h, wids, out_proj_list, mod_proj_str,
                     modis_grid)
    } , container = spatialbut_group
  )

  # button to retrieve tiles from bounding box ----

  if (!exists("modis_grid")) {
    modis_grid <- get(load(file.path(MODIStsp_dir,
                                     "ExtData/MODIS_Tiles.RData")))
  }  # Laod MODIS grid ancillary file

  tiles_from_bbox <- gbutton(
    text    = "Update Tiles from bounding box",
    handler = function(h, ...) {
      gh_tiles_from_bbox(h, wids, mod_proj_str, modis_grid)
    },
    container = spatialbut_group
  )

  # enable/disable the bbox_from_file button
  gWidgets::enabled(bbox_from_file) <- ifelse(
    gWidgets::svalue(wids$output_ext) != "Select MODIS Tiles",
    TRUE,
    FALSE
  )

  # enable/disable the tiles selectors
  gWidgets::enabled(tiles_group) <- ifelse(
    gWidgets::svalue(wids$output_ext) != "Select MODIS Tiles",
    FALSE,
    TRUE
  )

  # disable corner labels if "Full Extent" requested ----
  gWidgets::enabled(bbox_group) <- ifelse(
    general_opts$full_ext == "Select MODIS Tiles", FALSE, TRUE
  )

  gWidgets::enabled(tiles_from_bbox) <- ifelse(
    gWidgets::svalue(wids$output_ext) != "Select MODIS Tiles",
    TRUE,
    FALSE
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
  output_proj_lab   <- glabel(text      = "Output Projection: ",
                              container = output_proj_group)
  gWidgets::font(output_proj_lab) <- list(family = "sans", weight = "bold")

  wids$proj_choice <- gcombobox(

    items     = out_proj_names,
    container = output_proj_group,
    selected  = match(general_opts$proj, out_proj_names),
    handler   = function(h, ...) {
      gh_changeproj(h, wids, out_proj_list, bbox_out)
    }
  )

  # Text widget showing the current output proj4string ----
  outproj_user_lab       <- glabel(text      = "  PROJ4 String:",
                                   container = output_proj_group)
  gWidgets::font(outproj_user_lab) <- list(family = "sans", weight = "bold")

  wids$output_proj4       <- gtext(text      = general_opts$output_proj4,
                                   container = output_proj_group,
                                   width     = 300,
                                   height    = 20,
                                   editable  = FALSE,
                                   expand    = TRUE)

  gWidgets::svalue(wids$output_proj4) <- ifelse(
    gWidgets::svalue(wids$proj_choice) == "User Defined",
    general_opts$output_proj4,
    out_proj_list[[gWidgets::svalue(wids$proj_choice)]]) #nolint

  # Button to change the user defined projection ----
  wids$change_proj_but <- gbutton(
    text      = "Change",
    container = output_proj_group,
    handler   = function(h, ...) {

      choice <- warn_projmess1()
      if (choice){
        old_proj <- gWidgets::svalue(wids$output_proj4)
        selproj <- ginput(
          paste("Please insert a valid proj4string,",
                "an EPSG code or an UTM grid zone (e.g. 32N):"),
          parent     = NULL,
          do.buttons = TRUE,
          size       = 800,
          horizontal = TRUE
        )
      }

      if (length(selproj) != 0 && selproj != "" && !is.na(selproj))  {

        newproj <- check_proj4string(selproj, abort = FALSE, verbose = FALSE)

        # Check if proj4string is valid
        if (is.na(sel_output_proj)) {
          gmessage(
            message = strwrap(paste(
              "The projection is not recognized,",
              "so the previously selected projection will be kept."
            )),
            title   = strwrap("Invalid projection")
          )
        } else {

          gWidgets::svalue(wids$output_proj4) <- newproj
          proj  <- gui_get_proj(sp::CRS(newproj))
          units <- gui_get_units(sp::CRS(newproj), proj)
          gWidgets::svalue(wids$pixsize2_lab) <- units
          # If valid proj4string, and output is a bounding box, recompute
          # bounding box in output proj coordinates
          if (gWidgets::svalue(wids$output_ext) != "Select MODIS Tiles") {

            bbox_in  <- as.numeric(c(gWidgets::svalue(wids$output_xmin),
                                     gWidgets::svalue(wids$output_ymin),
                                     gWidgets::svalue(wids$output_xmax),
                                     gWidgets::svalue(wids$output_ymax)))

            bbox_out <- reproj_bbox(bbox_in,
                                    old_proj,
                                    newproj,
                                    enlarge = FALSE)

            gui_update_bboxlabels(bbox_out,
                                  units,
                                  wids,
                                  reset = TRUE)
          }
        }
      }

    }

  )

  gWidgets::size(wids$change_proj_but) <- c(58, 22)

  # Grey-out proj change widgets if !User Defined proj
  gWidgets::enabled(wids$output_proj4) <- FALSE
  if (general_opts$proj == "User Defined") {
    gWidgets::enabled(wids$change_proj_but)  <- TRUE
  } else {
    gWidgets::enabled(wids$change_proj_but)  <- FALSE
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
        gWidgets::svalue(wids$output_res)  <- "Native"
      } else {
        gWidgets::enabled(wids$output_res) <- TRUE
        gWidgets::svalue(wids$output_res)  <- ""
      }
    }
  )

  gWidgets::size(wids$output_res_sel) <- list(width = 106)

  # input field to define/see output resolution ----
  pixsize_lab  <- glabel(text      = "  Pixel Size:",
                         container = output_res_group)
  gWidgets::font(pixsize_lab) <- list(family = "sans", weight = "bold")

  wids$output_res <- gedit(text      = general_opts$out_res,
                           container = output_res_group,
                           width     = 10)

  if (gWidgets::svalue(wids$output_res_sel) == "Native") {
    gWidgets::svalue(wids$output_res)  <- "Native"
    gWidgets::enabled(wids$output_res) <- FALSE
  } else {
    gWidgets::svalue(wids$output_res)  <- general_opts$out_res
    gWidgets::enabled(wids$output_res) <- TRUE
  }

  # Initial set-up of the output projection on the basis of current
  # values in widgets

  sel_output_proj <- sp::CRS(
    if (gWidgets::svalue(wids$proj_choice) == "User Defined") {
      gWidgets::svalue(wids$output_proj4)
    } else {
      out_proj_list[[gWidgets::svalue(wids$proj_choice)]]
    }
  )

  proj  <- gui_get_proj(sel_output_proj)
  units <- gui_get_units(sel_output_proj, proj)
  wids$pixsize2_lab <- glabel(text = units, container = output_res_group)

  # Dropdown menu to select Resampling Method ----

  addSpring(output_res_group)
  resmeth_lab <- glabel(text      = "    Resampling Method:  ",
                        container = output_res_group)
  gWidgets::font(resmeth_lab) <- list(family = "sans", weight = "bold")

  resamp_array       <- c("near", "mode")
  wids$output_resmeth <- gcombobox(items     = resamp_array,
                                   container = output_res_group,
                                   selected  = match(general_opts$resampling,
                                                     resamp_array))

  #   __________________________________________________________________________
  #   Initialize Widgets for Format and reprocess options                   ####

  options_frame <- gframe(
    text       = strwrap("<span foreground='red' size='x-large'>
                         Output Options </span>"),
    markup     = TRUE,
    container  = main_group,
    expand     = TRUE,
    horizontal = FALSE
  )

  opt_group     <- ggroup(container  = options_frame,
                          horizontal = TRUE,
                          expand     = TRUE)

  # Out format ----
  format_lab       <- glabel(text      = "Output Format:",
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

  gWidgets::size(wids$format) <- c(70, 30)
  addSpace(opt_group, 10)
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
                             selected  = match(general_opts$compress[[1]],
                                               compress_dict))
  current_sel_form <- gWidgets::svalue(wids$format)
  # grey out compression if ENVI output
  gWidgets::enabled(compress_group) <- ifelse(
    current_sel_form != "GTiff", FALSE, TRUE
  )

  gWidgets::size(wids$compress) <- c(110, 30)
  addSpring(opt_group)

  # RasterStacks and NoData Yes/No ----
  other_group <- ggroup(container = options_frame, horizontal = TRUE)
  nodata_lab  <- glabel(text = "Modify NoData: ", container = opt_group)
  wids$nodata <- gradio(items      = c("Yes", "No"),
                        text       = "Select",
                        container  = opt_group,
                        selected   = match(general_opts$nodata_change,
                                           c("Yes", "No")),
                        horizontal = TRUE)
  gWidgets::font(nodata_lab) <- list(family = "sans", weight = "bold")

  nodata_help <- gbutton(
    text = " ? ",
    handler = function(h, ...) {
      gh_help(h, "nodata_help", help_messages,NULL)
    }, container = opt_group, expand = FALSE)

  # Virtual raster creation menu ----
  timeseries_lab <- glabel(text = "Save Time Series as:",
                           container = other_group)
  gWidgets::font(timeseries_lab) <- list(family = "sans", weight = "bold")

  list_virt <- c("R rasterStack", "ENVI Meta", "GDAL vrt")

  curr_checked <- c(FALSE, FALSE, FALSE)
  curr_checked[match(general_opts$ts_format, list_virt)] <- TRUE

  wids$timeseries <- gcheckboxgroup(
    items     = c("R rasterStack", "ENVI Meta", "GDAL vrt"),
    container = other_group,
    checked   = curr_checked,
    horizontal = TRUE)

  timeseries_help <- gbutton(
    text = " ? ",
    handler = function(h, ...) {
      gh_help(h, "timeseries_help", help_messages,
              info_addr = "http://lbusett.github.io/MODIStsp/articles/output.html", #nolint
              ...)
    }, container = other_group, expand = FALSE)


  # Apply scale/offset ----
  addSpace(other_group, 15)
  addSpring(other_group)
  scale_lab <- glabel(text = "Apply Scale/Offset: ", container = other_group)

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

  scale_help <- gbutton(text = " ? ", handler = function(h, ...) {
    gh_help(h, "scale_help", help_messages, NULL)
  }, container = other_group, expand = FALSE)

  #   __________________________________________________________________________
  #   Initialize Widgets for output folders selection                       ####

  # Main output folder ----
  outfold_frame <- gframe(
    text = strwrap("<span foreground='red' size='x-large'>
                   Main MODIStsp output folder </span>"),
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

  outfold_help <- gbutton(text = " ? ", handler = function(h, ...) {
    gh_help(h, "outfold_help", help_messages,
            info_addr = "http://lbusett.github.io/MODIStsp/articles/output.html", #nolint
            ...)
  }, container = outfold_group, expand = FALSE)

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

  reproc_help <- gbutton(text = " ? ", handler = function(h, ...) {
    gh_help(h, "reproc_help", help_messages, NULL)
  }, container = outfold_group, expand = FALSE)

  # HDF output folder ----

  outfoldmod_frame <- gframe(
    text      = strwrap(
      "<span foreground='red' size='large'>
      Folder for permanent storage of original MODIS HDF images </span>"),
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
    width     = 30,
    expand    = TRUE
  )
  fold_choose <- gbutton(
    text    = "Browse",
    handler = function(h, ...) {
      choice <- try(gfile(
        type = "selectdir",
        text = "Select the Output Folder for storage of MODiS original HDFsimages..." #nolint
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

  outhdffold_help <- gbutton(text = " ? ", handler = function(h, ...) {
    gh_help(h, "outhdffold_help", help_messages, NULL)
  }, container = outfoldmod_group, expand = FALSE)

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

  delete_help <- gbutton(text = " ? ", handler = function(h, ...) {
    gh_help(h, "delete_help", help_messages, NULL)
  }, container = outfoldmod_group, expand = FALSE)

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
        gui_load_options(choice, wids, prod_opt_list, compress_dict)
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

  #nocov end

}  # END OF MAIN FUNCTION
