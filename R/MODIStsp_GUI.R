#' MODIStsp_GUI
#' @description
#'	Function used to generate and handle the GUI used to allow selection of MODIStsp processing parameters
#'  If the "previous options" file (MODIStsp_Previous.json) already exists, it is loaded and used to reinstate
#' 	the GUI to its last state. Otherwise, the previous options file is created by launching the MODIStsp_read_xml fucntion
#'
#' @param general_opts General options data frame passed by MODIStsp_main. Contains paths and other variables used to initialize the GUI
#' 						if a previous options file is not existing.
#' @param prod_opt_list List of MODIS products specifications (read from MODIStsp_ProdOpts.xml file)
#' @param scrollWindow logical parameter passed by MODIStsp main function.
#' @return Quit - Logical - tells the main if running processing or exiting (also, Processing options are saved in "previous" file and (if "Save options" is pressed) in user's selected file)
#' @author Lorenzo Busetto, phD (2014-2015) \email{busetto.l@@irea.cnr.it}
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom hash keys
#' @importFrom raster crop extent raster plot
#' @importFrom sp CRS
#' @importFrom utils browseURL
#' @importFrom grDevices dev.new
#' @import gWidgets

MODIStsp_GUI <- function(general_opts, prod_opt_list, scrollWindow, MODIStsp_dir, previous_jsfile, prodopts_file){
  
  # assign("Quit", T, envir = globalenv())	# Assigng "Quit" to true
  
  GUI.env <- new.env()
  GUI.env$Quit <- TRUE
  #- ------------------------------------------------------------------------------- -#
  #  Start Building the GUI
  #- ------------------------------------------------------------------------------- -#
  
  main_win <- gbasicdialog(title = "Select Main Processing Options", parent = NULL, do.buttons = FALSE)
  main_frame1 <- ggroup(container = main_win, horizontal = TRUE, expand = FALSE, use.scrollwindow=scrollWindow)
  main_frame2 <- ggroup(container = main_frame1, horizontal = FALSE, expand = FALSE)
  # frame1 and 2 with expand=FALSE grant that widgets are not "too much expanded", nor horizontally neither vertically
  main_group <- ggroup(container = main_frame2, horizontal = FALSE, expand = FALSE)
  if (scrollWindow) {getToolkitWidget(main_win)$maximize()}
  mod_prod_cat <- as.data.frame(t(sapply(prod_opt_list,function(x){c(x[[1]]$cat01,x[[1]]$cat02)}))); names(mod_prod_cat) <- c('cat01','cat02')
  mod_prod_cat$cat <- apply(mod_prod_cat,1,paste,collapse=' - ')
  
  sel_prod <- general_opts$sel_prod # get the product name selectedin the previous options file
  mod_prod_list <- names(prod_opt_list)
  sel_cat <- mod_prod_cat$cat[match(sel_prod, mod_prod_list)]
  
  out_proj_names <- c("Sinusoidal","UTM 32N","Latlon WGS84","User Defined" )
  out_proj_list <- hash("Sinusoidal" = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs",
                        "UTM 32N" = "+init=epsg:32632 +proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
                        "Latlon WGS84" = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                        "User Defined" = "")
  MOD_proj_str <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
  
  #- ------------------------------------------------------------------------------- -#
  # Widgets for product selection and bands selection
  #- ------------------------------------------------------------------------------- -#
  satprod_frame <- gframe(text = "<span foreground='red' size='x-large'>MODIS Product, Platform and Layers selection</span>",
                          markup = TRUE, horizontal = FALSE, container = main_group)
  
  # set dummy global variables holding the initial values of selected bands
  GUI.env$temp_wid_bands         <- general_opts$bandsel
  GUI.env$temp_wid_bands_indexes <- general_opts$indexes_bandsel
  GUI.env$temp_wid_bands_quality <- general_opts$quality_bandsel
  
  #- ------------------------------------------------------------------------------- -#
  # Widgets for category selection
  #- ------------------------------------------------------------------------------- -#
  satprod1_group <- ggroup(horizontal = TRUE, container = satprod_frame)
  cat_label <- glabel(text = " Category:", container = satprod1_group)
  cat_wid <- gdroplist(items = unique(mod_prod_cat$cat), container = satprod1_group, horizontal = TRUE,
                       selected = match(sel_cat, unique(mod_prod_cat$cat)),
                       handler = function(h,...) {
                         
                         # Select only products of this category
                         sel_prod <- mod_prod_list[mod_prod_cat$cat==svalue(cat_wid)][1]
                         prod_wid[] <- mod_prod_list[mod_prod_cat$cat==svalue(cat_wid)]
                         svalue(prod_wid) <- sel_prod
                         # Select the last version (it assumes that versions in xml file are in increasing order)
                         vers_wid[] <- names(prod_opt_list[[sel_prod]])
                         svalue(vers_wid) <- prod_opt_list[[sel_prod]][[length(prod_opt_list[[sel_prod]])]]$v_number
                         # Disable sensor choice for combined datasets
                         if (prod_opt_list[[sel_prod]][[svalue(vers_wid)]]$combined == 1) {
                           enabled(sens_wid) <- FALSE
                           sens_wid[] <- "Combined"
                           svalue(sens_wid) <- "Combined"
                         } else {
                           (enabled(sens_wid) <- TRUE)
                           sens_wid[] <- c("Terra","Aqua","Both")
                           svalue(sens_wid) <- general_opts$sensor
                         }
                         # On product change, automatically modify the default projection - latlon for tiled, Sinu for nontiled
                         if (prod_opt_list[[sel_prod]][[svalue(vers_wid)]]$tiled == 0) {
                           enabled(tiles_group) <- FALSE
                           svalue(proj_wid) <- "Latlon WGS84"
                         } else {
                           (enabled(tiles_group) <- TRUE)
                           svalue(proj_wid) <- "Sinusoidal"
                         }
                         # reset dummy variables for band selection to 0 on product change
                         
                         GUI.env$temp_wid_bands <- GUI.env$temp_wid_bands_indexes <- GUI.env$temp_wid_bands_quality <- 0
                       })
  size(cat_wid) <- list(width=325)
  
  #- ------------------------------------------------------------------------------- -#
  # Widgets for Product selection
  #- ------------------------------------------------------------------------------- -#
  addSpring(satprod1_group, horizontal=TRUE)
  prod_label <- glabel(text = "   Product:", container = satprod1_group)
  prod_wid <- gdroplist(items = mod_prod_list[mod_prod_cat$cat==svalue(cat_wid)], 
                        container = satprod1_group, horizontal = TRUE,
                        selected = match(general_opts$sel_prod, mod_prod_list[mod_prod_cat$cat==svalue(cat_wid)]),
                        handler = function(h,...) {
                          sel_prod <- if (!is.null(svalue(prod_wid))) {svalue(prod_wid)} else {sel_prod}
                          # Select the last version (it assumes that versions in xml file are in increasing order)
                          vers_wid[] <- names(prod_opt_list[[sel_prod]])
                          svalue(vers_wid) <- prod_opt_list[[sel_prod]][[length(prod_opt_list[[sel_prod]])]]$v_number
                          # Disable sensor choice for combined datasets
                          if (prod_opt_list[[sel_prod]][[svalue(vers_wid)]]$combined == 1) {
                            enabled(sens_wid) <- FALSE
                            sens_wid[] <- "Combined"
                            svalue(sens_wid) <- "Combined"
                          } else {
                            (enabled(sens_wid) <- TRUE)
                            sens_wid[] <- c("Terra","Aqua","Both")
                            svalue(sens_wid) <- general_opts$sensor
                          }
                          # On product change, automatically modify the default projection - latlon for tiled, Sinu for nontiled
                          if (prod_opt_list[[sel_prod]][[svalue(vers_wid)]]$tiled == 0) {
                            enabled(tiles_group) <- FALSE
                            svalue(proj_wid) <- "Latlon WGS84"
                          } else {
                            (enabled(tiles_group) <- TRUE)
                            svalue(proj_wid) <- "Sinusoidal"
                          }
                          # reset dummy variables for band selection to 0 on product change
                          GUI.env$temp_wid_bands <- GUI.env$temp_wid_bands_indexes <- GUI.env$temp_wid_bands_quality <- 0
                        })
  size(prod_wid) <- list(width=325)
  
  font(prod_label) <- font(cat_label) <- list(family = "sans",weight = "bold")
  
  #- ------------------------------------------------------------------------------- -#
  # Widgets for Sensor selection
  #- ------------------------------------------------------------------------------- -#
  satprod2_group <- ggroup(horizontal = TRUE, container = satprod_frame)

  # addSpace(satprod_frame, 20, horizontal = TRUE)
  sens_label <- glabel(text = " Platform:", container = satprod2_group)
  sens_wid <- gdroplist(items = c("Terra"), container = satprod2_group, text = "Select Layers", selected = 1)
  if (prod_opt_list[[general_opts$sel_prod]][[general_opts$prod_version]]$combined == 1) {
    enabled(sens_wid) <- FALSE
  } else {
    sens_wid[] <- c("Terra","Aqua","Both")
    svalue(sens_wid) <- general_opts$sensor
  }
  size(sens_wid) <- list(width=150)
  
  addSpace(satprod2_group, 20, horizontal = TRUE)
  
  #- ------------------------------------------------------------------------------- -#
  # Widgets for Version selection
  #- ------------------------------------------------------------------------------- -#
  vers_label <- glabel(text = " Version:", container = satprod2_group)
  vers_wid <- gdroplist(items = sapply(prod_opt_list[[sel_prod]],function(x){x[["v_number"]]}),
                        container = satprod2_group, text = "Select Version", selected = match(general_opts$prod_version, names(prod_opt_list[[sel_prod]])),
                        handler = function(h,...) {
                          # reset dummy variables for band selection to 0 on product change
                          GUI.env$temp_wid_bands <- GUI.env$temp_wid_bands_indexes <- GUI.env$temp_wid_bands_quality <- 0
                        })
  size(vers_wid) <- list(width=100)
  
  addSpace(satprod2_group, 20, horizontal = TRUE)
  
  #- ------------------------------------------------------------------------------- -#
  # Widgets for Layers selection
  #- ------------------------------------------------------------------------------- -#
  band_label <- glabel(text = " Processing layers:", container = satprod2_group)
  band_wid <- gbutton(text = "   Click To Select   ", border = TRUE,				# Child widget for processing bands selection
                      handler = function(h,....) {
                        
                        prod_opt_list <- get(load(prodopts_file))
                        general_opts <- RJSONIO::fromJSON(previous_jsfile)
                        check_names <- prod_opt_list[[svalue(prod_wid)]][[svalue(vers_wid)]]$band_fullnames					# retrieve band names of sel. product
                        check_wid <- GUI.env$temp_wid_bands												# retrieve currently selected original layers
                        selgroup <- gbasicdialog(title = "Select Processing Layers", parent = NULL, do.buttons = FALSE, horizontal = TRUE)
                        
                        # widgets for band selection - original ----
                        cbox_total <- ggroup(container = selgroup, horizontal = TRUE)
                        cbox <- gframe(text = "<span foreground='red' size='large'>Original MODIS Layers</span>", markup = TRUE, container = cbox_total, horizontal = FALSE)
                        bands_wid <- gcheckboxgroup(items = check_names, checked = as.logical(check_wid),
                                                    container = cbox, use.table = FALSE)
                        
                        # widgets for band selection - quality
                        check_names_quality <- prod_opt_list[[svalue(prod_wid)]][[svalue(vers_wid)]]$quality_fullnames # retrieve quality band names (if existing for sel. product)
                        if ( !is.null(check_names_quality) ) {
                          check_wid_quality <- GUI.env$temp_wid_bands_quality						    # retrieve currently selected quality layers (if existing for sel. product)
                          cbox_quality <- gframe(text = "<span foreground='red' size='large'>Quality Indicators</span>", markup = TRUE, container = cbox_total, horizontal = FALSE)
                          bands_wid_quality <- gcheckboxgroup(items = check_names_quality, checked = as.logical(check_wid_quality),
                                                              container = cbox_quality, use.table = FALSE)
                        }
                        
                        # widgets for band selection - indexes ----
                        check_names_indexes <- c(prod_opt_list[[svalue(prod_wid)]][[svalue(vers_wid)]]$indexes_fullnames, as.list(general_opts$custom_indexes[[svalue(prod_wid)]][[svalue(vers_wid)]])$indexes_fullnames) # retrieve indexes band names (if existing for sel. product)
                        if (!is.null(check_names_indexes)) {
                          check_wid_indexes <- GUI.env$temp_wid_bands_indexes							# retrieve currently selected indexes layers
                          cbox_indexes <- gframe(text = "<span foreground='red' size='large'>Additional Spectral Indexes</span>", markup = TRUE, container = cbox_total, horizontal = FALSE)
                          bands_wid_indexes <- gcheckboxgroup(items = check_names_indexes, checked = as.logical(check_wid_indexes),
                                                              container = cbox_indexes, use.table = FALSE)
                          band_indexes_space <- glabel(text = "", container = cbox_indexes)
                          band_wid_newindex <- gbutton(text = "Add custom index", border = TRUE,
                                                       handler = function(h,...) {
                                                         # Run addindex() function ----
                                                         MODIStsp_addindex(option_jsfile = previous_jsfile)
                                                         general_opts <- RJSONIO::fromJSON(previous_jsfile)
                                                         pos_wid <- which(check_names %in% svalue(bands_wid))   # ? which layers selected ? --> store in GUI.env$temp_wid_bands array
                                                         tmp_arr_bands <- array(data = 0 , dim = length(check_names))
                                                         tmp_arr_bands[pos_wid] <- 1
                                                         GUI.env$temp_wid_bands <- tmp_arr_bands
                                                         if (length(which(check_names_indexes != "") > 0)) {    # ? which indexes selected ? --> store in GUI.env$temp_wid_bands_indexes array
                                                           pos_wid <- which(check_names_indexes %in% svalue(bands_wid_indexes))
                                                           tmp_arr_ind <- array(data = 0 , dim = length(check_names_indexes))
                                                           tmp_arr_ind[pos_wid] <- 1
                                                           GUI.env$temp_wid_bands_indexes <- tmp_arr_ind
                                                         }
                                                         if (length(which(check_names_quality != "") > 0)) {    # ? which quality selected ? --> store in GUI.env$temp_wid_bands_quality array
                                                           pos_wid <- which(check_names_quality %in% svalue(bands_wid_quality))
                                                           tmp_arr_qual <- array(data = 0 , dim = length(check_names_quality))
                                                           tmp_arr_qual[pos_wid] <- 1
                                                           GUI.env$temp_wid_bands_quality <- tmp_arr_qual
                                                         }
                                                         dispose(selgroup)
                                                       },
                                                       container = cbox_indexes, expand = FALSE)
                        }
                        
                        # Start/Cancel widgets ----
                        bands_group <- ggroup(container = selgroup, horizontal = TRUE)
                        accept_but <- gbutton(text = "Accept", container = bands_group, handler = function(button,...){ # On accept, retrieve and save selected layers
                          
                          pos_wid <- which(check_names %in% svalue(bands_wid))   # ? which layers selected ? --> store in GUI.env$temp_wid_bands array
                          tmp_arr_bands <- array(data = 0 , dim = length(check_names))
                          tmp_arr_bands[pos_wid] <- 1
                          GUI.env$temp_wid_bands <- tmp_arr_bands
                          if (length(which(check_names_indexes != "") > 0)) {    # ? which indexes selected ? --> store in GUI.env$temp_wid_bands_indexes array
                            pos_wid <- which(check_names_indexes %in% svalue(bands_wid_indexes))
                            tmp_arr_ind <- array(data = 0 , dim = length(check_names_indexes))
                            tmp_arr_ind[pos_wid] <- 1
                            GUI.env$temp_wid_bands_indexes <- tmp_arr_ind
                          }
                          if (length(which(check_names_quality != "") > 0)) {    # ? which quality selected ? --> store in GUI.env$temp_wid_bands_quality array
                            pos_wid <- which(check_names_quality %in% svalue(bands_wid_quality))
                            tmp_arr_qual <- array(data = 0 , dim = length(check_names_quality))
                            tmp_arr_qual[pos_wid] <- 1
                            GUI.env$temp_wid_bands_quality <- tmp_arr_qual
                            
                          }
                          
                          dispose(selgroup)			 # close layers selection child widget
                          
                        })
                        # if Cancel, reset selected layers to previous choice and exit ----
                        cancel_but <- gbutton(text = "Cancel", container = bands_group, handler = function(button,...){
                          
                          if (exists("check_wid", where = GUI.env)) {
                            GUI.env$temp_wid_bands <- check_wid
                          }
                          if (exists("check_wid_indexes", where = GUI.env)) {
                            GUI.env$temp_wid_bands_indexes <- check_wid_indexes
                          }
                          if (exists("check_wid_quality", where = GUI.env)) {
                            GUI.env$temp_wid_bands_quality <- check_wid_quality
                          }
                          dispose(selgroup)
                        })
                        
                        # Widget for "www" button ----
                        addSpring(bands_group, horizontal = TRUE)
                        www_but <- gbutton(text = "Product details", container = bands_group, handler = function(button,...) browseURL(prod_opt_list[[svalue(prod_wid)]][[svalue(vers_wid)]]$www))
                        
                        visible(selgroup, set = TRUE)    # visualize band selection widgets
                        
                      },container = satprod2_group, expand = FALSE)
  
  font(vers_label) <- font(sens_label) <- font(band_label) <- list(family = "sans",weight = "bold")
  
  #- ------------------------------------------------------------------------------- -#
  # Widgets for authentication/download mode selection
  #- ------------------------------------------------------------------------------- -#
  download_frame <- gframe(text = "<span foreground='red' size='x-large'>Download Method</span>", markup = TRUE, container = main_group, horizontal = TRUE, expand = TRUE)
  # start and end date ----
  methods_group <- ggroup(container = download_frame, horizontal = TRUE)
  
  method_lab <- glabel(text = " Download Server:", container = methods_group)
  server_wid <- gdroplist(items = c("http","ftp","offline"), text = "Select", container = methods_group, 
                          selected = match(general_opts$download_server, c("http","ftp","offline")),handler = function(h,....) {
                            current_sel <- svalue(server_wid)
                            if (current_sel != "http") {
                              enabled(authenticate_group) <- FALSE
                            } else {
                              enabled(authenticate_group) <- TRUE
                            }
                          })
  addSpace(methods_group, 20, horizontal = TRUE)
  authenticate_group = ggroup(container = methods_group)
  user_lab <- glabel(text = " User Name:", container = authenticate_group)
  user_wid <- gedit(text = general_opts$user, container = authenticate_group, width = 15)
  addSpace(authenticate_group, 20)
  password_lab <- glabel(text = " Password:", container = authenticate_group)
  password_wid <- gedit(text = general_opts$password, container = authenticate_group, width = 15)
  visible(password_wid) <- FALSE
  
  if (svalue(server_wid) != "http") {
    enabled(authenticate_group) <- FALSE
  } else {
    (enabled(authenticate_group) <- TRUE)
  }
  
  font(method_lab) <- list(family = "sans",weight = "bold")
  font(user_lab) <- font(password_lab) <- list(family = "sans",weight = "bold")
  
  #- ------------------------------------------------------------------------------- -#
  # Widgets for Dates selection
  #- ------------------------------------------------------------------------------- -#
  dates_frame <- gframe(text = "<span foreground='red' size='x-large'>Processing Period</span>", markup = TRUE, container = main_group, horizontal = TRUE, expand = TRUE)
  # start and end date ----
  dates_group <- ggroup(container = dates_frame, horizontal = TRUE)
  
  start_date_lab <- glabel(text = " Starting Date (yyyy-mm-dd):  ", container = dates_group)
  start_date_wid <- gedit(text = general_opts$start_date, container = dates_group, width = 15)
  addSpace(dates_group,9)
  end_date_lab <- glabel(text = " Ending Date (yyyy-mm-dd):  ", container = dates_group)
  end_date_wid <- gedit(text = general_opts$end_date, container = dates_group, width = 15)
  
  font(start_date_lab) <- font(end_date_lab) <- list(family = "sans",weight = "bold")
  
  #- ------------------------------------------------------------------------------- -#
  # Widgets for Tiles selection
  #- ------------------------------------------------------------------------------- -#
  Spatial_Frame <- gframe(text = "<span foreground='red' size='x-large'>Spatial Extent </span>", markup = TRUE,
                          container = main_group, horizontal = FALSE, expand = TRUE)
  output_ext_group <- ggroup(container = Spatial_Frame, horizontal = TRUE)
  output_ext_lab <- glabel(text = " Output Extent:",
                           container = output_ext_group)
  font(output_ext_lab) <- list(family = "sans",weight = "bold")
  #	size(output_ext_lab) <- c(120,20)
  output_ext_wid <-  gcombobox(c("Full Tiles Extent","Resized"), container = output_ext_group,
                               selected = match(general_opts$full_ext, c("Full Tiles Extent","Resized")), handler = function(h,....) {
                                 current_sel <- svalue(output_ext_wid)
                                 # On "Full Tiles Extent" selection, disable the bbox fields
                                 if (current_sel == "Full Tiles Extent") {
                                   enabled(bbox_group) <- F
                                   enabled(tiles_from_bbox) <- F
                                   enabled(bbox_from_file) <- F
                                 } else {# On "Resized" selection, enable the bbox fields
                                   (enabled(bbox_group) <- T)
                                   enabled(tiles_from_bbox) <- T
                                   enabled(bbox_from_file) <- T
                                 }
                               })
  
  
  # button to retrieve tiles from bounding box ----
  
  
  ## Function to update the selected tiles with the intersection with the bounding box
  update_tiles <- function(bbox,...) {
    bbox_mod <- reproj_bbox(bbox, svalue(output_proj4_wid), MOD_proj_str, enlarge = TRUE)
    d_bbox_mod_tiled <- crop(modis_grid,extent(bbox_mod))
    svalue(start_x_wid)  <- min(d_bbox_mod_tiled$H)
    svalue(end_x_wid)  <- max(d_bbox_mod_tiled$H)
    svalue(start_y_wid) <- min(d_bbox_mod_tiled$V)
    svalue(end_y_wid) <- max(d_bbox_mod_tiled$V)
  }
  
  if (!exists("modis_grid")) {
    modis_grid <- get(load(file.path(MODIStsp_dir, "ExtData/MODIS_Tiles.RData")))
  }  # Laod MODIS grid ancillary file
  tiles_from_bbox <- gbutton(text = "Retrieve Tiles from bounding box", border = TRUE,
                             handler = function(h,...) {
                               bbox <- as.numeric(c(svalue(output_ULeast_wid),svalue(output_LRnorth_wid),
                                                    svalue(output_LReast_wid),svalue(output_ULnorth_wid)))
                               # Check if bbox is consistent
                               
                               n_bbox_compiled <- length(which(is.finite(bbox)))
                               if (svalue(output_ext_wid) != "Full Tiles Extent" & n_bbox_compiled == 0) {
                                 gmessage("Please specify an output bounding box!", title = "Warning") #; check_bbox <- FALSE
                               } else if (svalue(proj_wid) == "User Defined" &  nchar(svalue(output_proj4_wid)) == 0) {
                                 gmessage("Please specify an output projection", title = "Warning") #; check_bbox <- FALSE
                               } else if (n_bbox_compiled < 4) {
                                 gmessage("Error in Selected Output extent", title = "Warning") #; check_bbox <- FALSE
                               } else if (bbox[1] > bbox[3] | bbox[2] > bbox[4]) {
                                 gmessage("Error in Selected Output extent", title = "Warning") #; check_bbox <- FALSE
                               } else {
                                 update_tiles(bbox) # If all checks pass, retrieve the tiles
                               }
                             }, container = output_ext_group )
  if (svalue(output_ext_wid) != "Full Tiles Extent") {
    enabled(tiles_from_bbox) <- T
  } else {
    (enabled(tiles_from_bbox) <- F)
  }
  
  
  # Button to loAD extent from SHP or KML file ----
  
  bbox_from_file <- gbutton(text = "Load Extent from a spatial file", border = TRUE,
                            handler = function(h,...) {
                              choice <- gfile(type = "open", text = "Select a vector or raster file", # File selection widget
                                              filter = list( "All files" = list(patterns = "*"),
                                                             "Vector layers" = list(patterns = c("*.shp","*.kml")), # TODO add formats to the lists!
                                                             "Raster layers" = list(patterns = c("*.tif","*.dat"))))
                              if (!is.na(choice)) {
                                # Show window until the process had finished
                                wait_window <- gwindow(title = "Please wait", container = TRUE, width = 400, height = 40)
                                size(wait_window) <- c(100,8)
                                addHandlerUnrealize(wait_window, handler = function(h,...) {return(TRUE)})
                                wait_window_lab <- glabel(text = paste("Retrieving the Extent, please wait..."), editable = FALSE,
                                                          container = wait_window)
                                
                                # Convert bbox coordinates in those of output projection
                                out_proj_crs <- if (svalue(proj_wid) != "User Defined") {
                                  out_proj_list[[svalue(proj_wid)]]
                                } else {
                                  general_opts$user_proj4
                                }
                                
                                # Create the bounding box in the chosen projection retrieving it from the specified file
                                bbox_out <- try(bbox_from_file(file_path = choice,out_crs = out_proj_crs),silent = TRUE)
                                if (class(bbox_out) == "try-error") {
                                  gmessage(bbox_out, title = "Error")
                                } else {
                                  # set bbox according to shape
                                  svalue(output_ULeast_wid) <- formatC(bbox_out[1,1], digits = ifelse(units == "deg",4,1), format = "f")
                                  svalue(output_ULnorth_wid) <- formatC(bbox_out[2,2], digits = ifelse(units == "deg",4,1), format = "f")
                                  svalue(output_LReast_wid) <- formatC(bbox_out[1,2], digits = ifelse(units == "deg",4,1), format = "f")
                                  svalue(output_LRnorth_wid) <- formatC(bbox_out[2,1], digits = ifelse(units == "deg",4,1), format = "f")
                                  # Set tiles according with the bounding box
                                  update_tiles(bbox_out)
                                }
                                
                                dispose(wait_window)
                              }
                              
                            }, container = output_ext_group)
  
  if (svalue(output_ext_wid) != "Full Tiles Extent") {
    enabled(bbox_from_file) <- T
  } else {
    (enabled(bbox_from_file) <- F)
  }
  
  
  # Group to select which tiles should be downloaded ----
  spatial_group <- ggroup(container = Spatial_Frame, horizontal = TRUE)
  tiles_group <- gframe(text = "<b><i> Required MODIS Tiles </i></b>", markup = TRUE,
                        container = spatial_group, horizontal = FALSE, expand = FALSE, pos = 0.5)
  
  # horizontal ----
  x_group <- ggroup(container = tiles_group, horizontal = TRUE,spacing = 5)
  start_x_lab <- glabel(text = " Horizontal:", container = x_group)
  start_x_start <- glabel(text = "Start", container = x_group)
  start_x_wid <- gspinbutton(1, 35, text = "Select", container = x_group, value = general_opts$start_x)
  end_x_lab <- glabel(text = "End", container = x_group)
  end_x_wid <- gspinbutton(1, 35, text = "Select", container = x_group, value = general_opts$end_x)
  
  # map button
  # tileb_group <- ggroup(container = tiles_group, horizontal = TRUE ,spacing = 10)
  # addSpring(tileb_group, horizontal=TRUE)
  show_map <- gbutton(text = "Show Tiles Map", border = TRUE,
                      handler = function(h,....) {
                        dev.new(width = 8, height = 4.8, noRStudioGD = TRUE)
                        plot(raster(file.path(MODIStsp_dir, "/ExtData/MODIS_Tiles.gif")))
                      },
                      container = x_group )
  
  # vertical ----
  y_group <- ggroup(container = tiles_group, horizontal = TRUE ,spacing = 5)
  start_y_lab <- glabel(text = " Vertical:    ", container = y_group)
  start_y_start <- glabel(text = "Start", container = y_group)
  start_y_wid <- gspinbutton(0,17, text = "Select", container = y_group, value = general_opts$start_y)
  end_y_lab <- glabel(text = "End", container = y_group)
  end_y_wid <- gspinbutton(0,17, text = "Select", container = y_group, value = general_opts$end_y)
  
  font(start_x_lab) <-  font(start_y_lab)  <- list(family = "sans",weight = "bold")
  #	size(start_y_lab) = c(120,20)
  # 	size(start_y_wid) = size(end_y_wid) = size(end_y_lab) = size(start_y_start) = c(35,25)

  if (prod_opt_list[[general_opts$sel_prod]][[general_opts$prod_version]]$tiled == 0) {
    enabled(tiles_group) <- FALSE
  } else {
    (enabled(tiles_group) <- TRUE)
  }
  
  # Extent ----
  addSpace(tiles_group, 1, horizontal=TRUE)
  addSpring(spatial_group, horizontal=TRUE)
  bounding_group <- gframe(text = "<b><i> Output Bounding Box (in output projection!) </i></b>", markup=TRUE, 
                           container = spatial_group, horizontal = FALSE, expand = FALSE,  pos = 0.5)
  font(bounding_group) <- list(family = "sans",weight = "bold")
  addSpace(bounding_group, 1, horizontal=TRUE)
  
  # bounding box ----
  bbox_group <- ggroup(horizontal = TRUE, container = bounding_group)
  # output_bbox_lab <- glabel(text = " Bounding Box for output images <span weight='bold'>(in output projection!)</span>", 
  #                           markup=TRUE, container = bbox_group, expand = TRUE)
  #	size(output_ext_lab) <- c(120,15)
  
  min_group <- ggroup(horizontal = FALSE, container = bbox_group)
  latW_group <- ggroup(horizontal=TRUE, container=min_group)
  addSpring(latW_group, horizontal = TRUE)
  output_ULeast_lab <- glabel("Left East. (xmin)", container = latW_group)
  output_ULeast_wid <- gedit(text = general_opts$bbox[1], container = latW_group, width = 10)
  longS_group <- ggroup(horizontal=TRUE, container=min_group)
  addSpring(longS_group, horizontal = TRUE)
  output_LRnorth_lab <- glabel("Lower North. (ymin)", container = longS_group)
  output_LRnorth_wid <- gedit(text = general_opts$bbox[2], container = longS_group, width = 10)
  #	size(output_ULeast_lab) = size(output_LReast_lab) <- c(160,20)
  
  addSpace(bbox_group, 10, horizontal = TRUE)
  
  max_group <- ggroup(horizontal = FALSE, container = bbox_group)
  latE_group <- ggroup(horizontal=TRUE, container=max_group)
  addSpring(latE_group, horizontal = TRUE)
  output_LReast_lab <- glabel("Right East. (xmax)", container = latE_group)
  output_LReast_wid <- gedit(text = general_opts$bbox[3], container = latE_group, width = 10)
  longN_group <- ggroup(horizontal=TRUE, container=max_group)
  addSpring(longN_group, horizontal = TRUE)
  output_ULnorth_lab <- glabel("Upper North. (ymax)", container = longN_group)
  output_ULnorth_wid <- gedit(text = general_opts$bbox[4], container = longN_group, width = 10)
  
  font(output_ULeast_lab) <- font(output_LReast_lab) <-
    font(output_LRnorth_lab) <- font(output_ULnorth_lab) <- list(family = "sans")
  #	size(output_LRnorth_lab) = size(output_ULnorth_lab) = c(160,20)
  
  if (general_opts$full_ext == "Full Tiles Extent") {
    enabled(bbox_group) <- FALSE
  } else {
    enabled(bbox_group) <- TRUE
  }  # Grey out bbox if NAtive extent
  
  #- ------------------------------------------------------------------------------- -#
  # Widgets for Projection, resolution and bbox selection
  #- ------------------------------------------------------------------------------- -#
  output_proj_frame <- gframe(text = "<span foreground='red' size='x-large'>Reprojection and Resize Options </span>", markup = T,
                              container = main_group, horizontal = FALSE, expand = TRUE)
  
  output_proj_group <- ggroup(container = output_proj_frame, horizontal = TRUE)
  
  # Projection ----
  output_proj_lab <- glabel(text = " Output Projection:  ", container = output_proj_group)
  font(output_proj_lab) <- list(family = "sans",weight = "bold")
  #	size(output_proj_lab) = c(120,20)
  proj_wid <- gcombobox(out_proj_names, container = output_proj_group,
                        selected = match(general_opts$proj, out_proj_names), handler = function(h,....) {
                          
                          current_sel <- svalue(proj_wid)
                          GUI.env$old_proj4 <- svalue(output_proj4_wid)
                          
                          if (current_sel != "User Defined") {
                            enabled(output_proj4_wid) <- FALSE
                            enabled(change_proj_but) <- FALSE
                            
                            svalue(output_proj4_wid) <- out_proj_list[[svalue(proj_wid)]]
                            sel_output_proj <- CRS(svalue(output_proj4_wid))
                            # Get the units and kind of proj
                            
                            proj <- head(strsplit(tail(strsplit(sel_output_proj@projargs, "+proj=")[[1]],1)," +")[[1]],1)
                            m_units <- ifelse(length(strsplit(sel_output_proj@projargs, "+units=")[[1]]) > 1,
                                              head(strsplit(tail(strsplit(sel_output_proj@projargs, "+units=")[[1]],1)," +")[[1]],1),
                                              "Unknown")
                            units <- ifelse(proj == "longlat", "dec. degrees",m_units)
                            svalue(pixsize2_lab) <- units
                            #----- If valid proj4string, and output is a bounding box, recompute the bounding box in output proj coordinates
                            if (svalue(output_ext_wid) != "Full Tiles Extent") {
                              bbox_in <- as.numeric(c(svalue(output_ULeast_wid),svalue(output_LRnorth_wid),svalue(output_LReast_wid),svalue(output_ULnorth_wid)))
                              bbox_out <- reproj_bbox(bbox_in, GUI.env$old_proj4, sel_output_proj@projargs, enlarge = FALSE)
                              
                              svalue(output_ULeast_wid)  <- formatC(bbox_out[1,1], digits = ifelse(units == "dec. degrees",4,1), format = "f")
                              svalue(output_ULnorth_wid) <- formatC(bbox_out[2,2], digits = ifelse(units == "dec. degrees",4,1), format = "f")
                              svalue(output_LReast_wid)  <- formatC(bbox_out[1,2], digits = ifelse(units == "dec. degrees",4,1), format = "f")
                              svalue(output_LRnorth_wid) <- formatC(bbox_out[2,1], digits = ifelse(units == "dec. degrees",4,1), format = "f")
                            }
                          }
                          
                          else {
                            old_sel_projwid <- hash::keys(out_proj_list)[which(hash::values(out_proj_list) == GUI.env$old_proj4)]    # Retrieve previous selection of proj_wid
                            (enabled(output_proj4_wid) <- F)
                            (enabled(change_proj_but) <- T)
                            selproj <- ginput(message = "Please Insert a valid Proj4 string				", parent = NULL, do.buttons = TRUE, size = 800, horizontal = TRUE)
                            if (!is.na(selproj)) {
                              sel_output_proj <- try(CRS(selproj),silent = TRUE)
                              if (class(sel_output_proj) == "try-error") {  # On error, send out a message and reset proj_wid and proj4 wid to previous values
                                gmessage(sel_output_proj, title = "Proj4 String Not Recognized")
                                svalue(output_proj4_wid) <- GUI.env$old_proj4
                                svalue(proj_wid) <- old_sel_projwid
                                
                              } else {
                                old_proj <- svalue(output_proj4_wid)
                                svalue(output_proj4_wid) <- sel_output_proj
                                #----- If valid proj4string, and output is a bounding box, recompute the bounding box in output proj coordinates
                                
                                bbox_in <- as.numeric(c(svalue(output_ULeast_wid),svalue(output_LRnorth_wid),svalue(output_LReast_wid),svalue(output_ULnorth_wid)))
                                bbox_out <- reproj_bbox(bbox_in, GUI.env$old_proj4, sel_output_proj@projargs, enlarge = FALSE)
                                
                                # Get the units and kind of proj
                                
                                proj <- head(strsplit(tail(strsplit(sel_output_proj@projargs, "+proj=")[[1]],1)," +")[[1]],1)
                                m_units <- ifelse(length(strsplit(sel_output_proj@projargs, "+units=")[[1]]) > 1,
                                                  head(strsplit(tail(strsplit(sel_output_proj@projargs, "+units=")[[1]],1)," +")[[1]],1),
                                                  "Unknown")
                                units <- ifelse(proj == "longlat", "dec. degrees",m_units)
                                svalue(pixsize2_lab) <- units
                                
                                svalue(output_ULeast_wid) <- formatC(bbox_out[1,1], digits = ifelse(units == "dec. degrees",4,1), format = "f")
                                svalue(output_ULnorth_wid) <- formatC(bbox_out[2,2], digits = ifelse(units == "dec. degrees",4,1), format = "f")
                                svalue(output_LReast_wid) <- formatC(bbox_out[1,2], digits = ifelse(units == "dec. degrees",4,1), format = "f")
                                svalue(output_LRnorth_wid) <- formatC(bbox_out[2,1], digits = ifelse(units == "dec. degrees",4,1), format = "f")
                              }
                            }
                          }
                        })
  #	size(proj_wid) = c(120,20)
  
  # addSpring(output_proj_group, horizontal = TRUE)
  outproj_user_lab <- glabel(text = "  PROJ4 String:", container = output_proj_group)
  font(outproj_user_lab) <- list(family = "sans",weight = "bold")
  #	size(outproj_user_lab) = c(120,20)
  
  output_proj4_wid <- gtext(text = general_opts$proj, container = output_proj_group, width = 250, height = 30,editable = FALSE, expand = TRUE)
  svalue(output_proj4_wid) <- out_proj_list[[svalue(proj_wid)]]
  #	size(output_proj4_wid) = c(250,20)
  change_proj_but <- gbutton(text = "Change", container = output_proj_group, handler = function(h,....) {  # Button to change the user define projection
    selproj <- ginput(message = "Please Insert a valid Proj4 string				", parent = NULL, do.buttons = TRUE, size = 800, horizontal = TRUE)
    if (!is.na(selproj)) {
      sel_output_proj <- try(CRS(selproj),silent = TRUE)
      if (class(sel_output_proj) == "try-error") {	 	# Check if proj4string is valid
        gmessage(sel_output_proj, title = "Proj4 String Not Recognized")
        svalue(output_proj4_wid) <- ""
      } else {
        old_proj <- svalue(output_proj4_wid)
        svalue(output_proj4_wid) <- sel_output_proj
        # Get the units and kind of proj
        
        proj <- head(strsplit(tail(strsplit(sel_output_proj@projargs, "+proj=")[[1]],1)," +")[[1]],1)
        m_units <- ifelse(length(strsplit(sel_output_proj@projargs, "+units=")[[1]]) > 1,
                          head(strsplit(tail(strsplit(sel_output_proj@projargs, "+units=")[[1]],1)," +")[[1]],1),
                          "Unknown")
        units <- ifelse(proj == "longlat", "dec. degrees",m_units)
        svalue(pixsize2_lab) <- units
        #----- If valid proj4string, and output is a bounding box, recompute the bounding box in output proj coordinates
        if (svalue(output_ext_wid) != "Full Tiles Extent") {
          bbox_in <- as.numeric(c(svalue(output_ULeast_wid), svalue(output_LRnorth_wid), svalue(output_LReast_wid), svalue(output_ULnorth_wid)))
          bbox_out <- reproj_bbox(bbox_in, GUI.env$old_proj4, sel_output_proj@projargs, enlarge = FALSE)
          
          svalue(output_ULeast_wid) <- formatC(bbox_out[1,1], digits = ifelse(units == "dec. degrees",4,1), format = "f")
          svalue(output_ULnorth_wid) <- formatC(bbox_out[2,2], digits = ifelse(units == "dec. degrees",4,1), format = "f")
          svalue(output_LReast_wid) <- formatC(bbox_out[1,2], digits = ifelse(units == "dec. degrees",4,1), format = "f")
          svalue(output_LRnorth_wid) <- formatC(bbox_out[2,1], digits = ifelse(units == "dec. degrees",4,1), format = "f")
        }
      }
    }
  })
  size (change_proj_but) <- c(58, 30)
  if (general_opts$proj == "User Defined") {
    
    enabled(output_proj4_wid) <- TRUE
    enabled(change_proj_but) <- TRUE
    
  } else {
    (enabled(change_proj_but) <- FALSE)
    (enabled(output_proj4_wid) <- FALSE)
  }
  
  
  # Resolution ----
  output_res_group <- ggroup(container = output_proj_frame, horizontal = TRUE)
  output_res_lab <- glabel(text = " Output Resolution: ", container = output_res_group)
  font(output_res_lab) <- list(family = "sans",weight = "bold")
  #	size(output_res_lab) = c(120,20)
  
  output_res_sel_wid  <-  gcombobox(c("Native","Resampled"), container = output_res_group,
                                    selected = match(general_opts$out_res_sel, c("Native","Resampled")), handler = function(h,....) {
                                      current_sel <- svalue(output_res_sel_wid)
                                      if (current_sel == "Native") {
                                        enabled(output_res_wid) <- FALSE
                                        svalue(output_res_wid) <- paste("native")
                                      } else {
                                        enabled(output_res_wid) <- TRUE
                                        svalue(output_res_wid) <- ""
                                      }
                                    })
  
  size(output_res_sel_wid) <- size(proj_wid) <-list(width=120)
  
  pixsize_lab <- glabel(text = "  Pixel Size:", container = output_res_group)
  font(pixsize_lab) <- list(family = "sans",weight = "bold")
  #	size(pixsize_lab) <- c(120,20)
  
  output_res_wid <- gedit(text = general_opts$out_res , container = output_res_group, width=10)
  #	size(output_res_wid) <- c(140,20)
  #	if (general_opts$out_res_sel == "Native") { enabled(output_res_wid) <- F} else {(enabled(output_res_wid) <- T)}
  if (svalue(output_res_sel_wid) == "Native") {
    svalue(output_res_wid) <- paste("native")
    enabled(output_res_wid) <- FALSE
  } else {
    svalue(output_res_wid) <- general_opts$out_res
    enabled(output_res_wid) <- TRUE
  }
  
  sel_output_proj <- (CRS(if (svalue(proj_wid) == "User Defined") {
    svalue(output_proj4_wid)
  } else {
    out_proj_list[[svalue(proj_wid)]]
  }))
  proj <- head(strsplit(tail(strsplit(sel_output_proj@projargs, "+proj=")[[1]],1)," +")[[1]],1)
  m_units <- ifelse(length(strsplit(sel_output_proj@projargs, "+units=")[[1]]) > 1,
                    head(strsplit(tail(strsplit(sel_output_proj@projargs, "+units=")[[1]],1)," +")[[1]],1),
                    "Unknown")
  units <- ifelse(proj == "longlat", "dec. degrees",m_units)
  pixsize2_lab <- glabel(text = units, container = output_res_group)
  
  # Resampling ----
  # resopts_group <- ggroup(container = output_proj_frame, horizontal = TRUE)
  addSpring(output_res_group, horizontal = TRUE)
  resmeth_lab <- glabel(text = "    Resampling Method:  ", container = output_res_group)
  font(resmeth_lab) <- list(family = "sans",weight = "bold")
  #	size(resmeth_lab) = c(120,20)
  # resamp_array <- c("near","bilinear","cubic", "cubicspline","lanczos","average","mode")
  resamp_array <- c("near","mode")
  output_resmeth_wid <-  gcombobox(resamp_array, container = output_res_group, selected = match(general_opts$resampling, resamp_array))
  #	size(output_resmeth_wid) <- c(120,20)
  
  #- ------------------------------------------------------------------------------- -#
  # Widgets for Format and reprocess options
  #- ------------------------------------------------------------------------------- -#
  options_frame <- gframe(text = "<span foreground='red' size='x-large'>Processing Options</span>", markup = TRUE, container = main_group, expand = TRUE, horizontal = FALSE)
  opt_group <- ggroup(container = options_frame, horizontal = TRUE, expand = TRUE)
  
  # Out format ----
  format_lab <- glabel(text = " Output Files Format:    ", container = opt_group)
  font(format_lab) <- list(family = "sans",weight = "bold")
  format_wid <- gdroplist(items = c("ENVI","GTiff"), text = "Select", container = opt_group, selected = match(general_opts$out_format, c("ENVI","GTiff")),handler = function(h,....) {
    current_sel <- svalue(format_wid)
    if (current_sel != "GTiff") {
      enabled(compress_group) <- FALSE
    } else {
      (enabled(compress_group) <- TRUE)
    }
  })
  addSpace(opt_group, 80, horizontal = TRUE)
  
  # Compression ----
  compress_group = ggroup(container=opt_group, horizontal=TRUE)
  compress_dict <- c("None","PACKBITS","LZW","DEFLATE")
  names(compress_dict) <- c("None","Low (PACKBITS)","Medium (LZW)","High (DEFLATE)")
  compress_lab <- glabel(text = "          GTiff Compression: ", container = compress_group)
  font(compress_lab) <- list(family = "sans",weight = "bold")
  compress_wid <- gcombobox( names(compress_dict), container = compress_group,selected <- match(general_opts$compress, names(compress_dict)))
  if (general_opts$out_format == "GTiff") {
    enabled(compress_group) <- TRUE
  } else {
    enabled(compress_group) <- FALSE
  } # grey out compression if ENVI output
  
  # virtual and NODATA ----
  other_group <- ggroup(container = options_frame, horizontal = TRUE)
  
  timeseries_lab <- glabel(text = " Create Virtual Rasters: ", container = other_group)
  font(timeseries_lab) <- list(family = "sans",weight = "bold")
  timeseries_wid <- gcombobox( c("None","ENVI Meta Files","GDAL vrt Files","ENVI and GDAL"), container = other_group,
                               selected <- match(general_opts$ts_format, c("None","ENVI Meta Files","GDAL vrt Files","ENVI and GDAL")), handler = function(h,....) {
                                 current_sel <- svalue(timeseries_wid)
                               })
  addSpring(other_group, horizontal=TRUE)
  
  rts_lab <- glabel(text = "Create RasterStacks: ", container = other_group)
  rts_wid <- gradio(items = c("Yes","No"), text = "Select", container = other_group, selected = match(general_opts$rts, c("Yes","No")), horizontal = TRUE)
  font(rts_lab) <- list(family = "sans",weight = "bold")
  addSpace(other_group,15)
  nodata_lab <- glabel(text = "Change NODATA values: ", container = other_group)
  nodata_wid <- gradio(items = c("Yes","No"), text = "Select", container = other_group, selected = match(general_opts$nodata_change, c("Yes","No")), horizontal = TRUE)
  font(nodata_lab) <- list(family = "sans",weight = "bold")
  #- ------------------------------------------------------------------------------- -#
  # Widgets for output folders selection
  #- ------------------------------------------------------------------------------- -#
  
  # Main output folder ----
  outfold_frame <- gframe(text = "<span foreground='red' size='x-large'>Main Output Folder for Time Series storage</span>", markup = T, container = main_group, expand=TRUE, fill=TRUE)    			# Frame group
  outfold_group <- ggroup(horizontal = TRUE, container = outfold_frame, expand=TRUE, fill=TRUE)  				# Main group
  outfold_wid <- gedit(text = format(general_opts$out_folder, justify = "right"), container = outfold_group, width = 46, expand=TRUE)			# Selected file
  fold_choose <- gbutton("Browse", handler = function(h,...) {
    choice <- gfile(type = "selectdir", text = "Select the Output Folder for MODIS data...")		# File selection widget
    if (!is.na(choice)) {
      svalue(outfold_wid) <- choice						## On new selection, set value of the label widget
      general_opts$out_folder <- format(choice, justify = "left")	# 	On new selection,  Set value of the selected variable
    }
  }, container = outfold_group)
  
  # Reprocessing options checkbox ----
  addSpace(outfold_group, 5, horizontal = TRUE)
  reprocess_lab <- glabel(text = "ReProcess Existing Data: ", container = outfold_group)
  font(reprocess_lab) <- list(family = "sans",weight = "bold")
  reprocess_wid <- gradio(items = c("Yes","No"), text = "Select", container = outfold_group, selected = match(general_opts$reprocess, c("Yes","No")), horizontal = TRUE)
  
  
  # HDF output folder ----
  outfoldmod_frame <- gframe(text = "<span foreground='red' size='x-large'>Output Folder for Original HDF files download</span>", markup = TRUE, container = main_group, expand=TRUE, fill=TRUE)    			# Frame group
  outfoldmod_group <- ggroup(horizontal = TRUE, container = outfoldmod_frame, expand=TRUE, fill=TRUE)  				# Main group
  outfoldmod_wid <- gedit(text = format(general_opts$out_folder_mod, justify = "right") , container = outfoldmod_group, width = 46, expand=TRUE)			# Selected file
  fold_choose <- gbutton("Browse", handler = function(h,...) {
    choice <- gfile(type = "selectdir", text = "Select the Output Folder for storage of original HDFs...")		# File selection widget
    if (!is.na(choice)) {
      svalue(outfoldmod_wid) <- choice						## On new selection, set value of the label widget
      general_opts$out_folder_mod <- format(choice, justify = "left")	# 	On new selection,  Set value of the selected variable
    }
  }, container = outfoldmod_group)
  
  
  # HDF delete option checkbox ----
  addSpace(outfoldmod_group, 5, horizontal = TRUE)
  delete_lab <- glabel(text = "Delete original HDF files: ", container = outfoldmod_group)
  font(delete_lab) <- list(family = "sans",weight = "bold")
  delete_wid <- gradio(items = c("Yes","No"), text = "Select", container = outfoldmod_group, selected = 2, horizontal = T)
  
  
  #- ------------------------------------------------------------------------------- -#
  # Start/Quit/Save/Load buttons
  #- ------------------------------------------------------------------------------- -#
  
  # Various checks and generation of general_opts common to both Start and Save buttons
  prepare_to_save_options <- function(general_opts,GUI.env,...) {
    
    # workaround to retrieve custom index
    general_opts$custom_indexes <- RJSONIO::fromJSON(previous_jsfile)$custom_indexes
    
    general_opts$sel_prod <- mod_prod_list[which(mod_prod_list == svalue(prod_wid))]						# Products options
    general_opts$prod_version <- prod_opt_list[[general_opts$sel_prod]][[which(sapply(prod_opt_list[[general_opts$sel_prod]],function(x){x$v_number}) == svalue(vers_wid))]]$v_number
    # sel_prod = general_opts$sel_prod    # When saving, set sel_prod to current selection (may be good to change)
    general_opts$sensor <- svalue(sens_wid)
    
    if (exists("temp_wid_bands", where = GUI.env)) {
    
      general_opts$bandsel <- GUI.env$temp_wid_bands			#retrieve selected bands
      
    }
    if (exists("temp_wid_bands_indexes", where = GUI.env)) {
      general_opts$indexes_bandsel <- GUI.env$temp_wid_bands_indexes #retrieve selected indexes
      
    }
    if (exists("temp_wid_bands_indexes", where = GUI.env)) {
      general_opts$quality_bandsel <- GUI.env$temp_wid_bands_quality 	#retrieve selected quality ind.
      
    }
    
    general_opts$user = svalue(user_wid)
    general_opts$password = svalue(password_wid)
    general_opts$download_server = svalue(server_wid)
    
    general_opts$start_date <- svalue(start_date_wid)
    general_opts$end_date <- svalue(end_date_wid)
    
    general_opts$start_x <- svalue(start_x_wid)		# Retrieve Tiles options
    general_opts$end_x <- svalue(end_x_wid)
    general_opts$start_y <- svalue(start_y_wid)
    general_opts$end_y <- svalue(end_y_wid)
    
    general_opts$proj <- svalue(proj_wid)		# Retrieve Proj and extent options
    general_opts$user_proj4 <- svalue(output_proj4_wid)
    general_opts$out_res_sel <- (svalue(output_res_sel_wid))
    general_opts$out_res <- (svalue(output_res_wid))
    general_opts$resampling <- svalue(output_resmeth_wid)
    general_opts$full_ext <- svalue(output_ext_wid)
    general_opts$bbox <- ((c(svalue(output_ULeast_wid),svalue(output_LRnorth_wid),
                             svalue(output_LReast_wid),svalue(output_ULnorth_wid))))
    
    general_opts$reprocess <- svalue(reprocess_wid)  # Retrieve reprocess, delete and nodata
    general_opts$delete_hdf <- svalue(delete_wid)
    
    general_opts$nodata_change <- svalue(nodata_wid)
    general_opts$rts <- svalue(rts_wid)
    
    general_opts$out_format <- svalue(format_wid)   # Retrieve format, virtual and compression
    general_opts$ts_format <- svalue(timeseries_wid)
    general_opts$compress <- compress_dict[svalue(compress_wid)]
    
    general_opts$out_folder <- svalue(outfold_wid)		# # Retrieve Folder options
    general_opts$out_folder_mod <- svalue(outfoldmod_wid)
    
    GUI.env$check_save_opts <- TRUE
    # Send warning if HDF deletion selected
    if (general_opts$delete_hdf == "Yes") {
      GUI.env$check_save_opts <- gconfirm("Warning! HDF files in Original MODIS folder will be deleted at the end of processing! Are you sure? ", title = "Warning", icon = "warning")
    }
    
    #- Perform checks on options consistency ---------------
    
    # Check if at least 1 layer selected
    
    if (max(general_opts$bandsel) +
        ifelse((length(general_opts$indexes_bandsel) > 0), max(general_opts$indexes_bandsel),0) +
        max(general_opts$quality_bandsel) == 0) {
      gmessage("No Output bands or indexes selected - Please Correct!", title = "Warning")
      GUI.env$check_save_opts <- FALSE
    }
    
    # Check if dates, processing extent and tiles selection make sense
    if (as.Date(general_opts$start_date) > as.Date(general_opts$end_date)) {
      gmessage("Ending date earlier than starting date - Please correct!", title = "Warning")
      GUI.env$check_save_opts <- FALSE
    }
    
    if (class(try(as.Date(general_opts$start_date), silent = TRUE)) == "try-error" | class(try(as.Date(general_opts$end_date), silent = TRUE)) == "try-error") {
      gmessage("One or both dates are not in correct format - Please correct!", title = "Warning")
      GUI.env$check_save_opts <- FALSE
    }
    
    if ((general_opts$start_x > general_opts$end_x ) | (general_opts$start_y > general_opts$end_y )) {
      gmessage("Error in Selected Tiles", title = "Warning")
      GUI.env$check_save_opts <- FALSE
    }
    
    # Check if bbox is consistent
    
    suppressWarnings(general_opts$bbox <- as.numeric(general_opts$bbox))
    
    general_opts$bbox <- as.numeric(general_opts$bbox)
    n_bbox_compiled <- length(which(is.finite(general_opts$bbox)))
    if (general_opts$full_ext == "Resized") {
      if (n_bbox_compiled == 4) {
        if ( (general_opts$bbox[1] > general_opts$bbox[3]) | (general_opts$bbox[2] > general_opts$bbox[4])) {
          gmessage("Error in Selected Output extent", title = "Warning")
          GUI.env$check_save_opts <- FALSE
        }
      }
      if ((n_bbox_compiled < 4) & (n_bbox_compiled >= 0)) {
        gmessage("Error in Selected Output extent", title = "Warning")
        GUI.env$check_save_opts <- FALSE
      }
    }
    
    # Check if selected tiles are consistent with the bounding box
    if (general_opts$full_ext == "Resized" & GUI.env$check_save_opts == TRUE) {
      bbox_mod <- reproj_bbox( general_opts$bbox, svalue(output_proj4_wid), MOD_proj_str, enlarge = TRUE)
      d_bbox_mod_tiled <- crop(modis_grid,extent(bbox_mod))
      required_tiles <- paste0("H",apply(expand.grid("H" = min(d_bbox_mod_tiled$H):max(d_bbox_mod_tiled$H),
                                                     "V" = min(d_bbox_mod_tiled$V):max(d_bbox_mod_tiled$V)), 1, paste, collapse = "_V"))
      selected_tiles <- paste0("H",apply(expand.grid("H" = svalue(start_x_wid):svalue(end_x_wid),
                                                     "V" = svalue(start_y_wid):svalue(end_y_wid)), 1, paste, collapse = "_V"))
      
      # If the bounding box does not interrsect with the tiles, return an error
      if (!any(required_tiles %in% selected_tiles)) {
        GUI.env$check_save_opts <- gconfirm(paste("The selected tiles does not intersect the output bounding box. Do you want to automatically retrieve the required tiles?"),
                                     handler = function(h,...) {
                                       selected_tiles <<- required_tiles
                                       general_opts$start_x <<- min(d_bbox_mod_tiled$H)
                                       general_opts$end_x <<- max(d_bbox_mod_tiled$H)
                                       general_opts$start_y <<- min(d_bbox_mod_tiled$V)
                                       general_opts$end_y <<- max(d_bbox_mod_tiled$V)
                                     }, title = "Warning")
      }
      
      # If not all the required tiles are selected, ask to select them
      if (!all(required_tiles %in% selected_tiles) & GUI.env$check_save_opts) {
        gconfirm(paste("The following tiles not currently selected are required to cover the output bounding box (",paste(required_tiles[!(required_tiles %in% selected_tiles)],collapse = ", "),
                       "). Do you want to add them to the processing? Otherwise, nodata will be produced in the non-covered area."),
                 handler = function(h,...) {
                   selected_tiles <<- required_tiles
                   general_opts$start_x <<- min(d_bbox_mod_tiled$H)
                   general_opts$end_x <<- max(d_bbox_mod_tiled$H)
                   general_opts$start_y <<- min(d_bbox_mod_tiled$V)
                   general_opts$end_y <<- max(d_bbox_mod_tiled$V)
                 }, title = "Warning")
      }
      
      # If some selected tiles are not useful, ask to remove them
      if (!all(selected_tiles %in% required_tiles) & GUI.env$check_save_opts) {
        gconfirm(paste("The following tiles are not required to cover the output bounding box (",paste(selected_tiles[!(selected_tiles %in% required_tiles)], collapse = ", "),
                       "). Do you want to remove them from processing?"),
                 handler = function(h,...) {
                   selected_tiles <<- required_tiles
                   general_opts$start_x <<- min(d_bbox_mod_tiled$H)
                   general_opts$end_x <<- max(d_bbox_mod_tiled$H)
                   general_opts$start_y <<- min(d_bbox_mod_tiled$V)
                   general_opts$end_y <<- max(d_bbox_mod_tiled$V)
                 }, title = "Warning")
      }
      
    }
    
    # check if folders are defined
    if (general_opts$out_folder == "" & GUI.env$check_save_opts) {
      gmessage("Please Select an output folder!", title = "Warning")
      GUI.env$check_save_opts <- FALSE
    }
    if (general_opts$out_folder_mod == "" & GUI.env$check_save_opts) {
      gmessage("Please Select an output folder for storing original HDFs!", title = "Warning")
      GUI.env$check_save_opts <- FALSE
    }
    
    if (general_opts$resampling == "mode" & GUI.env$check_save_opts) {
      check_mode = gconfirm(paste("Warning! You selected 'mode' resampling. Be aware that 'mode'", 
                                  "resampling can provide inconsistent results in areas affected",
                                  "by mixed high and low quality data, and in properly keeping track",
                                  "of quality indicators! Do you wish to continue?"), title = "Warning")
      if (check_mode == FALSE) {
        GUI.env$check_save_opts <- FALSE
      }
    }
    
    # check that user/password were provided in case of html download
    if ((general_opts$user == "" | general_opts$password == "") & general_opts$download_server=="http") {
      gmessage("Username and password are mandatory in case of 'http' download! Please provide them or choose 'ftp' download.", title = "Warning")
      GUI.env$check_save_opts <- FALSE
    }
    
    return(general_opts)
    
  }
  
  
  but_group <- ggroup(container = main_group, horizontal = TRUE)
  
  start_but <- gbutton(text = "Start Processing", container = but_group, handler = function(h,....) {# If "Start" pressed, retrieve selected values and save in previous file
    general_opts <- prepare_to_save_options(general_opts, GUI.env)
    if (GUI.env$check_save_opts) {					# If check passed, save previous file and return
      write(RJSONIO::toJSON(general_opts),previous_jsfile)
      # assign("Quit", F, envir = globalenv()) # If "Start", set "Quit to F
      GUI.env$Quit <- FALSE
      # rm(GUI.env$temp_wid_bands, envir = globalenv())
      # rm(GUI.env$temp_wid_bands_indexes, envir = globalenv())
      # rm(GUI.env$temp_wid_bands_indexes, envir = globalenv())
      dispose(main_win)
      
    }
  })
  
  # On "Quit", exit
  quit_but <- gbutton(text = "Quit Program", container = but_group, handler = function(h,...) { # If "Quit", set "Quit to T and exit
    # assign("Quit", TRUE, envir = globalenv())
    GUI.env$Quit <- TRUE
    dispose(main_win)
    
  })
  
  addSpring(but_group, horizontal = TRUE)
  
  # On "Load", ask for a old options file and load it --------
  load_but <- gbutton(text = "Load Options", container = but_group, handler = function(h,....){
    
    choice <- gfile(type = "open", text = "Select file for loading processing options...")	# ask for file
    
    continue_load <- if (length(grep("\\.json$",choice))==0) {
      gconfirm("The selected file does not seem to be a JSON file. Do you want to continue?", title = "Warning", icon = "warning")
    } else {
      TRUE
    }
    if (continue_load) {
      
      general_opts <- RJSONIO::fromJSON(choice)  # load file and reset all widgets to values found in the loaded file
      svalue(cat_wid)  <- with(prod_opt_list[[general_opts$sel_prod]][[general_opts$prod_version]],paste(cat01,cat02,sep=" - "))
      svalue(prod_wid) <- general_opts$sel_prod
      svalue(vers_wid) <- general_opts$prod_version
      svalue(sens_wid) <- general_opts$sensor
      
      GUI.env$temp_wid_bands <- general_opts$bandsel				# set dummy variables holding the initial values of selected bands
      GUI.env$temp_wid_bands_indexes <- general_opts$indexes_bandsel
      GUI.env$temp_wid_bands_quality <- general_opts$quality_bandsel
      
      # svalue(start_day_wid) <- general_opts$start_day
      # svalue(start_month_wid) <- general_opts$start_month
      # svalue(start_year_wid) <- general_opts$start_year
      # svalue(end_day_wid) <- general_opts$end_day
      # svalue(end_month_wid) <- general_opts$end_month
      # svalue(end_year_wid) <- general_opts$end_year
      svalue(server_wid) = general_opts$download_server
      svalue(user_wid) = general_opts$user
      svalue(password_wid) = general_opts$password
      
      svalue(start_date_wid) <- general_opts$start_date # Dates options
      svalue(end_date_wid) <- general_opts$end_date
      
      svalue(start_x_wid) <- general_opts$start_x  		# Tiles options
      svalue(end_x_wid) <- general_opts$end_x
      svalue(start_y_wid) <- general_opts$start_y
      svalue(end_y_wid) <- general_opts$end_y
      
      svalue(proj_wid) <- general_opts$proj 	# Proj and extent options
      svalue(output_proj4_wid) <- general_opts$user_proj4
      svalue(output_res_sel_wid) <- general_opts$out_res_sel
      svalue(output_res_wid) <- general_opts$out_res
      svalue(output_resmeth_wid) <- general_opts$resampling
      svalue(output_ext_wid) <- general_opts$full_ext
      svalue(output_ULeast_wid) <- general_opts$bbox[1]
      svalue(output_LReast_wid) <- general_opts$bbox[3]
      svalue(output_LRnorth_wid) <- general_opts$bbox[2]
      svalue(output_ULnorth_wid) <- general_opts$bbox[4]
      svalue(reprocess_wid) <- general_opts$reprocess
      svalue(delete_wid) <- general_opts$delete_hdf
      svalue(nodata_wid) <- general_opts$nodata_change
      
      
      svalue(format_wid) <- general_opts$out_format
      svalue(timeseries_wid) <- general_opts$ts_format
      svalue(compress_wid) <- general_opts$compress
      
      svalue(outfold_wid) <- 	general_opts$out_folder # Folder options
      svalue(outfoldmod_wid) <- general_opts$out_folder_mod
    }
  })
  
  # On "Save", ask for a file name and save options (must be a JSON file !)  --------
  save_but <- gbutton(text = "Save Options", container = but_group, handler = function(h,....) {
    
    choice <- gfile(type = "save", text = "Select file for saving processing options...")		# File selection widget
    choice <- paste0(gsub("\\.json$","",choice),".json") # add file extension if missing
    
    if (!is.na(choice)) {
      general_opts <- prepare_to_save_options(general_opts, GUI.env)
      if (GUI.env$check_save_opts) {					# If check passed, save previous file and return
        write(RJSONIO::toJSON(general_opts),choice)
      }
    }
  })
  
  visible(main_win, set = TRUE) ## show the selection GUI
  return(GUI.env$Quit)
  
}  # END
