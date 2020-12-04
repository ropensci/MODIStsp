# MODIStsp_change_options <- function(
#   options_filein  = NULL,
#   options_fileout = NULL,
#   start_date   = NULL, end_date = NULL, 
#   out_folder   = NULL,  out_folder_mod = NULL,  
#   reprocess    = FALSE, 
#   delete_hdf   = FALSE, 
#   sensor       = NULL,  
#   download_server = NULL,
#   user       = NULL,  password = NULL, 
#   start_x    = NULL,  start_y  = NULL,  
#   end_x      = NULL,  end_y    = NULL, 
#   full_ext   = NULL,  bbox     = NULL,  
#   out_res    = NULL,  
#   out_format = NULL,  compress = NULL, 
#   outproj    = NULL, 
#   nodata_change = NULL, 
#   scale_val  = NULL, 
#   rts        = NULL, 
#   bandsel    = NULL,  bandnames = NULL, 
#   indexes_bandsel   = NULL,  indexes_bandnames  = NULL, 
#   indexes_formula   = NULL,  indexes_nodata_out = NULL, 
#   quality_bandnames = NULL,  quality_bandsel    = NULL, 
#   resampling = NULL, 
#   ts_format  = NULL, 
#   use_aria       = TRUE,
#   download_range = NULL,
#   n_retries) {
#   
#   in_opts <- load_opts(options_filein)
#   
#   #- Perform checks on options consistency ---------------
#   
#   # Check if at least 1 layer selected
#   
#   if (max(in_opts$bandsel) +
#       ifelse(length(in_opts$indexes_bandsel) > 0,
#              max(in_opts$indexes_bandsel),
#              0) + max(in_opts$quality_bandsel) == 0) {
#     stop("No Output bands or indexes selected - Please Correct!")
#   }
# 
#   # Check if dates, processing extent and tiles selection make sense
#   if (as.Date(in_opts$start_date) > as.Date(in_opts$end_date)) {
#     gWidgets::gmessage(
#       message = "Ending date earlier than starting date - Please correct!",
#       title   = "Warning"
#     )
#     gui_env$check_save_opts <- FALSE
#   }
#   
#   if (
#     class(try(as.Date(in_opts$start_date),
#               silent = TRUE)) == "try-error" |
#     class(try(as.Date(in_opts$end_date),
#               silent = TRUE)) == "try-error"
#   ) {
#     gWidgets::gmessage(
#       message = "One or both dates are in wrong format - Please correct!",
#       title   = "Warning"
#     )
#     gui_env$check_save_opts <- FALSE
#   }
#   
#   if (in_opts$start_x > in_opts$end_x |
#       in_opts$start_y > in_opts$end_y) {
#     gWidgets::gmessage(message = "Error in Selected Tiles! Please correct!",
#                        title   = "Warning")
#     gui_env$check_save_opts <- FALSE
#   }
#   
#   # Check if bbox is consistent
#   suppressWarnings(in_opts$bbox <- as.numeric(in_opts$bbox))
#   in_opts$bbox <- as.numeric(in_opts$bbox)
#   n_bbox_compiled   <- length(which(is.finite(in_opts$bbox)))
#   if (in_opts$full_ext == "Resized") {
#     if (n_bbox_compiled == 4) {
#       if (in_opts$bbox[1] > in_opts$bbox[3] |
#           in_opts$bbox[2] > in_opts$bbox[4]) {
#         gWidgets::gmessage(message = "Error in Selected Output extent",
#                            title   = "Warning")
#         gui_env$check_save_opts <- FALSE
#       }
#     } else {
#       if (n_bbox_compiled >= 0) {
#         gWidgets::gmessage(
#           message = "Error in Selected Output extent. Please correct!",
#           title   = "Warning")
#         gui_env$check_save_opts <- FALSE
#       }
#     }
#   }
#   # Check if selected tiles are consistent with the bounding box
#   if (in_opts$full_ext == "Resized" & gui_env$check_save_opts) {
#     bbox_mod         <- reproj_bbox(
#       in_opts$bbox,
#       svalue(output_proj4_wid), mod_proj_str,
#       enlarge = TRUE
#     )
#     d_bbox_mod_tiled <- raster::crop(modis_grid, raster::extent(bbox_mod))
#     required_tiles   <- paste0(
#       "H",
#       apply(expand.grid("H" = min(d_bbox_mod_tiled$H):max(d_bbox_mod_tiled$H),
#                         "V" = min(d_bbox_mod_tiled$V):max(d_bbox_mod_tiled$V)
#       ), 1, paste, collapse = "_V")
#     )
#     
#     selected_tiles <- paste0(
#       "H",
#       apply(expand.grid(
#         "H" = svalue(start_x_wid):svalue(end_x_wid),
#         "V" = svalue(start_y_wid):svalue(end_y_wid)),
#         1, paste, collapse = "_V")
#     )
#     
#     # If the bounding box does not intersect with the tiles, return a warning
#     # asking to automatically retrieve from extent
#     if (!any(required_tiles %in% selected_tiles)) {
#       gui_env$check_save_opts <- gWidgets::gconfirm(
#         strwrap("The selected tiles do not intersect the output bounding
#                 box. \n\n Do you want to discard your choice and retrieve
#                 automatically the required tiles from the bounding box?",
#                 width = 200),
#         handler = function(h, ...) {
#           selected_tiles       <<- required_tiles
#           in_opts$start_x <<- min(d_bbox_mod_tiled$H)
#           in_opts$end_x   <<- max(d_bbox_mod_tiled$H)
#           in_opts$start_y <<- min(d_bbox_mod_tiled$V)
#           in_opts$end_y   <<- max(d_bbox_mod_tiled$V)
#         }
#         , title = "Warning"
#       )
#     }
#     
#     # If not all the required tiles are selected, ask to select them
#     if (!all(required_tiles %in% selected_tiles) & gui_env$check_save_opts) {
#       gWidgets::gconfirm(
#         message = strwrap(paste(
#           "The following tiles not currently selected are required to cover
#           the output bounding box (",
#           paste(required_tiles[!(required_tiles %in% selected_tiles)],
#                 collapse = ", "),
#           "). \n\n Do you want to add them to the processing? Otherwise,
#           nodata will be produced in the non-covered area.")),
#         handler = function(h, ...) {
#           selected_tiles       <<- required_tiles
#           in_opts$start_x <<- min(d_bbox_mod_tiled$H)
#           in_opts$end_x   <<- max(d_bbox_mod_tiled$H)
#           in_opts$start_y <<- min(d_bbox_mod_tiled$V)
#           in_opts$end_y   <<- max(d_bbox_mod_tiled$V)
#         }
#         , title = "question"
#           )
# }
#     
#     # If some selected tiles are not useful, ask to remove them
#     if (!all(selected_tiles %in% required_tiles) & gui_env$check_save_opts) {
#       gWidgets::gconfirm(
#         message = strwrap(paste(
#           "The following tiles are not required to cover the output
#           bounding box (",
#           paste(selected_tiles[!(selected_tiles %in% required_tiles)],
#                 collapse = ", "),
#           "). \n\n Do you want to remove them from processing?")
#         ),
#         handler = function(h, ...) {
#           selected_tiles       <<- required_tiles
#           in_opts$start_x <<- min(d_bbox_mod_tiled$H)
#           in_opts$end_x   <<- max(d_bbox_mod_tiled$H)
#           in_opts$start_y <<- min(d_bbox_mod_tiled$V)
#           in_opts$end_y   <<- max(d_bbox_mod_tiled$V)
#         }
#         , title = "Warning"
#         )
#     }
#   }
#   
#   # check if folders are defined
#   if (in_opts$out_folder == "" & gui_env$check_save_opts) {
#     gWidgets::gmessage(
#       message = "Please Select an output folder MODIStsp outputs!",
#       title   = "Warning"
#     )
#     gui_env$check_save_opts <- FALSE
#   }
#   if (in_opts$out_folder_mod == "" & gui_env$check_save_opts) {
#     gWidgets::gmessage(
#       message = "Select an output folder for storage of original HDFs!",
#       title   = "Warning")
#     gui_env$check_save_opts <- FALSE
#   }
#   
#   # Issue Warning on Mode resamling
#   if (in_opts$resampling == "mode" & gui_env$check_save_opts) {
#     check_mode <- gWidgets::gconfirm(
#       message = strwrap(
#         "Warning! You selected 'mode' resampling. Be aware that 'mode'
#           resampling can give inconsistent results in areas affected by
#           mixed high and low quality data, and fail in properly keeping
#           track of quality indicators! \n\n Do you wish to continue?",
#         width = 200),
#       title   = "Warning"
#     )
#     if (check_mode == FALSE) {
#       gui_env$check_save_opts <- FALSE
#     }
#   }
#   
#   # check that user/password were provided in case of html download
#   if (in_opts$download_server == "http" &
#       (in_opts$user == "" | in_opts$password == "") &
#       gui_env$check_save_opts
#   ) {
#     gWidgets::gmessage(
#       message = strwrap("Username and password are mandatory in case of
#                           `http` download! \n\n Please provide them or
#                            choose 'ftp' download.", width = 200),
#       title   = "Warning")
#     gui_env$check_save_opts <- FALSE
#   }
#   
#   return(in_opts)
#   #nocov end
#   ifelse(is.null)
#   
# }