#' @title gh_selcat
#' @description Handler for the actions to be taken when the category of product
#'  is changed
#' @importFrom gWidgets svalue enabled
#' @noRd
#'
gh_selcat <- function(h, wids,
                      mod_prod_list, mod_prod_cat,
                      prod_opt_list, general_opts) {
  #nocov start
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
    gWidgets::enabled(bbox_group)  <- TRUE
    gWidgets::svalue(wids$output_ext)   <- "Define Custom Area"
    gWidgets::svalue(wids$proj_choice)  <- "Native"
    gWidgets::svalue(wids$output_proj4) <-
      "+init=epsg:4008 +proj=longlat +ellps=clrk66 +no_defs"
    gWidgets::svalue(wids$output_xmin) <- -180
    gWidgets::svalue(wids$output_xmax) <-  180
    gWidgets::svalue(wids$output_ymin) <- -90
    gWidgets::svalue(wids$output_ymax) <-  90
  } else {
    gWidgets::svalue(wids$output_xmin) <- ""
    gWidgets::svalue(wids$output_xmax) <- ""
    gWidgets::svalue(wids$output_ymin) <- ""
    gWidgets::svalue(wids$output_ymax) <- ""
    if (gWidgets::svalue(wids$output_ext) == "Define Custom Area") {
      gWidgets::enabled(bbox_group)   <- TRUE
      gWidgets::enabled(tiles_group)  <- FALSE
    } else {
      gWidgets::enabled(bbox_group)   <- FALSE
      gWidgets::enabled(tiles_group)  <- TRUE
    }
    gWidgets::svalue(wids$proj_choice)    <- "Native"
    gWidgets::svalue(wids$output_proj4) <-
      "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs" #nolint
  }

  # reset dummy variables for band selection to 0 on product change
  gui_env$temp_wid_bands         <- 0
  gui_env$temp_wid_bands_indexes <- 0
  gui_env$temp_wid_bands_quality <- 0
  #nocov end
}
