#' @title gh_sellayers
#' @description GUI Handler for events that occurr when the "Change selection"
#'  button is clicked
#' @importFrom jsonlite fromJSON
#' @importFrom gWidgets svalue gbasicdialog ggroup gframe gcheckboxgroup
#'  addSpring gbutton font visible
#' @importFrom utils browseURL
#' @noRd
#' @inheritParams MODIStsp_GUI

gh_sellayers <- function(prodopts_file,
                         opts_jsfile,
                         gui_env,
                         out_proj_list,
                         help_messages) {
  #nocov start
  prod_opt_list <- get(load(prodopts_file))
  general_opts  <- jsonlite::fromJSON(opts_jsfile)
  curr_prod     <- gWidgets::svalue(wids$prod)
  curr_vers     <- gWidgets::svalue(wids$vers)
  curr_opts     <- prod_opt_list[[curr_prod]]
  # retrieve band names available for sel. product
  check_names   <- curr_opts[[curr_vers]]$band_fullnames
  # retrieve currently selected original layers
  wids$check    <- gui_env$temp_wid_bands
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
      # find which layers selected and store in gui_env$temp_wid_bands
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
      gh_help(h, "layers_help", help_messages, NULL)
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

    wids$bands_quality <- gWidgets::gcheckboxgroup(
      items     = check_names_quality,
      checked   = as.logical(check_wid_quality),
      container = cbox_quality,
      use.table = FALSE
    )
    gWidgets::addSpring(cbox_quality)
    qi_help <- gWidgets::gbutton(
      text    = " ? ", handler = function(h, ...) {
        gh_help(h, "qi_help", help_messages, NULL)
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
  if (!is.null(check_names_indexes) & length(check_names_indexes) != 0) {
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
      gh_help(h, "si_help_addindex", help_messages, NULL)
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
  #nocov end
}
