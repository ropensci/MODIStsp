#' @title Add custom spectral indexes
#' @description Function used to add a user-defined Spectral Index to the
#'   default list of computable spectral indexes. Execution without the GUI
#'   (i.e., to add a new index from a script) is also possible (see examples). 
#' @details
#' - The function asks the user to provide the info related to the new desired
#'   Spectral Index using a GUI interface, checks for correctness of provided
#'   information (e.g., correct bandnames, computable formula, etc...).
#'   If the index is legit, it modifies the MODIStsp_Previous.json (or of the
#'   json file provided by the user) so to allow computation of the additional
#'   index within MODIStsp.
#' - To remove all custom-added spectral indexes, simply delete the
#'   MODIStsp_Previous.json file within the /Previous subfolder of the folder
#'   in which the package was installed, or the alternative JSON specified by
#'   the parameter "opts_jsfile".
#' - The function can be run either from within the main MODIStsp GUI,
#'   or within a stand-alone script (using GUI = FALSE). In the latter case, it 
#'   modifies either the MODIStsp_Previous.RData options file, or the
#'   options_file specified by the user to add the new index, without user
#'   interaction.
#' @param opts_jsfile `character` full path of a JSON file
#'  containing the processing options in which the new indexes has to be saved
#'  (default: MODIStsp_Previous.JSON in subfolder Previous).
#' @param prodopts_file `character`: full path of the RData file containing.
#'   if NULL, use MODIStsp_ProdOpts.RData in subfolder Previous, Default: NULL
#' @param selprod `character` Name of the product to which the new index should
#'   be added (Note: the index will be added to all other products allowing its
#'   computation !). If NULL, as in non-interactive execution, no
#'   check on available band names is skipped and the index (if valid) is
#'   added to all products supporting it, Default: NULL
#' @param selvers `character` Version of the product to which the new index
#'   should be added (Note: the index will be added to all other products
#'   allowing its computation !). If NULL, as in non-interactive execution, the
#'   check on available band names is skipped and the index (if valid) is
#'   added to all products supporting it, Default: NULL
#' @param gui `logical` if TRUE, a GUI is opened to define the new
#'    index; otherwise use the "new_indexbandname", "new_indexfullname" and
#'    "new_indexformula" parameters to define it non-interactively,
#'    Default: TRUE
#' @param new_indexbandname `character` short name (acronym) of the new
#'   spectral index (Ignored if gui == TRUE), Default: NULL
#' @param new_indexfullname `character` extended name (acronym) of the new
#'   spectral index (Ignored if gui == TRUE), Default: NULL
#' @param new_indexformula `character` string containing the formula of
#'   the new spectral indexes (Ignored if gui == TRUE). Variables allowed in
#'   the formula are the names of the bands:
#'   b1_Red, b2_NIR, b3_Blue, b4_Green, b5_SWIR, b6_SWIR and b7_SWIR.
#'   Default: NULL
#' @param new_indexnodata_out `character` nodata value to use for rasters
#'   containing the new index
#' @param MODIStsp_dir `character` main folder containing MODIStsp R files,
#'   Default: retrieved from package installation folder
#' @importFrom gWidgets gbasicdialog ggroup glabel size font gedit gbutton
#'  svalue addSpace gframe addSpring gmessage visible
#' @importFrom pacman p_load p_exists
#' @return The function is called for its side effects. On success, the
#'  MODIStsp_Previous.RData file is modified so to allow computation of the 
#'  additional indexes.
#' @author Lorenzo Busetto, phD (2014-2017) \email{lbusett@@gmail.com}
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @seealso [MODIStsp_resetindexes]
#' @export
#'
#' @examples
#' # Run the GUI to interactively define a new index
#'  \dontrun{
#'  MODIStsp_addindex()}
#'
#' # Open the GUI to define a new index and save it in index in a custom json
#' # options file.
#' \dontrun{
#' MODIStsp_addindex(opts_jsfile = "X:/yourpath/youroptions.json")
#' }
#'
#' # Define the new index in non-interactive execution, without specifying an 
#' # options file (thus modifying MODIStsp_previous.json)
#' 
#' \dontrun{
#' MODIStsp_addindex(gui = FALSE, new_indexbandname = "SSI",
#'   new_indexfullname = "Simple Useless Index",
#'   new_indexformula = "b2_NIR+b1_Red")
#' }

MODIStsp_addindex <- function(
  opts_jsfile       = NULL,
  prodopts_file       = NULL,
  selprod             = NULL,
  selvers             = NULL,
  gui                 = TRUE,
  new_indexbandname   = "",
  new_indexfullname   = "",
  new_indexformula    = "",
  new_indexnodata_out = "32767",
  MODIStsp_dir = system.file(package = "MODIStsp")) { #nolint
  
  # Initialization and retrieval of parameters ----
  if (gui) {
    #nocov start
    if (!pacman::p_exists("gWidgetsRGtk2", local = TRUE)) {
      message("Library 'gWidgetsRgtk2' is required to run MODIStsp_addindex ",
              "in interactive mode but it is not installed! \n\n",
              "Do you want to install it now?",
              type = " y / n")
      inst_gw <- readline()
      if (inst_gw == "y") {
        pacman::p_load("gWidgetsRGtk2")
      } else {
        stop("MODIStsp can not work in Interactive mode without gWidgetsRGtk2!",
             "\n Aborting!")
      }
      
    }
    options("guiToolkit" = "RGtk2")
    #nocov end
  } 
  
  previous_jsfile <- ifelse(is.null(opts_jsfile),
                            system.file("ExtData/Previous", 
                                        "MODIStsp_Previous.json",
                                        package = "MODIStsp"),
                            opts_jsfile)
  prodopts_file   <- ifelse(is.null(prodopts_file),
                            system.file("ExtData/Previous",
                                        "MODIStsp_ProdOpts.RData", 
                                        package = "MODIStsp"),
                            prodopts_file)
  
  general_opts    <- load_opts(previous_jsfile)
  
  # Restore MODIS products if existing, otherwise retrieve  from xml file ----
  prod_opt_list <- get(load(prodopts_file))
  #how many product available ? = elements in root
  n_products <- length(prod_opt_list)
  # Valid names for reflectance bands
  refbands_names <- c("b1_Red", "b2_NIR", "b3_Blue", "b4_Green", "b5_SWIR",
                      "b6_SWIR", "b7_SWIR")
  
  # if selprod and selver not passed, i.e. outside a common execution from the
  # gui, skip check on bands available for a specific product
  if (is.null(selprod) | is.null(selvers)) {
    avail_refbands <- refbands_names
  } else {
    #nocov start
    # Names of bands available for selected product
    avail_prodbands <- prod_opt_list[[selprod]][[selvers]]$bandnames
    
    # reflectence bands available for selected product
    match_refbands <- avail_prodbands[match(refbands_names, avail_prodbands)]
    avail_refbands <- match_refbands[!is.na(match_refbands)] 
    #nocov end
  }
  
  # GUI Initialization -----
  if (gui) {
    #nocov start
    main_win <- gWidgets::gbasicdialog(
      title = "Insert the new Spectral Index information and formula",
      parent = NULL,
      do.buttons = FALSE,
      visible = TRUE,
      spacing = 10,
      handler = function(h, ...) {
      }
    )
    
    main_group <- gWidgets::ggroup(container  = main_win, horizontal = FALSE,
                                   expand = TRUE)
    
    indexbandname_group <- gWidgets::ggroup(container  = main_group,
                                            horizontal = TRUE,
                                            expand     = TRUE)
    indexbandname_label <- gWidgets::glabel(
      text = "Spectral Index Acronym (e.g., SR)",
      markup    = TRUE,
      container = indexbandname_group
    )
    gWidgets::size(indexbandname_label) <- c(500, 20)
    gWidgets::font(indexbandname_label) <- list(family = "sans", size = 10,
                                                weight = "bold")
    
    sel_indexbandname <- gWidgets::gedit(
      text       = new_indexbandname,
      label      = "Please Insert a valid Proj4 string        ",
      container  = indexbandname_group,
      size       = 800,
      horizontal = TRUE
    )
    
    indexbandfullname_group <- gWidgets::ggroup(container  = main_group,
                                                horizontal = TRUE,
                                                expand     = TRUE)
    
    indexbandfullname_label <- gWidgets::glabel(
      text  = "Spectral Index Full Name (e.g., Simple Ratio (b2_NIR/b1_Red))",
      markup    = TRUE,
      container = indexbandfullname_group
    )
    gWidgets::size(indexbandfullname_label) <- c(500, 20)
    gWidgets::font(indexbandfullname_label) <- list(family = "sans", size = 10,
                                                    weight = "bold")
    
    sel_indexbandfullname <- gWidgets::gedit(
      text       = new_indexfullname,
      container  = indexbandfullname_group,
      size       = 800,
      horizontal = TRUE
    )
    
    indexformula_group <- gWidgets::ggroup(container  = main_group,
                                           horizontal = TRUE,
                                           expand     = TRUE)
    
    indexformula_label <- gWidgets::glabel(
      text      = "Spectral Index Formula (e.g., (b2_NIR/b1_Red) )",
      markup    = TRUE,
      container = indexformula_group
    )
    gWidgets::size(indexformula_label) <- c(500, 20)
    gWidgets::font(indexformula_label) <- list(family = "sans", size = 10,
                                               weight = "bold")
    
    sel_indexformula <- gWidgets::gedit(text       = new_indexformula,
                                        container  = indexformula_group,
                                        size       = 800,
                                        horizontal = TRUE)
    
    # Button for testing the new index
    but_group <- gWidgets::ggroup(container = main_group, horizontal = TRUE)
    
    # If "Set" clicked, retrieve selected values and save in previous file
    set_but <- gWidgets::gbutton(
      text = "---  Set New Index  ---", container = but_group,
      handler = function(h, ...) {
        new_indexbandname <- gWidgets::svalue(sel_indexbandname)
        new_indexfullname <- gWidgets::svalue(sel_indexbandfullname)
        new_indexformula  <- gWidgets::svalue(sel_indexformula)
        # Check if formual is good. If so, add it in the options file ----
        # for products for which the formula is computable (i.e., they have the
        # required bands)
        
        catch_err <- check_formula_errors(new_indexbandname,
                                          new_indexfullname,
                                          new_indexformula,
                                          n_products,
                                          prod_opt_list,
                                          refbands_names,
                                          avail_refbands,
                                          general_opts)
        
        if (catch_err == 0) {
          save_formula(refbands_names,
                       req_bands = attr(catch_err, "req_bands"),
                       new_indexbandname,
                       new_indexfullname,
                       new_indexformula,
                       new_indexnodata_out,
                       general_opts = if (exists("general_opts")) {
                         general_opts
                       } else {
                         NULL #nocov
                       },
                       prod_opt_list,
                       previous_jsfile)
        }
        # Issue error warnings in the GUI if something went wrong!
        switch(
          as.character(catch_err),
          "0" = gWidgets::svalue(notes_lab) <- format(
            strwrap("The new Spectral Index was correctly added! \n\n
                     To use it, click 'DONE', then re-open the 'Select 
                     Processing Layer' Window.", width = 80),
            justify = "centre"
          ),
          "1" = gWidgets::svalue(notes_lab) <- strwrap(
            paste("ERROR ! The Formula of the new Index is not computable.
            Please check it !\n Valid Band Names are: \n\n",
                  paste(avail_refbands, collapse = ", "),
                  ".")),
          "2" = gWidgets::svalue(notes_lab) <- strwrap(
            "ERROR ! Index full or short name is already present.\n\n
             Please specify different ones."
          ),
          "3" = gWidgets::svalue(notes_lab) <- strwrap(
            "ERROR ! Please provide valid values for the Index Acronym, 
             its fullname and the Formula."
          )
        )
        
        ifelse(
          as.character(catch_err) == "0",
          gWidgets::font(notes_lab) <- list(family = "sans", size = 9,
                                            weight = "bold", 
                                            color = "darkgreen"),
          gWidgets::font(notes_lab) <- list(family = "sans", size = 9,
                                            weight = "bold",
                                            color = "red", style = "italic")
        )
      }
    )
    
    gWidgets::size(set_but) <- list(width = 500)
    gWidgets::font(set_but) <- list(family = "sans", size = 10, color = "red",
                                    weight = "bold")
    gWidgets::addSpace(main_group, 3)
    notes_frame <- gWidgets::gframe(text = "--- Hints ---",
                                    pos = 0.5,
                                    container = main_group,
                                    horizontal = TRUE,
                                    expand = TRUE)
    notes_group <- gWidgets::ggroup(
      container  = notes_frame,
      expand     = TRUE,
      horizontal = FALSE
    )
    notes_lab <- gWidgets::glabel(
      text       = strwrap(paste(
        "ERROR ! The Formula of the new Index is not computable. Please 
        check it !\n\n 
        Valid Band Names are: ", paste(avail_refbands, collapse = ", "),
        "."
      )),
      container  = notes_group,
      horizontal = TRUE, editable = FALSE
    )
    gWidgets::size(notes_lab) <- c(600, 50)
    gWidgets::font(notes_lab) <- list(family = "sans",  size = 9,
                                      weight = "bold")
    gWidgets::addSpring(but_group)
    
    finish_but <- gWidgets::gbutton(
      text      = "Done !",
      container = but_group,
      handler   = function(h, ...) {
        dispose(main_win)
        gWidgets::gmessage(
          message = "ReOpen the 'Select Layers' window to use the new indexes",
          title = "Done!"
        )
        return(TRUE)
      }
    )
    gWidgets::font(finish_but) <- list(family = "sans",  weight = "bold")
    gWidgets::visible(main_win, set = TRUE)
    
    
    # end of gui actions ----
    #nocov end
  } else {
    
    #   ________________________________________________________________________
    #   Actions on non-interactive execution                                ####
    
    # Check if formula is good. If so, add it in the options file
    # for all products for which the formula is computable (i.e., they have the
    # required bands)
    catch_err <- check_formula_errors(new_indexbandname,
                                      new_indexfullname,
                                      new_indexformula,
                                      n_products,
                                      prod_opt_list,
                                      refbands_names,
                                      avail_refbands,
                                      general_opts)
    if (catch_err == 0) {
      
      save_formula(refbands_names,
                   req_bands = attr(catch_err, "req_bands"),
                   new_indexbandname,
                   new_indexfullname,
                   new_indexformula,
                   new_indexnodata_out,
                   general_opts = if (exists("general_opts")) {
                     general_opts
                   } else {
                     NULL #nocov
                   },
                   prod_opt_list,
                   previous_jsfile)
      message("The new Index was correctly added!\n It will be available at",
              "the next execution of MODIStsp().")
    } else if (catch_err == 1) {
      stop("The formula of the new index is wrong or not computable for this product. ", #nolint
           "Please check it.\n",
           "(Valid band names are: ", paste(avail_refbands, collapse = ", "),
           ".")
    } else if (catch_err == 2) {
      stop("The index acronym and/or full name are already present;\n",
           "Please specify different ones.")
    } else if (catch_err == 3) {
      stop("Some parameters are still blank; please provide valid values for\n",
           "the index name, the index fullname and the formula.")
    }
    
  } # end of non-gui actions
  
}

#' @title save_formula
#' @description Function called from `MODIStsp_addindex`to add the formula of a new 
#'  index to the json options file if no errors are detected in `check_formula_errors.
#' @inheritParams MODIStsp_addindex
#' @return The function is used for its side effects. It updates the
#'  previous_jsfile with the info on the new indexes
#' @noRd
#' @importFrom jsonlite write_json
save_formula <- function(refbands_names,
                         req_bands,
                         new_indexbandname,
                         new_indexfullname,
                         new_indexformula,
                         new_indexnodata_out,
                         general_opts,
                         prod_opt_list,
                         previous_jsfile) {
  
  # initialize list of custom indexes, if it does not exist yet
  if (is.null(general_opts$custom_indexes)) {
    #nocov start
    general_opts$custom_indexes <- list()
    
    for (prod in names(prod_opt_list)) {
      general_opts$custom_indexes[[prod]] <- list()
      
      for (vers in names(prod_opt_list[[prod]])) {
        general_opts$custom_indexes[[prod]][[vers]] <- list(
          "indexes_bandnames"  = character(0),
          "indexes_fullnames"  = character(0),
          "indexes_formulas"   = character(0),
          "indexes_nodata_out" = character(0)
        )
      }
    }
    #nocov end
  }
  # cycle on available products to add the new index to all products
  # allowing its computation
  
  for (prod in names(prod_opt_list)) {
    # cycle on available product versions
    for (vers in names(prod_opt_list[[prod]])) {
      # check if bands required for index computation are available for the
      # product
      check <- 0
      current_custindexes <- as.list(general_opts$custom_indexes[[prod]][[vers]]) #nolint
      for (reqband in refbands_names[req_bands]) {
        if (reqband %in% prod_opt_list[[prod]][[vers]]$bandnames) {
          check <- check + 1
        }
      } #End Cycle on reqband
      
      # if all required bands are available in product, add the new index to
      # the indexes list for the product in the previous_opts file.
      # In this way, at next execution, the new index should be available.
      # Moreover, loading and use of old RData options files won't be broken
      # if an index is added later than their creation.
      
      n_req_bands <- sum(req_bands)
      if (n_req_bands == check) {
        tmp_indexes_bandnames <- c(current_custindexes$indexes_bandnames,
                                   new_indexbandname)
        tmp_indexes_fullnames <- c(current_custindexes$indexes_fullnames,
                                   new_indexfullname)
        tmp_indexes_formulas <- c(current_custindexes$indexes_formulas,
                                  new_indexformula)
        tmp_indexes_nodata_out <- c(current_custindexes$indexes_nodata_out,
                                    new_indexnodata_out)
        
        general_opts$custom_indexes[[prod]][[vers]] <- list(
          "indexes_bandnames"  = tmp_indexes_bandnames,
          "indexes_fullnames"  = tmp_indexes_fullnames,
          "indexes_formulas"   = tmp_indexes_formulas,
          "indexes_nodata_out" = tmp_indexes_nodata_out
        )
        rm(tmp_indexes_bandnames,
           tmp_indexes_fullnames,
           tmp_indexes_formulas,
           tmp_indexes_nodata_out)
      }
    }
  }  #End Cycle on products
  
  # Save the products list and the chars of the products (including
  # custom indexes) in previous file.
  jsonlite::write_json(general_opts, previous_jsfile, pretty = TRUE,
                       auto_unbox = TRUE)
  
  return(general_opts)
  
}
