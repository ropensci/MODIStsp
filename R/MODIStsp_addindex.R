#' @title add custom spectral indexes
#' @description Function used to add a user-defined Spectral Index to the
#'   default list of computable spectral indexes. Execution without the GUI
#'   (i.e., to add a new index from a script) is also possible (see examples)
#' @details
#' - The function asks the user to provide the info related to the new desired
#'   Spectral Index using a GUI interface, checks for correctness of provided
#'   information (e.g., correct bandnames, computable formula, etc...).
#'   If the index is legit, it modifies the MODIStsp_Previous.RData file so to
#'   allow computation of the additional index within MODIStsp.
#' - To remove all custom-added spectral indexes, simply delete the
#'   MODIStsp_Previous.RData file within the /Previous subfolder of the folder
#'   in which the package was installed, or the alternative JSON specified by
#'   the parameter "option_jsfile".
#' - The function can be run either from within the main MODIStsp GUI,
#'   or a stand-alone script. In the latter case, it modifies either the
#'   MODIStsp_Previous.RData options file, or the options_file specified by
#'   the user to add the new index.
#' @param option_jsfile `character` full path of a JSON file
#'  containing the processing options in which the new indexes has to be saved
#'  (default: MODIStsp_Previous.JSON in subdir Previous).
#' @param prodopts_file `character`: full path of the RData file containing.
#'   if NULL, use MODIStsp_ProdOpts.RData in subdir Previous, Default: NULL
#' @param selprod `character` Name of the product to which the new index should
#'   be added (Note: the index will be added to all other products allowing its
#'   computation !). If NULL, as in non-interactive execution, no
#'   check on available band names is skipped and the index (if valid) is
#'   added to all products supporting it, Default: NULL
#' @param selvers `character` Version of the product to which the new index
#'   should be added (Note: the index will be added to all other products
#'   allowing its computation !). If NULL, as in non-interactive execution, no
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
#' @param new_indexnodata_out `character` nodata value to assign to the rasters
#'   containing the new index
#' @param MODIStsp_dir `character` main folder containing MODIStsp R files,
#'   Default: retrieved from package installation folder
#' @importFrom gWidgets gbasicdialog ggroup glabel gedit size gbutton svalue
#' @importFrom pacman p_load p_exists
#' @importFrom XML xmlParse xmlRoot xmlSize xmlToList
#' @importFrom stringr str_detect
#' @return NULL - the MODIStsp_Previous.RData file is modified so to allow
#'   computation of the additional index
#' @author Lorenzo Busetto, phD (2014-2015) \email{busetto.l@@irea.cnr.it}
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
#'
#' @examples
#' # Run the GUI to interactively define the function
#'  \dontrun{
#'  MODIStsp_addindex()}
#'
#' # Run the GUI and save the new index in a custom json file
#' \dontrun{
#' MODIStsp_addindex(option_jsfile = "X:/yourpath/youroptions.json")}
#'
#' # Define the new index in non-interactive execution
#' \dontrun{
#' MODIStsp_addindex(gui = FALSE, new_indexbandname = "SSsava4",
#'   new_indexfullname = "Simple Stupid vasvasavs",
#'   new_indexformula = "b2_NIR+b1_Red")
#'   }
#'
#'

MODIStsp_addindex <- function(option_jsfile       = NULL,
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
  }
  
  previous_dir    <- file.path(MODIStsp_dir, "Previous")
  previous_jsfile <- ifelse(is.null(option_jsfile),
                            file.path(previous_dir, "MODIStsp_Previous.json"),
                            option_jsfile)
  prodopts_file   <- ifelse(is.null(prodopts_file),
                            file.path(previous_dir, "MODIStsp_ProdOpts.RData"),
                            prodopts_file)
  general_opts    <- RJSONIO::fromJSON(previous_jsfile)
  
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
    # Names of bands available for selected product
    avail_prodbands <- prod_opt_list[[selprod]][[selvers]]$bandnames
    
    # reflectence bands available for selected product
    match_refbands <- avail_prodbands[match(refbands_names, avail_prodbands)]
    avail_refbands <- match_refbands[!is.na(match_refbands)]
  }
  
  # GUI Initialization -----
  if (gui) {
    main_win <- gbasicdialog(
      title = "Insert the new Spectral Index information and formula",
      parent = NULL,
      do.buttons = FALSE,
      visible = TRUE,
      spacing = 10,
      handler = function(h, ...) {
      }
    )
    
    main_group <- ggroup(container  = main_win, horizontal = FALSE,
                         expand = TRUE)
    
    indexbandname_group <- ggroup(container  = main_group, horizontal = TRUE,
                                  expand     = TRUE)
    indexbandname_label <- glabel(text = "Spectral Index Acronym (e.g., SR)",
                                  markup    = TRUE,
                                  container = indexbandname_group)
    size(indexbandname_label) <- c(500, 20)
    font(indexbandname_label) <- list(family = "sans", size = 10,
                                      weight = "bold")
    
    sel_indexbandname <- gedit(text       = new_indexbandname,
                               label      = "Please Insert a valid Proj4 string        ", #nolint
                               container  = indexbandname_group,
                               size       = 800,
                               horizontal = TRUE
    )
    
    indexbandfullname_group <- ggroup(container  = main_group,
                                      horizontal = TRUE,
                                      expand     = TRUE)
    
    indexbandfullname_label <- glabel(
      text  = "Spectral Index Full Name (e.g., Simple Ratio (b2_NIR/b1_Red))",
      markup    = TRUE,
      container = indexbandfullname_group
    )
    size(indexbandfullname_label) <- c(500, 20)
    font(indexbandfullname_label) <- list(family = "sans", size = 10,
                                          weight = "bold")
    
    sel_indexbandfullname <- gedit(text       = new_indexfullname,
                                   container  = indexbandfullname_group,
                                   size       = 800,
                                   horizontal = TRUE)
    
    indexformula_group <- ggroup(container  = main_group,
                                 horizontal = TRUE,
                                 expand     = TRUE)
    indexformula_label <- glabel(
      text      = "Spectral Index Formula (e.g., (b2_NIR/b1_Red) )",
      markup    = TRUE,
      container = indexformula_group
    )
    size(indexformula_label) <- c(500, 20)
    font(indexformula_label) <- list(family = "sans", size = 10,
                                     weight = "bold")
    
    sel_indexformula <- gedit(text       = new_indexformula,
                              container  = indexformula_group,
                              size       = 800,
                              horizontal = TRUE)
    
    # Button for testing the new index
    but_group <- ggroup(container = main_group, horizontal = TRUE)
    
    # If "Set" clicked, retrieve selected values and save in previous file
    set_but <- gbutton(
      text = "---  Set New Index  ---", container = but_group,
      handler = function(h, ...) {
        new_indexbandname <- svalue(sel_indexbandname)
        new_indexfullname <- svalue(sel_indexbandfullname)
        new_indexformula  <- svalue(sel_indexformula)
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
                         NULL
                       },
                       prod_opt_list,
                       previous_jsfile)
        }
        # Issue error warnings in the GUI if something went wrong!
        switch(
          as.character(catch_err),
          "0" = svalue(notes_lab) <- format(
            glue::glue("The new Spectral Index was correctly added! \n",
                       "To use it, click 'DONE', then re-open the 'Select \n",
                       "Processing Layer' Window."),
            justify = "centre"
          ),
          "1" = svalue(notes_lab) <- glue::glue(
            "ERROR ! The Formula of the new Index is not computable. ",
            "Please check it !\n Valid Band Names are: \n",
            paste(avail_refbands, collapse = ", "),
            "."),
          "2" = svalue(notes_lab) <- glue::glue(
            "ERROR ! Index full or short name is already present.\n",
            "Please specify different ones."
          ),
          "3" = svalue(notes_lab) <- glue::glue(
            "ERROR ! Please provide valid values for the Index Acronym, ",
            "its fullname and the Formula."
          )
        )
        
        ifelse(
          as.character(catch_err) == "0",
          font(notes_lab) <- list(family = "sans", size = 9, weight = "bold"),
          font(notes_lab) <- list(family = "sans", size = 9, weight = "bold",
                                  color = "red", style = "italic")
        )
      }
    )
    
    size(set_but) <- list(width = 500)
    font(set_but) <- list(family = "sans", size = 10, color = "red",
                          weight = "bold")
    addSpace(main_group, 3)
    notes_frame <- gframe(text = "--- Hints ---",
                          pos = 0.5,
                          container = main_group,
                          horizontal = TRUE,
                          expand = TRUE)
    notes_group <- ggroup(
      container  = notes_frame,
      expand     = TRUE,
      horizontal = FALSE
    )
    notes_lab <- glabel(
      text       = glue::glue(
        "ERROR ! The Formula of the new Index is not computable. Please ",
        "check it !\n ",
        "Valid Band Names are: ", paste(avail_refbands, collapse = ", "),
        "."
      ),
      container  = notes_group,
      horizontal = TRUE, editable = FALSE
    )
    size(notes_lab) <- c(600, 50)
    font(notes_lab) <- list(family = "sans",  size = 9, weight = "bold")
    addSpring(but_group)
    
    finish_but <- gbutton(
      text      = "Done !",
      container = but_group,
      handler   = function(h, ...) {
        dispose(main_win)
        gmessage(
          message = "ReOpen the 'Select Layers' window to use the new index",
          title = "Done!"
        )
        return(TRUE)
      }
    )
    font(finish_but) <- list(family = "sans",  weight = "bold")
    visible(main_win, set = TRUE)
    
    # end of gui actions ----
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
                     NULL
                   },
                   prod_opt_list,
                   previous_jsfile)
      message("The new Index was correctly added!\n It will be available at\n",
              "the next execution of MODIStsp().")
    } else if (catch_err == 1) {
      stop("The formula of the new index is not computable for this product. ",
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
  
} # End of MAIN function

# Helpers ####

# Function to check for errors in formula ----
# (it is called from GUI when "Add" button is chosen, or when function starts
# in non-interactive mode
#' @title check_formula_errors
#' @description Function to check for errors in formula. It is called from
#'   the GUI when "Add" button is chosen, or when function starts in
#'   non-interactive mode
#' @inheritParams MODIStsp_addindex
#' @noRd
#' @return `numeric` error code between 0 and 3. 0 means all checks passed
#'   and formula can be saved

check_formula_errors <- function(new_indexbandname,
                                 new_indexfullname,
                                 new_indexformula,
                                 n_products,
                                 prod_opt_list,
                                 refbands_names,
                                 avail_refbands,
                                 general_opts) {
  
  catch_err <- 0 # error 0: no errors
  
  # Check that the name, the fullname and the formula fields are not null
  if (any(c(new_indexbandname, new_indexfullname, new_indexformula) == "")) {
    catch_err <- 3 # error 3: blank parameters
  }
  
  # Look for valid band names in index formula
  req_bands <- c(stringr::str_detect(new_indexformula, "b1_Red"),
                 stringr::str_detect(new_indexformula, "b2_NIR"),
                 stringr::str_detect(new_indexformula, "b3_Blue"),
                 stringr::str_detect(new_indexformula, "b4_Green"),
                 stringr::str_detect(new_indexformula, "b5_SWIR"),
                 stringr::str_detect(new_indexformula, "b6_SWIR"),
                 stringr::str_detect(new_indexformula, "b7_SWIR"))
  
  # Create dummy variables named as the required bands, assign random values
  # to them, and then verify if formula is computable by evaluate/parse and
  # check for errors
  
  if (req_bands[1] == TRUE) b1_Red   <- 5
  if (req_bands[2] == TRUE) b2_NIR   <- 6
  if (req_bands[3] == TRUE) b3_Blue  <- 7
  if (req_bands[4] == TRUE) b4_Green <- 8
  if (req_bands[5] == TRUE) b5_SWIR  <- 9
  if (req_bands[6] == TRUE) b6_SWIR  <- 15
  if (req_bands[7] == TRUE) b7_SWIR  <- 25
  
  if (max(req_bands == 1)) {
    try_parse <- try(eval(parse(text = new_indexformula)), silent = TRUE)
    if (class(try_parse) == "try-error") {
      catch_err <- 1
    }
  } else {
    catch_err <- 1
  } # error 1: error in the formula
  
  ## generate the list of all the index names
  all_indexes_bandnames <- all_indexes_fullnames <- NA
  # cycle on available products
  for (prod in names(prod_opt_list)) {
    # cycle on available product versions
    for (vers in names(prod_opt_list[[prod]])) {
      current_prodopts    <- as.list(prod_opt_list[[prod]][[vers]])
      current_custindexes <- as.list(general_opts$custom_indexes[[prod]][[vers]]) #nolint
      all_indexes_bandnames <- c(all_indexes_bandnames,
                                 current_prodopts$indexes_bandnames)
      all_indexes_fullnames <- c(all_indexes_fullnames,
                                 current_prodopts$indexes_fullnames)
      if (!is.null(current_custindexes)) {
        all_indexes_bandnames <- c(all_indexes_bandnames,
                                   current_custindexes$indexes_bandnames)
        all_indexes_fullnames <- c(all_indexes_fullnames,
                                   current_custindexes$indexes_fullnames)
      }
    }
  }
  all_indexes_bandnames <- unique(all_indexes_bandnames)
  all_indexes_fullnames <- unique(all_indexes_fullnames)
  
  # verify that the index name and fullname is not already present
  if (catch_err == 0 & (new_indexbandname %in% all_indexes_bandnames |
                        new_indexfullname %in% all_indexes_fullnames)) {
    catch_err <- 2 # error 2: index name or fullname already present
  }
  # verify that the index is computable for the selected product
  if (catch_err == 0) {
    # see if any of the bands required for the new index are NOT available for
    # the product
    if (is.na(max(match(refbands_names[req_bands], avail_refbands)))) {
      # error 1 again: index is ok, but not computable for the currently
      # selected product so we don't save it !
      catch_err <- 1
    }
  }
  
  attr(catch_err, "req_bands") <- req_bands
  return(catch_err)
  
} # end of check_formula_errors()


#' @title save_formula
#' @description Function to add the formula to the optins file if no
#'   errors are detected (internal - called from MODIStsp_add_index)
#' @inheritParams MODIStsp_addindex
#' @return NULL - the function updates the previous_jsfile with the info
#'   on the new indexe
#' @noRd
save_formula <- function(refbands_names,
                         req_bands,
                         new_indexbandname,
                         new_indexfullname,
                         new_indexformula,
                         new_indexnodata_out,
                         general_opts,
                         prod_opt_list,
                         previous_jsfile) {
  
  # initialise list of custom indexes, if it does not exist yet
  if (is.null(general_opts$custom_indexes)) {
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
  write(RJSONIO::toJSON(general_opts), previous_jsfile)
  
  return(general_opts)
  
} # end of save_formula()
