#' MODIStsp_addindex
#' @description Function used to add a user-defined Spectral Index to the default list of computable spectral indexes
#' Execution without the GUI (i.e., to add a new index from a script) is also possible (see examples)
#' @details The function asks the user to provide the info related to the new desired Spectral Index using a GUI interface,
#' checks for correctness of provided information (e.g., correct bandnames, computable formula, etc...). If the index is legit,
#' it modifies the MODIStsp_Previous.RData file so to allow computation of the additional index within MODIStsp.
#' To remove all custom-added spectral indexes, simply delete the MODIStsp_Previous.RData file within the /Previous subfolder of the
#' folder in which the package was installed, or the alternative RData specified by the parameter "option_file".
#' The function can be run either from within the main MODIStsp GUI, or a standalone script. In the latter case, it modifies either the
#' MODIStsp_Previous.RData options file, or the optins_file specified by the user, to add the new index.
#' @param option_file settings (optional): full path of the RData file containing the processing options in which the new indexes
#'  are saved (default: MODIStsp_Previous.RData in subdir Previous).
#' @param gui logical value (default: TRUE): if TRUE, the GUI is opened to define the new index; otherwise use the "new_indexbandname",
#'  "new_indexfullname" and "new_indexformula" parameters to define it non-interactively.
#' @param new_indexbandname (optional if gui=TRUE): short name (acronym) of the new spectral index.
#' @param new_indexfullname (optional if gui=TRUE): extended name of the new spectral index.
#' @param new_indexformula (optional if gui=TRUE): string containing the formula of the new spectral indexes. Variables accepted to
#'  compute it are the names of the bands: b1_Red, b2_NIR, b3_Blue, b4_Green, b5_SWIR, b6_SWIR and b7_SWIR.
#' @param new_indexnodata_out (optional): nodata value to assign to the rasters containing the new index
#' @param MODIStsp_dir (optional): main folder containing MODIStsp R files (used only to launche MODSItsp from outside the package using MODIStsp_std.R)
#' @return NULL - the MODIStsp_Previous.RData file is modified so to allow computation of the additional index
#'
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
#' # Run the GUI and save the new index in a custom RData file
#' \dontrun{
#' MODIStsp_addindex(option_file = "X:/yourpath/youroptions.RData")}
#'
#' # Define the new index non-interactively
#' \dontrun{
#' MODIStsp_addindex(gui = FALSE, new_indexbandname = "SSD",
#'   new_indexfullname = "Simple Stupid Difference",
#'   new_indexformula = "b2_NIR-b1_Red")}
#'
#'

MODIStsp_addindex <- function(option_file=NA, gui=TRUE, new_indexbandname="", new_indexfullname="",
                              new_indexformula="", new_indexnodata_out = "32767", MODIStsp_dir=NA) {

  # Initialization and retrieval of parameters ----
  if (gui) {
    require(gWidgetsRGtk2)
    options("guiToolkit" = "RGtk2")
  }

  if (is.na(MODIStsp_dir)) {
    MODIStsp_dir <- system.file(package = "MODIStsp")
  }
  previous_dir <- file.path(MODIStsp_dir,"Previous")
  previous_file <- ifelse(is.na(option_file), file.path(previous_dir, "MODIStsp_Previous.RData"), option_file)
  if (file.exists(previous_file)) {
    load(previous_file)
  }
  xml_file <- file.path(MODIStsp_dir, "ExtData/MODIStsp_ProdOpts.xml")

  # Restore previous options file if existing, otherwise create a "new" one with default values, by retrieving data from xml file
  # (Allows to run even if MODIStsp_Previuos missing)
  if (!exists("mod_prod_list") | !exists("prod_opt_list")) {
    warning("The previously saved options file is missing or corrupted; a new default one will be generated...")
    MODIStsp_read_xml(previous_file = previous_file,xml_file = xml_file )
    load(previous_file)
  } else if (is.null(attr(mod_prod_list,"GeneratedBy")) | is.null(attr(prod_opt_list,"GeneratedBy"))) {
    warning("The previously saved options file is missing or corrupted; a new default one will be generated...")
    MODIStsp_read_xml(previous_file = previous_file,xml_file = xml_file )
    load(previous_file)
  }

  # retrieve information from xml ----
  xmlfile <- xmlParse(xml_file)  # initialize xml parsing
  xmltop <- xmlRoot(xmlfile) #gives content of root
  n_products <- xmlSize(xmltop) #how many product available ? = elements in root

  # Valid names for reflectance bands
  refbands_names <- c("b1_Red","b2_NIR","b3_Blue","b4_Green","b5_SWIR","b6_SWIR", "b7_SWIR")

  # Function to check for errors in formula ----
  # (it is called from GUI when "Add" button is chosen, or when function starts in non-interactive mode
  check_formula_errors <- function(new_indexbandname, new_indexfullname, new_indexformula,
                                   n_products, prod_opt_list, refbands_names) {

    catch_err <- 0 # error 0: no errors

    # Check that both the name, the fullname and the formula fields are not null
    if (new_indexbandname == "" | new_indexfullname == "" | new_indexformula == "") {
      catch_err <- 3 # error 3: parameters blank
    }

    # Look for valid band names in index formula
    req_bands <- c(str_detect(new_indexformula,"b1_Red"),
                   str_detect(new_indexformula,"b2_NIR"),
                   str_detect(new_indexformula,"b3_Blue"),
                   str_detect(new_indexformula,"b4_Green"),
                   str_detect(new_indexformula,"b5_SWIR"),
                   str_detect(new_indexformula,"b6_SWIR"),
                   str_detect(new_indexformula,"b7_SWIR"))

    # Create dummy varaibles named as the required bands, assign random values to the
    # them, and then verify if formula is copmputable by evaluate/parse and check for errors

    if (req_bands[1] == T) {b1_Red <- 5}
    if (req_bands[2] == T) {b2_NIR <- 6}
    if (req_bands[3] == T) {b3_Blue <- 7}
    if (req_bands[4] == T) {b4_Green <- 8}
    if (req_bands[5] == T) {b5_SWIR <- 9}
    if (req_bands[6] == T) {b6_SWIR <- 15}
    if (req_bands[7] == T) {b7_SWIR <- 25}

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
    for (prod in 1:n_products) {  # cycle on available products
      all_indexes_bandnames <- c(all_indexes_bandnames, prod_opt_list[[prod]]$indexes_bandnames)
      all_indexes_fullnames <- c(all_indexes_fullnames, prod_opt_list[[prod]]$indexes_fullnames)
    }
    all_indexes_bandnames <- unique(all_indexes_bandnames)
    all_indexes_fullnames <- unique(all_indexes_fullnames)

    # verify that the index name and fullname is not already present
    if (new_indexbandname %in% all_indexes_bandnames | new_indexfullname %in% all_indexes_fullnames) {
      catch_err <- 2 # error 2: index name or fullname already present
    }

    attr(catch_err,"req_bands") <- req_bands
    return(catch_err)

  } # end of check_formula_errors()

  # Function to add the formula in previous file ----
  # (it is called if no errors are detected)
  save_formula <- function(n_products, xmltop, refbands_names, req_bands, new_indexbandname, new_indexfullname,
                           new_indexformula, new_indexnodata_out, general_opts, mod_prod_list, previous_file) {

    for (prod in 1:n_products) {  # cycle on available products
      nbands <- xmlSize(xmltop[[prod]][["bands"]])  # number of original layers
      bandnames <- NULL ;  band_fullname <- NULL ;  datatype <- NULL ;   nodata_in <- NULL ;  nodata_out <- NULL
      for (band in 1:nbands) { # get bandnames of available products
        bandnames <- c(bandnames,xmlToList(xmltop[[prod]][["bands"]][[band]][["bandname"]]))
      } #End Cycle on band

      # check if bands required for index computation are avilable for the product
      check <- 0
      for (reqband in refbands_names[which(req_bands == T)]) {
        if (reqband %in% bandnames) {
          check <- check + 1
        }
      } #End Cycle on reqband

      # if all required bands are available in product, add the new index to the indexes list for the product in the previous_opts file.
      # in this way, at next execution, the new index should be available. Moreover, loading and use of old RData options files
      # won't be broken if an index is added later than their creation.
      n_req_bands <- sum(req_bands)
      if (n_req_bands == check ) {
        prod_opt_list[[prod]]$indexes_bandnames <- c(prod_opt_list[[prod]]$indexes_bandnames,new_indexbandname)
        prod_opt_list[[prod]]$indexes_fullnames <- c(prod_opt_list[[prod]]$indexes_fullnames,new_indexfullname)
        prod_opt_list[[prod]]$indexes_formulas <- c(prod_opt_list[[prod]]$indexes_formulas,new_indexformula)
        prod_opt_list[[prod]]$indexes_nodata_out <- c(prod_opt_list[[prod]]$indexes_nodata_out,new_indexnodata_out)
        prod_opt_list[[prod]]$indexes_bandsel <- c(prod_opt_list[[prod]]$indexes_bandsel,0)
      }

    }  #End Cycle on products

    # Save the products list and the chars of the products in previous file
    if (!is.null(general_opts)) {
      save(general_opts, prod_opt_list, mod_prod_list, file = previous_file)
    } else {
      save(prod_opt_list, mod_prod_list, file = previous_file)
    }

    return(NULL)

  } # end of save_formula()


  # GUI Initialization -----
  if (gui) {

    main_win <- gbasicdialog(title = "Insert the new Spectral Index information and formula", parent = NULL, do.buttons = F,
                             visible = T, spacing = 10)
    main_group <- ggroup(container = main_win, horizontal = FALSE, expand = T)

    indexbandname_group <- ggroup(container = main_group, horizontal = T, expand = T)
    indexbandname_label <- glabel(text = "<span weight = 'bold'> Spectral Index Acronym (e.g., SR) </span>", markup = T, container = indexbandname_group)
    size(indexbandname_label) <- c(400,20)
    sel_indexbandname <- gedit(text = new_indexbandname, label = "Please Insert a valid Proj4 string        ",
                               container = indexbandname_group, size = 800, horizontal = T)

    indexbandfullname_group <- ggroup(container = main_group, horizontal = T, expand = T)
    indexbandfullname_label <- glabel(text = "<span weight = 'bold'> Spectral Index Full Name (e.g., Simple Ratio (b2_NIR/b1_Red) )</span>", markup = T,
                                      container = indexbandfullname_group)
    size(indexbandfullname_label) <- c(400,20)
    sel_indexbandfullname <- gedit(text = new_indexfullname, container = indexbandfullname_group, size = 800, horizontal = T)

    indexformula_group <- ggroup(container = main_group, horizontal = T, expand = T)
    indexformula_label <- glabel(text = "<span weight = 'bold'>  Spectral Index Formula (e.g., (b2_NIR/b1_Red) ) </span>", markup = T,
                                 container = indexformula_group)
    size(indexformula_label) <- c(400,20)
    sel_indexformula <- gedit(text = new_indexformula, container = indexformula_group, size = 800, horizontal = T)

    but_group <- ggroup(container = main_group, horizontal = TRUE)

    start_but <- gbutton(text = "Add", container = but_group, handler = function(h,...) {# If "Start" pressed, retrieve selected values and save in previous file

      new_indexbandname <- svalue(sel_indexbandname)
      new_indexfullname <- svalue(sel_indexbandfullname)
      new_indexformula <- svalue(sel_indexformula)

      # Check if formual is good. If so, add it in the options file ----
      # for products for which the formula is computable (i.e., they have the required bands)

      catch_err <- check_formula_errors(new_indexbandname = new_indexbandname, new_indexfullname = new_indexfullname, new_indexformula = new_indexformula,
                                        n_products = n_products, prod_opt_list = prod_opt_list, refbands_names = refbands_names)
      if (catch_err == 0) {
        save_formula(n_products = n_products, xmltop = xmltop, refbands_names = refbands_names, req_bands = attr(catch_err,"req_bands"),
                     new_indexbandname = new_indexbandname, new_indexfullname = new_indexfullname, new_indexformula = new_indexformula,
                     new_indexnodata_out = new_indexnodata_out, general_opts = if (exists("general_opts")) general_opts else NULL,
                     mod_prod_list = mod_prod_list, previous_file = previous_file)
        if (exists("Quit")) {
          gmessage("The new Spectral Index was correctly added! To use it, Re-open the 'Select Processing Layer' window.")
        } else {
          gmessage("The new Spectral Index was correctly added!")
        }
        dispose(main_win)
      } else if (catch_err == 1) {
        gmessage(paste0("The formula of the new index is not computable. Please check it (Valid band names are: ",paste(refbands_names,collapse = ", "),"."))
      } else if (catch_err == 2) {
        gmessage("The index acronym and/or the full name are already present; please specify different ones.")
      } else if (catch_err == 3) {
        gmessage("Some parameters are still blank; please provide valid values for the index name, the index fullname and the formula.")
      }
    })

    # On quit, do nothing

    quit_but <- gbutton(text = if (exists("Quit")) {
      "Cancel"
    } else {
      "Quit"
    }, container = but_group, handler = function(h,...){ # If "Quit" exit
      dispose(main_win)
    })

    visible(main_win, set = T)

    # end of gui actions ----
    # Actions on non-interactive execution
  } else {

    # Check if formual is good. If so, add it in the options file ----
    # for products for which the formula is computable (i.e., they have the required bands)
    catch_err <- check_formula_errors(new_indexbandname = new_indexbandname, new_indexfullname = new_indexfullname, new_indexformula = new_indexformula,
                                      n_products = n_products, prod_opt_list = prod_opt_list, refbands_names = refbands_names)
    if (catch_err == 0) {
      save_formula(n_products = n_products, xmltop = xmltop, refbands_names = refbands_names, req_bands = attr(catch_err,"req_bands"),
                   new_indexbandname = new_indexbandname, new_indexfullname = new_indexfullname, new_indexformula = new_indexformula,
                   new_indexnodata_out = new_indexnodata_out, general_opts = if (exists("general_opts")) general_opts else NULL,
                   mod_prod_list = mod_prod_list, previous_file = previous_file)
      cat("The new Spectral Index was correctly added! It will be available from the next running of MODIStsp().\n")
    } else if (catch_err == 1) {
      stop(paste0("The formula of the new index is not computable. Please check it (Valid band names are: ",paste(refbands_names,collapse = ", "),"."))
    } else if (catch_err == 2) {
      stop("The index acronym and/or the full name are already present; please specify different ones.")
    } else if (catch_err == 3 ) {
      stop("Some parameters are still blank; please provide valid values for the index name, the index fullname and the formula.")
    }

  } # end of non-gui actions

}
