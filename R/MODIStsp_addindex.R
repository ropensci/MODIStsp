#' @title Add custom spectral indexes
#' @description Function used to add a user-defined Spectral Index to the
#'   default list of computable spectral indexes. Execution without the GUI
#'   (i.e., to add a new index from a script) is also possible (see examples).
#' @details
#' - The function asks the user to provide the info related to the new desired
#'   Spectral Index, checks for correctness of provided
#'   information (e.g., correct bandnames, computable formula, etc...).
#'   If the index is legit, it modifies the MODIStsp_addindex.json file
#'    so to allow computation of the additional index within MODIStsp for all
#'    products containing the required reflectance bands.
#' - To remove all custom-added spectral indexes, run MODIStsp_resetindexes()
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
#' @return The function is called for its side effects. On success, the
#'  MODIStsp_indexes.json
#'  is modified so to allow computation of the additional indexes.
#' @author Lorenzo Busetto, phD (2014-2017)
#' @author Luigi Ranghetti, phD (2015)
#' @note License: GPL 3.0
#' @seealso [MODIStsp_resetindexes]
#' @export
#'
#' @examples
#' # Run the GUI to interactively define a new index
#'  \dontrun{
#'  MODIStsp_addindex()}
#'
#' # Define the new index in non-interactive execution
#'
#' \dontrun{
#' MODIStsp_addindex(new_indexbandname = "SSI",
#'   new_indexfullname = "Simple Useless Index",
#'   new_indexformula = "b2_NIR+b1_Red")
#' }

MODIStsp_addindex <- function(
  new_indexbandname   = "",
  new_indexfullname   = "",
  new_indexformula    = "",
  new_indexnodata_out = "32767") { #nolint


  indexes_file <- (system.file("ExtData",
                               "MODIStsp_indexes.json",
                               package = "MODIStsp"))

  # Restore MODIS products if existing, otherwise retrieve  from xml file ----
  prod_opt_list <- load_prodopts()
  #how many product available ? = elements in root
  n_products <- length(prod_opt_list)
  # Valid names for reflectance bands
  refbands_names <- c("b1_Red", "b2_NIR", "b3_Blue", "b4_Green", "b5_SWIR",
                      "b6_SWIR", "b7_SWIR")


  # if selprod and selver not passed, i.e. outside a common execution from the
  # gui, skip check on bands available for a specific product
  avail_refbands <- refbands_names
  #   ________________________________________________________________________
  #   Actions on non-interactive execution                                ####

  # Check if formula is good. If so, add it in the options file
  # for all products for which the formula is computable (i.e., they have the
  # required bands)
  catch_err <- check_formula_errors(new_indexbandname,
                                    new_indexfullname,
                                    new_indexformula,
                                    prod_opt_list,
                                    refbands_names,
                                    avail_refbands)
  if (catch_err == 0) {

    save_formula(refbands_names,
                 req_bands = attr(catch_err, "req_bands"),
                 new_indexbandname,
                 new_indexfullname,
                 new_indexformula,
                 new_indexnodata_out,
                 prod_opt_list)
    message("The new Index was correctly added!\n It will be available at ",
            "the next execution of MODIStsp().")
  } else if (catch_err == 1) {
    stop("The formula of the new index is wrong or not computable. ", #nolint
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

} # end of MODIStsp_addindex


#' @title save_formula
#' @description Function called from `MODIStsp_addindex`to add the formula of a new
#'  index to the json options file if no errors are detected in `check_formula_errors.
#' @inheritParams MODIStsp_addindex
#' @return The function is used for its side effects. It updates the
#'  MODIStsp_indexes.json file with the info on the new indexes
#' @noRd
#' @importFrom jsonlite write_json
save_formula <- function(refbands_names,
                         req_bands,
                         new_indexbandname,
                         new_indexfullname,
                         new_indexformula,
                         new_indexnodata_out,
                         prod_opt_list) {

  indexes_file <- system.file("ExtData",
                              "MODIStsp_indexes.json",
                              package = "MODIStsp")

  if (file.exists(indexes_file)) {
    custom_indexes <- jsonlite::read_json(indexes_file)
  }
  if (length(custom_indexes) == 1) {
    custom_indexes <- NULL
  }

  # initialize list of custom indexes, if it does not exist yet
  if (is.null(custom_indexes)) {
    #nocov start
    custom_indexes <- list()

    for (prod in names(prod_opt_list)) {
      custom_indexes[[prod]] <- list()

      for (vers in names(prod_opt_list[[prod]])) {
        custom_indexes[[prod]][[vers]] <- list(
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
      current_custindexes <- as.list(custom_indexes[[prod]][[vers]]) #nolint
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

        custom_indexes[[prod]][[vers]] <- list(
          "indexes_bandnames"  = tmp_indexes_bandnames,
          "indexes_fullnames"  = tmp_indexes_fullnames,
          "indexes_formulas"   = tmp_indexes_formulas,
          "indexes_nodata_out" = tmp_indexes_nodata_out
        )
        rm(tmp_indexes_bandnames,
           tmp_indexes_fullnames,
           tmp_indexes_formulas,
           tmp_indexes_nodata_out)
      } else {

      }
    }
  }  #End Cycle on products

  # Save the products list and the chars of the products (including
  # custom indexes) in previous file.

  jsonlite::write_json(custom_indexes, indexes_file, pretty = TRUE,
                       auto_unbox = TRUE)

  return(custom_indexes)

}
