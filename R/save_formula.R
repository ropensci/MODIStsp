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
  
  # initialise list of custom indexes, if it does not exist yet
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