#' @title check_formula_errors
#' @description Function to check for errors in formula within `MODIStsp_addindex`.
#'  It is called from the GUI when "Add" button is chosen, or when the function starts
#'  in non-interactive mode.
#' @inheritParams MODIStsp_addindex
#' @importFrom pacman p_exists p_load
#' @importFrom stringr str_detect
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
  
  if (req_bands[1]) b1_Red   <- 5
  if (req_bands[2]) b2_NIR   <- 6
  if (req_bands[3]) b3_Blue  <- 7
  if (req_bands[4]) b4_Green <- 8
  if (req_bands[5]) b5_SWIR  <- 9
  if (req_bands[6]) b6_SWIR  <- 15
  if (req_bands[7]) b7_SWIR  <- 25
  
  if (any(req_bands)) {
    try_parse <- try(eval(parse(text = new_indexformula)), silent = TRUE)
    if (class(try_parse) == "try-error") {
      # error 1: error in the formula: expression not computable
      catch_err <- 1
    }
  } else {
    # error 1: error in the formula: no valid bands provided
    catch_err <- 1
  } 
  
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
      catch_err <- 1 #nocov (only possible on interactive exec.)
    }
  }
  
  attr(catch_err, "req_bands") <- req_bands
  return(catch_err)
  
} # end of check_formula_errors()
