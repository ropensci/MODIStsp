#' @title Retrieve the names of MODIS layers for a product
#' @description
#'	Function used to retrieve the names of original MODIS layers, quality layers
#'	and eventually available spectral indexes given a MODIS product code.
#'	It is useful to identify the names of the layers to be processed when
#'	launching MODIStsp from the CLI.
#' @param prodname character containing the code of the desired MODIS product.
#'  NOTE: for products available separately for Terra and Aqua (e.g., MOD13Q1/MYD13Q1),
#'  use the code M*D_code_ (e.g., M*D13Q1)
#' @param prodver character containing the version of the desired MODIS product.
#'  Currently versions `"006"` (default) and `"061"` can be chosen.
#' @return list, containing the slots: `prodname`, `bandnames`, `quality_bandnames` and
#'  `indexes_bandnames`, `band_fullnames`, `quality_fullnames`, `indexes_fullnames`
#'
#' @author Lorenzo Busetto, phD (2014-2020)
#' @author Luigi Ranghetti, phD (2021)
#' @note License: GPL 3.0
#' @export
#' @examples
#'
#' # Get layers of product M*13Q1 based on code
#' MODIStsp_get_prodlayers("M*13Q1")
#'
#' # Get layers of product M*13Q1 based on full name
#' MODIStsp_get_prodlayers("Vegetation Indexes_16Days_250m (M*D13Q1)")
#'
#' # Get indexes names of product M*13Q1 based on full name
#' MODIStsp_get_prodlayers("MCD43C4")$indexes_bandnames
#'
#'
MODIStsp_get_prodlayers <- function(prodname, prodver = "006") {

  stopifnot(inherits(prodname, "character"))
  prod_opt_list <- load_prodopts()
  selprod <- prod_opt_list[grep(prodname, names(prod_opt_list))]

  # try using directly the string the string if code not found ----
  # assuming that the full product name is passed (useful 4 example in internal
  # functions)
  if (length(selprod) == 0){
    if (prodname %in% names(prod_opt_list)) {
      selprod <- prod_opt_list[prodname]
    } else {
      stop("Invalid product name. Aborting! prodname should be a MODIS
         product code (e.g., \"M*D13Q1\"), or full MODIStsp product name
         (e.g., \"Vegetation Indexes_16Days_250m (M*D13Q1)\")")
    }
  }

  prodname <- names(selprod)
  selprod <- selprod[[1]][[prodver]]
  if (length(selprod) == 0){
    if (!prodver %in% c("006","061")) {
      stop('Invalid product version (currently, values "006" and "061" are supported).')
    } else {
      stop(paste0(
        "Version ",prodver," is not available for product ",prodname,".\n",
        "If you think this is an error, please open a new GitHub issue at ",
        "'https://github.com/ropensci/MODIStsp/issues' and report it."
      ))
    }
  }
  
  bandnames         <- selprod[["bandnames"]]
  band_fullnames    <- selprod[["band_fullnames"]]
  quality_bandnames <- selprod[["quality_bandnames"]]
  quality_fullnames <- selprod[["quality_fullnames"]]
  indexes_bandnames <- selprod[["indexes_bandnames"]]
  indexes_fullnames <- selprod[["indexes_fullnames"]]

  cust_ind      <- jsonlite::read_json(system.file("ExtData", "MODIStsp_indexes.json",
                                                   package = "MODIStsp"))

  if (length(cust_ind) == 1) {
    cust_ind <- NULL
  } else {
    cust_ind  <- cust_ind[[prodname]][[prodver]]
  }

  indexes_bandnames <- c(indexes_bandnames,
                         cust_ind$indexes_bandnames)
  indexes_fullnames <- c(indexes_fullnames,
                         cust_ind$indexes_fullnames)

  prodnames   <- list(prodname          = prodname,
                      bandnames         = bandnames,
                      bandfullnames     = band_fullnames,
                      quality_bandnames = quality_bandnames,
                      quality_fullnames = quality_fullnames,
                      indexes_bandnames = indexes_bandnames,
                      indexes_fullnames = indexes_fullnames)
  prodnames
}

#' @title Retrieve the names of all available product
#' @description
#'	Function used to retrieve the names of available MODIS products
#' @return character array of product names
#'
#' @author Lorenzo Busetto, phD (2014-2020)
#' @note License: GPL 3.0
#' @export
#' @examples
#' # Get MODIStsp product names
#' MODIStsp_get_prodnames()


MODIStsp_get_prodnames <- function() {
  prod_opt_list <- load_prodopts()
  names(prod_opt_list)
}
