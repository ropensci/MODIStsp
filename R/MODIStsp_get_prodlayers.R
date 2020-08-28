#' @title Retrieve the names of MODIS layers for a product
#' @description
#'	Function used to retrieve the names of original MODIS layers, quality layers
#'	and eventually available spectral indexes given a MODIS product code.
#'	It is useful to identify the names of the layers to be processed when
#'	launching MODIStsp from the CLI.
#' @param prodname character containing the code of the desired MODIS product.
#'  NOTE: for products available separately for Terra and Aqua (e.g., MOD13Q1/MYD13Q1),
#'  use the code M*D_code_ (e.g., M*D13Q1)
#' @return list, containing the slots: `bandnames`, `quality_bandnames` and
#'  `indexes_bandnames`
#'
#' @author Lorenzo Busetto, phD (2014-2020) \email{lbusett@@gmail.com}
#' @note License: GPL 3.0
#' @export
#' @examples
#'
#' MODIStsp_get_prodlayers("M*13Q1")
#'
#' MODIStsp_get_prodlayers("MCD43C4")
#'
#'
MODIStsp_get_prodlayers <- function(prodname) {

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
  selprod <- selprod[[1]][["6"]]
  bandnames         <- selprod[["bandnames"]]
  quality_bandnames <- selprod[["quality_bandnames"]]
  indexes_bandnames <- selprod[["indexes_bandnames"]]
  prodnames   <- list(prodname          = prodname,
                      bandnames         = bandnames,
                      quality_bandnames = quality_bandnames,
                      indexes_bandnames = indexes_bandnames)
  prodnames
}

MODIStsp_get_prodnames <- function() {
  prod_opt_list <- load_prodopts()
  names(prod_opt_list)
}
