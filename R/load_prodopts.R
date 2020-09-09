#' @title Load characteristics of the different MODIS products
#' @description FUNCTION_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details Load characteristics of the different MODIS products from `prodopts_file`
#   or load them from the XML options file and create the `prodopts_file` RData
#   file if the `prodopts_file` got lost or is outdated (this because reading
#   from RData is much faster, but the XML allows for easier maintenance and
#   update of the MODIS products descriptions)
#' @rdname load_prodopts
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom utils packageVersion

load_prodopts <- function() {

  # RData file containing products' characteristics
  prodopts_file <- system.file("ExtData", "MODIStsp_ProdOpts.RData",
                               package = "MODIStsp")
  # dir.create(prodopts_dir, showWarnings = FALSE, recursive = TRUE)
  # prodopts_file <- file.path(prodopts_dir, "MODIStsp_ProdOpts.RData")
  # # XML file describing MODIS products
  xml_file <- system.file("ExtData", "MODIStsp_ProdOpts.xml",
                          package = "MODIStsp")

  if (file.exists(prodopts_file)) {
    prod_opt_list <- get(load(prodopts_file))
    if (is.null(attr(prod_opt_list, "MODIStspVersion"))) {
      reload_prodlist <- TRUE
    } else {
      # load if prod_opt_list is old
      reload_prodlist <- attr(prod_opt_list, "MODIStspVersion") <
        utils::packageVersion("MODIStsp")
    }
  } else {
    reload_prodlist <- TRUE
  }
  if (reload_prodlist) {
    #nocov start
    mess_text <- "Reading the MODIS products' characteristics from XML. Please wait!" #nolint
    message(mess_text)

    MODIStsp_read_xml(prodopts_file = prodopts_file,
                      xml_file      = xml_file)
    prod_opt_list <- get(load(prodopts_file))

  } # End IF on load prodopts
  prod_opt_list
  #nocov end
}
