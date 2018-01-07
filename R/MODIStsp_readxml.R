#' @title Read MODIS products characteristics from XML
#' @description function used to parse the XML file used to store the characteristics of
#' MODIS Land Products and store them in the "prod_opts" data frame
#' @details The function parses the XML file product by product, stores data in a data frame
#' and saves the data frame within the "MODIStsp_previous" RData file as a list of lists
#' @param prodopts_file string filename of the RData in which to store the data
#' parsed from the XML file
#' @param xml_file string filename of the XML file containing the MODIS products
#' characteristics
#' @return NULL - retrieved data are stored in the specified RData file
#'
#' @author Lorenzo Busetto, phD (2014-2017) \email{lbusett@@gmail.com}
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom xml2 as_list read_xml
#'
MODIStsp_read_xml <- function(prodopts_file,
                              xml_file) {

  prod_opt_list <- list()

  xmldata <- xml2::as_list(xml2::read_xml(xml_file) )

  # names of the single products
  names_products <- names(xmldata)
  names_products <- names_products[names_products != "comment"]
  # cycle on available products
  for (prod in names_products) {

    prodopts      <- list()	# initialize the prodopts list
    prodopts_name <- xmldata[[prod]][["name"]][[1]]
    # number of available versions
    n_versions    <- length(xmldata[[prod]][["versions"]])

    for (n_version in 1:n_versions) {

      prod_data     <- xmldata[[prod]][["versions"]][[n_version]]
      # General info
      version_name <- prod_data[["v_number"]][[1]]
      prodopts[[version_name]] <- list()	# one element per version
      
      prodopts[[version_name]]$v_number <- version_name
      
      prodopts[[version_name]]$prod_fullname <- prod_data[["prod_fullname"]][[1]] #nolint
      prodopts[[version_name]]$main_out_folder <-
        prod_data[["main_out_folder"]][[1]]

      prodopts[[version_name]]$native_res <- prod_data[["native_res"]][[1]]

      prodopts[[version_name]]$tiled      <- prod_data[["tiled"]][[1]]

      prodopts[[version_name]]$combined   <- prod_data[["combined"]][[1]]

      prodopts[[version_name]]$cat01      <- xmldata[[prod]][["cat_01"]][[1]]
      prodopts[[version_name]]$cat02      <- xmldata[[prod]][["cat_02"]][[1]]

      file_prefix_terra <- xmldata[[prod]][["file_prefix_terra"]][[1]]
      http_terra        <- prod_data[["http_terra"]][[1]]
      ftp_terra         <- prod_data[["ftp_terra"]][[1]]

      file_prefix_aqua <- xmldata[[prod]][["file_prefix_aqua"]][[1]]
      http_aqua        <- prod_data[["http_aqua"]][[1]]
      ftp_aqua         <- prod_data[["ftp_aqua"]][[1]]

      prodopts[[version_name]]$www <- prod_data[["www"]][[1]]
      prodopts[[version_name]]$file_prefix <- list(
        "Terra" = file_prefix_terra,
        "Aqua" = file_prefix_aqua
      )
      prodopts[[version_name]]$http <- list("Terra" = http_terra,
                                            "Aqua" = http_aqua)
      prodopts[[version_name]]$ftp  <- list("Terra" = ftp_terra,
                                            "Aqua" = ftp_aqua)

      # Band info
      nbands       <- length(prod_data[["bands"]])
      bandnames    <- band_fullname <- datatype <- nodata_in <-
        nodata_out <- scale_factor <- offset <- NULL
      # get chars of original layers
      for (band in seq_len(nbands)) {
        bandnames     <- c(bandnames,
                           prod_data[["bands"]][[band]][["bandname"]][[1]])
        band_fullname <- c(band_fullname,
                           prod_data[["bands"]][[band]][["fullname"]][[1]])
        datatype      <- c(datatype,
                           prod_data[["bands"]][[band]][["datatype"]][[1]])
        nodata_in     <- c(nodata_in,
                           prod_data[["bands"]][[band]][["nodata_in"]][[1]])
        nodata_out    <- c(nodata_out,
                           prod_data[["bands"]][[band]][["nodata_out"]][[1]])
        scale_factor  <- c(scale_factor,
                           prod_data[["bands"]][[band]][["scale_factor"]][[1]])
        offset        <- c(offset,
                           prod_data[["bands"]][[band]][["offset"]][[1]])
      }
      #End Cycle on bands - store in prodopts ----
      prodopts[[version_name]]$bandnames      <- bandnames
      prodopts[[version_name]]$band_fullnames <- band_fullname
      datatype <- as.factor(datatype)

      # Convert MODIS datatypes to R/Gdal datatypes
      #
      levels(datatype)[levels(datatype) == "8-bit signed integer"]    <- "Byte"
      levels(datatype)[levels(datatype) == "8-bit unsigned integer"]  <- "Byte"
      levels(datatype)[levels(datatype) == "16-bit signed integer"]   <- "Int16"
      levels(datatype)[levels(datatype) == "16-bit unsigned integer"] <- "UInt16" #nolint
      levels(datatype)[levels(datatype) == "32-bit signed integer"]   <- "Int32"
      levels(datatype)[levels(datatype) == "32-bit unsigned integer"] <- "UInt32" #nolint

      prodopts[[version_name]]$datatype     <- datatype
      prodopts[[version_name]]$nodata_in    <- nodata_in
      prodopts[[version_name]]$nodata_out   <- nodata_out
      prodopts[[version_name]]$scale_factor <- scale_factor
      prodopts[[version_name]]$offset       <- offset

      # Indices info

      # number of Spectral Indexes
      nindexes <- length(prod_data[["indexes"]])
      if (nindexes > 0) {
        indexes_bandnames <- indexes_fullnames <- indexes_formulas <-
          indexes_nodata_out <- NULL

        # get charcteristics of indexes
        for (index in seq_len(nindexes)) {
          indexes_bandnames  <- c(indexes_bandnames,
                                  prod_data[["indexes"]][[index]][["indexes_bandname"]][[1]]) #nolint
          indexes_fullnames  <- c(indexes_fullnames,
                                  prod_data[["indexes"]][[index]][["indexes_fullname"]][[1]]) #nolint
          indexes_formulas   <- c(indexes_formulas,
                                  prod_data[["indexes"]][[index]][["indexes_formula"]][[1]]) #nolint
          indexes_nodata_out <- c(indexes_nodata_out,
                                  prod_data[["indexes"]][[index]][["indexes_nodata_out"]][[1]]) #nolint

        }

        #End Cycle on index - store in prodopts ----
        prodopts[[version_name]]$indexes_bandnames  <- indexes_bandnames
        prodopts[[version_name]]$indexes_fullnames  <- indexes_fullnames
        prodopts[[version_name]]$indexes_formulas   <- indexes_formulas
        prodopts[[version_name]]$indexes_nodata_out <- indexes_nodata_out

      }   #end if on indexes existence

      # Quality flag info
      nquality <- length(prod_data[["quality_indicators"]])
      if (nquality > 0 ) {
        quality_bandnames <- quality_fullnames <- quality_source <-
          quality_bitN <- NULL
        # get charcteristics of QIs
        for (quality in seq_len(nquality)) {
          quality_bandnames <- c(
            quality_bandnames,
            prod_data[["quality_indicators"]][[quality]][["quality_bandname"]][[1]] #nolint
          )
          quality_fullnames <- c(
            quality_fullnames,
            prod_data[["quality_indicators"]][[quality]][["quality_fullname"]][[1]] #nolint
          )
          quality_source <- c(
            quality_source,
            prod_data[["quality_indicators"]][[quality]][["quality_source"]][[1]] #nolint
          )

          quality_bitN <- c(
            quality_bitN,
            prod_data[["quality_indicators"]][[quality]][["quality_bitN"]][[1]]
          )

        }

        #End Cycle on quality - store in prodopts
        prodopts[[version_name]]$quality_bandnames  <- quality_bandnames
        prodopts[[version_name]]$quality_fullnames  <- quality_fullnames
        prodopts[[version_name]]$quality_source     <- quality_source
        prodopts[[version_name]]$quality_bitN       <- quality_bitN
        # nodata in for quality bands (dummy - always 255)
        prodopts[[version_name]]$quality_nodata_in  <- rep(
          "255", length(prodopts[[version_name]]$quality_bandnames)
        )
        # nodata out for quality bands (dummy - always 255)
        prodopts[[version_name]]$quality_nodata_out <- rep(
          "255", length(prodopts[[version_name]]$quality_bandnames))

      } # end if on quality existence

    } # end of n_versions cycle

    # At each cycle, add product name to mod_prod_list and prodopts to
    # prod_opt_list

    prod_opt_list[[prodopts_name]] <- prodopts

  }  #End Cycle on products

  # Add attributes to these 3 lists (this is used as a check when charging them)
  attr(prod_opt_list, "GeneratedBy")     <- "MODIStsp"
  attr(prod_opt_list, "MODIStspVersion") <- packageVersion("MODIStsp")

  # Save the products list and the chars of the products in previous file
  dir.create(dirname(prodopts_file), recursive = TRUE, showWarnings = FALSE)
  save(prod_opt_list, file = prodopts_file)

}
