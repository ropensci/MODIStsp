
#' MODIStsp_read_xml
#' @description function used to parse the XML file used to store the characteristics of
#' MODIS Land Products and store them in the "prod_opts" data frame
#' @details The function parses the XML file product by product, stores data in a data frame
#' and saves the data frame within the "MODIStsp_previous" RData file as a list of lists
#' @param previous_file string filename of the RData in which to store the data parsed from the XML file
#' @param xml_file string filename of the XML file containing the MODIS products characteristics
#' @return NULL - retrieved data are stored in the specified RData file
#'
#' @author Lorenzo Busetto, phD (2014-2015) \email{busetto.l@@irea.cnr.it}
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom XML xmlParse xmlRoot xmlSize xmlToList
#' @importFrom plyr revalue
#' @importFrom hash hash
MODIStsp_read_xml <- function(previous_file = previous_file, xml_file = xml_file) {

  prod_opt_list <- NULL
  mod_prod_list <- NULL

  xmlfile <- xmlParse(xml_file)  # initialize xml parsing

  xmltop <- xmlRoot(xmlfile) #gives content of root

  n_products <- xmlSize(xmltop) #how many product available ? = elements in root

  for (prod in 1:n_products) { # cycle on available products

    prodopts <- list()	# initialize the prodopts list
    prodopts$product <- xmlToList(xmltop[[prod]][["name"]])
    prodopts$main_out_folder <- xmlToList(xmltop[[prod]][["main_out_folder"]])
    prodopts$native_res <- xmlToList(xmltop[[prod]][["native_res"]])
    prodopts$tiled <- xmlToList(xmltop[[prod]][["tiled"]])
    file_prefix_terra <- xmlToList(xmltop[[prod]][["file_prefix_terra"]])
    http_terra <- xmlToList(xmltop[[prod]][["http_terra"]])
    file_prefix_aqua <- xmlToList(xmltop[[prod]][["file_prefix_aqua"]])
    http_aqua <- xmlToList(xmltop[[prod]][["http_aqua"]])
    prodopts$www <- xmlToList(xmltop[[prod]][["www"]])
    prodopts$file_prefix <- hash("Terra" = file_prefix_terra,"Aqua" = file_prefix_aqua)
    prodopts$http <- hash("Terra" = http_terra,"Aqua" = http_aqua)
    prodopts$multiband_bsq <- T
    nbands <- xmlSize(xmltop[[prod]][["bands"]])  # number of original layers
    bandnames <- NULL
    band_fullname <- NULL
    datatype <- NULL
    nodata_in <- NULL
    nodata_out <- NULL
    for (band in 1:nbands) { # get chars of original layers
      bandnames <- c(bandnames,xmlToList(xmltop[[prod]][["bands"]][[band]][["bandname"]]))
      band_fullname <- c(band_fullname,xmlToList(xmltop[[prod]][["bands"]][[band]][["fullname"]]))
      datatype <- c(datatype,xmlToList(xmltop[[prod]][["bands"]][[band]][["datatype"]]))
      nodata_in <- c(nodata_in,xmlToList(xmltop[[prod]][["bands"]][[band]][["nodata_in"]]))
      nodata_out <- c(nodata_out,xmlToList(xmltop[[prod]][["bands"]][[band]][["nodata_out"]]))
    } #End Cycle on band
    prodopts$bandnames <- bandnames # store in prodopts
    prodopts$band_fullnames <- band_fullname
    datatype <- as.factor(datatype)
    datatype <- revalue(datatype, c("8-bit signed integer" = "Byte",			# Convert MODIS datatypes to R/Gdal datatypes
                                    "8-bit unsigned integer" = "Byte",
                                    "16-bit signed integer" = "Int16",
                                    "16-bit unsigned integer" = "UInt16",
                                    "32-bit signed integer" = "Int32",
                                    "32-bit unsigned integer" = "UInt32"),warn_missing = F)
    prodopts$datatype <- datatype
    prodopts$nodata_in <- nodata_in
    prodopts$nodata_out <- nodata_out
    prodopts$bandsel <- rep(0, length(prodopts$bandnames))

    nindexes <- xmlSize(xmltop[[prod]][["indexes"]])		# number of Spectral Indexes
    if (nindexes > 0 ) {
      indexes_bandnames <- NULL
      indexes_fullnames <- NULL
      indexes_formulas <- NULL
      indexes_nodata_out <- NULL
      for (index in 1:nindexes) {   # get charcteristics of indexes
        indexes_bandnames <- c(indexes_bandnames,xmlToList(xmltop[[prod]][["indexes"]][[index]][["indexes_bandname"]]))
        indexes_fullnames <- c(indexes_fullnames,xmlToList(xmltop[[prod]][["indexes"]][[index]][["indexes_fullname"]]))
        indexes_formulas <- c(indexes_formulas,xmlToList(xmltop[[prod]][["indexes"]][[index]][["indexes_formula"]]))
        indexes_nodata_out <- c(indexes_nodata_out,xmlToList(xmltop[[prod]][["indexes"]][[index]][["indexes_nodata_out"]]))
      } #End Cycle on index

      prodopts$indexes_bandnames <- indexes_bandnames		# store in prodopts
      prodopts$indexes_fullnames <- indexes_fullnames
      prodopts$indexes_formulas <- indexes_formulas
      prodopts$indexes_nodata_out <- indexes_nodata_out
      prodopts$indexes_bandsel <- rep(0, length(prodopts$indexes_bandnames))
    }   #end if on indexes existence
    nquality <- xmlSize(xmltop[[prod]][["quality_indicators"]])	# number of QIs
    if (nquality > 0 ) {
      quality_bandnames <- NULL
      quality_fullnames <- NULL
      quality_source <- NULL
      quality_bitN <- NULL
      for (quality in 1:nquality) {  # get charcteristics of QIs
        quality_bandnames <- c(quality_bandnames,xmlToList(xmltop[[prod]][["quality_indicators"]][[quality]][["quality_bandname"]]))
        quality_fullnames <- c(quality_fullnames,xmlToList(xmltop[[prod]][["quality_indicators"]][[quality]][["quality_fullname"]]))
        quality_source <- c(quality_source,xmlToList(xmltop[[prod]][["quality_indicators"]][[quality]][["quality_source"]]))
        quality_bitN <- c(quality_bitN,xmlToList(xmltop[[prod]][["quality_indicators"]][[quality]][["quality_bitN"]]))
      } #End Cycle on quality

      prodopts$quality_bandnames <- quality_bandnames
      prodopts$quality_fullnames <- quality_fullnames
      prodopts$quality_source <- quality_source
      prodopts$quality_bitN <- quality_bitN
      prodopts$quality_nodata_in <- rep("255", length(prodopts$quality_bandnames))  # nodata in for quality bands (dummy - always 255)
      prodopts$quality_nodata_out <- rep("255", length(prodopts$quality_bandnames)) # nodata out for quality bands (always 255)
      prodopts$quality_bandsel <- rep(0, length(prodopts$quality_bandnames))  	 #Selection of desired quality bands (all zeroes)

    } # end if on quality existence

    # At each cycle, add product name to mod_prod_list and prodopts to prod_opt_list
    mod_prod_list <- c(mod_prod_list, prodopts$product )
    prod_opt_list[[prodopts$product ]] <- prodopts


  }  #End Cycle on products

  # Add attributes to these 3 lists (this is used as a check when charging them)
  attr(mod_prod_list,"GeneratedBy") <- "MODIStsp"
  attr(prod_opt_list,"GeneratedBy") <- "MODIStsp"

  # Save the products list and the chars of the products in previous file
  dir.create(dirname(previous_file), recursive = TRUE, showWarnings = FALSE)
  save(prod_opt_list, mod_prod_list, file = previous_file)

}
