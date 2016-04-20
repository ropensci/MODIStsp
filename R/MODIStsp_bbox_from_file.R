#' bbox_from_file
#' @description
#' Ancillary function used to retrieve the bounding box of a specified spatial file recognised by GDAL or ORG:
#' the function reads the extent using gdalinfo or ogrinfo, converting it in a specified CRS.
#'
#' @param file_path The path of the spatial file.
#' @param out_crs The output projection (string format).
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#'
#' @importFrom gdalUtils gdalinfo gdalsrsinfo ogrinfo
#' @importFrom sp CRS
#' @importFrom rgeos show
#' @note License: GPL 3.0
#'
#' @importFrom sp CRS
#' @importFrom gdalUtils gdalsrsinfo gdalinfo ogrinfo
bbox_from_file <- function(file_path, out_crs) {

  # Retrieve CRS using gdal: if fails, then the file is not a valid spatial file
  reference_crs <- try(gdalsrsinfo(file_path, as.CRS = TRUE), silent = TRUE)
  reference_gdalinfo <- suppressWarnings(try(gdalinfo(file_path), silent = TRUE))
  reference_ogrinfo <- suppressWarnings(try(ogrinfo(file_path,al = TRUE,so = TRUE), silent = TRUE))

  if (class(reference_crs) == "try-error" | (!is.null(attr(reference_gdalinfo,"status")) & !is.null(attr(reference_ogrinfo,"status")))) {
    stop(paste("File format not recognized by GDAL or OGR.",
               if (class(reference_crs) == "try-error") {
                 paste("\n\nDetails:",reference_crs)
                 } else {
                   ""
                   } ))
  } else if (is.na(reference_crs@projargs)) {
    stop("The CRS of the file is not recognized!") # TODO: try to retrieve from WKT, or ask to insert as proj.4 string
  } else {

    # If it does not fail, then retrieve the bounding box
    if (is.null(attr(reference_ogrinfo,"status"))) {
      reference_ogrinfo <- ogrinfo(file_path, al = TRUE,so = TRUE)
      reference_bbox <- matrix(na.omit(as.numeric(unlist(strsplit(gsub("([^0-9.\\-]+|( - ))+"," ", reference_ogrinfo[grep("Extent:",reference_ogrinfo)] )," ")))), nrow = 2)
    } else if (is.null(attr(reference_gdalinfo,"status"))) {
      reference_gdalinfo <- gdalinfo(file_path)
      reference_bbox <- cbind( na.omit(as.numeric(unlist(strsplit(gsub("[^0-9.\\-]+"," ",reference_gdalinfo[grep("^Lower Left",reference_gdalinfo)])," "))))[1:2],
                               na.omit(as.numeric(unlist(strsplit(gsub("[^0-9.\\-]+"," ",reference_gdalinfo[grep("^Upper Right",reference_gdalinfo)])," "))))[1:2])
    }

    # Convert the bounding box in the chosen projection (ensuring to full include the original one)
    bbox_out <- reproj_bbox(reference_bbox, reference_crs@projargs, out_crs, enlarge = TRUE)

    # Get the units and kind of proj

    proj <- head(strsplit(tail(strsplit(CRS(out_crs)@projargs, "+proj=")[[1]],1)," +")[[1]],1)
    units <- ifelse(proj == "longlat", "deg","metric")

  }

  return(bbox_out)
}
