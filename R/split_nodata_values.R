#' @title Split NODATA values or create matrix for reclassification
#' @name split_nodata_values
#' @description Internal functions:
#'  [split_nodata_values] splits the ranges of NODATA saved
#'  in the xml product file to a readable vector of NoData values;
#'  [create_nodata_rcl] creates the matrix for the reclassification of NODATA
#'  values to be used with [raster::reclassify] function.
#' @param nodata_in Character vector corresponding to input NoData values
#'  as saved in the xml product file (one or more values per band).
#' @param nodata_out Character vector corresponding to output NoData values
#'  as saved in the xml product file (one single value per band).
#' @param take_all Logical: if TRUE (default), all the NoData values are considered;
#'  if FALSE, only the last one is taken.
#'  See "details" for the meaning of this parameter.
#' @return [split_nodata_values]  returns a list with the same length
#'  of `nodata_in` vector, in which each element
#'  is a vector with all the NoData values.
#' @details MODIS products can have more than one NoData values (sometimes
#'  with different meanings, e.g. 255 = "fill" and 254 = "detector saturated"
#'  in [MOD09A1](https://nsidc.org/data/mod10a1) product). By setting
#'  "Change NoData values" to "Yes" in the GUI, all the NoData values are
#'  coerced to one single new NoData value; conversely, setting it to "No"
#'  only one value is assumed to be NoData.
#'  The parameter `take_all` is assumed to be used in this way, by using this
#'  function with `take_all = TRUE` with "Change NoData values" = "Yes" and
#'  `take_all = FALSE` with "Change NoData values" = "No".
#'
#'  In the xml product file, NoData ranges are set as:
#'  - `x` for products with single NoData values;
#'  - `x,y,z` for products with a vector of NoData values;
#'  - `x:y` for products with a range of NoData values;
#'  - `x:y,z` for a combination of NoData ranges and/or values.
#'
#'  In [split_nodata_values] *NoData values are assumed to be integer*:
#'  this means that intervals are splitted in integer values
#'  (e.g. "250:255" becomes "250 251 252 253 254 255").
#'  Conversely, function [create_nodata_rcl] creates intervals, so it can also
#'  manage float values (in practice, this should not make difference within
#'  MODIS products, since NoData values are always integer values).
#'
#'  This function interprets these strings and convert them in vectors with
#'  single values.
#'  Notice that the first NoData value is the only one which is considered if
#'  'Change NoData values' was set to 'No'.
#' @author Luigi Ranghetti, phD (2018)

split_nodata_values <- function(nodata_in, take_all = TRUE) {

  # split values separated by ","
  nodata_split1 <- strsplit(as.character(nodata_in), ",")

  # create vectors where ranges are found
  nodata_split2 <- lapply(nodata_split1,function(x) {strsplit(x, ":")})
  nodata_split3 <- lapply(nodata_split2, function(x) {
    unlist(lapply(x, function(y) {seq(y[1],y[length(y)])}))
  })

  # return the list
  if (take_all) {
    nodata_split3
  } else {
    # lapply(nodata_split3, function(x){x[length(x)]})
    lapply(nodata_split3, function(x){x[1]})
  }

}

#' @name create_nodata_rcl
#' @return [create_nodata_rcl] returns a list of matrices in the format
#'  specified for parameter `rcl` in [raster::reclassify].
#'  The parameter `right` is intended to be used as `right = NA`.
#' @rdname split_nodata_values
#' @examples
#' MODIStsp:::create_nodata_rcl(c("255","250,254:255"), c("255","255"))

create_nodata_rcl <- function(nodata_in, nodata_out) {

  # check vector lengths
  if (length(nodata_in) != length(nodata_out)) {
    stop(paste0(
      "Error in split_nodata_values: ",
      "nodata_in and nodata_out must have the same length."
    ))
  }

  # split values separated by ","
  nodata_split1 <- strsplit(as.character(nodata_in), ",")

  # create matrices for reclassification
  nodata_split2 <- lapply(nodata_split1,function(x) {strsplit(x, ":")})
  nodata_rcl <- lapply(seq_along(nodata_split2), function(i) {
    do.call(rbind, lapply(seq_along(nodata_split2[[i]]), function(j) {
      as.numeric(c(nodata_split2[[i]][[j]][1],
                   nodata_split2[[i]][[j]][length(nodata_split2[[i]][[j]])],
                   nodata_out[i]))
    }))
  })
  nodata_rcl <- lapply(nodata_rcl, function(x) {
    dimnames(x) <- list(NULL,c("from","to","becomes")); x
  })

  # return the matrix
  nodata_rcl

}
