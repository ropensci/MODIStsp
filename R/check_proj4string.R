#' @title Check the validity of the input projection
#' @description helper function used to check that the input projection
#'  (passed as UTM zone, EPSG code, PROJ4 string or [CRS] object)
#'  is a valid string or CRS.
#' @param projection `character` or `integer` corresponding to the
#'  proj4string to be checked, the EPSG code or a numeric UTM zone; alternatively,
#'  a [CRS] object is accepted.
#' @param abort `logical` if TRUE, the function aborts in case an invalid
#'  projection is passed. Otherwise, the function returns "NA", Default: TRUE
#' @return `character` proj4string of the object or file
#' @note This function was forked from package `sprawl`, version 0.3.0.
#'
#' @importFrom sp proj4string CRS
#' @importFrom rgdal checkCRSArgs CRSargs
#' @name check_proj4string
#' @rdname check_proj4string
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @author Luigi Ranghetti, phD (2017) <ranghetti.l@irea.cnr.it>
#'
#' @examples
#'
#' check_proj4string("+init=epsg:32632")
#'
#' check_proj4string("32631")
#'
#' check_proj4string(32633)
#'
#' check_proj4string(30)
#'
#' check_proj4string("example of invalid string")
#'
#' library(sp)
#' check_proj4string(CRS("+init=epsg:32632"))
#'
#' library(raster)
#'  in_rast <- read_rast(system.file("extdata/OLI_test",
#'   "oli_multi_1000_b2.tif", package = "sprawl.data"))
#' check_proj4string(in_rast@crs)
#'
check_proj4string <- function(projection,
                              abort = FALSE,
                              verbose = TRUE) {
  UseMethod("check_proj4string")
}

#   ____________________________________________________________________________
#   Fallback method                                                         ####

#' @rdname check_proj4string
#' @method check_proj4string default
#' @export
check_proj4string.default  <- function(projection,
                                       abort = FALSE,
                                       verbose = TRUE) {
  call <- match.call()
  if (abort) {
    stop("check_proj4string --> ", call[[2]], " is not a valid CRS object or ",
         "proj4string. Aborting!")
  } else {
    if(verbose) {
      warning("check_proj4string --> ", call[[2]], " is not a valid CRS object or ",
              "proj4string. returning `NA`")
    }
    return(NA)
  }
}

#   ____________________________________________________________________________
#   Method for integer - check if it is a valid EPSG code or UTM zone       ####

#' @rdname check_proj4string
#' @method check_proj4string numeric
#' @export
#' @importFrom rgdal checkCRSArgs
check_proj4string.numeric  <- function(projection,
                                       abort = FALSE,
                                       verbose = TRUE) {
  
  # if it is 0<proj4string<=60, interpret as UTM zone
  if (projection > 0 & projection <= 60) {
    proj4string <- paste0("+proj=utm ",
                          "+zone=",projection," ",
                          "+datum=WGS84 +units=m +no_defs ",
                          "+ellps=WGS84 +towgs84=0,0,0")
    return(rgdal::checkCRSArgs(proj4string)[[2]])
  }
  
  # else, try to interpret as EPSG code
  proj4string <- paste0("+init=epsg:",projection)
  if (rgdal::checkCRSArgs(proj4string)[[1]] == FALSE) {
    if (abort == TRUE) {
      stop("check_proj4string --> Invalid EPSG code detected! Aborting!")
    } else {
      if(verbose) warning("check_proj4string --> Invalid EPSG code detected,
                          returning `NA`!")
      return(NA)
    }
  } else {
    return(rgdal::checkCRSArgs(proj4string)[[2]])
  }
  }

#   ____________________________________________________________________________
#   Method for character - check that it is a valid proj4string             ####

#' @rdname check_proj4string
#' @method check_proj4string character
#' @export
#' @importFrom rgdal checkCRSArgs
check_proj4string.character  <- function(projection,
                                         abort = FALSE,
                                         verbose = TRUE) {
  
  # if it is a number, use check_proj4string.integer method
  if (suppressWarnings(!is.na(as.numeric(projection)))) {
    return(check_proj4string.numeric(as.integer(projection),
                                     abort = abort))
  }
  
  if (rgdal::checkCRSArgs(projection)[[1]] == FALSE) {
    # browser()
    if (abort == TRUE) {
      stop("check_proj4string --> Invalid projection detected! Aborting!")
    } else {
      if(verbose) (warning("check_proj4string --> Invalid projection detected,
                           returning `NA`!"))
      return(NA)
    }
  } else {
    return(rgdal::checkCRSArgs(projection)[[2]])
  }
}

#   ____________________________________________________________________________
#   Method for CRS - get proj4string                                        ####

#' @rdname check_proj4string
#' @method check_proj4string CRS
#' @export
check_proj4string.CRS  <- function(projection,
                                   abort = FALSE,
                                   verbose = TRUE) {
  return(CRSargs(projection))
}