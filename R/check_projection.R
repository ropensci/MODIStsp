#' @title Check the validity of the input projection
#' @description helper function used to check that the input projection
#'  (passed as UTM zone, EPSG code, WKT string) is a valid projection for MODIStsp.
#' @param projection `character` or `integer` corresponding to the
#'  an EPSG code, a UTM zone (e.g. "32N") or a WKT representation of  a projection;
#' @param abort `logical` if TRUE, the function aborts in case an invalid invalid
#'  projection is passed. Otherwise, the function returns "NA", Default: TRUE
#' @param verbose `logical` if TRUE, return messages
#' @return `character` proj4string of the object or file
#' @note This function was forked from package `sprawl`, version 0.3.0.
#' @export
#' @importFrom sf st_crs
#' @importFrom stringr str_pad
#' @name check_projection
#' @rdname check_projection
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @author Luigi Ranghetti, phD (2017) <ranghetti.l@irea.cnr.it>
#'
#' @examples
#'
#' \dontrun{
#' check_projection("32632")
#'
#' check_projection("32631")
#'
#' check_projection(32633)
#'
#' check_projection(30, abort = FALSE)
#'
#' check_projection("example of invalid string", abort = FALSE)
#'
#' proj_wkt <- sf::st_as_text(sf::st_crs(32632))
#' check_projection(proj_wkt)
#'}

check_projection <- function(projection,
                             abort = FALSE,
                             verbose = TRUE) {
  UseMethod("check_projection")
}

#   ____________________________________________________________________________
#   Fallback method                                                         ####

#' @rdname check_projection
#' @method check_projection default
#' @export
check_projection.default  <- function(projection,
                                      abort = FALSE,
                                      verbose = TRUE) {
  call <- match.call()
  if (abort) {
    stop("check_projection --> ", call[[2]], " is not a valid epsg code or ", #nolint
         "WKT projection description. Aborting!")
  } else {
    if (verbose) {
      warning("check_projection --> ", call[[2]], " is not a valid epsg code or ", #nolint
              "WKT projection description. returning `NA`")
    }
    return(NA)
  }
}

#   ____________________________________________________________________________
#   Method for integer - check if it is a valid EPSG code                   ####

#' @rdname check_projection
#' @method check_projection numeric
#' @export
#' @importFrom sf st_crs
check_projection.numeric  <- function(projection,
                                      abort = FALSE,
                                      verbose = TRUE) {

  proj <- sf::st_crs(projection)
  if (is.na(proj$epsg)){
    if (abort == TRUE) {
      stop("check_projection --> Invalid EPSG code detected! Aborting!")
    } else {
      if (verbose) warning("check_projection --> Invalid EPSG code detected,
                          returning `NA`!")
      return(NA)
    }
  } else {
    return(projection)
  }
}

#   ____________________________________________________________________________
#   Method for character - check that it is a valid proj4string             ####

#' @rdname check_projection
#' @method check_projection character
#' @export
#' @importFrom sf st_crs
#' @importFrom stringr str_pad
check_projection.character  <- function(projection,
                                        abort = FALSE,
                                        verbose = TRUE) {

  # if it is a number, use check_projection.integer method
  if (suppressWarnings(!is.na(as.numeric(projection)))) {
    return(check_projection.numeric(as.integer(projection),
                                    abort = abort))
  }

  # check if it is a UTM zone - return the corresponding EPSG
  if (grepl("^[0-9]+[NnSs]$",projection)) {
    utm_zone <- stringr::str_pad(gsub("[NnSs]$","",projection), 2, "left", "0")
    utm_ns <- toupper(gsub("?[0-9]+","",projection))
    projection <- as.numeric(paste0("32",
                                    if (utm_ns == "N") {"6"} else {"7"},
                                    utm_zone))
    return(projection)
  }

  # Finally, see if it is a WKT - return verbatim
  projection_crs <- try(sf::st_crs(projection))

  if (inherits(projection_crs, "try-error") || is.na(projection_crs$proj4string)) {
    if (abort == TRUE) {
      stop("check_projection --> Invalid projection detected! Aborting!")
    } else {
      if (verbose) (warning("check_projection --> Invalid projection detected,
                           returning `NA`!"))
      return(NA)
    }
  } else {
    return(projection)
  }
}

#   ____________________________________________________________________________
#   Method for st_crs - get st_crs                                        ####

#' @rdname check_projection
#' @method check_projection crs
#' @export
check_projection.crs <- function(projection,
                                 abort = FALSE,
                                 verbose = TRUE) {
  return(projection)

}
