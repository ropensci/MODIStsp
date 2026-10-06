#' @title Find the names of MODIS images corresponding to the selected dates
#   and tiles
#' @description Accessory function to find the names of HDF images corresponding
#' to a given date and interval of spatial tiles. Names (and their Earthdata
#' Cloud download URLs) are retrieved by querying the NASA Common Metadata
#' Repository (CMR).
#' @param http `character` LP DAAC-style product URL carried in the product
#'   options DB. Only used here to derive the product `short_name`/`version`.
#' @param used_server `character` can assume values "http" or "offline".
#' @param n_retries `numeric` number of times the access to the CMR server
#'   should be retried in case of error before quitting, Default: 20.
#' @param date_dir `character` folder name (a.k.a. acquisition date, format
#'  "YYYY.MM.DD") for which MODIS files are to be identified.
#' @param v `integer array` containing a sequence of the vertical tiles of interest
#'   (e.g., c(18,19)).
#' @param h `integer array` containing a sequence of the horizontal tiles of interest
#'   (e.g., c(3,4)).
#' @param tiled `numeric [0/1]` indicates if the product to be downloaded is
#'   tiled or not tiled. 1 = tiled product; 0 = non-tiled product (resolution 0.05 deg).
#' @param out_folder_mod  `character` folder where hdf files have to be stored.
#' @param gui `logical` indicates if processing was called within the GUI environment
#'   or not. If not, processing messages are redirected direct to the log file.
#' @return `character array` containing names of HDF images corresponding to the
#'  requested tiles available for the product in the selected date. On "http"
#'  the returned vector carries an attribute `"urls"`, a named character vector
#'  mapping each HDF file name to its Earthdata Cloud download URL.
#' @author Original code by Babak Naimi (\code{.getModisList}, in
#' \href{https://r-gis.net/?q=ModisDownload}{ModisDownload.R})
#' modified to adapt it to MODIStsp scheme by:
#' @author Lorenzo Busetto, phD (2014-2016)
#' @author Luigi Ranghetti, phD (2016)
#' @note License: GPL 3.0
#' @importFrom stringr str_split str_pad
get_mod_filenames <- function(http,
                              used_server,
                              n_retries,
                              date_dir,
                              v,
                              h,
                              tiled,
                              out_folder_mod,
                              gui) {

  urls <- NULL

  if (used_server == "http") {
    #   ________________________________________________________________________
    #   Retrieve available hdf files for the date from NASA CMR             ####

    prod_vers <- parse_prod_version(http)

    granules <- get_mod_granules_cmr(
      short_name = prod_vers[["short_name"]],
      version    = prod_vers[["version"]],
      date_from  = date_dir,
      date_to    = date_dir,
      v = v, h = h, tiled = tiled,
      n_retries  = n_retries
    )

    getlist <- granules$hdf_name
    urls    <- stats::setNames(granules$url, granules$hdf_name)

  }

  # __________________________________________________________________________
  # Retrieve the list of hdf files matching the product / version / date ####
  # in case of offline mode
  if (used_server == "offline") {

    getlist <- list.files(out_folder_mod, "\\.hdf$")
    sel_prod_vers <- unlist(
      stringr::str_split(
        gsub("https:\\/\\/[A-Za-z0-9\\.]+\\/[A-Z]+\\/([A-Z0-9]+)\\.([0-9]+)\\/", "\\1 \\2", #nolint
             http
        ), " ")
    )
    getlist <- getlist[grep(paste0(sel_prod_vers[1], "\\.A",
                                   strftime(as.Date(
                                     date_dir,
                                     format = "%Y.%m.%d"
                                   ), "%Y%j"), "(?:\\.h[0-9]{2}v[0-9]{2})?\\.",
                                   sel_prod_vers[2],
                                   "\\.[0-9]+\\.hdf$"),
                            getlist)]

  }

  #   __________________________________________________________________________
  #   create the array of MODIS files to be processed by retrieveing from   ####
  #   getlist the images corresponding to the h and v tiles of interest

  Modislist <- c()
  if (tiled == 1) {
    for (vv in v) {
      for (hh in h) {
        vc <- stringr::str_pad(vv, 2, "left", "0")
        hc <- stringr::str_pad(hh, 2, "left", "0")
        ModisName <- grep(
          pattern = ".hdf$",
          x       = grep(paste0("h", hc, "v", vc), getlist, value = TRUE),
          value = TRUE)
        if (length(ModisName) >= 1)
          Modislist <- c(Modislist, ModisName[1])
      }
    }
  } else {
    Modislist <- grep(".hdf$", getlist, value = TRUE)
  }

  # Attach the download URLs (http mode only) so MODIStsp_download does not
  # need to query CMR again.
  if (!is.null(urls) && length(Modislist) > 0) {
    attr(Modislist, "urls") <- urls[Modislist]
  }
  return(Modislist)
}
