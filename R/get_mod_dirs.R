#' @title Get list of MODIS acquisition dates from NASA CMR
#' @description Accessory function to get the full list of acquisition dates
#'  available for the selected MODIS product within the time range selected for
#'  processing. Dates are retrieved by querying the NASA Common Metadata
#'  Repository (CMR) instead of scraping the (now retired) LP DAAC Data Pool
#'  HTML directory listings.
#' @param http `character` LP DAAC-style product URL carried in the product
#'   options DB (e.g. "https://e4ftl01.cr.usgs.gov/MOLT/MOD13Q1.061/"). Only
#'   used here to derive the product `short_name` and `version`.
#' @param download_server `character ["http" | "offline"]` download service
#'   to be used; if NA, the script tries to download with http.
#' @param yy `character` Year for which the available acquisition dates are to
#'  be identified.
#' @param n_retries `numeric` number of times the access to the CMR server
#'   should be retried in case of error before quitting, Default: 20.
#' @param gui `logical`` indicates if processing was called from the GUI
#'   environment or not. If not, processing messages are sent to a log file
#'   instead than to the console/GTK progress windows.
#' @param out_folder_mod  `character` output folder for MODIS HDF storage.
#' @param v `integer array` vertical tiles of interest (used to restrict the CMR
#'   query for tiled products). Default: NULL.
#' @param h `integer array` horizontal tiles of interest. Default: NULL.
#' @param tiled `numeric [0/1]` 1 = tiled product; 0 = non-tiled. Default: 1.
#' @return `character array` listing all available folders (a.k.a. dates) for
#'   the requested MODIS product, for the years included in the time range
#'   selected for processing.
#' @author Original code by Babak Naimi (\code{.getModisList}, in
#' \href{https://r-gis.net/?q=ModisDownload}{ModisDownload.R})
#' modified to adapt it to MODIStsp scheme by:
#' @author Lorenzo Busetto, phD (2014-2017)
#' @author Luigi Ranghetti, phD (2016-2017)
#' @note License: GPL 3.0
#' @importFrom stringr str_sub str_split

get_mod_dirs <- function(http,
                         download_server,
                         yy,
                         n_retries,
                         gui,
                         out_folder_mod,
                         v = NULL,
                         h = NULL,
                         tiled = 1) {

  #   __________________________________________________________________________
  #   retrieve available acquisition dates from NASA CMR                    ####

  if (download_server == "http") {

    prod_vers <- parse_prod_version(http)

    granules <- try(
      get_mod_granules_cmr(
        short_name = prod_vers[["short_name"]],
        version    = prod_vers[["version"]],
        date_from  = paste0(yy, ".01.01"),
        date_to    = paste0(yy, ".12.31"),
        v = v, h = h, tiled = tiled,
        n_retries  = n_retries
      ),
      silent = TRUE
    )

    if (inherits(granules, "try-error")) {
      message(
        "[", date(), "] Error: the NASA CMR server seems to be unreachable! ",
        "Please try again later. Aborting!"
      )
      date_dirs <- character()
      attr(date_dirs, "server") <- "unreachable"
      return(date_dirs)
    }

    date_dirs <- sort(unique(granules$date))
    attr(date_dirs, "server") <- "http"

  }
  #   __________________________________________________________________________
  #   In offline mode, retrieve the dates of acquisition of hdfs already
  #   available in `out_folder_mod`

  if (download_server == "offline") {

    # Retrieve the list of hdf files matching the product / version
    items <- list.files(out_folder_mod, "\\.hdf$")
    sel_prod_vers <- unlist(stringr::str_split(gsub(
      "https:\\/\\/[A-Za-z0-9\\.]+\\/[A-Z]+\\/([A-Z0-9]+)\\.([0-9]+)\\/", "\\1 \\2", #nolint
      http), " "))
    items <- items[grep(paste0(
      sel_prod_vers[1], "\\.A20[0-9]{5}\\.(?:h[0-9]{2}v[0-9]{2}\\.)?",  #nolint
      sel_prod_vers[2], "\\.[0-9]+\\.hdf$"), items)]

    # Extract dates

    date_dirs <- unique(strftime(as.Date(gsub(
      paste0(sel_prod_vers[1], "\\.A(20[0-9]{5})\\..*"),"\\1", #nolint
      items), format = "%Y%j"), "%Y.%m.%d"))
    attr(date_dirs, "server") <- "offline"
  }

  return(date_dirs)

}

#' @title Parse MODIS product short name and version from an LP DAAC URL
#' @description Helper extracting the product `short_name` and `version` from the
#'   LP DAAC-style product URL stored in the product options DB, e.g.
#'   "https://e4ftl01.cr.usgs.gov/MOLT/MOD13Q1.061/" -> c("MOD13Q1", "061").
#' @param http `character` product URL.
#' @return named `character` vector with `short_name` and `version`.
#' @keywords internal
parse_prod_version <- function(http) {
  # take the last non-empty path segment, e.g. "MOD13Q1.061"
  http <- sub("/+$", "", http)
  last <- basename(http)
  parts <- strsplit(last, "\\.")[[1]]
  c(short_name = parts[1], version = parts[2])
}
