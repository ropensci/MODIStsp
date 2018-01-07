#' @title Find the names of MODIS images corresponding to the selected dates
#   and tiles
#' @description Accessory function to find the names of HDF images corresponding
#' to a given date and interval of spatial tiles within the lpdaac archive.
#' @param http `character` url of http site on lpdaac corresponding to a given MODIS
#'   product.
#' @param ftp `character` url of ftp site corresponding to a given MODIS product.
#' @param used_server `character` can assume values "http" or "ftp" depending on the
#'  used download server; it cannot be NA.
#' @param user `character` username for earthdata server (Ignored if used_server = "ftp").
#' @param password `character` password for earthdata server  (Ignored if used_server = "ftp").
#' @param n_retries `numeric` number of times the access to the http/ftp server
#'   should be retried in case of error before quitting, Default: 20.
#' @param date_dir `character array` array of folder names corresponding to acquisition
#'  containing dates where MODIS files to be downloaded are to be identified
#'  (return array from `get_mod_dates`).
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
#'  requested tiles available for the product in the selected date
#' @author Original code by Babak Naimi (\code{.getModisList}, in
#' \href{http://r-gis.net/?q=ModisDownload}{ModisDownload.R})
#' modified to adapt it to MODIStsp scheme and to http archive (instead than old
#'  FTP) by:
#' @author Lorenzo Busetto, phD (2014-2016) \email{lbusett@@gmail.com}
#' @author Luigi Ranghetti, phD (2016) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom gWidgets gconfirm
#' @importFrom httr GET timeout authenticate content
#' @importFrom stringr str_split str_pad
get_mod_filenames <- function(http,
                              ftp,
                              used_server,
                              user,
                              password,
                              n_retries,
                              date_dir,
                              v,
                              h,
                              tiled,
                              out_folder_mod,
                              gui) {
  success <- FALSE
  if (used_server == "http") {
    #   ________________________________________________________________________
    #   Retrieve available hdf files in case of http download               ####

    # http folders are organized by date subfolders containing all tiles
    while (!success) {

      response <- httr::RETRY("GET",
                              paste0(http, date_dir, "/"),
                              httr::authenticate(user, password),
                              times = n_retries,
                              pause_base = 0.1,
                              pause_cap = 10,
                              quiet = FALSE)

      # On interactive execution, after n_retries attempt ask if quit or ----
      # retry
      if (response$status_code != 200) {
        if (gui) {
          #nocov start
          confirm <- gWidgets::gconfirm(
            "http server seems to be down! Do you want to retry?",
            icon = "question")
          if (!confirm) stop("You selected to abort processing. Goodbye!")
          #nocov end
        } else {
          stop("[", date(), "] Error: http server seems to be down! ",
                  "Switching to ftp!")
        }
      } else {
        getlist <- strsplit(httr::content(response, "text", encoding = "UTF-8"),
                            "\r*\n")[[1]]
        getlist <- getlist[grep(
          ".*>([A-Z0-9]+\\.A[0-9]+\\.?[hv0-9]*\\.[0-9]+\\.[0-9]+\\.hdf)<.*", #nolint
          getlist)]
        getlist <- gsub(
          ".*>([A-Z0-9]+\\.A[0-9]+\\.?[hv0-9]*\\.[0-9]+\\.[0-9]+\\.hdf)<.*", "\\1", #nolint
          getlist)
        success <- TRUE

      }
    }
  }
  success <- FALSE
  if (used_server == "ftp") {
    #   ______________________________________________________________________
    #   Retrieve available hdf files in case of ftp download             ####

    # ftp folders are organized by /year/date subfolders, so there is an
    # additional level to be "parsed" wrt http
    date_year <- strftime(as.Date(date_dir, format = "%Y.%m.%d"), "%Y")
    date_doy  <- strftime(as.Date(date_dir, format = "%Y.%m.%d"), "%j")

    while (!success) {
      response <- httr::RETRY("GET",
                              paste0(ftp, date_year, "/", date_doy, "/"),
                              # httr::authenticate(user, password),
                              times = n_retries,
                              pause_base = 0.1,
                              pause_cap = 10,
                              quiet = FALSE)
      # On interactive execution, after n_retries attempt ask if quit or ----
      # retry
      if (response$status_code != 226) {
        if (gui) {
          #nocov start
          confirm <- gWidgets::gconfirm(
            "ftp server seems to be down! Do you want to retry?",
            icon = "question")
          if (!confirm) stop("You selected to abort processing. Goodbye!")
          #nocov end
        } else {
          stop("[", date(), "] Error: ftp server seems to be down! ",
               "Aborting! Please try again later!")
        }
      } else {

        getlist <- strsplit(httr::content(response, "text", encoding = "UTF-8"),
                            "\r*\n")[[1]]
        getlist <- stringr::str_extract(
          getlist,
          "[A-Z0-9]+\\.A[0-9]+\\.?[hv0-9]*\\.[0-9]+\\.[0-9]+\\.hdf")
        success <- TRUE
      }
    }
  }

  # __________________________________________________________________________
  # Retrieve the list of hdf files matching the product / version / date ####
  # in case of offline mode
  if (used_server == "offline") {

    getlist <- list.files(out_folder_mod, "\\.hdf$")
    sel_prod_vers <- unlist(
      stringr::str_split(
        gsub("http:\\/\\/[A-Za-z0-9\\.]+\\/[A-Z]+\\/([A-Z0-9]+)\\.([0-9]+)\\/", "\\1 \\2", #nolint
             http
        ), " ")
    )
    getlist <- getlist[grep(paste0(sel_prod_vers[1], "\\.A",
                                   strftime(as.Date(
                                     date_dir,
                                     format = "%Y.%m.%d"
                                   ), "%Y%j"), "\\.h[0-9][0-9]v[0-9][0-9]\\.",
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
  return(Modislist)
}
