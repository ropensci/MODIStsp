#' @title Get list of MODIS data folders from http/ftp server
#' @description Accessory function to get the full list of directories on the
#'  lpdaac http site containing data included in the time range selected for
#'  processing (modified after Barry Rowlingson function):
#' @param http `character` http site on lpdaac corresponding to the selected MODIS
#'   product
#' @param ftp `character` ftp site corresponding to the selected MODIS product
#' @param download_server `character ["http" | "ftp" | "offline"]` download service
#'   to be used; if NA, the script tries to download with http, using ftp if the
#'   download fails (Used in non-interactive execution to allow trying to use
#'   ftp if http fails)
#' @param user `character` username for earthdata http server
#' @param password `character` password for earthdata http server
#' @param yy `character` Year for which the folder containing HDF images are to 
#'  be identified
#' @param n_retries `numeric` number of times the access to the http/ftp server
#'   should be retried in case of error before quitting, Default: 20
#' @param gui `logical`` indicates if processing was called from the GUI
#'   environment or not. If not, processing messages are sent to a log file 
#'   instead than to the console/GTK progress windows. 
#' @param out_folder_mod  `character` output folder for MODIS HDF storage
#' @param .Platform `character` os platform (from call to .Platform)
#' @return `character arraty` listing all available folders (a.k.a. dates) for
#'   the requested MODIS product on lpdaac http or ftp archive, for the years
#'   included in the time range selected for processing.
#' @author Original code by Babak Naimi (\code{.getModisList}, in
#' \href{http://r-gis.net/?q=ModisDownload}{ModisDownload.R})
#' modified to adapt it to MODIStsp scheme and to http archive (instead than old
#' FTP) by:
#' @author Lorenzo Busetto, phD (2014-2017) \email{busetto.l@@irea.cnr.it}
#' @author Luigi Ranghetti, phD (2016-2017) \email{lbusett@@gmail.com}
#' @note License: GPL 3.0
#' @importFrom gWidgets gconfirm
#' @importFrom httr GET authenticate timeout content
#' @importFrom stringr str_extract str_split str_split_fixed str_sub

get_mod_dirs <- function(http, ftp, download_server,
                         user, password,
                         yy,
                         n_retries,
                         gui,
                         out_folder_mod,
                         .Platform) {
  
  # make sure that the http address terminates with a "/" (i.e., it is a
  # folder, not a file)
  if (stringr::str_sub(http, -1) != "/") {
    http <- paste(http, "/", sep = "")
  }
  success <- FALSE
  #   __________________________________________________________________________
  #   retrieve list of folders in case of http download                    ####
  
  if (download_server == "http") {
    # send request to server
    response <- try(httr::RETRY("GET",
                                http,
                                httr::authenticate(user, password),
                                times = n_retries,
                                pause_base = 0.1,
                                pause_cap = 3,
                                quiet = FALSE))
    # On interactive execution, after n_retries attempt ask if quit or ----
    # retry
    
    if (class(response) == "try-error") {
      if (gui) { 
        #nocov start
        switch <- gWidgets::gconfirm(
          "http server seems to be down! Do you want to switch to ftp?",
          icon = "question")
        if (!switch) {
          stop("You selected to abort processing. Goodbye!")
        } else {
          download_server <- "ftp"
          success <- TRUE
        }
        #nocov end
      } else {
        message("[", date(), "] Error: http server seems to be down! ",
                "Switching to ftp!")
        if (ftp == "Not Available") {
          stop("The product is not available on ftp. Aborting!")
        }
        download_server <- "ftp"
        success <- TRUE
      }
    } else {
      # On httr success get the directory names (available dates) ----
      items <- strsplit(httr::content(response, "text", encoding = "UTF-8"),
                        "\r*\n")[[1]]
      date_dirs <- gsub(
        ".*>(20[0-9][0-9]\\.[01][0-9]\\.[0-3][0-9])\\/<.*", "\\1", items
      )
      date_dirs <- date_dirs[grep(paste0(yy, "\\.[01][0-9]\\.[0-3][0-9]"),
                                  date_dirs)]
      attr(date_dirs, "server") <- "http"
      success <- TRUE
    }
    
  }
  success <- FALSE
  # retrieve processign dates in case of "ftp" download ----
  if (download_server == "ftp") {
    
    while (!success) {
      # send request to server
      year_ftp <- paste0(ftp, yy, "/")
      response <- try(httr::RETRY("GET",
                                  year_ftp,
                                  times = n_retries,
                                  pause_base = 0.1,
                                  pause_cap = 3,
                                  quiet = FALSE))
      # On interactive execution, after n_retries attempt ask if quit or ----
      # retry
      if (class(response) == "try-error") {
        if (gui) {
          #nocov start
          confirm <- gWidgets::gconfirm(
            "ftp server seems to be down! Do you want to retry?",
            icon = "question")
          if (!confirm) stop("You selected to abort processing. Goodbye!")
          #nocov end
        } else {
          # break on failure
          stop("[",
               date(),
               "] Error: ftp server seems to be down! Please Retry Later!")
        }
      } else {
        # On ftp success get the directory names of available dates ----
        items     <- strsplit(httr::content(response,
                                            "text", encoding = "UTF-8"),
                              "\r*\n")[[1]]
        doys      <- as.numeric(
          stringr::str_split_fixed(items, "[0-9][0-9]:[0-9][0-9]\\s", 2)[, 2]
        )
        date_dirs <- format(as.Date(doys - 1, origin = paste0(yy, "-01-01")),
                            format = "%Y.%m.%d")
        attr(date_dirs, "server") <- "ftp"
        success <- TRUE
      }
    }
  }
  
  #   __________________________________________________________________________
  #   In offline mode, retrieve the dates of acquisition of hdfs already
  #   available in `out_folder_mod`
  
  if (download_server == "offline") {
    
    # Retrieve the list of hdf files matching the product / version
    items <- list.files(out_folder_mod, "\\.hdf$")
    sel_prod_vers <- unlist(stringr::str_split(gsub(
      "http:\\/\\/[A-Za-z0-9\\.]+\\/[A-Z]+\\/([A-Z0-9]+)\\.([0-9]+)\\/", "\\1 \\2", #nolint
      http), " "))
    items <- items[grep(paste0(
      sel_prod_vers[1], "\\.A20[0-9][0-9][0-3][0-9][0-9]\\.h[0-9][0-9]v[0-9][0-9]\\.",  #nolint
      sel_prod_vers[2], "\\.[0-9]+\\.hdf$"), items)]
    
    # Extract dates
    date_dirs <- strftime(as.Date(gsub(
      paste0(sel_prod_vers[1], "\\.A(20[0-9][0-9][0-3][0-9][0-9])\\..*"),"\\1", #nolint
      items), format = "%Y%j"), "%Y.%m.%d")
    attr(date_dirs, "server") <- "offline"
  }
  
  return(date_dirs)
  
}