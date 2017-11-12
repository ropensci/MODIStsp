#' @title get list of MODIS data folders from http/ftp server
#' @description Accessory function to get the full list of directories on the
#'  lpdaac http site containing data included in the time range selected for
#'  processing (modified after Barry Rowlingson function):
#' @param http `character` http site on lpdaac corresponding to the selcted MODIS
#'   product
#' @param ftp `character` ftp site corresponding to the selected MODIS product
#' @param downlad_server `character ["http" | "ftp" | "offline"]` download service
#'   to be used; if NA, the script tries to download with http, using ftp if the
#'   download fails (Used in non-interactive execution to allow trying to use
#'   ftp if http fails)
#' @param user `character` username for earthdata http server
#' @param password `character` password for earthdata http server
#' @param n_retries `numeric` number of times the access to the http/ftp server
#'   should be retried in case of error befor quitting, Default: 20
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
#' @author Luigi Ranghetti, phD (2016-2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom gWidgets gconfirm
#' @importFrom httr GET authenticate timeout content
#' @importFrom stringr str_extract str_split str_split_fixed str_sub
#' @importFrom utils download.file

get_mod_dirs <- function(http, ftp, download_server,
                         user, password, 
                         n_retries = 20, 
                         yy,
                         gui,
                         out_folder_mod,
                         .Platform) {
  
  if (stringr::str_sub(http ,-1) != "/") {
    http <- paste(http, "/", sep = "")
  }
  if (.Platform$OS.type == "unix") {
    options("download.file.method" = "wget")
  }
  else {
    options("download.file.method" = "auto")
  }
  items <- 0
  class(items) <- "try-error"
  ce <- 0
  
  #   __________________________________________________________________________
  #   retrieve list of folders in case of http download                    ####
  
  if (download_server == "http") {
    
    success = FALSE
    while (!success) {
      # send request to server
      response <- httr::RETRY("GET",
                              http,
                              httr::authenticate(user, password),
                              times = n_retries,
                              pause_base = 0.1, 
                              pause_cap = 10, 
                              quiet = FALSE)
      # On interactive execution, after n_retries attempt ask if quit or ----
      # retry
      if (response$status_code != 200) {
        if (gui) {
          confirm <- gWidgets::gconfirm(
            "http server seems to be down! Do you want to retry?",
            icon = "question",
            handler = function(h, ...) {})
          if (!confirm) stop("You selected to abort processing. Goodbye!")
        } else {
          warning("[", date(), "] Error: http server seems to be down! ",
                  "Switching to ftp!")
          download_server = "ftp"
          success = TRUE
        }
      } else {
        # On httr success get the directory names (available dates) ----
        items <- strsplit(httr::content(response, "text",
                                        encoding = "UTF-8"), "\r*\n")[[1]]
        date_dirs <- gsub(
          ".*>(20[0-9][0-9]\\.[01][0-9]\\.[0-3][0-9])\\/<.*", "\\1", items
        )
        date_dirs <- date_dirs[grep(paste0(yy, "\\.[01][0-9]\\.[0-3][0-9]"),
                                    date_dirs)]
        attr(date_dirs, "server") <- "http"
        success = TRUE
      }
    }
  }
  # retrieve processign dates in case of "ftp" download ----
  if (download_server == "ftp") {
    while (!success) {
      # send request to server
      yeard_dir <- paste0(ftp, yy, "/")
      response <- httr::RETRY("GET",
                              ftp,
                              times = n_retries,
                              pause_base = 0.1, 
                              pause_cap = 10, 
                              quiet = FALSE)
      # On interactive execution, after n_retries attempt ask if quit or ----
      # retry
      if (response$status_code != 226) {
        if (gui) {
          confirm <- gWidgets::gconfirm(
            "ftp server seems to be down! Do you want to retry?",
            icon = "question",
            handler = function(h, ...) {})
          if (!confirm) stop("You selected to abort processing. Goodbye!")
        } else {
          # break on failure
          stop("[",
               date(),
               "] Error: ftp server seems to be down! Please Retry Later!")
        }
      } else {
        # On ftp success get the directory names of available dates ----
        items     <- strsplit(httr::content(response,
                                            "text",
                                            encoding = "UTF-8"), "\r*\n")[[1]]
        doys      <- as.numeric(
          stringr::str_split_fixed(items, "[0-9][0-9]:[0-9][0-9]\\s", 2)[,2]
        )
        date_dirs <- format(as.Date(doys - 1, origin = paste0(yy, "-01-01")),
                            format = "%Y.%m.%d")
        attr(date_dirs, "server") <- "ftp"
        success <- TRUE
      }
    }
  }
  
  #   ____________________________________________________________________________
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

#' @title get_mod_dates
#' @description Accessory function to find the folders corresponding to the requested
#' dates period within the full list retrieved by get_moddirs
#' @param dates 2- element string array specifying start/end dates (yyyy.mm.dd)
#' for which the http addresses of folders in lpdaac should be retrieved
#' (e.g., c("2015.1.1", "2015.12.31)
#' @param date_dirs data frame full list of folders in lpdaac archive for product of interest
#' @return array of folder names containing data for the MODIS product acquired in
#' the period specified by "dates"
#' @author Luigi Ranghetti, phD (2016) \email{ranghetti.l@@irea.cnr.it}
#' @author Lorenzo Busetto, phD (2017) \email{busetto.l@@irea.cnr.it}
#' @note License: GPL 3.0
#'
get_mod_dates <- function(dates, date_dirs) {
  
  tmp_dates     <- as.Date(dates, format = "%Y.%m.%d")
  tmp_date_dirs <- as.Date(date_dirs, format = "%Y.%m.%d")
  if (length(dates) == 1) {
    date_dirs <- date_dirs[tmp_date_dirs == tmp_dates[1]]
  }
  if (length(dates) == 2) {
    date_dirs <- date_dirs[tmp_date_dirs >= tmp_dates[1] &
                             tmp_date_dirs <= tmp_dates[2]]
  }
  if (length(dates) == 4) {
    # deals with the case of seasonal download, when yy != start_year &
    # yy != end_year
    date_dirs <- c(
      date_dirs[tmp_date_dirs >= tmp_dates[1] & tmp_date_dirs <= tmp_dates[2]],
      date_dirs[tmp_date_dirs >= tmp_dates[1] & tmp_date_dirs <= tmp_dates[4]]
    )
  }
  
  return(date_dirs)
}
# ------------------------------ ----------------------------------------------#
# Accessory function to find the image names corresponding to the selected dates
# and tiles
# ------------------------------ ----------------------------------------------#

#' @title get_mod_filenames
#' @description Accessory function to find the names of HDF images corresponding
#' to a given date and interval of spatial tiles within the lpdaac archive.
#' @param http `character` url of http site on lpdaac corresponding to a given MODIS 
#'   product.
#' @param ftp `character` url offtp site corresponding to a given MODIS product.
#' @param used_server `character` can assume values "http" or "ftp" depending on the
#'  used download server; it cannot be NA.
#' @param user `character` username for earthdata server (Ignored if used_server = "ftp").
#' @param password `character` password for earthdata server  (Ignored if used_server = "ftp").
#' @param n_retries `numeric` number of times the access to the http/ftp server
#'   should be retried in case of error befor quitting, Default: 20.
#' @param date_dir `character array` array of folder names corresponding to acquisition 
#'  containing dates where MODIS files to be downloaded are to be identified
#'  (return array from `get_mod_dates`).
#' @param v `integer array` containing a sequence of the vertical tiles of interest
#'   (e.g., c(18,19)).
#' @param h `integer array` containing a sequence of the horizontal tiles of interest
#'   (e.g., c(3,4)).
#' @param tiled `numeric [0/1]` indicates if the product to be downloaded is 
#'   tiled or not tiled. 1 = tiled product; 0 = non-tiled product (resolution 0.05 deg).
#' @param out_folder_mod  `character` folder where hdf files have to be storedd.
#' @param gui `logical` indicates if processing was called within the GUI environment
#'   or not. If not, processing messages are redirected direct to the log file.
#' @return `character array` containing names of HDF images corresponding to the
#'  requested tiles available for the product in the selected date
#' @author Original code by Babak Naimi (\code{.getModisList}, in
#' \href{http://r-gis.net/?q=ModisDownload}{ModisDownload.R})
#' modified to adapt it to MODIStsp scheme and to http archive (instead than old
#'  FTP) by:
#' @author Lorenzo Busetto, phD (2014-2016) \email{busetto.l@@irea.cnr.it}
#' @author Luigi Ranghetti, phD (2016) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom gWidgets gconfirm
#' @importFrom httr GET timeout authenticate content
#' @importFrom RCurl getURL
#' @importFrom stringr str_split str_pad
get_mod_filenames <- function(http, ftp, used_server, user, password, n_retries, 
                              date_dir, v, h, tiled, out_folder_mod, gui) {
  getlist <- 0
  class(getlist) <- "try-error"
  ce <- 0
  
  if (used_server == "http") {
    #   ________________________________________________________________________
    #   Retrieve available hdf files in case of http download               ####
    
    # http folders are organized by date subfolders containing all tiles
    while (class(getlist) == "try-error") {
      getlist <- try(httr::GET(paste0(http, date_dir, "/"),
                               httr::timeout(5),
                               httr::authenticate(user, password)))
      
      if (class(getlist) == "try-error") {
        # if error on response, retry
        Sys.sleep(1)
        ce <- ce + 1
        message("Trying to reach ftp server - attempt ", ce)
      } else {
        # If good response, get the result (all files available for the date)
        if (getlist$status_code == 200) {
          getlist <- strsplit(httr::content(getlist, "text"), "\r*\n")[[1]]
        } else {
          ce <- ce + 1
          message("Trying to reach http server - attempt ", ce)
        }
      }
      
      if (class(getlist) != "try-error") {
        getlist <- getlist[
          grep(".*>([A-Z0-9]+\\.A[0-9]+\\.?[hv0-9]*\\.[0-9]+\\.[0-9]+\\.hdf)<.*", #nolint
               getlist)
          ]
        getlist <- gsub(
          ".*>([A-Z0-9]+\\.A[0-9]+\\.?[hv0-9]*\\.[0-9]+\\.[0-9]+\\.hdf)<.*", "\\1", #nolint
          getlist
        )
        
      } else if (ce == 5)  {
        if (gui) {
          # ask to retry only if gui=TRUE
          confirm <- gWidgets::gconfirm(
            "http server seems to be down! Do you want to retry?",
            icon = "question",
            handler = function(h, ...) {})
        } else {
          confirm <- FALSE
        }
        if (confirm == "FALSE") {
          warning("[", date(),
                  "] Error: http server seems to be down! Please Retry Later!")
          if (gui) {
            gmessage("User selected to quit!", icon = "info")
            # if user selected to quit, exit program
            stop("You selected to abort processing. Goodbye!")
          } else {
            # on non-interactive, if limit exceeded try ftp downolad as backup
            used_server = "ftp"
            break()
          }
        } else {
          ce <- 0   # if retry, reset the counter
        }
      }
    }
  }
  if (used_server == "ftp") {
    #   ______________________________________________________________________
    #   Retrieve available hdf files in case of ftp download             ####
    
    # ftp folders are organized by /year/date subfolders, so there is an
    # additional level to be "parsed" wrt http
    date_year <- strftime(as.Date(date_dir, format = "%Y.%m.%d"), "%Y")
    date_doy  <- strftime(as.Date(date_dir, format = "%Y.%m.%d"), "%j")
    while (class(getlist) == "try-error") {
      
      getlist <- try(
        strsplit(
          x = RCurl::getURL(paste(ftp, date_year, "/", date_doy, "/", sep = ""),
                            ftp.use.epsv = FALSE,
                            dirlistonly = TRUE,
                            .opts = list(timeout = 10, maxredirs = 5,
                                         verbose = FALSE)),
          split = "\r*\n")[[1]],
        silent = TRUE
      )
      
      if (class(getlist) == "try-error") {
        Sys.sleep(1)
        message("Trying to reach ftp server - attempt ", ce)
        ce <- ce + 1
        if (ce == 50) {
          if (gui) {
            confirm <- gWidgets::gconfirm(
              "ftp server seems to be down! Do you want to retry?",
              icon = "question", handler = function(h, ...) {})
          } else {
            confirm <- FALSE
          }
          if (confirm == "FALSE") {
            warning("[", date(),
                    "] Error: ftp server seems to be down! Please Retry Later!")
            if (gui) {
              gmessage("User selected to quit!", icon = "info")
              # if user selected to quit, exit program
              stop("You selected to abort processing. Goodbye!")
            } else {
              stop("ftp server is down. Please retry later. Aborting!")
            }
            # on NOT retry, quit the program
          } else {
            ce <- 0   # if retry, reset the counter
          }
        }
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
