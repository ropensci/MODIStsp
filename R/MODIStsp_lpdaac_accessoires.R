MODIStsp_lpdaac_accessoires <- function() {

}
#' @title lpdaac_getmod_dirs
#' @description Accessory function to get the full list of directories on the lpdaac
#' http site (modified after Barry Rowlingson function):
#'
#' @param http string http site on lpdaac corresponding to a given MODIS product
#' @param ftp string ftp site corresponding to a given MODIS product
#' @param used_server string can assume values "http" or "ftp" depending on the used
#' download server; if NA, the script tries to download with http, using ftp if the download fails
#' @param user username for earthdata server
#' @param password password for earthdata server
#' @param gui logical indicates if processing was called within the GUI environment
#' or not. If not, direct processing messages to the log
#' @param out_folder_mod  output folder for original HDF storage
#' @param .Platform string os platform (from call to .Platform)
#' @return list of all available folders (a.k.a. dates) for the requested MODIS product on lpdaac archive
#'
#' @author Original code by Babak Naimi (\code{.getModisList}, in
#' \href{http://r-gis.net/?q=ModisDownload}{ModisDownload.R})
#' modified to adapt it to MODIStsp scheme and to http archive (instead than old FTP) by:
#' @author Lorenzo Busetto, phD (2014-2017) \email{busetto.l@@irea.cnr.it}
#' @author Luigi Ranghetti, phD (2016-2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom gWidgets gconfirm
#' @importFrom httr GET authenticate timeout content
#' @importFrom stringr str_extract str_split str_split_fixed
#' @importFrom utils download.file

lpdaac_getmod_dirs <- function(ftp, http, used_server = NA, user = user,
                               password = password, gui, out_folder_mod,
                               .Platform) {

  if (strsplit(http, "")[[1]][length(strsplit(http, "")[[1]])] != "/") {
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

  # Try HTTP download
  if (is.na(used_server)) {
    check_used_server  <- TRUE
  } else {
    if (used_server == "http") {
      check_used_server  <- TRUE
    } else {
      check_used_server <- FALSE
    }
  }

  if (class(items) == "try-error" & check_used_server) {

    while (class(items) == "try-error") {

      response <- try(httr::GET(http, httr::authenticate(user, password), httr::timeout(10)))   # send request to server
      # if error on response, retry
      if (class(response) == "try-error") {
        Sys.sleep(1)
        ce <- ce + 1
        message("Trying to reach http server - attempt ", ce)
      } else {
        # If good response, get the result
        if (response$status_code == 200) {
          items <- strsplit(httr::content(response, "text", encoding = "UTF-8"), "\r*\n")[[1]]
        } else {
          ce <- ce + 1
          message("Trying to reach http server - attempt ", ce)
        }
      }
      # on good response, parse the contents to retrieve the date folders
      if (class(items) != "try-error") {
        # run only if http download works
        # get the directory names (available dates)
        date_dirs <- gsub(".*>(20[0-9][0-9]\\.[01][0-9]\\.[0-3][0-9])\\/<.*", "\\1", items)
        date_dirs <- date_dirs[grep("20[0-9][0-9]\\.[01][0-9]\\.[0-3][0-9]", date_dirs)]
        attr(date_dirs, "server") <- "http"
      } else {
        if (ce == 50)  {
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
            warning("[", date(), "] Error: http server seems to be down! Please Retry Later!")
            if (gui) {
              gmessage("User selected to quit!", icon = "info")

              # if user selected to quit, exit programstop()
            } else {
              # on non-interactive, if limit exceeded try ftp downolad as backup
              break()
            }
            # on NOT retry, quit the program
          } else {
            ce <- 0   # if retry, reset the counter
          }
        }
      }
    }
  }

  # Try FTP download: If method = ftp, or if method = http and limit exceeded on
  #  non-interactive execution
  if (is.na(used_server)) {
    check_used_server  <- TRUE
  } else {
    if (used_server == "ftp") {
      check_used_server <- TRUE
    } else {
      check_used_server  <- FALSE
    }
  }

  if (class(items) == "try-error" & check_used_server) {

    while (class(items) == "try-error") {
      # send request to server
      response <- try(suppressWarnings(httr::GET(ftp, httr::timeout(10))))
      # if error on response, retry
      if (class(response) == "try-error") {
        Sys.sleep(1)
        ce <- ce + 1
        message("Trying to reach ftp server - attempt ", ce)
      } else {
        # If good response, get the result
        if (response$status_code == 226) {
          items <- strsplit(httr::content(response, "text", encoding = "UTF-8"), "\r*\n")[[1]]
        } else {
          ce <- ce + 1
          message("Trying to reach ftp server - attempt ", ce)
        }
      }

      if (class(items) != "try-error") {
        # run only if ftp download works
        items_1   <- stringr::str_extract(items, "20[0-9][0-9]$")
        items_1   <- items_1[!is.na(items_1)]
        response_1 <- list() # replaces the line above to allow to repeat the try 10 times
        for (sel_year in items_1) {
          ce <- 1; while (ce < 10) {
            # try to download each year for 10 times
            response_1[[sel_year]] <- suppressWarnings(try(httr::GET(paste0(ftp, sel_year, "/"), httr::timeout(10))))
            if (class(response_1[[sel_year]]) != "response") {
              # if error on response, retry
              Sys.sleep(1)
              ce <- ce + 1
              message("Trying to reach ftp server - attempt ", ce)
            } else if (response_1[[sel_year]]$status_code != 226) {
              ce <- ce + 1
              message("Trying to reach ftp server - attempt ", ce)
            } else {
              ce <- ce + 10
            }
          }
        }
        # remove unsuccessful years
        response_1 <- response_1[sapply(response_1, class) == "response"] 

        response_2 <- lapply(response_1, function(x) {
          httr::content(x, "text", encoding = "UTF-8")
        })
        response_3 <- lapply(stringr::str_split(response_2, "\n"), function(x){x[nchar(x) > 0]})
        items_2    <- sapply(response_3, function(x){
          stringr::str_split_fixed(x, " +", 9)[, 9]})
        names(items_2) <- sapply(response_1, function(x){x$url
        })
        full_dirs <- unlist(lapply(seq_along(items_2), function(x) {
          paste0(names(items_2[x]), items_2[[x]], "/")
        }))
        date_dirs <- sapply(strsplit(full_dirs, "/"), function(x) {
          strftime(as.Date(paste(x[length(x) - 1], x[length(x)]),
                           format = "%Y %j"), "%Y.%m.%d")
        })
        attr(date_dirs, "server") <- "ftp"

      } else {
        if (ce == 50)  {
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
            warning("[", date(), "] Error: ftp server seems to be down! Please Retry Later!")
            if (gui) {
              gmessage("User selected to quit!", icon = "info")
              stop()    # if user selected to quit, exit program
            } else {
              break()  # on non-interactive, if limit exceeded try ftp downolad as backup
            }
            # on NOT retry, quit the program
          } else {
            ce <- 0   # if retry, reset the counter
          }
        }
      }
    }
  }

  # If the server is definitively down, or if "offline" method was chosen, use
  # the dates already available in local directory
  if (!exists("date_dirs")) {

    # Retrieve the list of hdf files matching the product / version
    items <- list.files(out_folder_mod, "\\.hdf$")
    sel_prod_vers <- unlist(stringr::str_split(gsub(
      "http:\\/\\/[A-Za-z0-9\\.]+\\/[A-Z]+\\/([A-Z0-9]+)\\.([0-9]+)\\/", "\\1 \\2",
      http), " "))
    items <- items[grep(paste0(
      sel_prod_vers[1], "\\.A20[0-9][0-9][0-3][0-9][0-9]\\.h[0-9][0-9]v[0-9][0-9]\\.",
      sel_prod_vers[2], "\\.[0-9]+\\.hdf$"), items)]

    # Extract dates
    date_dirs <- strftime(as.Date(gsub(
      paste0(sel_prod_vers[1], "\\.A(20[0-9][0-9][0-3][0-9][0-9])\\..*"),"\\1",
      items), format = "%Y%j"), "%Y.%m.%d")
    attr(date_dirs, "server") <- "offline"
  }

  return(date_dirs)

}

#' @title lpdaac_getmod_dates
#' @description Accessory function to find the folders corresponding to the requested
#' dates period within the full list retrieved by lpdaac_getmod_dirs
#' @param dates 2- element string array specifying start/end dates (yyyy.mm.dd)
#' for which the http addresses of folders in lpdaac should be retrieved
#' (e.g., c("2015.1.1", "2015.12.31)
#' @param date_dirs data frame full list of folders in lpdaac archive for product of interest
#' @return array of folder names containing data for the MODIS product acquired in
#' the period specified by "dates"
#' @author Luigi Ranghetti, phD (2016) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#'
lpdaac_getmod_dates <- function(dates, date_dirs) {
  if (length(dates) == 1) {
    date_dirs <- date_dirs[which(date_dirs == dates[1])]
  } else if (length(dates) == 2) {
    date_dirs <- date_dirs[
      as.Date(date_dirs, format = "%Y.%m.%d") >= as.Date(dates, format = "%Y.%m.%d")[1] &
        as.Date(date_dirs, format = "%Y.%m.%d") <= as.Date(dates, format = "%Y.%m.%d")[2]
      ]

  } else if (length(dates) == 4) {
    date_dirs <- c(
      date_dirs[as.Date(date_dirs, format = "%Y.%m.%d") >= as.Date(dates, format = "%Y.%m.%d")[1] &
                  as.Date(date_dirs, format = "%Y.%m.%d") <= as.Date(dates, format = "%Y.%m.%d")[2]],
      date_dirs[as.Date(date_dirs, format = "%Y.%m.%d") >= as.Date(dates, format = "%Y.%m.%d")[3] &
                  as.Date(date_dirs, format = "%Y.%m.%d") <= as.Date(dates, format = "%Y.%m.%d")[4]]
    )
  } else {
    date_dirs <- NULL
  }

  return(date_dirs)
}
# ---------------------------------- ----------------------------------------------#
# Accessory function to find the image names corresponding to the selected dates and tiles
# ---------------------------------- ----------------------------------------------#

#' @title lpdaac_getmod_names
#' @description Accessory function to find the names of HDF images corresponding
#' to a given date and interval of spatial tiles within the lpdaac archive
#' @param http string http site on lpdaac corresponding to a given MODIS product
#' @param ftp string ftp site corresponding to a given MODIS product
#' @param used_server string can assume values "http" or "ftp" depending on the
#'  used download server; it cannot be NA
#' @param user username for earthdata server
#' @param password password for earthdata server
#' @param date_dir array of folder names containing data for the MODIS product
#' acquired in a given period (return array from lpdaac_getmod_dates)
#' @param v int. array containing a sequence of the vertical tiles of interest
#'   (e.g., c(18,19))
#' @param h int. array containing a sequence of the horizontal tiles of interest
#'   (e.g., c(3,4))
#' @param tiled 0/1 1 = tiled product; 0 = non-tiled product (resolution 0.05 deg)
#' @param gui logical indicates if processing was called within the GUI environment
#' or not. If not, direct processing messages to the log
#' @param out_folder_mod  output folder for original HDF storage
#' @return Modislist names of HDF images corresponding to the requested tiles available
#' for the product in the selected date
#'
#' @author Original code by Babak Naimi (\code{.getModisList}, in
#' \href{http://r-gis.net/?q=ModisDownload}{ModisDownload.R})
#' modified to adapt it to MODIStsp scheme and to http archive (instead than old FTP) by:
#' @author Lorenzo Busetto, phD (2014-2016) \email{busetto.l@@irea.cnr.it}
#' @author Luigi Ranghetti, phD (2016) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom gWidgets gconfirm
#' @importFrom httr GET timeout authenticate content
#' @importFrom RCurl getURL
#' @importFrom stringr str_split str_pad
lpdaac_getmod_names <- function(http, ftp, used_server, user, password, date_dir,
                                v, h, tiled, out_folder_mod, gui) {
  getlist <- 0
  class(getlist) <- "try-error"
  ce <- 0
  if (used_server == "http") {

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
        # If good response, get the result
        if (getlist$status_code == 200) {
          getlist <- strsplit(httr::content(getlist, "text"), "\r*\n")[[1]]
        } else {
          ce <- ce + 1
          message("Trying to reach http server - attempt ", ce)
        }
      }

      if (class(getlist) != "try-error") {
        getlist <- getlist[
          grep(".*>([A-Z0-9]+\\.A[0-9]+\\.?[hv0-9]*\\.[0-9]+\\.[0-9]+\\.hdf)<.*",
               getlist)
          ]
        getlist <- gsub(
          ".*>([A-Z0-9]+\\.A[0-9]+\\.?[hv0-9]*\\.[0-9]+\\.[0-9]+\\.hdf)<.*", "\\1",
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
          warning("[", date(), "] Error: ftp server seems to be down! Please Retry Later!")
          if (gui) {
            gmessage("User selected to quit!", icon = "info")
            # if user selected to quit, exit program
            stop()
          } else {
            # on non-interactive, if limit exceeded try ftp downolad as backup
            break()
          }
          # on NOT retry, quit the program
        } else {
          ce <- 0   # if retry, reset the counter
        }
      }
    }
  } else if (used_server == "ftp") {
    date_year <- strftime(as.Date(date_dir, format = "%Y.%m.%d"), "%Y")
    date_doy <- strftime(as.Date(date_dir, format = "%Y.%m.%d"), "%j")
    while (class(getlist) == "try-error") {

      getlist <- try(
        strsplit(RCurl::getURL(paste(ftp, date_year, "/", date_doy, "/", sep = ""),
                        ftp.use.epsv = FALSE,
                        dirlistonly = TRUE,
                        .opts = list(timeout = 10,
                                     maxredirs = 5,
                                     verbose = FALSE )),
                 "\r*\n")[[1]], silent = TRUE
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
            warning("[", date(), "] Error: ftp server seems to be down! Please Retry Later!")
            if (gui) {
              gmessage("User selected to quit!", icon = "info")
              # if user selected to quit, exit program
              stop()
            } else {
              # on non-interactive, if limit exceeded try ftp downolad as backup
              break()
            }
            # on NOT retry, quit the program
          } else {
            ce <- 0   # if retry, reset the counter
          }
        }
      }
    }
  } else if (used_server == "offline") {

    # Retrieve the list of hdf files matching the product / version / date
    getlist <- list.files(out_folder_mod, "\\.hdf$")
    sel_prod_vers <- unlist(
      stringr::str_split(
        gsub("http:\\/\\/[A-Za-z0-9\\.]+\\/[A-Z]+\\/([A-Z0-9]+)\\.([0-9]+)\\/", "\\1 \\2",
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

  Modislist <- c()
  if (tiled == 1) {
    for (vv in v) {
      for (hh in h) {
        vc <- stringr::str_pad(vv, 2, "left", "0")
        hc <- stringr::str_pad(hh, 2, "left", "0")
        ModisName <- grep(".hdf$", grep(paste0("h", hc, "v", vc), getlist, value = TRUE),
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
