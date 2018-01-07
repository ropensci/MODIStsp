#' @title MODIStsp download function
#' @description Internal function dealing with download of MODIS hdfs from
#'  http or ftp remote servers for a given date.
#' @param modislist `character array` List of MODIS images to be downloaded for
#'  the selected date (as returned from `get_mod_filenames`). Can be a single
#'  image, or a list of images in case different tiles are needed!
#' @param out_folder_mod `character` Folder where the hdfs are to be stored
#' @param download_server `character ["http" | "ftp"]` Server to be used.
#' @param http `character` Address of the http server for the selected product.
#' @param ftp `character` Address of the http server for the selected product.
#' @param n_retries `numeric` Max number of retry attempts on download. If
#'  download fails more that n_retries times consecutively, abort
#' @param date_dir `character array` Sub-folder where the different images
#'  can be found (element of the list returned from `get_mod_dirs`, used in case
#'  of http download to generate the download addresses).
#' @param year `character` Acquisition year of the images to be downloaded
#'  (Used in case of ftp download to generate the download addresses).
#' @param DOY `character array` Acquisition doys of the images to be downloaded
#'  (Used in case of ftp download to generate the download addresses).
#' @param user `character` Username for http download (Ignored in case of ftp
#'  download).
#' @param password `character` Password for http download (Ignored in case of ftp
#'  download).
#' @param sens_sel `character ["terra" | "aqua"]` Selected sensor.
#' @param date_name `character` Date of acquisition of the images to be downloaded.
#' @param gui `logical` Indicates if on an interactive or non-interactive execution
#'  (only influences where the log messages are sent).
#' @param mess_lab pointer to the gWidget used to issue processing messages in 
#'  when gui = TRUE.
#' @param verbose `logical` If FALSE, suppress processing messages, Default: TRUE
#' @inheritParams MODIStsp_process
#' @return The function is called for its side effects
#' @rdname MODIStsp_download
#' @author Lorenzo Busetto, phD (2014-2017) \email{lbusett@@gmail.com}
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @importFrom httr RETRY authenticate content GET progress write_disk
#' @importFrom xml2 as_list
#' @importFrom gWidgets gconfirm
#' @importFrom RCurl getURL

MODIStsp_download <- function(modislist,
                              out_folder_mod,
                              download_server,
                              http,
                              ftp,
                              n_retries,
                              use_aria,
                              date_dir,
                              year,
                              DOY,
                              user,
                              password,
                              sens_sel,
                              date_name,
                              gui, 
                              mess_lab, 
                              verbose) {

  # Cycle on the different files to download for the current date
  for (file in seq_along(modislist)) {
    modisname <- modislist[file]

    #   ________________________________________________________________________
    # Try to retrieve the file size of the remote HDF so that if a local    ####
    # file exists but size is different it can be redownloaded
    #
    local_filename  <- file.path(out_folder_mod, modisname)
    if (file.exists(local_filename))  {
      local_filesize <- file.info(local_filename)$size
    } else {
      local_filesize <- 0
    }

    if (download_server == "http") {
      remote_filename <- paste0(http, date_dir, "/", modisname)
    }
    if (download_server == "ftp")  {
      remote_filename <- paste0(ftp, year, "/", DOY, "/", modisname)
    }
    if (download_server == "offline") {
      remote_filename <- NA
    }
    success <- FALSE
    # On http download, try to catch size information from xml file ----
    if (download_server == "http") {
      while (success == FALSE) {
        size_string <- httr::RETRY("GET",
                                   paste0(remote_filename, ".xml"),
                                   httr::authenticate(user, password),
                                   times = n_retries,
                                   pause_base = 0.1,
                                   pause_cap = 10,
                                   quiet = FALSE)

        # if user/password are not valid, notify
        if (size_string["status_code"] == 401) {
          stop("Username and/or password are not valid. Please retry with the
             correct ones or try with ftp download.")
        }

        if (size_string$status_code == 200) {
          remote_filesize <- as.integer(
            xml2::as_list(
              httr::content(
                size_string, encoding = "UTF-8"))[["GranuleURMetaData"]][["DataFiles"]][["DataFileContainer"]][["FileSize"]] #nolint
          )
          success <- TRUE
        } else {
          # If the remote xml file was not accessible, n_retries times,
          # retry or abort
          if (gui) {
            #nocov start
            confirm <- gWidgets::gconfirm(
              paste0(download_server,
                     " server seems to be down! Do you want to retry?"),
              icon = "question")
            if (!confirm) {
              stop("You selected to quit! Goodbye!")
            }
            #nocov end
          } else {
            stop("[", date(), "] Error: server seems to be down! Please retry ",
                 "Later!")
          }
        }
      }
    } else {
      if (download_server == "ftp") {
        # On ftp download, use getURL to find out the remote file size ----
        attempt <- 0
        while (attempt < n_retries) {
          size_string <- try(RCurl::getURL(
            remote_filename,
            nobody = 1L, header = 1L,
            # keep a long timeout, since traversing ftp folders can be slow!
            .opts = list(timeout = 240, maxredirs = 5, verbose = FALSE))
          )
          if (class(size_string) != "try-error") {
            remote_filesize <- as.integer(gsub("[^:]+: ([0-9]+)\\r.*", "\\1",
                                               size_string))
            # on success, set a high value to attempt so to end the while loop
            attempt <- n_retries + 1
          } else {
            # wait one second before trying again!
            Sys.sleep(1)
            attempt <- attempt + 1
          }
          # If the remote xml file was not accessible n_retries times,
          # retry or abort
          if (attempt == n_retries) {
            if (gui) {
              #nocov start
              confirm <- gWidgets::gconfirm(
                paste0(download_server,
                       " server seems to be down! Do you want to retry?"),
                icon = "question")
              if (confirm) {
                attempt <- 0
              } else {
                stop("You selected to quit! Goodbye!")
              }
              #nocov end
            } else {
              stop("[", date(), "] Error: server seems to be down! Please retry ", #nolint
                   "later!")
            }
          }
        }
      } else {
        # On offline mode, don't perform file size check ----
        remote_filesize <- local_filesize
      }
    }

    #   ________________________________________________________________________
    #   Download required HDF images                                        ####
    #   (If HDF not existing locally, or existing with different size)
    #

    if (!file.exists(local_filename) | local_filesize != remote_filesize) {

      # update messages
      mess_text <- paste("Downloading", sens_sel, "Files for date:",
                         date_name, ":", which(modislist == modisname),
                         "of: ", length(modislist))
      # Update progress window
      if (verbose) process_message(mess_text, gui, mess_lab, verbose)
      success <- FALSE
      attempt <- 0
      #  _______________________________________________________________________
      #  while loop: try to download n_retries times  ####
      while (attempt < n_retries) {

        if (download_server == "http") {
          # http download - aria
          if (use_aria == TRUE) {
            aria_string <- paste0(
              Sys.which("aria2c"), " -x 6 -d ",
              dirname(local_filename),
              " -o ", basename(remote_filename),
              " ", remote_filename,
              " --allow-overwrite --file-allocation=none --retry-wait=2",
              " --http-user=", user,
              " --http-passwd=", password)

            # intern=TRUE for Windows, FALSE for Unix
            download <- try(system(aria_string,
                                   intern = Sys.info()["sysname"] == "Windows"))
          } else {
            # http download - httr
            download <- try(httr::GET(remote_filename,
                                      httr::authenticate(user, password),
                                      httr::progress(),
                                      httr::write_disk(local_filename,
                                                       overwrite = TRUE)))
          }
        } else {
          # ftp download - aria
          if (use_aria == TRUE) {
            aria_string <- paste0(
              Sys.which("aria2c"), " -x 6 -d ",
              dirname(local_filename),
              " -o ", basename(remote_filename),
              " ", remote_filename,
              " --allow-overwrite --file-allocation=none --retry-wait=2"
            )
            download <- try(system(aria_string,
                                   intern = Sys.info()["sysname"] == "Windows"))
          } else {
            # ftp download - httr
            download <- try(httr::GET(remote_filename,
                                      httr::progress(),
                                      httr::write_disk(local_filename,
                                                       overwrite = TRUE)))
          }
        }

        # Check for errors on download try
        if (class(download) == "try-error" |
            !is.null(attr(download, "status"))) {
          attempt <- attempt + 1
          if (verbose) message("[", date(), "] Download Error - Retrying...")
          unlink(local_filename)  # On download error, delete incomplete files
          Sys.sleep(1)    # sleep for a while....
        } else {
          if (download_server == "http" & use_aria == FALSE) {

            if (download$status_code != 200 &
                length(httr::content(download, 
                                     "text",
                                     encoding = "UTF-8")) == 1) {
              # on error, delete last HDF file (to be sure no incomplete
              # files are left behind and send message)
              if (verbose) {
                message("[", date(), "] Download Error - Retrying...")
              }
              unlink(local_filename)
            }
          }
        }
        # final check on local file size: Only exit if local file size equals
        # remote filesize to  prevent problems on incomplete download!
        local_filesize <- file.info(local_filename)$size
        if (local_filesize == remote_filesize & !is.na(local_filesize)) {
          # on success, bump attempt number so to exit the while cycle
          attempt <- n_retries + 1
          success <- TRUE
        } else {
          attempt <- attempt + 1
        }
      }
      if (attempt == n_retries & success == FALSE) {
        if (gui) {
          #nocov start
          confirm <- gWidgets::gconfirm(
            paste0(download_server,
                   " server seems to be down! Do you want to retry?"),
            icon = "question")
          if (confirm) {
            attempt <- 0
          } else {
            unlink(local_filename)
            stop("You selected to quit! Goodbye!")
          }
          #nocov end
        } else {
          unlink(local_filename)
          stop("[", date(), "] Error: server seems to be down! Please retry ",
               "Later!")
        }
      }
    }
  }
}
