#' @title MODIStsp_download
#' @description FUNCTION_DESCRIPTION
#' @param modislist PARAM_DESCRIPTION
#' @param out_folder_mod PARAM_DESCRIPTION
#' @param modisname PARAM_DESCRIPTION
#' @param download_server PARAM_DESCRIPTION
#' @param http PARAM_DESCRIPTION
#' @param date_dirs PARAM_DESCRIPTION
#' @param date PARAM_DESCRIPTION
#' @param ftp PARAM_DESCRIPTION
#' @param year PARAM_DESCRIPTION
#' @param DOY PARAM_DESCRIPTION
#' @param user PARAM_DESCRIPTION
#' @param password PARAM_DESCRIPTION
#' @param sens_sel PARAM_DESCRIPTION
#' @param date_name PARAM_DESCRIPTION
#' @param gui PARAM_DESCRIPTION
#' @inheritParams MODIStsp_process
#' @return The function is called for its side effects
#' @details DETAILS
#' @examples 
#' \dontrun{
#' #EXAMPLE1
#'  }
#' @rdname MODIStsp_download
#' @export 
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom XML xmlToList xmlParse
#' @importFrom httr content GET authenticate progress timeout
#' @importFrom gWidgets gconfirm

MODIStsp_download <- function(modislist, 
                              out_folder_mod,
                              download_server, 
                              http, 
                              date_dirs, 
                              date, 
                              ftp, 
                              year, 
                              DOY, 
                              user, 
                              password, 
                              sens_sel, 
                              date_name, 
                              gui) {
  
  for (modisname in modislist) {
    # Check file size (if the local file size is different, re-download)
    local_filename  <- file.path(out_folder_mod, modisname)
    local_filesize  <- file.info(local_filename)$size
    if (download_server == "http") {
      remote_filename <- paste0(http, date_dirs[date], "/", modisname)
    }
    if (download_server == "ftp")  {
      remote_filename <- paste0(ftp, year, "/", DOY, "/", modisname)
    }
    if (download_server == "offline") {
      remote_filename <- NA
    }
    
    # On http or ftp download, try to catch size information from xml file ----
    if (download_server != "offline") {
      remote_size_tries  <- 30 # numbers of tryouts for xml metafile
      size_string        <- NA
      class(size_string) <- "try-error"
      
      while (remote_size_tries > 0) {
        size_string <- if (download_server == "http") {
          try(GET(paste0(remote_filename, ".xml"), authenticate(user, password),
                  timeout(240)))
        } else if (download_server == "ftp") {
          try((getURL(remote_filename, nobody = 1L, header = 1L,
                      .opts = list(timeout = 240, maxredirs = 5, verbose = FALSE))))
        }
        # Check if download was good: check class of xmldown and status of xmldown
        if (class(size_string) == "try-error") {
          remote_size_tries <- remote_size_tries - 1
        } else {
          remote_size_tries <- 0
        }
      }
      
      # if user/password are not valid, notify
      if (download_server == "http" & size_string["status_code"] == 401) {
        stop("Username and/or password are not valid. Please retry with the
             correct ones or try with ftp download.")
      }
      
      # if the xml was available, check the size; otherwise, set as the local size to
      # skip the check
      if (class(size_string) == "try-error") {
        remote_filesize <- local_filesize
      } else {
        remote_filesize <- if (download_server == "http") {
          as.integer(XML::xmlToList(XML::xmlParse(httr::content(size_string, encoding = "UTF-8"))
          )[["GranuleURMetaData"]][["DataFiles"]][["DataFileContainer"]][["FileSize"]])
        } else if (download_server == "ftp") {
          as.integer(gsub("[^:]+: ([0-9]+)\\r.*", "\\1", size_string))
        }
      }
      
    } else {
      # On offline mode, don't perform file size check.
      remote_filesize <- local_filesize
    }
    
    # Perform download ----
    # If HDF not existing or with different size, download.
    if (!file.exists(local_filename) | local_filesize != remote_filesize) {
      er <- 5; class(er) <- "try-error"; ce <- 0
      
      local_filesize <- 0
      # while loop: Only exit if local file size equals remote filesize
      while (local_filesize != remote_filesize) {
        # repeat until no error or > 30 tryyouts
        while (er != 0) {
          mess_text <- paste("Downloading", sens_sel, "Files for date",
                             date_name, ":", which(modislist == modisname),
                             "of", length(modislist))
          # Update progress window
          if (gui) {
            svalue(mess_lab) <- paste("---", mess_text, "---")
            Sys.sleep(0.05)
            message("[", date(), "] ", mess_text)
          } else {
            message("[", date(), "] ", mess_text)
          }
          if (download_server == "http") {
            # http download
            if (use_aria == TRUE) {
              aria_string <- paste(Sys.which("aria2c"), " -x 6 -d ", dirname(local_filename),
                                   " -o ", basename(remote_filename), " ", remote_filename,
                                   " --allow-overwrite --file-allocation=none --retry-wait=2",
                                   " --http-user=", user, " --http-passwd=", password, sep = "")
              # intern=TRUE for Windows, FALSE for Unix
              download <- try(system(aria_string, intern = Sys.info()["sysname"] == "Windows"))
            } else {
              download <- try(httr::GET(remote_filename, httr::authenticate(user, password),
                                        httr::progress(), httr::timeout(2400)))
            }
          } else {
            # ftp download
            if (use_aria == TRUE) {
              aria_string <- paste(Sys.which("aria2c"), " -x 6 -d ",
                                   dirname(local_filename),
                                   " -o ", basename(remote_filename), " ", remote_filename,
                                   " --allow-overwrite --file-allocation=none --retry-wait=2", sep = "")
              download <- try(system(aria_string, intern = Sys.info()["sysname"] == "Windows"))
            } else {
              download <- try(httr::GET(remote_filename, httr::progress(),
                                        httr::timeout(2400)))
            }
          }
          # Check for errors on download try
          if (class(download) == "try-error" | !is.null(attr(download, "status"))) {
            er <- 5
            ce <- ce + 1
            message("[", date(), "] Download Error - Retrying...")
            unlink(local_filename)  # On download error, delete bad files
            Sys.sleep(1)    # sleep for a while....
          } else {
            if (download_server == "http" & use_aria == FALSE) {
              
              if (download$status_code != 200 & length(content(download, "text",
                                                               encoding = "UTF-8")) == 1) {
                # on error, delete last HDF file (to be sure no incomplete
                # files are left behind and send message)
                message("[", date(), "] Download Error - Retrying...")
                unlink(local_filename)
                Sys.sleep(1)
                er <- 5
                ce <- ce + 1
              } else {
                writeBin(download$content, local_filename)
                er <- 0
              }
            } else {
              er <- 0
              if (use_aria == FALSE) {
                writeBin(download$content, local_filename)
              }
            }
          }
          
          if (ce == 30) {
            # Ask if Stop after 30 failed attempts
            if (gui) {
              confirm <- gWidgets::gconfirm(paste0(download_server,
                                                   " server seems to be down! Do you want to retry?"),
                                            icon = "question",
                                            handler = function(h, ...){})
            } else {
              confirm <- "FALSE"
            }
            if (confirm == "FALSE") {
              warning("[", date(), "] Error: server seems to be down! Please Retry Later!")
              unlink(local_filename)
              stop()
            }
          }
        }  # end while on download tries
        
        # Futher check on downloaded file size
        # # Find the size of the new file downloaded to allow comparison with remote
        local_filesize <- file.info(local_filename)$size
        if (is.na(local_filesize)) {
          local_filesize <- 0
          er <- 5
          ce <- ce + 1
        }
      } # end here the while loop on file size check
      
    }  # end IF on HDF existence
  } # End cycle for downloading the images in modislist vector
}