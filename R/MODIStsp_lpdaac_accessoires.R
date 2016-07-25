MODIStsp_lpdaac_accessoires <- function() {
}

#' lpdaac_getmod_dirs
#' @description Accessory function to get the full list of directories on the lpdaac http site (modified after Barry Rowlingson function):
#'
#' @param http string http site on lpdaac corresponding to a given MODIS product
#' @param ftp string ftp site corresponding to a given MODIS product
#' @param used_server string can assume values "http" or "ftp" depending on the used download server; if NA, the script tries to download with http, using ftp if the download fails
#' @param gui logical indicates if processing was called within the GUI environment or not. If not, direct processing messages to the log
#' @param .Platform string os platform (from call to .Platform)
#' @return list of all available folders (a.k.a. dates) for the requested MODIS product on lpdaac archive
#'
#' @author Original code by Babak Naimi (.getModisList, in ModisDownload.R - http://r-gis.net/?q=ModisDownload )
#' Modified to adapt it to MODIStsp scheme and to http archive (instead than old FTP) by Lorenzo Busetto, phD (2014-2015) \email{busetto.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom gWidgets gconfirm
#' @importFrom RCurl getURL
lpdaac_getmod_dirs <- function(ftp, http, used_server=NA, gui, .Platform) {

  if (strsplit(http,"")[[1]][length(strsplit(http,"")[[1]])] != "/") {
    http <- paste(http,"/",sep = "")
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
  check_used_server <- if (is.na(used_server)) TRUE else if (used_server != "ftp") TRUE else FALSE
  if (class(items) == "try-error" & check_used_server) {
    while (class(items) == "try-error") {
      items <- try(strsplit(getURL(http, followLocation = TRUE, .opts = list(timeout = 10, maxredirs = 5, verbose = T)), "\r*\n")[[1]],
                   silent = TRUE)
      if (class(items) == "try-error") {
        Sys.sleep(1)
        ce <- ce + 1
        message("Trying to reach http server - attempt ", ce)
        print(ce)
        if (ce == 50)  {
          confirm <- if (gui) { # ask to retry only if gui=TRUE
            gconfirm("http server seems to be down! Do you want to retry ? ", icon = "question", handler = function(h,...) {} )
          } else FALSE
          if (confirm == "FALSE") {
            warning("[",date(),"] Error: http server seems to be down! Please Retry Later!")
            break()
          }
        }
      }
    }
    if (class(items) != "try-error") { # run only if ftp download works
      items <- items[-1]
      # get the directory names (available dates)
      date_dirs <- unlist(lapply(strsplit(items, ">"), function(x){
        x[length(x) - 1]
      }))
      date_dirs <- date_dirs[seq(3,length(date_dirs) - 2)]
      date_dirs <- unlist(lapply(strsplit(date_dirs, "/"), function(x){
        x[1]
      }))
      attr(date_dirs,"server") <- "http"
    }
  }
  
  # Try FTP download
  check_used_server <- if (is.na(used_server)) TRUE else if (used_server != "http") TRUE else FALSE
  if (class(items) == "try-error" & check_used_server) {
    while (class(items) == "try-error") { # try it only if HTTP failed
      items <- try(strsplit(getURL(ftp,  ftp.use.epsv = FALSE, dirlistonly = TRUE, .opts = list(timeout = 10, maxredirs = 5, verbose = TRUE)), "\r*\n")[[1]], silent = TRUE)
      if (class(items) == "try-error") {
        Sys.sleep(1)
        ce <- ce + 1
        message("Trying to reach ftp server - attempt ", ce)
        print(ce)
        if (ce == 50)  {
          confirm <- if (gui) {
            gconfirm("ftp server seems to be down! Do you want to retry ? ", icon = "question", handler = function(h,...) {} )
          } else FALSE
          if (confirm == "FALSE") {
            warning("[",date(),"] Error: ftp server seems to be down! Please Retry Later!")
            stop()
          }
        }
      }
    }
    if (class(items) != "try-error") { # run only if ftp download works
      items_1 <- items[!is.na(as.integer(items))] # maintaining only directories with numeric names (year)
      items_2 <- strsplit(getURL(paste0(ftp,items_1,"/"), ftp.use.epsv = FALSE, dirlistonly = TRUE, .opts = list(timeout = 10, maxredirs = 5, verbose = FALSE)), "\r*\n")
      full_dirs <- unlist(lapply(seq_along(items_2),function(x){paste0(names(items_2[x]),items_2[[x]],"/")}))
      date_dirs <- sapply(strsplit(full_dirs,"/"),function(x){strftime(as.Date(paste(x[length(x)-1],x[length(x)]),format="%Y %j"),"%Y.%m.%d")})
      attr(date_dirs,"server") <- "ftp"
    }
  }

  return(date_dirs)

}

#' lpdaac_getmod_dates
#'
#' @description Accessory function to find the folders corresponding to the requested dates period within the full list retrieved by lpdaac_getmod_dirs
#'
#' @param dates 2- element string array specifying start/end dates (yyyy.mm.dd) for which the http addresses of folders in lpdaac should be retrieved
#' (e.g., c("2015.1.1", "2015.12.31)
#' @param date_dirs data frame full list of folders in lpdaa archive for product of interest
#' @return array of folder names containing data for the modis product acquired in the period specified by "dates"
#'
#' @author Original code by Babak Naimi (.getModisList, in ModisDownload.R - http://r-gis.net/?q=ModisDownload )
#' Modified to adapt it to MODIStsp scheme and to http archive (instead than old FTP) by Lorenzo Busetto, phD (2014-2015)
#' email: busetto.l@@irea.cnr.it
#' license GPL 3.0
#' @import RCurl
lpdaac_getmod_dates <- function(dates, date_dirs) {

  if (length(dates) > 1) {
    start.date <- strsplit(dates[1],"\\.")[[1]]
    end.date <- strsplit(dates[2],"\\.")[[1]]
    wr <- c()
    for (i in 1:length(date_dirs)) {
      d <- unlist(strsplit(date_dirs[i],"\\."))
      if (length(d) == 3)
        if (as.numeric(d[1]) >= as.numeric(start.date[1]) & as.numeric(d[1]) <= as.numeric(end.date[1]) ) wr <- c(wr,i)
    }

    if (length(wr) > 0) date_dirs <- date_dirs[wr] else return(NULL)
    wr <- c()
    for (i in 1:length(date_dirs)) {
      d <- unlist(strsplit(date_dirs[i],"\\."))
      if (as.numeric(d[2]) < as.numeric(start.date[2]) & as.numeric(d[1]) == as.numeric(start.date[1])) wr <- c(wr,i)
      if (as.numeric(d[2]) > as.numeric(end.date[2]) & as.numeric(d[1]) == as.numeric(end.date[1])) wr <- c(wr,i)
    }

    if (length(wr) > 0) date_dirs <- date_dirs[-wr]
    if (length(date_dirs) == 0) return(NULL)
    wr <- c()
    for (i in 1:length(date_dirs)) {
      d <- unlist(strsplit(date_dirs[i],"\\."))
      if (as.numeric(d[3]) < as.numeric(start.date[3]) & as.numeric(d[1]) == as.numeric(start.date[1]) & as.numeric(d[2]) == as.numeric(start.date[2])) wr <- c(wr,i)
      if (as.numeric(d[3]) > as.numeric(end.date[3]) & as.numeric(d[1]) == as.numeric(end.date[1]) & as.numeric(d[2]) == as.numeric(end.date[2])) wr <- c(wr,i)
    }
    if (length(wr) > 0) date_dirs <- date_dirs[-wr]
    if (length(date_dirs) == 0) return(NULL)
  } else date_dirs <- date_dirs[which(date_dirs == dates[1])]

  return(date_dirs)
}
# ---------------------------------- ----------------------------------------------#
# Accessory function to find the image names corresponding to the selected dates and tiles
# ---------------------------------- ----------------------------------------------#

#' lpdaac_getmod_names
#' @description Accessory function to find the names of HDF images corresponding to a given date and interval of spatial tiles within the lpdaac archive
#' @param http string http site on lpdaac corresponding to a given MODIS product
#' @param ftp string ftp site corresponding to a given MODIS product
#' @param used_server string can assume values "http" or "ftp" depending on the used download server; it cannot be NA
#' @param date_dir array of folder names containing data for the modis product acquired in a give period (return array from lpdaac_getmod_dates)
#' @param v int. array containing a sequence of the vertical tiles of interest (e.g., c(18,19))
#' @param h int. array containing a sequence of the horizontal  tiles of interest (e.g., c(3,4))
#' @param tiled 0/1 1 = tiled product; 0 = nontiled product (resolution 0.05 deg)
#' @param gui logical indicates if processing was called within the GUI environment or not. If not, direct processing messages to the log
#' @return Modislist names of HDF images corresponding to the requested tiles available for the product in the selected date
#'
#' @author Original code by Babak Naimi (.getModisList, in ModisDownload.R - http://r-gis.net/?q=ModisDownload )
#' Modified to adapt it to MODIStsp scheme and to http archive (instead than old FTP) by Lorenzo Busetto, phD (2014-2015)
#' email: busetto.l@@irea.cnr.it
#' license  GPL 3.0
#' @import RCurl
lpdaac_getmod_names <- function(http, ftp, used_server, date_dir, v, h, tiled, gui) {
  getlist <- 0
  class(getlist) <- "try-error"
  ce <- 0
  
  if (used_server == "http") {
    
    while (class(getlist) == "try-error") {
      getlist <- try(strsplit(getURL(paste(http,date_dir, "/", sep = ""), followLocation = TRUE,
                                     .opts = list(timeout = 10, maxredirs = 5, verbose = FALSE)), "\r*\n")[[1]],silent = TRUE)
      if (class(getlist) == "try-error") {
        Sys.sleep(1)
        message("Trying to reach http server - attempt ", ce)
        ce <- ce + 1
        if (ce == 50) {
          confirm <- if (gui) {
            gconfirm("http server seems to be down! Do you want to retry ? ", icon = "question", handler = function(h,...){})
          } else FALSE
          if (confirm == "FALSE") {
            warning("[",date(),"] Error: http server seems to be down! Please Retry Later!")
            stop()
          }
        }
      }
    }
    getlist <- getlist[-1]
    getlist <- unlist(lapply(strsplit(getlist, ">"), function(x){
      x[length(x) - 1]
    }))
    getlist <- getlist[seq(3,length(getlist) - 2)]
    getlist <- unlist(lapply(strsplit(getlist, "<"), function(x){
      x[1]
    }))
  
  } else if (used_server == "ftp") {

    date_year <- strftime(as.Date(date_dir,format="%Y.%m.%d"), "%Y")
    date_doy <- strftime(as.Date(date_dir,format="%Y.%m.%d"), "%j")
    while (class(getlist) == "try-error") {
      getlist <- try(strsplit(getURL(paste(ftp,date_year,"/",date_doy,"/", sep = ""), ftp.use.epsv = FALSE, dirlistonly = TRUE, 
                                     .opts = list(timeout = 10, maxredirs = 5, verbose = TRUE)), "\r*\n")[[1]],silent = TRUE)
      if (class(getlist) == "try-error") {
        Sys.sleep(1)
        message("Trying to reach ftp server - attempt ", ce)
        ce <- ce + 1
        if (ce == 50) {
          confirm <- if (gui) {
            gconfirm("ftp server seems to be down! Do you want to retry ? ", icon = "question", handler = function(h,...){})
          } else FALSE
          if (confirm == "FALSE") {
            warning("[",date(),"] Error: ftp server seems to be down! Please Retry Later!")
            stop()
          }
        }
      }
    }

  }
  
  Modislist <- c()
  if (tiled == 1 ) {
    for (vv in v) {
      for (hh in h) {
        if (vv < 10) vc <- paste0("0",as.character(vv))
        else vc <- as.character(vv)
        if (hh < 10) hc <- paste0("0",as.character(hh))
        else hc <- as.character(hh)
        ModisName <- grep(".hdf$",grep(paste0("h",hc,"v",vc),getlist, value = TRUE), value = TRUE)
        if (length(ModisName) >= 1) Modislist <- c(Modislist,ModisName[1])
      }
    }
  } else {
    Modislist <- grep(".hdf$",getlist, value = TRUE)
  }
  return(Modislist)

}
