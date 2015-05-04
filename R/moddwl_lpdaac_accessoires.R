moddwl_lpdaac_accessoires = function () {
}

#' lpdaac_getmod_dirs
#' @description Accessory function to get the full list of directories on the lpdaac http site (modified after Barry Rowlingson function):
#' 
#' @param http string http site on lpdaac corresponding to a given MODIS product
#' @param .Platform string os platform (from call to .Platform)
#' @return list of all available folders (a.k.a. dates) for the requeted MODIS product on lpdaac archive
#' 
#' @author Original code by XXXXXX 
#' Modified to adapt it to moddwl scheme and to http archive (instead than old FTP) by Lorenzo Busetto, phD (2014-2015)
#' email: busetto.l@@irea.cnr.it
#' license CC BY-NC 3.0
#' @export
lpdaac_getmod_dirs <- function(http, .Platform) {
	
	if (strsplit(http,'')[[1]][length(strsplit(http,'')[[1]])] != "/") {http <- paste(http,"/",sep="")}
	if (.Platform$OS.type=="unix") options('download.file.method'='wget')  else options('download.file.method'='auto')
	items <- 0
	class(items) <- "try-error"
	ce <- 0
	while(class(items) == "try-error") {
		items <- try(strsplit(getURL(http, followLocation = TRUE, .opts = list(timeout = 5, maxredirs = 5, verbose = T)), "\r*\n")[[1]],silent=TRUE)
		if (class(items) == "try-error") {
			Sys.sleep(1)
			ce <- ce + 1
			print(ce)
			if (ce == 50) stop("Error: http server is down!!")
		}
	}
	items <- items[-1]
	# get the directory names (available dates)
	date_dirs <- unlist(lapply(strsplit(items, ">"), function(x){x[length(x)-1]}))
	date_dirs = date_dirs[seq(3,length(date_dirs)-2)]
	date_dirs <- unlist(lapply(strsplit(date_dirs, "/"), function(x){x[1]}))
	
	return(date_dirs)
	
}

#' lpdaac_getmod_dates
#' @description Accessory function to find the folders corresponding to the requested dates period within the full list retrieved by lpdaac_getmod_dirs
#' 
#' @param dates 2- element string array specifying start/end dates (yyyy.mm.dd) for which the http addresses of folders in lpdaac should be retrieved
#' (e.g., c("2015.1.1", "2015.12.31)
#' @param date_dirs data frame full list of folders in lpdaa archive for product of interest 
#' @return array of folder names containing data for the modis product acquired in the period specified by "dates" 
#' 
#' @author Original code by XXXXXX 
#' Modified to adapt it to moddwl scheme and to http archive (instead than old FTP) by Lorenzo Busetto, phD (2014-2015)
#' email: busetto.l@@irea.cnr.it
#' license CC BY-NC 3.0
#' @export
lpdaac_getmod_dates <- function(dates, date_dirs) {
	
	if (length(dates) > 1) {
		start.date <- strsplit(dates[1],'\\.')[[1]]
		end.date <- strsplit(dates[2],'\\.')[[1]]
		wr <- c()
		for (i in 1:length(date_dirs)) {
			d <- unlist(strsplit(date_dirs[i],"\\."))
			if (length(d) == 3)
				if (as.numeric(d[1]) >= as.numeric(start.date[1]) & as.numeric(d[1]) <= as.numeric(end.date[1]) ) wr <- c(wr,i)
		}
		
		if (length(wr) > 0) date_dirs <- date_dirs[wr] else return (NULL)
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
#' @details 
#' @param http string http site on lpdaac corresponding to a given MODIS product
#' @param date_dirs array of folder names containing data for the modis product acquired in a give period (return array from lpdaac_getmod_dates)
#' @param date string date for which the HDF filenames has to be retrieved
#' @param v int. array containing a sequence of the vertical tiles of interest (e.g., c(18,19))
#' @param h int. array containing a sequence of the horizontal  tiles of interest (e.g., c(3,4))
#' @return Modislist names of HDF images corresponding to the requested tiles available for the product in the selected date 
#' 
#' @author Original code by XXXXXX 
#' Modified to adapt it to moddwl scheme and to http archive (instead than old FTP) by Lorenzo Busetto, phD (2014-2015)
#' email: busetto.l@@irea.cnr.it
#' license CC BY-NC 3.0
#' @export
lpdaac_getmod_names <- function(http, date_dirs, date, v, h) {
	getlist <- 0
	class(getlist) <- "try-error"
	ce <- 0
	while(class(getlist) == "try-error") {
		getlist <- try(strsplit(getURL(paste(http,date_dirs[date], "/", sep=""), followLocation = TRUE, .opts = list(timeout = 20, maxredirs = 2, verbose = F)), "\r*\n")[[1]],silent=TRUE)
		if (class(getlist) == "try-error") {
			Sys.sleep(5)
			ce <- ce + 1
			if (ce == 21) stop("Error: http server is down!!")
		}
	}
	getlist <- getlist[-1]
	getlist <- unlist(lapply(strsplit(getlist, ">"), function(x){x[length(x)-1]}))
	getlist = getlist[seq(3,length(getlist)-2)]
	getlist <- unlist(lapply(strsplit(getlist, "<"), function(x){x[1]}))
	Modislist <- c()
	for (vv in v) {
		for (hh in h) {
			if (vv < 10) vc <- paste('0',as.character(vv),sep='')
			else vc <- as.character(vv)
			if (hh < 10) hc <- paste('0',as.character(hh),sep='')
			else hc <- as.character(hh)
			ModisName <- grep(".hdf$",grep(paste('h',hc,'v',vc,sep=''),getlist,value=TRUE),value=TRUE)
			if (length(ModisName) >= 1) Modislist <- c(Modislist,ModisName[1])
		}
	}
	
	return(Modislist)
	
}
