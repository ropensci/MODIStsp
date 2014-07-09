LB_MOD_DWL_ACCESSOIRES = function () {
  
  
}

# ---------------------------------- ----------------------------------------------#  
# Accessory function to get the full list of directories on the http site (modified after Barry Rowlingson function):
# ---------------------------------- ----------------------------------------------#	

GET_MOD_DIRS <- function(FTP, .Platform) {
  if (strsplit(FTP,'')[[1]][length(strsplit(FTP,'')[[1]])] != "/") FTP <- paste(FTP,"/",sep="")
  if (.Platform$OS.type=="unix") options('download.file.method'='wget')  else options('download.file.method'='auto')
  items <- 0
  class(items) <- "try-error"
  ce <- 0
  while(class(items) == "try-error") {
    items <- try(strsplit(getURL(FTP), "\r*\n")[[1]],silent=TRUE)
    if (class(items) == "try-error") {
      Sys.sleep(10)
      ce <- ce + 1
      if (ce == 200) stop("Error: FTP server is down!!")
    }
  }
  items <- items[-1]
  # get the directory names (available dates)
  dirs <- unlist(lapply(strsplit(items, ">"), function(x){x[length(x)-1]}))
  dirs = dirs[seq(3,length(dirs)-2)]
  dirs <- unlist(lapply(strsplit(dirs, "/"), function(x){x[1]}))
  return(dirs)
}


# ---------------------------------- ----------------------------------------------#  
# Accessory function to find the folders corresponding to the selected dates  (thanks to Barry Rowlingson):
# ---------------------------------- ----------------------------------------------#	

GET_MOD_DIRS_DATES <- function(dates, dirs) {
  if (length(dates) > 1) {
    start.date <- strsplit(dates[1],'\\.')[[1]]
    end.date <- strsplit(dates[2],'\\.')[[1]]
    wr <- c()
    for (i in 1:length(dirs)) {
      d <- unlist(strsplit(dirs[i],"\\."))
      if (length(d) == 3)
        if (as.numeric(d[1]) >= as.numeric(start.date[1]) & as.numeric(d[1]) <= as.numeric(end.date[1]) ) wr <- c(wr,i)
    }
    if (length(wr) > 0) dirs <- dirs[wr]
    wr <- c()
    for (i in 1:length(dirs)) {
      d <- unlist(strsplit(dirs[i],"\\."))
      if (as.numeric(d[2]) < as.numeric(start.date[2]) & as.numeric(d[1]) == as.numeric(start.date[1])) wr <- c(wr,i)
      if (as.numeric(d[2]) > as.numeric(end.date[2]) & as.numeric(d[1]) == as.numeric(end.date[1])) wr <- c(wr,i)
    }
    if (length(wr) > 0) dirs <- dirs[-wr]
    wr <- c()
    for (i in 1:length(dirs)) {
      d <- unlist(strsplit(dirs[i],"\\."))
      if (as.numeric(d[3]) < as.numeric(start.date[3]) & as.numeric(d[1]) == as.numeric(start.date[1]) & as.numeric(d[2]) == as.numeric(start.date[2])) wr <- c(wr,i)
      if (as.numeric(d[3]) > as.numeric(end.date[3]) & as.numeric(d[1]) == as.numeric(end.date[1]) & as.numeric(d[2]) == as.numeric(end.date[2])) wr <- c(wr,i)
    }
    if (length(wr) > 0) dirs <- dirs[-wr]
  } else dirs <- dirs[which(dirs == dates[1])]
  return(dirs)
}
# ---------------------------------- ----------------------------------------------#	
# Accessory function to find the image names corresponding to the selected dates and tiles
# ---------------------------------- ----------------------------------------------#	

GET_MOD_NAMES <- function(FTP, dirs, i, v, h) {
  getlist <- 0
  class(getlist) <- "try-error"
  ce <- 0
  while(class(getlist) == "try-error") {
    getlist <- try(strsplit(getURL(paste(FTP,dirs[i], "/", sep="")), "\r*\n")[[1]],silent=TRUE)
    if (class(getlist) == "try-error") {
      Sys.sleep(5)
      ce <- ce + 1
      if (ce == 21) stop("Error: FTP server is down!!")
    }
  }
  getlist <- getlist[-1]
  getlist <- unlist(lapply(strsplit(getlist, ">"), function(x){x[length(x)-1]}))
  getlist = getlist[seq(3,length(getlist)-2)]
  getlist <- unlist(lapply(strsplit(getlist, "<"), function(x){x[1]}))
  # 	getlist <- unlist(lapply(strsplit(getlist, " "), function(x){x[length(x)]}))
  Modislist <- c()
  for (vv in v) {
    for (hh in h) {
      if (vv < 10) vc <- paste('0',as.character(vv),sep='')
      else vc <- as.character(vv)
      if (hh < 10) hc <- paste('0',as.character(hh),sep='')
      else hc <- as.character(hh)
      ModisName <- grep(".hdf$",grep(paste('h',hc,'v',vc,sep=''),getlist,value=TRUE),value=TRUE)
      if (length(ModisName) == 1) Modislist <- c(Modislist,ModisName)
    }
  }
  return(Modislist)
}