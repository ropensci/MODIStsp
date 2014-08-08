Moddwl_Accessoires = function () {
	
	
}

# ---------------------------------- ----------------------------------------------#
# Accessory function to get the full list of directories on the http site (modified after Barry Rowlingson function):
# ---------------------------------- ----------------------------------------------#

getmod_dirs <- function(FTP, .Platform) {
	
	if (strsplit(FTP,'')[[1]][length(strsplit(FTP,'')[[1]])] != "/") {FTP <- paste(FTP,"/",sep="")}
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
	ret
}


# ---------------------------------- ----------------------------------------------#
# Accessory function to find the folders corresponding to the selected dates  (thanks to Barry Rowlingson):
# ---------------------------------- ----------------------------------------------#

getmod_dates <- function(dates, dirs) {
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

getmod_names <- function(FTP, dirs, i, v, h) {
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

# ---------------------------------- ----------------------------------------------#
# Accessory function to create virtual BSQ files starting from the reflectances files
# ---------------------------------- ----------------------------------------------#
moddwl_refl_bsq <- function (product,out_prod_folder,bandnames,bandsel, file_prefix, yy, DOY  ) {
	
	if (product == "MOD09GA/MYD09GA") {
		
		ref_file_names = file.path(out_prod_folder, bandnames[11:17],paste(file_prefix,'_',bandnames[11:17],'_',yy,'_', DOY, sep = ''))
		out_meta_bands = bandsel[11:17]
		out_meta_files = ref_file_names[c(3,4,1,2,5,6,7)]   ; out_meta_bands = out_meta_bands[c(3,4,1,2,5,6,7)]
		out_meta_files = out_meta_files[which(out_meta_bands == 1)]
		head_file = paste(out_meta_files[1],'.hdr', sep = '')
		fileConn_hd<-file(head_file)
		nsamp = (strsplit(readLines(fileConn_hd)[4], '=')[[1]])[2]
		nrow = (strsplit(readLines(fileConn_hd)[5], '=')[[1]])[2]
		close(fileConn_hd)
		dir.create(file.path(out_prod_folder, 'Surf_Ref_BSQ'), recursive = T, showWarnings = F)
		meta_filename = file.path(out_prod_folder, 'Surf_Ref_BSQ',paste(file_prefix,'_',"Surf_Ref",'_',yy,'_', DOY, sep = ''))
		fileConn_meta<-file(meta_filename, 'w')      		# Open connection
		writeLines(c('ENVI META FILE'), fileConn_meta)		# Write first line
		# Write the lines of the META file corresponding to each input file
		for (ff in out_meta_files) {
			writeLines(c(paste('File : ', ff, sep = ''),
							paste('Bands: 1', sep = ''),
							paste('Dims: 1-',nsamp,' , 1-',nrow, sep = ''), ''),
					fileConn_meta)
		}
		close(fileConn_meta)  	# Close connection to META file
		
	}
	
	if (product == "MOD13Q1/MYD13Q1") {
		
		ref_file_names = file.path(out_prod_folder, bandnames[4:7],paste(file_prefix,'_',bandnames[4:7],'_',yy,'_', DOY, sep = ''))
		out_meta_bands = bandsel[4:7]
		out_meta_files = ref_file_names[c(3,1,2,4)]   ; out_meta_bands = out_meta_bands[c(3,1,2,4)]
		out_meta_files = out_meta_files[which(out_meta_bands == 1)]
		head_file = paste(out_meta_files[1],'.hdr', sep = '')
		fileConn_hd<-file(head_file)
		nsamp = (strsplit(readLines(fileConn_hd)[4], '=')[[1]])[2]
		nrow = (strsplit(readLines(fileConn_hd)[5], '=')[[1]])[2]
		close(fileConn_hd)
		dir.create(file.path(out_prod_folder, 'Surf_Ref_BSQ'), recursive = T, showWarnings = F)
		meta_filename = file.path(out_prod_folder, 'Surf_Ref_BSQ',paste(file_prefix,'_',"Surf_Ref",'_',yy,'_', DOY, sep = ''))
		fileConn_meta<-file(meta_filename, 'w')        	# Open connection
		writeLines(c('ENVI META FILE'), fileConn_meta)		# Write first line
		# Write the lines of the META file corresponding to each input file
		for (ff in out_meta_files) {
			writeLines(c(paste('File : ', ff, sep = ''),
							paste('Bands: 1', sep = ''),
							paste('Dims: 1-',nsamp,' , 1-',nrow, sep = ''), ''),
					fileConn_meta)
		}
		close(fileConn_meta)  	# Close connection to META file
		
	}
	
}

# ---------------------------------- ----------------------------------------------#
# Accessory function used to see if all expected out files for the selected date are already present.
# If so, check_files is set to TRUE, and MODIS hdf is not downloaded
# ---------------------------------- ----------------------------------------------#
moddwl_check_files = function(out_prod_folder, file_prefix,bandnames,bandsel,yy,DOY, out_format, indexes_bandnames, indexes_bandsel, quality_bandnames, quality_bandsel) {
	check = T
	for (band in 1:length(bandnames)) {
		if (bandsel [band] == 1) {
			
			outfile = paste(out_prod_folder, '/',bandnames[band],'_',yy,'_',DOY,'.hdf', sep = '')    # Create name for the HDF mosaic
			outrep_file = file.path(out_prod_folder, bandnames[band], paste(file_prefix,'_',sub("[.][^.]*$", "", basename(outfile), perl=TRUE),sep = ''))  # Create name for the TIFF reprojected  mosaic
			if (out_format =='GTiff') {outrep_file = paste(outrep_file, '.tif', sep = '')}
			if (out_format =='ENVI') {outrep_file = paste(outrep_file, '.dat', sep = '')}
			if (file.exists(outrep_file) == F) {check = F}
		}
	}
	
	for (band in seq(along = indexes_bandnames)) {
		if (indexes_bandsel [band] == 1) {
			outfile = paste(out_prod_folder, '/',indexes_bandnames[band],'_',yy,'_',DOY,'.hdf', sep = '')    # Create name for the HDF mosaic
			outder_file = file.path(out_prod_folder, indexes_bandnames[band], paste(file_prefix,'_',sub("[.][^.]*$", "", basename(outfile), perl=TRUE),sep = ''))  # Create name for the TIFF reprojected  mosaic
			if (out_format =='GTiff') {outder_file = paste(outder_file, '.tif', sep = '')}
			if (out_format =='ENVI') {outder_file = paste(outder_file, '.dat', sep = '')}
			if (file.exists(outder_file) == F) {check = F}
		}
	}
	
	for (band in seq(along = quality_bandnames)) {
		if (quality_bandsel [band] == 1) {
			outfile = paste(out_prod_folder, '/',quality_bandnames[band],'_',yy,'_',DOY,'.hdf', sep = '')    # Create name for the HDF mosaic
			outder_file = file.path(out_prod_folder, quality_bandnames[band], paste(file_prefix,'_',sub("[.][^.]*$", "", basename(outfile), perl=TRUE),sep = ''))  # Create name for the TIFF reprojected  mosaic
			if (out_format =='GTiff') {outder_file = paste(outder_file, '.tif', sep = '')}
			if (out_format =='ENVI') {outder_file = paste(outder_file, '.dat', sep = '')}
			if (file.exists(outder_file) == F) {check = F}
		}
	}
	return(check)
}