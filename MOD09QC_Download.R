
#'@title MOD09QC_Download
#' @author Lorenzo Busetto  - Derived from Original functions by Babak Naimi (naimi@r-gis.net) - Modified by Lorenzo Busetto for producing separated mosaics
#' for the different SDS,and for converting Quality data bits into meaningful QC and UI information (Thanks Tomislav Hengl as his script made the main core of this function [spatial-analyst.net])
#' (Thanks Tomislav Hengl as his script made the main core of this function [spatial-analyst.net] Reference: http://www.r-gis.net)
#' 
#'@description This function allows to automatically download, mosaic,  reproject and resize MOD13Q1 images for selected MODIS tiles and years 
#'
#' @param Start_Year: Starting year.  
#' @param End_Year: Ending Year. (All images available between start and end year will be processed)
#' @param OutPath: Output folder. Images will be organized in subfolders of this one
#' @param ovr: Overwrite flag (deprecated) 
#' @param del: Deletion flag. If TRUE (default), then original HDF files will be deleted after processing
#' @param reproj: Reprojection flag (deprecated - always TRUE - to be removed)  
#' @param opts : options for processing. Data frame with the following fields
#' 							FTP = Set LDAAC ftp site name)
#' 							MRTpath= Path to MRT bin folder (Needed for mosaicing)
#' 							MOD_prj_str = 	# proj4 string for MODIS ISIN projection
#' 							out_prj_str = Proj4 string for output projection
#'					 		pixel_size =  Desired output pixel size
#' 							bbox =    # Bounding box for output mosaic (In Out_Proj coordinates; Xmin, Ymin, Xmax, Ymax)
#'
#' @return 
#' NONE
#'
#' @author Lorenzo Busetto (2013)
#' @email: lorenzo.busetto@@jrc.ec.europa.eu
#' @Created_Date: Mar 19, 2013
#' @export

# ----- Start of Main download and processing Function -------------#

MOD09QC_Download <- function(Start_Date=Start_Date,End_Date = End_Date, OutPath = OutPath,  ovr = ovr, del = del, reproj = reproj, opts = opts,
												bandnames = bandnames,bands_subset=bands_subset,nodatas=nodatas,nodata_out=nodata_out) {
  
  Start_Year = unlist(strsplit(Start_Date, '[.]'))[1]    ;  End_Year = unlist(strsplit(End_Date, '[.]'))[1]    ;  dir.create(OutPath, recursive = TRUE)
	
# ---------------------------------- #	
# Start Cycle on selected years
# ---------------------------------- #	
	for (yy in Start_Year:End_Year) {
		
	  # Create string representing the dates to be processed - Modify as needed
	  if (yy == Start_Year & yy == End_Year) {dates = c(Start_Date,End_Date, sep = '')}
    if (yy == Start_Year & yy != End_Year) {dates = c(Start_Date,paste(as.character(yy),'.12.31', sep = ''))}
    if (yy != Start_Year & yy == End_Year) {dates = c(paste(as.character(yy),'.12.31',End_Date, sep = ''))}
		
		# Processing status message
		mess = gwindow(title = 'Processing Status', container = TRUE, width = 400, height = 40)
		size(mess) <- c(100,8)		;	addHandlerUnrealize(mess, handler = function(h,...) {return(TRUE)})	
		mess_lab = glabel(text =paste('--- Retrieving Files for Year ',as.character(yy),' ---'), editable = FALSE, container = mess)
		
		# Create a list of the folders containing images to be downloaded (Corresponding to the selected dates) 
		
		dirs <- get_MOD_dirs(FTP = opts$FTP[1], .Platform = .Platform)
		dirs = get_MOD_dirs_dates(dates = dates, dirs = dirs)
		if (length(dirs) < 1) stop("No available data for selected dates")
		
		# Start Cycling on directories containing images to be downloaded
		for (i in 1:length(dirs)) {
			
			# Create vector of image names corresponding to the selected tiles
			Modislist = get_MOD_names(FTP = opts$FTP[1], dirs = dirs, i = i, v = v, h = h)
			date_name <- sub(sub(pattern="\\.", replacement="_", dirs[i]), pattern="\\.", replacement="_", dirs[i])	#Create the date string
			
			# ---------------------------------- ----------------------------------------------#	
			# Start cycle for downloading the images in Modislist vector  
			# ---------------------------------- ----------------------------------------------#	
			
			if (length(Modislist) > 0) {
				for (ModisName in Modislist) {
					if (file.exists(ModisName)  == FALSE | ovr == TRUE ) {
						er <- 5		; 	class(er) <- "try-error" ;	ce <- 0
						while(er != 0) {
							print(paste('Downloading File: ', ModisName ))
							svalue(mess_lab) = paste('--- Downloading Files for date', date_name, ':' ,which(Modislist == ModisName),' of ', length(Modislist),' ---')    # Update progress window
							#old version
              er <- try(download.file(url=paste(opts$FTP[1],dirs[i], "/",ModisName,sep=''),destfile=ModisName,mode='wb',quiet=F, cacheOK=FALSE),silent=FALSE)   # Start download
           		if (er != 0) {
                print('Download Error -Retrying')
                Sys.sleep(10)   ;	ce <- ce + 1 ; 	if (ce == 21) stop("Error: FTP server is down!!")	}		# Stop after 21 failed attempts
						}
					}
				}  
				
				# End cycle for downloading the images in Modislist vector	
      
				print (paste(length(Modislist)," files for date of ",dirs[i]," were successfully downloaded!",sep=''))
				
				# ---------------------------------- ----------------------------------------------#	
				# Create the temporary parameter file for MRT mosaic function
				# ---------------------------------- ----------------------------------------------#	
					mosaicname = file(paste(opts$MRTpath[1], "/TmpMosaic.prm", sep=""), open="wt")
					write(paste(OutPath,"/",Modislist[1], sep=""), mosaicname)
					if (length(Modislist) >1) {for (j in 2:length(Modislist)) write(paste(getwd(),"/",Modislist[j], sep=""),mosaicname,append=T)}
					close(mosaicname)
					
					# ---------------------------------- ----------------------------------------------#	
					# Run the MRT tool to generate the mosaic HDFs. One separate HDF is generated for each selected band
					# Added by L.Busetto --- Instead of a single mosaic,  one different mosaic for each selected band will be created.
					# This is useful in the case of very large mosaics !
					# ---------------------------------- ----------------------------------------------#	
# 					browser()
					for (band in 1:length(bands_subset)) {														# Cycle on MODIS Bands
						bands = numeric(length(bands_subset))													# Create vector with length = bands, filled with zeroes
						er_mos = 1    ; er_rep = 1
						if (bands_subset[band] == 1) {	
# 							while(er_mos != 0 &  er_rep != 0) {
								svalue(mess_lab) =  (paste('--- Mosaicing ', bandnames[band],'files for date: ',date_name ,' ---'))
								bands[band]=1																			# IF band selected for processing, put its value to 1
								
								dir.create(file.path(OutPath, bandnames[band]), showWarnings = F, recursive = T)
								bands = paste(as.character(bands), collapse = '', sep = ' ')					# Convert to character
								outfile = paste(OutPath, '/',bandnames[band],'_',substr(Modislist[1],10,13),'_',substr(Modislist[1],14,16),'.hdf', sep = '')  	# Create name for the HDF mosaic
								er_mos <- system(paste(opts$MRTpath[1], '/mrtmosaic -i ', opts$MRTpath[1], '/TmpMosaic.prm' ,' -o ', outfile,' -s ',bands, sep=""))	# Launche MRT to create the mosaic
								if (er_mos != 0)  {stop()}
								# If reprojection requested, convert to output projection using gdalwarp
								if (reproj == T) {
									print (paste('Reprojecting ', bandnames[band],'files for date: ',date_name ))
									svalue(mess_lab) =  (paste('--- Reprojecting ', bandnames[band],'files for date: ',date_name,' ---'))
                  
									outrep_file = file.path(OutPath, bandnames[band],paste(sub("[.][^.]*$", "", basename(outfile), perl=TRUE),sep = ''))	# Create name for the TIFF reprojected  mosaic
									
                  if (file.exists(outrep_file) == F) {
									# ---------------------------------- ----------------------------------------------#	
									# Create the string for gdal and launch the reprojection in a command shell
									# ---------------------------------- ----------------------------------------------#	
									
									# If bounding box was passed, the output reproject file will satisfy the bbox
									if (length(opts$bbox) == 4) {
										#browser()
										er_rep = 	system(paste('gdalwarp -s_srs "',opts$MOD_prj_str[1], 	# Launch GDAL to crete the reprojected TIFF
														'" -t_srs "', opts$out_prj_str[1], '" -of ',out_format,' -r near -co compress=lzw',
														'-te',opts$bbox[1],opts$bbox[2],opts$bbox[3],opts$bbox[4], '-tr ',opts$pixel_size[1], opts$pixel_size[1],
														'-wo "INIT_DEST=NO_DATA"','-srcnodata ', nodatas[band],'-dstnodata ', nodata_out[band], '-overwrite',
														outfile,  outrep_file, #'-overwrite',outfile,  outrep_file,
														sep = ' '))	
									} 
									else {						# If bounding box was not passed, keep the original extent when creating the TIFF
										er_rep = 	system(paste('gdalwarp -s_srs "',opts$MOD_prj_str[1], 
														'" -t_srs "', opts$out_prj_str[1],'" -of ',out_format,' -r near -co compress=lzw',
														'-tr ',opts$pixel_size[1], opts$pixel_size[1],
														'-wo "INIT_DEST=NO_DATA"','-srcnodata ',  nodata_out[band],'-dstnodata -999',
														'-overwrite', outfile,  outrep_file, #'-overwrite',outfile,  outrep_file,
														sep = ' '))			
									}
									if (er_rep != 0)  {stop()}
								} 
					    	} #End if on file existence
								if (bandnames [band] == 'QA') {
									QA_File = outrep_file
									er = MOD09_QA_to_QC(OutPath, QA_File)
								}
# 							}
							unlink(outfile)														# Delete un-reprojected Mosaic HDF file
							xml_file = paste(outrep_file,'.aux.xml',sep = '')		# Delete xml files created by gdalwarp
							unlink(xml_file)
						}  # ENDIF band selected for processing
					}	# END Cycle on selected MODIS Bands
				#}
				if (del == T) {for (ModisName in Modislist) unlink(paste(getwd(), '/', ModisName, sep=""))}		# Delete original downloaded HDFs
				
			} # ENDIF on lenght of Modislist
			
			else {print(paste("No available image for selected Tiles in ",dirs[i], sep=""))}
			
		}	# End Cycling on directories containing images to be downloaded (i.e., dates)
		
		addHandlerUnrealize(mess_lab, handler = function(h,...) {return(FALSE)})		# Allow message lab to be closed since processing ended .
		dispose(mess_lab)
	}
	return('DONE')
}	

# ----- Accessory Functions -------------#

# ---------------------------------- ----------------------------------------------#	
# Accessory function to convert Quality data bits into QC and UI readable values (see https://lpdaac.usgs.gov/products/modis_products_table/mod13q1)
# ---------------------------------- ----------------------------------------------#	

MOD09_QA_to_QC <- function(OutPath, QA_File) {
	
# 	UI_Dir = file.path(OutPath,'UI','Single_Dates')			# Create folders for storing QC and UI images
	QC_Dir = file.path(OutPath,'QC')
# 	dir.create(UI_Dir, recursive = T)
	dir.create(QC_Dir, recursive = T)
	rasterOptions(setfileext = F)
	
	in_raster = raster(QA_File, format = 'GTiff')				# Open QA file
	in_values = getValues(in_raster)								# Get the values
	QC_vals = bitAnd (in_values,3)								# Get the QA (First 2 bits)
# 	UI_vals <- bitAnd(bitShiftR(in_values,2),15)			# Get the UI (Bits 3-6)
	
	# Save the QA to new file
	out_qc_raster = raster(in_raster)
	out_qc_raster = setValues(out_qc_raster, values=QC_vals) 
	QC_File = gsub( 'QA','QC',QA_File)
	writeRaster(out_qc_raster,QC_File, format = 'GTiff' ,overwrite = TRUE, datatype='INT1U')
	
	# Save the UI to new file
# 	out_ui_raster = raster(in_raster)
# 	out_ui_raster=setValues(out_ui_raster, value = UI_vals) 
# 	UI_File = gsub( 'QA','UI',QA_File)
# 	writeRaster(out_ui_raster,UI_File, format = 'ENVI' ,overwrite = TRUE, datatype='INT1U')
		
	# Remove automatically created XML files
# 	xml_file = paste(UI_File,'.aux.xml',sep = '')
# 	unlink(xml_file)
	xml_file = paste(QC_File,'.aux.xml',sep = '')
	unlink(xml_file)
	
#	}
}
# ---------------------------------- ----------------------------------------------#	
# Accessory function to get the full list of directories on the FTP site (thanks to Barry Rowlingson):
# ---------------------------------- ----------------------------------------------#	

get_MOD_dirs <- function(FTP, .Platform) {
	if (strsplit(FTP,'')[[1]][length(strsplit(FTP,'')[[1]])] != "/") FTP <- paste(FTP,"/",sep="")
	if (.Platform$OS.type=="unix") options('download.file.method'='wget')	else options('download.file.method'='auto')
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

get_MOD_dirs_dates <- function(dates, dirs) {
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

get_MOD_names <- function(FTP, dirs, i, v, h) {
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
