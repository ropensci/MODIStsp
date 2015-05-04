
#' moddwl_meta_create
#' @description	Function used to create virtual files from time series of single-band files corresponding to 
#' different acquisition dates
#' 
#' @details	The function takes as input the folder in which the single-band files are stored, and cretes a ENVI Meta file and/or
#' a GDAL vrt file that allows access to the full time series as if it was a single physical file
#' @param out_prod_folder string main output folder
#' @param meta_band string "name" of the band (or index, or qi) for which the virtual file is to be created
#' @param file_prefixes string file_prefixes for terra and aqua - used to identify the files corresponding to each sensor
#' @param sens_sel string name of the sensor for which the time serie has to be created (Aqua, Terra, Mixed) If "Mixed" and both terra and aqua
#' images are available, a "mixed" virtual file comprising data from both sensors ordered on DOY base is created
#' @param ts_format string required output format for virtual file (ENVI, GDAL, Both)
#' @param nodata_value string nodata value to be used for vrt files (equal to nodata value of inputs) 
#' @param out_format format of images used as "input" for the vrt and contained in out_prod_folder/band folders (ENVI or GTiff)
#' @return NULL - virtual files are stored in the "Time Series" subfolder of out_prod_folder
#' 
#' @author Lorenzo Busetto, phD (2014-2015)
#' email: busetto.l@@irea.cnr.it
#' Luigi Ranghetti, phD (2015)
#' license CC BY-NC 3.0
#' @export
moddwl_meta_create <- function(out_prod_folder, meta_band, file_prefixes,sens_sel,  ts_format, nodata_value,out_format ) {
		
	if (sens_sel == "Terra") {file_prefix = file_prefixes[["Terra"]]} 
	if (sens_sel == "Aqua")  {file_prefix = file_prefixes[["Aqua"]]}
	if (sens_sel == "Mixed") {file_prefix = paste(file_prefixes[["Terra"]], file_prefixes[["Aqua"]], sep = '_')} 
	
	if (out_format == 'ENVI') { # retrieve files list of the time serie (ENVI format)
		out_meta_files = list.files(file.path(out_prod_folder,meta_band), pattern = '\\.dat$', full.names = T)	# get list of ENVI files
		if (sens_sel != "Mixed")  {out_meta_files = out_meta_files [grep(file_prefix,out_meta_files)]}	# get list of ENVI files
		
		out_meta_files_hdr = list.files(file.path(out_prod_folder,meta_band), pattern = '\\.hdr$', full.names = T) # get list of hdr files
		if (sens_sel != "Mixed")  {out_meta_files_hdr = out_meta_files_hdr [grep(file_prefix,out_meta_files_hdr)]}	# get list of ENVI files
	}
	
	if (out_format == 'GTiff') {# retrieve files list of the time serie (GTiff format)
		out_meta_files = list.files(file.path(out_prod_folder,meta_band), pattern = '\\.tif$', full.names = T)	# get list of ENVI files
		if (sens_sel != "Mixed")  {out_meta_files = out_meta_files [grep(file_prefix,out_meta_files)]}	# get list of ENVI files
	}
	
	skip_flag = 0    # initialize skip_flag to 0 
	if ((sens_sel == "Mixed") &  #Set a flag to 1 if "mixed" was selected but either 0 AQUA or 0 TERRA files are in the time serie
			(length(grep(file_prefixes[['Aqua']],out_meta_files)) == 0) |   # in that case, the creation of META files for the mixed case is skipped !
			(length(grep(file_prefixes[['Terra']],out_meta_files)) == 0)
			) {skip_flag = 1 
	}    
	
	if (skip_flag != 1) {   # If skip_flag = 1 ( mixed TS, but data from terra or aqua missing) do nothing
		if (length(out_meta_files) > 0) {   # If no files available, skip metadata creation
			
			doys = (str_sub(basename(out_meta_files),-7,-5))		# retrieve the doys and years from filenames
			years = (str_sub(basename(out_meta_files),-12,-9))
			acq_order = order(as.numeric(paste(years,doys, sep = '')))    # find the files order (by acq.date
			
			doys = as.numeric(doys [acq_order])   ; years = as.numeric(years[acq_order])    # reorder doys and years
			out_meta_files = out_meta_files[acq_order]			 #  Reorder Files  according to acquisition date (useful to have a META file with bands
			# in the correct order
			
			if (ts_format == 'ENVI Meta Files' | ts_format == 'ENVI and GDAL') {
				
				if (out_format == 'ENVI') {
					
					# retrieve nsamp and nrow from first hdr file
					head_file = paste(out_meta_files_hdr[1], sep = '')	
					fileConn_hd<-file(head_file)
					nsamp = (strsplit(readLines(fileConn_hd)[4], '=')[[1]])[2]
					nrow = (strsplit(readLines(fileConn_hd)[5], '=')[[1]])[2]
					close(fileConn_hd)
				}
				
				if (out_format == 'GTiff') {
					
					# retrieve nsamp and nrow from first tif file
					nsamp = raster(out_meta_files[1])@ncols
					nrow = raster(out_meta_files[1])@nrows	
				}
				# Write the ENVI meta file
				meta_dir = file.path(out_prod_folder,'Time_Series','ENVI_META')
				dir.create(meta_dir, showWarnings = F,recursive = T)
				doy_min = min(doys[which(years == min(years))])		; year_min = min(years)
				doy_max = max(doys[which(years == max(years))])		; year_max = max(years)
				meta_filename = file.path(meta_dir,paste(file_prefix,meta_band,doy_min,year_min,doy_max,year_max,"META.dat", sep = '_'))  # define fileneame for meta
				fileConn_meta<-file(meta_filename, 'w')      		# Open connection
				writeLines(c('ENVI META FILE'), fileConn_meta)		# Write first line
				# Write the lines of the META file corresponding to each input file
				for (ff in out_meta_files) {
					writeLines(c(paste('File : ', ff, sep = ''),
									paste('Bands: 1', sep = ''),
									paste('Dims: 1-',nsamp,' , 1-',nrow, sep = ''), ''),
							fileConn_meta)
				}
				close(fileConn_meta)
				
				# Compute the "wavelengths" - DOYS elapsed from 01/01/2000
				temp_dates = strptime(paste(years, doys), format="%Y %j")
				elapsed = signif(difftime(temp_dates,strptime(paste(2000, 001), format="%Y %j"), units = 'days'),5)
				
				# Write the hdr file for the meta file
				
				fileConn_meta_hdr<-file(paste(file_path_sans_ext(meta_filename),'.hdr',sep = ''), 'w')
				writeLines(c('ENVI'), fileConn_meta_hdr)		# Write first line
				writeLines(c('Description = {ENVI META FILE}'), fileConn_meta_hdr)		# Dummy
				writeLines(paste('samples = ', nsamp, sep = ''), fileConn_meta_hdr)			#nsamp
				writeLines(paste('lines = ', nrow, sep = ''), fileConn_meta_hdr)			#lines
				writeLines(paste('bands = ', length(out_meta_files), sep = ''), fileConn_meta_hdr)   #nbands
				writeLines(paste('header offset = 0'), fileConn_meta_hdr)		# Dummy
				writeLines(c('file type = ENVI Meta File'), fileConn_meta_hdr)			# File type - fundamental
				writeLines(c('read procedures = {envi_read_spatial_m, envi_read_spectral_m}'), fileConn_meta_hdr)		# Fundamental ! Check if working on old versions
				writeLines(c('band names = {', paste(basename(out_meta_files),collapse=","),'}'), fileConn_meta_hdr)		# Band names
				writeLines(c('wavelength units = DOY'), fileConn_meta_hdr)		# Dummy
				writeLines(c('wavelength = {', paste(as.numeric(elapsed),collapse=","),'}'), fileConn_meta_hdr)		# Wavelengths
				close(fileConn_meta_hdr)
			}
			if (ts_format == 'GDAL vrt Files' | ts_format == 'ENVI and GDAL') {
				
				meta_dir = file.path(out_prod_folder,'Time_Series','GDAL_vrt')
				dir.create(meta_dir, showWarnings = F,recursive = T)
				
				doy_min = min(doys[which(years == min(years))])		; year_min = min(years)
				doy_max = max(doys[which(years == max(years))])		; year_max = max(years)
				dates = as.POSIXct(paste(format(strptime(doys, format="%j"), format="%m-%d"),years, sep = '-'),format='%m-%d-%Y')
				meta_filename = file.path(meta_dir,paste(file_prefix,meta_band,doy_min,year_min,doy_max,year_max,"GDAL_vrt.vrt", sep = '_'))
				gdalbuildvrt(out_meta_files,meta_filename, separate = T, srcnodata = nodata_value, vrtnodata = nodata_value)
#			raster = raster(meta_filename)
#			attributes(raster)$doys	 = doys
#			attributes(raster)$years = years
#			attributes(raster)$dates = dates
#			save(raster, file = meta_filename)
				
			} # end If on necessity to build R Stack files
		}
	} # End if on check of existence of both aqua and terra files for "mixed" metafiles creation
}