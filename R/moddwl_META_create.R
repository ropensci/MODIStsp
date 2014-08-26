#' @Title moddwl_meta_create
#' @Description	Function used to create a virtual META file from time series of single-band files corresponding to 
#' different acquisition dates
#' 
#' @details	The function takeas as input the folder in which the single-band files are stored, and cretes a ENVI Meta file
#' that allows access to the full time series as if it was a "real" file
#'
#' @param out_prod_folder 
#' @param meta_band
#' @param file_prefix
#' @param ts_format 
#' @returnType 
#'
#' @return 
#' 
#' @author Lorenzo Busetto, phD (2014)
#' email: busetto.l@@irea.cnr.it
#'
#' @license GPL(>2)
#' @export

moddwl_meta_create <- function(out_prod_folder, meta_band, file_prefixes,sens_sel,  ts_format, nodata_value ) {
	
	if (sens_sel == "Terra") {file_prefix = file_prefixes[["Terra"]]} 
	if (sens_sel == "Aqua")  {file_prefix = file_prefixes[["Aqua"]]}
	if (sens_sel == "Mixed") {file_prefix = paste(file_prefixes[["Terra"]], file_prefixes[["Aqua"]], sep = '_')} 
	
	out_meta_files = list.files(file.path(out_prod_folder,meta_band), pattern = '*.dat', full.names = T)	# get list of ENVI files
	if (sens_sel != "Mixed")  {out_meta_files = out_meta_files [grep(file_prefix,out_meta_files)]}	# get list of ENVI files
	
	out_meta_files_hdr = list.files(file.path(out_prod_folder,meta_band), pattern = '*.hdr', full.names = T) # get list of hdr files
	if (sens_sel != "Mixed")  {out_meta_files_hdr = out_meta_files_hdr [grep(file_prefix,out_meta_files_hdr)]}	# get list of ENVI files
	
	doys = as.numeric(str_sub(basename(out_meta_files),-7,-5))
	years = as.numeric(str_sub(basename(out_meta_files),-12,-9))
	acq_order = order(as.numeric(paste(years,doys, sep = '')))    # find the files order (by acq.date
	
	doys = doys [acq_order]   ; years = years[acq_order]    # reorder doys and years
	out_meta_files = out_meta_files[acq_order]			 #  Reorder Files  
	
	if (ts_format == 'ENVI Meta Files' | ts_format == 'Both') {
		# retrieve nsamp and nrow from first hdr file
		head_file = paste(out_meta_files_hdr[1], sep = '')	
		fileConn_hd<-file(head_file)
		nsamp = (strsplit(readLines(fileConn_hd)[4], '=')[[1]])[2]
		nrow = (strsplit(readLines(fileConn_hd)[5], '=')[[1]])[2]
		close(fileConn_hd)
		# Write the meta file
		meta_dir = file.path(dirname(out_meta_files[1]),'Time_Series','ENVI_META')
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
		
		if (ts_format == 'R Stack Files' | ts_format == 'Both') {
			
			meta_dir = file.path(dirname(out_meta_files[1]),'Time_Series','R_Stack')
			dir.create(meta_dir, showWarnings = F,recursive = T)
   		doy_min = min(doys[which(years == min(years))])		; year_min = min(years)
			doy_max = max(doys[which(years == max(years))])		; year_max = max(years)
			dates = as.POSIXct(paste(format(strptime(doys, format="%j"), format="%m-%d"),years, sep = '-'),format='%m-%d-%Y')
			meta_filename = file.path(meta_dir,paste(file_prefix,meta_band,doy_min,year_min,doy_max,year_max,"Stack.RData", sep = '_'))
			raster = stack(out_meta_files)
			attributes(raster)$doys	 = doys
			attributes(raster)$years = years
			attributes(raster)$dates = dates
			save(raster, file = meta_filename)
			
		} # end If on necessity to build R Stack files
	}
}