#' @Title moddwl_meta_create
#' @Description	Function used to create a virtual META file from time series of single-band files corresponding to 
#' different acquisition dates
#' 
#' @details	The function takeas as input the folder in which the single-band files are stored, and cretes a ENVI Meta file
#' that allows access to the full time series as if it was a "real" file
#'
#' @param out_prod_folder 
#' @param meta_band 
#' @returnType 
#'
#' @return 
#' 
#' @author Lorenzo Busetto, phD (2014)
#' email: busetto.l@@irea.cnr.it
#'
#' @license GPL(>2)
#' @export

moddwl_meta_create <- function(out_prod_folder, meta_band, file_prefix ) {
	
	out_meta_files = list.files(file.path(out_prod_folder,meta_band), pattern = '.dat', full.names = T)	# get list of ENVI files
	out_meta_files_hdr = list.files(file.path(out_prod_folder,meta_band), pattern = '.hdr', full.names = T) # get list of hdr files
	
	# retrieve nsamp and nrow from first hdr file
	head_file = paste(out_meta_files_hdr[1], sep = '')	
	fileConn_hd<-file(head_file)
	nsamp = (strsplit(readLines(fileConn_hd)[4], '=')[[1]])[2]
	nrow = (strsplit(readLines(fileConn_hd)[5], '=')[[1]])[2]
	close(fileConn_hd)
	
	# Write the meta file
	meta_dir = file.path(dirname(out_meta_files[1]),'METAFILES')
	dir.create(meta_dir, showWarnings = F,recursive = T)
	meta_filename = file.path(meta_dir,paste(file_prefix,'_',meta_band,'_',"META.dat", sep = ''))  # define fileneame for meta
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
	doys = as.numeric(str_sub(basename(out_meta_files),-7,-5))
	years = as.numeric(str_sub(basename(out_meta_files),-12,-9))
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
	
#	gdalbuildvrt(gdalfile=out_meta_files,output.vrt=file.choose(), format = 'ENVI',separate=TRUE,verbose=TRUE)
	
}