#' @Title moddwl_process_NDVI
#' @Description function used to compute NDVI starting from Red and NIR MODIS bands
#' 
#' @details
#'
#' @param derived_band code of the input product (e.g., 'NDVI'))
#' @param out_prod_folder main output folder for the processing
#' @param bandnames bandnames of theproduct. used to identify the correct reflectance files
#' @param file_prefix file_prefix (e.g., MOD13 or MYD13)
#' @param yy year of preocessed image
#' @param DOY doy of processed image
#' @param nodata_out nodata value in inputs
#' @param derived_nodata_out value to be assigned to nodata in output image 
#' @param out_format format to be used for output (e.g., "ENVI")
#' @returnType 
#'
#' @return 
#' NULL
#' @author Lorenzo Busetto, phD (2014)
#' email: busetto.l@@irea.cnr.it
#'
#' @license GPL(>2)
#' @export

moddwl_process_NDVI = function(out_filename, out_prod_folder ,bandnames ,
		file_prefix , yy , DOY ,nodata_out, derived_nodata_out, out_format) {
	
		
	# Retrieve necessary filenames (Red and NIR)
	red_bandname = bandnames[grep('b1',bandnames)]
	nir_bandname = bandnames[grep('b2',bandnames)]
	file_red  =	file.path(out_prod_folder, red_bandname,paste(file_prefix,'_',red_bandname,'_',yy,'_', DOY, '.dat', sep = ''))
	file_nir  =	file.path(out_prod_folder, nir_bandname,paste(file_prefix,'_',nir_bandname,'_',yy,'_', DOY, '.dat', sep = ''))
	
	# Open the bands' rasters
	nir = raster(file_nir)   ; 	NAvalue(nir)<- as.numeric(nodata_out [12])
	red = raster(file_red)	 ;  NAvalue(red)<- as.numeric(nodata_out [11])
	
	f_NDVI <- function(x, y) {
		(x-y)/(x+y)
	}
	
	NDVI <- overlay(nir, red, fun=f_NDVI)  # Compute Derived band
	
	# Save output and remove aux file

	writeRaster(NDVI, out_filename, format = out_format,NAflag = as.numeric(derived_nodata_out), overwrite = T)
	xml_file = paste(out_filename,'.aux.xml',sep = '')		# Delete xml files created by writeRaster
	unlink(xml_file)
	gc()
	
}