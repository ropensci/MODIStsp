#' @Title moddwl_process_QA_bits
#' @Description	Function used to extract quality indicators from a MODIS band containing quality data in different bit fields
#' 
#' @details	The function opens the band containing the quality data, extracts the values corresponding to the desired quality
#' 	indicator (e.g., cloud state) on the basis of the position of the correponding bits in the "total" binary values, and 
#' 	creates in output a Byte raster contatining the values of the indicator. Name of the output raster indicates the quality indicator
#' 	processed and the year and DOY
#'	
#' @param derived_band "name" of the quality indicator (e.g., QA_500; CS_1Km; ...)
#' @param in_raster_name name of the MODIS band containing data from which the bit field corresponding to the quality indicator must be extracted
#' @param bitN position of the bits corresponding to the quality indicator (e.g., 0-1 = first two bits; 2-5: bits from 2 to 5, ecc) 
#' @param source "name" of the MODIS band containing the values from which to extract the quality indicator (e.g., State_1km) 
#' @param out_prod_folder main folder used for storing the output data in moddwl processing. New quality indicator files are stored in 
#' 	out_prod_folder\derived_band
#' @param file_prefix files_prefixes used to create output file names in moddwl (e.g., MOD13Q1)
#' @param yy year of considered image
#' @param DOY DOY of considered image
#' @param out_format output format (ENVI or GTiff)
#' @param nodata_out nodata values of the MODIS band containing data from which the bit field corresponding to the quality indicator must be extracted
#' @returnType 
#'
#' @return 
#' 
#' @author Lorenzo Busetto, phD (2014)
#' email: busetto.l@@irea.cnr.it
#' Modified after the "modis.qc.R" script by Yann Chemin (2008) (https://r-forge.r-project.org/scm/viewvc.php/pkg/RemoteSensing/R/modis.qc.R?view=markup&root=remotesensing&pathrev=79)
#'
#' @license GPL(>2)
#' @export
moddwl_process_QA_bits <- function(out_filename,in_raster_name,bitN, source, out_prod_folder, 
		file_prefix, yy, DOY, out_format, nodata_out,quality_nodata_in , quality_nodata_out) {
	in_raster_file  =	file.path(out_prod_folder, in_raster_name,paste(file_prefix,'_',in_raster_name,'_',yy,'_', DOY, sep = '')) #define name of input file
	if (out_format=='GTiff')  in_raster_file  =  paste0(in_raster_file,'.tif')
	if (out_format=='ENVI')   in_raster_file  =  paste0(in_raster_file,'.dat')
	
	in_raster = raster(in_raster_file, format = out_format)				# Open input file
	NAvalue(in_raster)<- as.numeric(nodata_out)									# reassign nodata
	in_values = getValues(in_raster)								# Get the values
	
	bits = as.numeric(unlist(strsplit(bitN,'-')))		# retrieve positions of the bits to be extracted

	if (bits[1] > 0) {in_values = bitShiftR(in_values,bits [1])}	# if bits not at the start of the binary word, shift them 
	if (length(bits) > 1) bitfield_vals = bitAnd(in_values,2^(bits[2]-bits[1]+1)-1)	else (bitfield_vals = bitAnd(in_values,2^(1)-1))				# retrieve the values (Don't know how it works but it's fast !)
	in_raster = setValues(in_raster, values=bitfield_vals)	# Set the retrieved values in the raster
	
	writeRaster(in_raster,out_filename, format = out_format ,overwrite = TRUE, datatype = 'INT1U', NAflag = quality_nodata_out)	# save file
	xml_file = paste(out_filename,'.aux.xml',sep = '')		# Delete xml files created by writeRaster
	unlink(xml_file)
	gc()	# clean up 
	
} #END