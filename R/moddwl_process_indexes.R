# TODO: Add comment
# 
# Author: LB
###############################################################################


moddwl_process_indexes = function(out_filename,indexes_band, formula,bandnames,nodata_out,
		indexes_nodata_out,out_prod_folder, file_prefix, yy, DOY, out_format) {
	
	# Retrieve necessary filenames (get names of single band files on the basis of Index formula)
	
	call_string = 'Index = rasterEngine('
	fun_string = 'index <- function('
	for(band in seq(along = bandnames)) {
		bandsel = bandnames[band]
		if (length(grep(bandsel, formula)) > 0) {
			temp_bandname = bandnames[grep(bandsel,bandnames)]
			temp_file  =	file.path(out_prod_folder, temp_bandname,paste(file_prefix,'_',temp_bandname,'_',yy,'_', DOY, '.dat', sep = ''))
			temp_raster =  raster(temp_file)
			NAvalue(temp_raster)<- as.numeric(nodata_out [band])
			assign(temp_bandname, temp_raster)
			call_string = paste(call_string,temp_bandname,'=',temp_bandname,',', sep = '' )
			fun_string = paste(fun_string,temp_bandname,'=',temp_bandname,',', sep = '' )
		}
	}
	fun_string = paste(fun_string, ' form = formula,...){comp_index <-round(10000*',formula, ');	return((comp_index))}')
	call_string = paste(call_string, 'formula = formula, fun=index)')
	eval(parse(text = fun_string))
	sfQuickInit(cpus=4)
	eval(parse(text = call_string))
	sfQuickStop()

	# Save output and remove aux file
	NAvalue(Index) = indexes_nodata_out	
	writeRaster(Index, out_filename, format = out_format,NAflag = as.numeric(indexes_nodata_out), overwrite = T)
	xml_file = paste(out_filename,'.aux.xml',sep = '')		# Delete xml files created by writeRaster
	unlink(xml_file)
	gc()
	
}


