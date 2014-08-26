# TODO: Add comment
# 
# Author: LB
###############################################################################


moddwl_process_indexes = function(out_filename,indexes_band, formula,bandnames,nodata_out,
		indexes_nodata_out,out_prod_folder, file_prefix, yy, DOY, out_format) {
	
	# Retrieve necessary filenames (get names of single band files on the basis of Index formula)
	
	for(band in seq(along = bandnames)) {
		bandsel = bandnames[band]
		if (length(grep(bandsel, formula)) > 0) {
			temp_bandname = bandnames[grep(bandsel,bandnames)]
			temp_file  =	file.path(out_prod_folder, temp_bandname,paste(file_prefix,'_',temp_bandname,'_',yy,'_', DOY, '.dat', sep = ''))
			temp_raster =  raster(temp_file)
			NAvalue(temp_raster)<- as.numeric(nodata_out [band])
			assign(temp_bandname, temp_raster)
		}
	}

	Index <- eval(parse(text = formula))
#	NAvalue(Index) = indexes_nodata_out
	
	# Save output and remove aux file
	
	writeRaster(Index, out_filename, format = out_format,NAflag = as.numeric(indexes_nodata_out), overwrite = T)
	xml_file = paste(out_filename,'.aux.xml',sep = '')		# Delete xml files created by writeRaster
	unlink(xml_file)
#	rm(ls())
	gc()
	
}
