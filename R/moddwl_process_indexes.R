# TODO: Add comment
# 
# Author: LB
###############################################################################


moddwl_process_indexes = function(out_filename,indexes_band, formula,bandnames,nodata_out,
		indexes_nodata_out,out_prod_folder, file_prefix, yy, DOY, out_format) {
	
	# Retrieve necessary filenames (get names of single band files on the basis of Index formula)
	
	call_string = 'tmp_index = rasterEngine('
	call_string_2 = 'tmp_index = index('
	fun_string = 'index <- function('
	for(band in seq(along = bandnames)) {
		bandsel = bandnames[band]
		if (length(grep(bandsel, formula)) > 0) {
			temp_bandname = bandnames[grep(bandsel,bandnames)]
			temp_file  =	file.path(out_prod_folder, temp_bandname,paste(file_prefix,'_',temp_bandname,'_',yy,'_', DOY, sep = ''))
			if (out_format=='GTiff')  temp_file  =  paste0(temp_file,'.tif')
			if (out_format=='ENVI')   temp_file  =  paste0(temp_file,'.dat')
			temp_raster =  raster(temp_file)
			NAvalue(temp_raster)<- as.numeric(nodata_out [band])
			assign(temp_bandname, temp_raster)
			call_string = paste(call_string,temp_bandname,'=',temp_bandname,',', sep = '' )
			call_string_2 = paste(call_string_2,temp_bandname,'=',temp_bandname,',', sep = '' )
			fun_string = paste(fun_string,temp_bandname,'=',temp_bandname,',', sep = '' )
		}
	}
	
	fun_string = paste(fun_string,'...)','{comp_index <-round(10000*(',formula, '));	return((comp_index))}', sep = '')
	dir.create(file.path(out_prod_folder,'Temp'), showWarnings=FALSE)
	temp_raster = gsub("\\\\",'/', file.path(out_prod_folder,'Temp','tempraster'))
	call_string = paste(call_string, 'fun=index, datatype = "INT2S", overwrite = T, filename = "',temp_raster,'")', sep = '')
	call_string_2 = paste(call_string_2, ')', sep = '')
	eval(parse(text = fun_string))
	# instructions to run call_string
#	sfQuickInit(cpus=4)
#	eval(parse(text = call_string))
#	sfQuickStop()
	# instructions to run call_string_2 (replaced because rasterEngine dealed with NA as 0) 
	eval(parse(text = call_string_2))
	# Save output and remove aux file
	NAvalue(tmp_index) = as.numeric(indexes_nodata_out)
	writeRaster(tmp_index, out_filename, format = out_format,NAflag = as.numeric(indexes_nodata_out), datatype = 'INT2S', overwrite = T)
	xml_file = paste(out_filename,'.aux.xml',sep = '')		# Delete xml files created by writeRaster
	unlink(xml_file)
	temp_files = list.files(dirname(temp_raster),pattern = "tempraster.*", full.names = T)
	file.remove(temp_files)
	gc()
	
}


