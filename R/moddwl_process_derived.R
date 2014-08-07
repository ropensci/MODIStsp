# TODO: Add comment
# 
# Author: LB
###############################################################################

	
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
	
	
}
