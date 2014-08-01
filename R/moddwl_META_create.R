# - Accessory function for METADATA file creation.

moddwl_METABSQ_create <- function(product, yy, DOY,out_folder) {

  # Get files dimensions from the header of the first file
  head_file = paste(Out_Files[1],'.hdr', sep = '')
  fileConn_hd<-file(head_file)
  nsamp = (strsplit(readLines(fileConn_hd)[4], '=')[[1]])[2]
  nrow = (strsplit(readLines(fileConn_hd)[5], '=')[[1]])[2]
  close(fileConn_hd)

  # Define META FILE name
  if (Method ==1) {
    meta_filename = paste('S',Index,'_',Start_Year,'_',End_Year,'_META', sep = '')
    meta_filename = file.path(Out_Dir = file.path(Out_Folder, paste('S',Index, sep = ''),meta_filename),fsep = '/')
  }

  if (Method ==2) {
    meta_filename = paste('Med_S',Index,'_',Start_Year,'_',End_Year, '_META', sep = '')
    meta_filename = file.path(Out_Dir = file.path(Out_Folder, paste('Med_S',Index, sep = ''),meta_filename),fsep = '/')
  }

  fileConn_meta<-file(meta_filename, 'w')					# Open connection
  writeLines(c('ENVI META FILE'), fileConn_meta)		# Write first line

  # Write the lines of the META file corresponding to each input file
  for (ff in Out_Files) {
    writeLines(c(paste('File : ', ff, sep = ''),
                 paste('Bands: 1', sep = ''),
                 paste('Dims: 1-',nsamp,' , 1-',nrow, sep = ''), ''),
               fileConn_meta)
  }
  close(fileConn_meta)		# Close connection to META file
}
