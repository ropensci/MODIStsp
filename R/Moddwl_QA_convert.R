# ---------------------------------- ----------------------------------------------#
# Accessory function to convert Quality data bits into QC and UI readable values (see https://lpdaac.usgs.gov/products/modis_products_table/mod13q1)
# ---------------------------------- ----------------------------------------------#

Moddwl_QA_convert <- function(out_folder = out_folder, QA_file = QA_file, product = product) {

  #   UI_Dir = file.path(OutPath,'UI','Single_Dates')			# Create folders for storing QC and UI images
  QC_Dir = file.path(OutPath,'QC')
  # 	dir.create(UI_Dir, recursive = T)
  dir.create(QC_Dir, recursive = T)
  rasterOptions(setfileext = F)

  in_raster = raster(QA_File, format = 'GTiff')				# Open QA file
  in_values = getValues(in_raster)								# Get the values
  QC_vals = bitAnd (in_values,3)								# Get the QA (First 2 bits)
  # 	UI_vals <- bitAnd(bitShiftR(in_values,2),15)			# Get the UI (Bits 3-6)

  # Save the QA to new file
  out_qc_raster = raster(in_raster)
  out_qc_raster = setValues(out_qc_raster, values=QC_vals)
  QC_File = gsub( 'QA','QC',QA_File)
  writeRaster(out_qc_raster,QC_File, format = 'GTiff' ,overwrite = TRUE, datatype='INT1U')

  # Save the UI to new file
  # 	out_ui_raster = raster(in_raster)
  # 	out_ui_raster=setValues(out_ui_raster, value = UI_vals)
  # 	UI_File = gsub( 'QA','UI',QA_File)
  # 	writeRaster(out_ui_raster,UI_File, format = 'ENVI' ,overwrite = TRUE, datatype='INT1U')

  # Remove automatically created XML files
  # 	xml_file = paste(UI_File,'.aux.xml',sep = '')
  # 	unlink(xml_file)
  xml_file = paste(QC_File,'.aux.xml',sep = '')
  unlink(xml_file)

  #	}
}
