# ---------------------------------- ----------------------------------------------#
# Accessory function to convert Quality data bits into QC and UI readable values (see https://lpdaac.usgs.gov/products/modis_products_table/mod13q1)
# ---------------------------------- ----------------------------------------------#

moddwl_QA_convert <- function(out_prod_folder = out_prod_folder, QA_file = QA_file, product = product, format = format) {

  #   UI_Dir = file.path(OutPath,'UI','Single_Dates')			# Create folders for storing QC and UI images
  QC_dir = file.path(out_prod_folder,'QC')
  # 	dir.create(UI_Dir, recursive = T)
  dir.create(QC_dir, recursive = T)
  rasterOptions(setfileext = F)

  in_raster = raster(QA_file, format = format)				# Open QA file
  in_values = getValues(in_raster)								# Get the values
  QC_vals = bitAnd (in_values,3)								# Get the QA (First 2 bits)
  # 	UI_vals <- bitAnd(bitShiftR(in_values,2),15)			# Get the UI (Bits 3-6)

  # Save the QA to new file
  out_qc_raster = raster(in_raster)
  out_qc_raster = setValues(out_qc_raster, values=QC_vals)
  QC_file = gsub( 'QA','QC',QA_file)
  if (format =='GTiff') {QC_file = paste(QC_file, '.tif', sep = '')}
  writeRaster(out_qc_raster,QC_file, format = format ,overwrite = TRUE, datatype='INT1U')

  if (product == "MOD13Q1/MYD13Q1") {
#   Save the UI to new file for MOD13 Q1
  	out_ui_raster = raster(in_raster)
  	out_ui_raster=setValues(out_ui_raster, value = UI_vals)
  	UI_File = gsub( 'QA','UI',QA_File)
  	writeRaster(out_ui_raster,UI_File, format = 'ENVI' ,overwrite = TRUE, datatype='INT1U')
  }
  # Remove automatically created XML files
  # 	xml_file = paste(UI_File,'.aux.xml',sep = '')
  # 	unlink(xml_file)
  xml_file = paste(QC_file,'.aux.xml',sep = '')
  unlink(xml_file)

  #	}
}
