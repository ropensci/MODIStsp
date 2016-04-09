
#'  MODIStsp_process_QA_bits
#' @description function used to extract quality indicator from MODIS aggregated quality layers
#' @details On the basis of the name of the image containing the aggregated quality information
#' (in_raster_name) and of the position of the bit fields corresponding to the QI of interest in the bitfield representation
#' (bitN), the function extracts the correct information exploiting bitwise operators, and save the result in a new
#' raster image
#' @param out_filename string file name of the output raster files containing QI values
#' @param in_raster_name name of the MODIS band containing data from which the bit field corresponding to the quality indicator must be extracted
#' @param bitN position of the bits corresponding to the quality indicator (e.g., 0-1 = first two bits; 2-5: bits from 2 to 5, ecc)
#' @param source "name" of the MODIS band containing the values from which to extract the quality indicator (e.g., State_1km)
#' @param out_prod_folder main folder used for storing the output data in MODIStsp processing. New quality indicator files are stored in
#' 	out_prod_folder/derived_band
#' @param file_prefix files_prefixes used to create output file names in MODIStsp (e.g., MOD13Q1)
#' @param yy year of considered image
#' @param DOY DOY of considered image
#' @param out_format output format (ENVI or GTiff)
#' @param nodata_source nodata values of the MODIS band containing data from which the bit field corresponding to the quality indicator must be extracted
#' @param nodata_qa_in string in nodata for quality bands ("255")
#' @param nodata_qa_out string out nodata for quality bands ("255")
#' @return NULL
#'
#' @author Lorenzo Busetto, phD (2014-2015) \email{busetto.l@@irea.cnr.it}
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' Based on the "modis.qc.R" script by Yann Chemin (2008) (https://r-forge.r-project.org/scm/viewvc.php/pkg/RemoteSensing/R/modis.qc.R?view=markup&root=remotesensing&pathrev=79)
#'
#' license GPL 3.0
#' @import raster
#' @import bitops
MODIStsp_process_QA_bits <- function(out_filename,in_raster_name,bitN, source, out_prod_folder,
                                     file_prefix, yy, DOY, out_format, nodata_source,nodata_qa_in , nodata_qa_out) {
  in_raster_file <- file.path(out_prod_folder, in_raster_name,paste(file_prefix,"_",in_raster_name,"_",yy,"_", DOY, sep = "")) #define name of input "source" file
  if (out_format == "GTiff")  {
    in_raster_file <- paste0(in_raster_file,".tif")
  }
  if (out_format == "ENVI") {
    in_raster_file <- paste0(in_raster_file,".dat")
  }

  in_raster <- raster(in_raster_file, format = out_format)				# Open input file
  NAvalue(in_raster) <- as.numeric(nodata_source)					# reassign nodata
  in_values <- getValues(in_raster)								# Get the values

  bits <- as.numeric(unlist(strsplit(bitN,"-")))		# retrieve positions of the bits to be extracted

  if (bits[1] > 0) {
    in_values <- bitShiftR(in_values, bits[1])
  }	# if bits not at the start of the binary word, shift them using bitshifter
  if (length(bits) > 1) {
    bitfield_vals <- bitAnd(in_values, 2^(bits[2] - bits[1] + 1) - 1)   # retrieve the values using biAnd on the shifted word
  }	else {
    (bitfield_vals <- bitAnd(in_values, 2^(1) - 1))
  }

  in_raster <- setValues(in_raster, values = bitfield_vals)	# Set the retrieved values in the raster
  writeRaster(in_raster, out_filename, format = out_format, overwrite = TRUE, datatype = "INT1U", NAflag = as.numeric(nodata_qa_out))	# save file
  if (out_format == "ENVI") { # IF "ENVI", write the nodata value in the header
    fileConn_meta_hdr <- file(paste0(file_path_sans_ext(out_filename),".hdr"), "a")  # If output format is ENVI, add data ignore value to the header file
    writeLines(c("data ignore value = ", nodata_qa_out), fileConn_meta_hdr, sep = ' ')		# Data Ignore Value
    close(fileConn_meta_hdr)
  }
  xml_file <- paste(out_filename,".aux.xml",sep = "")		# Delete xml files created by writeRaster
  unlink(xml_file)
  gc()	# clean up

} #END
