#' @title MODIStsp helper function to compute Quality Indicators from HDF bit-field layers
#' @description function used to extract quality indicator from MODIS aggregated
#'  quality layers
#' @details On the basis of the name of the image containing the aggregated quality information
#' (`in_source_file``) and of the position of the bit fields corresponding to the QI of interest in
#' the bitfield representation (`bitN``), the function extracts the correct information exploiting
#' bitwise operators, and save the result in a new raster image
#' @param out_filename `character` file name of the output raster files
#'   containing QI values
#' @param in_source_file `character` name of the file created by MODIStsp
#'   containing the data required to compute the quality indicator
#' @param bitN `character` position of the bits corresponding to the quality
#'   indicator of interest (e.g., 0-1 = first two bits; 2-5: bits from 2 to 5,
#'   etc.)
#' @param out_format output format (ENVI or GTiff)
#' @param nodata_source `character` NoData values of the MODIS band containing
#'   data from which the bit field corresponding to the quality indicator must
#'   be extracted
#' @param nodata_qa_in `character` in NoData for quality bands ("255")
#' @param nodata_qa_out `character` out NoData for quality bands ("255")
#' @param compress `character` compression option for GTiff files
#' @return NULL
#' @author Lorenzo Busetto, phD (2017) \email{lbusett@@gmail.com}
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#'  Based on the "modis.qc.R" script by Yann Chemin (2008) (https://goo.gl/7Fhreo)
#'  license GPL 3.0
#' @importFrom bitops bitAnd bitShiftR
#' @importFrom raster raster NAvalue calc
#' @importFrom tools file_path_sans_ext
MODIStsp_process_QA_bits <- function(out_filename,
                                     in_source_file,
                                     bitN,
                                     out_format,
                                     nodata_source,
                                     nodata_qa_in,
                                     nodata_qa_out,
                                     compress) {

  dir.create(dirname(out_filename), showWarnings = FALSE, recursive = TRUE)
  # Open input file
  in_raster <- raster::raster(in_source_file, format = out_format)
  # reassign NoData to be sure
  raster::NAvalue(in_raster) <- as.numeric(nodata_source)
  # what bits do we need ?
  bits <- as.numeric(unlist(strsplit(bitN, "-")))

  # define the processing function on the basis of position of
  # required bit-fields
  bitfield_comp <- function(r, ...) {
    bit1 <-  bits[1]
    bit2 <- ifelse(length(bits) > 1,
                   2 ^ (bits[2] - bits[1] + 1) - 1,
                   2 ^ (1) - 1)
    bitops::bitAnd(bitops::bitShiftR(r, bit1), bit2)
  }

  raster::calc(in_raster,
               fun       = bitfield_comp,
               filename  = out_filename,
               format    = out_format,
               datatype  = "INT1U",
               options   = ifelse(out_format == "GTiff",
                                  paste0("COMPRESS=", compress),
                                  ""),
               NAflag    = as.numeric(nodata_qa_out),
               overwrite = TRUE)

  if (out_format == "ENVI") {
    # If output format is ENVI, add data ignore value to the header file
    fileConn_meta_hdr <- file(paste0(tools::file_path_sans_ext(out_filename),
                                     ".hdr"), "a")
    writeLines(c("data ignore value = ", nodata_qa_out), fileConn_meta_hdr,
               sep = " ")
    writeLines("", fileConn_meta_hdr)
    close(fileConn_meta_hdr)
  }
} #END
