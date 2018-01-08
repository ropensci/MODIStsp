#' @title MODIStsp helper for computing spectral indexes
#' @description function used to compute spectral indexes, given the index formula
#' @details the function parses the index formula to identify the required bands. On the basis
#'   of identified bands, it retrieves the reflectance bands required, gets the data into R raster
#'   objects, performs the computation and stores results in a GeoTiff or ENVI raster file
#' @param out_filename `character` basename of the file in to which save results
#' @param formula `character` Index formula, as derived from XML file and stored in prod_opts
#'   within previous_file
#' @param bandnames `character` array of names of original HDF layer. Used to identify the
#'   bands required for index computation
#' @param nodata_out `character` array of NoData values of reflectance bands
#' @param out_prod_folder `character` output folder for the product used to retrieve filenames
#'   of rasters of original bands to be used in computations
#' @param indexes_nodata_out `character` NoData value for resulting raster
#' @param file_prefix `character` used to retrieve filenames of rasters of original bands
#'   to be used in computations
#' @param compress `character` compression option for GTiff files
#' @param yy `character` year string used to retrieve filenames of rasters of original bands
#'   to be used in computations
#' @param DOY `character` doy string used to retrieve filenames of rasters of original bands to be
#'   used in computations
#' @param out_format `character` string used to retrieve filenames of rasters of original bands
#'   to be used in computations
#' @param scale_val `character` (Yes/No) if Yes, output values in are computed as float -1 - 1,
#' otherwise integer -10000 - 10000
#' @return NULL - new raster file saved in out_filename
#'
#' @author Lorenzo Busetto, phD (2017) \email{lbusett@@gmail.com}
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom raster stack NAvalue overlay
#' @importFrom tools file_path_sans_ext
MODIStsp_process_indexes <- function(out_filename,
                                     out_prod_folder,
                                     formula,
                                     bandnames,
                                     nodata_out,
                                     indexes_nodata_out,
                                     file_prefix,
                                     compress,
                                     yy,
                                     out_format,
                                     DOY,
                                     scale_val) {

  # create folder for index
  dir.create(dirname(out_filename),
             showWarnings = FALSE, recursive = TRUE)

  #   _________________________________________________________________________
  #   Create "fun_string" and "stack_string". These are parsable character  ####
  #   strings that can be parsed and evaluated to create on the fly the
  #   function to be applied to compute the index and a temporary rasterstack
  #   containing required data.

  # initialize the "fun_string" --> at the end, fun_string contains a complete
  # function definition.
  fun_string <- "index <- function("

  # initialize the "stack_string" for the overlay function --> at the end, it
  # contains a parsable string for creating a RasterStack containing
  # the bands required to compute the index
  stack_string <- "tmp_stack <- raster::stack("

  for (band in seq(along = bandnames)) {
    bandsel <- bandnames[band]
    # look if the bandname is present in the formula. If so, retrieve the
    # filename for that band and store its data in a R object that takes its
    # name from the band name, then associate it with the corresponding raster
    # file saved by MODIStsp beforehand
    if (length(grep(bandsel, formula)) > 0) {
      temp_bandname <- bandnames[grep(bandsel, bandnames)]
      # file name for the band, year, doy
      temp_file <- file.path(
        out_prod_folder, temp_bandname,
        paste0(file_prefix, "_", temp_bandname, "_", yy, "_", DOY,
               ifelse(out_format == "GTiff", ".tif", ".dat"))
      )
      temp_raster <- raster(temp_file)
      # assign NA value
      raster::NAvalue(temp_raster) <- as.numeric(nodata_out[band])
      # assign the data to a object with name = bandname
      assign(temp_bandname, temp_raster)
      # add an "entry" in fun_string (additional input parameter)
      fun_string  <- paste0(fun_string, temp_bandname, "=", temp_bandname, "," )
      # add an "entry" in stack_string (additional input in the stack)
      stack_string  <- paste0(stack_string, temp_bandname, ", ")
    }
  }
  # Finalize the fun_string: all the bands are in: now add the formula and
  # close parenthesis.
  if (scale_val == "Yes") {
    # if scale_val, indices are written as float -1 - 1
    fun_string <- paste0(fun_string, "...)", "{", formula, "}")
    dt  <- "FLT4S"
  } else {
    # otherwise, they are written as integer, with a scale factor of 10000
    # and INT2s datatype
    fun_string <- paste0(fun_string, "...)", "{round(10000*(", formula, "))}")
    dt  <- "INT2S"
  }
  # Finalize the stack_string: all the bands are in: now close parenthesis.
  stack_string <- paste0(substr(stack_string, 1, nchar(stack_string) - 2), ")")

  #   __________________________________________________________________________
  #   compute the index, by calling raster::overlay over fun_string, using  ####
  #   stack_string as input. NOTE: The fact that the bands are used in the
  #   correct "order" is guaranteed by the fact that they are added in the
  #   same order in "fun_string" and in "stack_string" in the cycle above !

  raster::overlay(eval(parse(text = stack_string)),
                  fun       = eval(parse(text = fun_string)),
                  filename  = out_filename,
                  format    = out_format,
                  datatype  = dt,
                  options   = ifelse(out_format == "GTiff",
                                     paste0("COMPRESS=", compress),
                                     ""),
                  NAflag    = as.numeric(indexes_nodata_out),
                  overwrite = TRUE)

  # IF "ENVI", write the NoData value in the header
  if (out_format == "ENVI") {

    # If output format is ENVI, add data ignore value to the header file
    fileConn_meta_hdr <- file(paste0(
      tools::file_path_sans_ext(out_filename),
      ".hdr"), "a")

    writeLines(c("data ignore value = ", as.numeric(indexes_nodata_out)),
               fileConn_meta_hdr, sep = " ")
    writeLines("", fileConn_meta_hdr)
    close(fileConn_meta_hdr)
  }
  # Delete xml files created by writeRaster
}
