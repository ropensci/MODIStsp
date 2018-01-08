#' @title MODIStsp helper for processing original HDF layers
#' @description Internal function used to performed the required spatial
#'  processing on MODIS original hdf layers (reprojection, resizing, resampling,
#'  mosaicing, computation of scaling factors). The function is based on the
#'  use of `gdal` routines.
#' @inheritParams MODIStsp_process
#' @param modislist `character array` List of MODIS images to be downloaded for
#'  the selected date (as returned from `get_mod_filenames`). Can be a single
#'  image, or a list of images in case different tiles are needed!
#' @param sens_sel `character ["terra" | "aqua"]` Selected sensor.
#' @param band `numeric` band number corresponding to the HDF layer to be
#'  processed
#' @param bandname `character` Name to the HDF layer to be processed.
#' @param date_name `character` Date of acquisition of the images to be
#'  downloaded.
#' @param datatype `character` Datatype to the HDF layer to be processed.
#' @param nodata_in `numeric` Original nodata value to the HDF layer to be
#'  processed.
#' @param nodata_out `numeric` Output nodata value to the HDF layer to be
#'  processed.
#' @param scale_factor `numeric` Scale factor to be applied to the HDF layer
#'  to be processed (Ignored if `scale_val` == FALSE).
#' @param offset `numeric` Offset to be applied to the HDF layer
#'  to be processed (Ignored if `scale_val` == FALSE).
#' @param outrep_file `character` Full path of the file where results of the
#'  processing are to be stored (created in `MODIStsp_process`)
#' @param mess_lab Pointer to the gWidget used to visualize processing messages
#'  in interactive execution.
#' @param verbose `logical` If FALSE, suppress processing messages, Default: TRUE
#' @return The function is called for its side effects
#' @rdname MODIStsp_process_bands
#' @author Lorenzo Busetto, phD (2014-2017) \email{lbusett@@gmail.com}
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @importFrom gdalUtils gdal_translate gdalbuildvrt gdalwarp gdalinfo
#' @importFrom stringr str_sub
#' @importFrom raster res raster calc
#' @importFrom tools file_path_sans_ext
#' @importFrom parallel detectCores
#' @importFrom gWidgets svalue
#' @importFrom stats na.omit
#'
MODIStsp_process_bands <- function(out_folder_mod, modislist,
                                   outproj_str, mod_proj_str, sens_sel,
                                   band, bandname, date_name, datatype,
                                   nodata_in, nodata_out,
                                   full_ext, bbox,
                                   scale_val, scale_factor, offset,
                                   out_format, outrep_file, compress,
                                   out_res_sel, out_res, resampling,
                                   gui, mess_lab, verbose) {

  files_in <- file.path(out_folder_mod, modislist)

  #   __________________________________________________________________________
  #   Initialize number of cores for gdalwarp (equal to ncpus - 2 OR 8 if  ####
  #   number of available cpus > 8 (this to avoid overloading servers with high
  #   number of cpus)

  ncores <- min(c(8, parallel::detectCores() - 2))

  # check to see if the patch to correct wrong resolution/bbox in some HDF4 ----
  # original layers (e.g. albedo) is needed

  # Retrieve information from hdf4 with gdalinfo
  gdalinfo_hdf_raw <- gdalUtils::gdalinfo(file.path(out_folder_mod,
                                                    modislist[1]))

  gdalinfo_hdf_1stlayer <- gsub(
    "^ *SUBDATASET_1_NAME=", "",
    gdalinfo_hdf_raw[grep("^ *SUBDATASET_1_NAME",
                          gdalinfo_hdf_raw)]
  )

  gdalinfo_hdf_resunit  <- gsub(
    "^ *NADIRDATARESOLUTION=[0-9.]+ ?", "",
    gdalinfo_hdf_raw[grep("^ *NADIRDATARESOLUTION",
                          gdalinfo_hdf_raw)]
  )

  gdalinfo_raw  <- if (length(gdalinfo_hdf_1stlayer) > 0) {
    # if more than a band is present, take gdalinfo from the first
    # band
    gdalUtils::gdalinfo(gdalinfo_hdf_1stlayer)
  } else {
    # otherwise, take from the HDF directly
    gdalinfo_hdf_raw
  }
  gdalinfo_bbox <- cbind(
    stats::na.omit(as.numeric(unlist(strsplit(gsub(
      "[^0-9.\\-]+",
      " ",
      gdalinfo_raw[grep("^Lower Left", gdalinfo_raw)]), " ")
    )))[1:2],

    stats::na.omit(as.numeric(unlist(strsplit(gsub(
      "[^0-9.\\-]+", " ",
      gdalinfo_raw[grep("^Upper Right", gdalinfo_raw)]),
      " "))))[1:2]
  )
  # if HDF file is in degrees and with a small bounding box, correct
  correct_hdf <- if (length(grep("(degree)|(Arc Second)",
                                 gdalinfo_hdf_resunit)) &
                     all(gdalinfo_bbox == c(-0.05, -0.025, 0.05, 0.025))) {
    TRUE
  } else {
    FALSE
  }

  #  --------------------------------------------------------#
  # Convert to output projection, extent and format using ####
  # gdalwarp (mosaic if necessary)

  if (outproj_str != mod_proj_str) {
    mess_text <- paste("Processing and Reprojecting",
                       sens_sel, bandname,
                       "files for date:", date_name)
  } else {
    mess_text <- paste("Processing", sens_sel, bandname,
                       "files for date:", date_name)
  }

  process_message(mess_text, gui, mess_lab, verbose)

  # filename of temporary vrt file
  outfile_vrt <- tempfile(fileext = ".vrt")

  # workaround to avoid gdalbuildvrt bug in creation of vrt
  # files for UInt32 data type -
  # create tiff files from the original hdfs, and
  # then use those to build the vrt, instead than directly
  # buildibng the vrt from HDFS

  if (datatype == "UInt32") {
    files_out   <- NULL
    for (file in seq_along(files_in)) {
      file_out <- tempfile(fileext = ".tif")
      gdalUtils::gdal_translate(files_in[file],
                                file_out,
                                sd_index  = band,
                                srcnodata = nodata_in,
                                vrtnodata = nodata_out,
                                overwrite = TRUE)
      files_in[file] <- file_out
    }

    outfile_vrt <- paste0(stringr::str_sub(outfile_vrt, 1, -5),
                          ".tif")
    gdalUtils::gdalwarp(files_in,
                        outfile_vrt,
                        sd        = band,
                        srcnodata = nodata_in,
                        vrtnodata = nodata_out,
                        multi     = TRUE,
                        wo        = paste0("NUM_THREADS=", ncores),
                        nomd      = TRUE,
                        overwrite = TRUE
    )

  } else {
    # Create a GDAL vrt file corresponding to the original
    # hdf4
    gdalUtils::gdalbuildvrt(files_in,
                            outfile_vrt,
                            sd = band,
                            srcnodata = nodata_in,
                            vrtnodata = nodata_out)
  }

  # apply the patch if an error in the original hdf4 file at
  # step 0 was detected
  if (correct_hdf) {
    outfile_vrt_or        <- outfile_vrt
    # filename of new temporary vrt file
    outfile_vrt           <- tempfile(fileext = ".vrt")
    outfile_vrt_cont      <- readLines(outfile_vrt_or)
    outfile_vrt_linegeom  <- grep("<GeoTransform>",
                                  outfile_vrt_cont)

    outfile_vrt_geom      <- as.numeric(unlist(strsplit(
      gsub("<GeoTransform>(.*)</GeoTransform>", "\\1",
           outfile_vrt_cont[outfile_vrt_linegeom]), ",")
    ))

    outfile_vrt_geom_corr <- outfile_vrt_geom * 3600
    outfile_vrt_cont[outfile_vrt_linegeom] <- paste(
      "<GeoTransform>",
      paste(outfile_vrt_geom_corr,
            collapse = ", "), "</GeoTransform>"
    )

    write(outfile_vrt_cont, outfile_vrt)
  }

  # If resize required,  convert bbox coordinates from t_srs
  # to modis_srs, to get the correct extent, then build a new
  # vrt file subsetting the previous vrt file

  if (full_ext == "Resized") {
    outfile_vrt_or <- outfile_vrt
    # filename of new temporary vrt file
    outfile_vrt    <- tempfile(fileext = ".vrt")
    # for resizing BEFORE reprojecting
    bbox_mod <- reproj_bbox(bbox, outproj_str, mod_proj_str,
                            enlarge = TRUE)
    # Create a resized and (if needed) mosaiced GDAL vrt file

    if (datatype == "UInt32") {
      # fix to avoid bug on gdalbuildvrt for UInt32 datasets;
      # create a tif instead than a vrt
      outfile_vrt <- paste0(str_sub(outfile_vrt, 1, -5), ".tif")
      gdalUtils::gdalwarp(outfile_vrt_or,
                          outfile_vrt,
                          te        = c(bbox_mod),
                          tap       = TRUE,
                          tr        = raster::res(raster::raster(outfile_vrt_or)), #nolint
                          sd        = band,
                          srcnodata = nodata_in,
                          vrtnodata = nodata_out,
                          ot        = datatype,
                          multi     = TRUE,
                          wo        = c("INIT_DEST = NO_DATA",
                                        paste0("NUM_THREADS=", ncores)),
                          nomd      = TRUE,
                          overwrite = TRUE)
    } else {
      gdalUtils::gdalbuildvrt(outfile_vrt_or,
                              outfile_vrt,
                              te        = c(bbox_mod),
                              tap       = TRUE,
                              tr        = raster::res(raster::raster(outfile_vrt_or)), #nolint
                              srcnodata = nodata_in,
                              vrtnodata = nodata_out,
                              sd        = band,
                              overwrite = TRUE)
    }
  }
  
  # If scale_factor="Yes", add a "step" before creating final
  # files (a temporary file is created in tempdir, then
  # later the scale and offset are applied to it and result
  # is saved in out_repfile)
  outrep_file_0 <- if (scale_val            == "Yes" &
                       !(scale_factor == 1     &
                         offset       == 0)) {
    tempfile(fileext = ifelse(out_format == "GTiff",
                              ".tif", ".dat"))
  } else {
    outrep_file
  }

  # Launch the spatial processing -
  # operations to be done depend on whether resize and/or
  # reprojection and/or resampling are necessary. Operations
  # are done on the temporary vrt file built above!

  # Identify which processing is needed
  reproj_type <- if (out_res_sel == "Native" &
                     outproj_str == mod_proj_str) {
    "GdalTranslate" #only save to new format and mosaic

  } else if (out_res_sel == "Resampled" &
             outproj_str == mod_proj_str) {
    "Resample1_Resize0" #Change of resolution

  } else if (out_res_sel == "Native"     &
             outproj_str != mod_proj_str &
             full_ext    == "Full Tiles Extent") {
    "Resample0_Resize0" #Change of projection

  } else if (out_res_sel == "Native"     &
             outproj_str != mod_proj_str &
             full_ext    == "Resized") {
    "Resample0_Resize1" #Change of projection and extent

  } else if (out_res_sel == "Resampled"  &
             outproj_str != mod_proj_str &
             full_ext    == "Full Tiles Extent") {
    "Resample1_Resize0" #Change of resolution and
    #projection

  } else if (out_res_sel == "Resampled"  &
             outproj_str != mod_proj_str &
             full_ext    == "Resized") {
    "Resample1_Resize1"  #Change of resolution,
    #projection and extent
  } else {
    "Error"
  }

  if (out_format == "GTiff") {
    switch(reproj_type,
           GdalTranslate = gdalUtils::gdal_translate(
             outfile_vrt,
             outrep_file_0,
             a_srs    = mod_proj_str,
             of       = out_format,
             ot       = datatype,
             a_nodata = nodata_out,
             co = paste("COMPRESS", compress, sep = "="),
             overwrite = TRUE
           ),
           Resample0_Resize0 = gdalUtils::gdalwarp(
             outfile_vrt, outrep_file_0,
             s_srs = mod_proj_str,
             t_srs = outproj_str,
             of    = out_format,
             r     = resampling,
             co    = paste("COMPRESS", compress, sep = "="),
             ot    = datatype,
             multi = TRUE,
             wo    = c("INIT_DEST = NO_DATA",
                       paste0("NUM_THREADS=", ncores)),
             nomd  = TRUE,
             overwrite = TRUE
           ),
           Resample0_Resize1 = gdalUtils::gdalwarp(
             outfile_vrt, outrep_file_0,
             s_srs  = mod_proj_str,
             t_srs  = outproj_str,
             of     = out_format,
             r      = resampling,
             te     = bbox,
             co     = paste("COMPRESS", compress, sep = "="),
             ot     = datatype,
             multi  = TRUE,
             wo     = c("INIT_DEST = NO_DATA",
                        paste0("NUM_THREADS=", ncores)),
             nomd   = TRUE,
             overwrite  = TRUE
           ),
           Resample1_Resize0 = gdalUtils::gdalwarp(
             outfile_vrt, outrep_file_0,
             s_srs  = mod_proj_str,
             t_srs  = outproj_str,
             of     = out_format,
             r      = resampling,
             tr     = rep(out_res, 2),
             co     = paste("COMPRESS", compress, sep = "="),
             ot     = datatype,
             multi  = TRUE,
             wo     = c("INIT_DEST = NO_DATA",
                        paste0("NUM_THREADS=", ncores)),
             nomd   = TRUE,
             overwrite = TRUE
           ),
           Resample1_Resize1 = gdalUtils::gdalwarp(
             outfile_vrt, outrep_file_0,
             s_srs     = mod_proj_str, t_srs = outproj_str,
             of        = out_format,
             r         = resampling,
             te        = bbox,
             tr        = rep(out_res, 2),
             co        = paste("COMPRESS", compress, sep = "="),
             ot        = datatype,
             multi     = TRUE,
             wo        = c("INIT_DEST = NO_DATA",
                           paste0("NUM_THREADS=", ncores)),
             nomd      = TRUE,
             overwrite = TRUE
           ),
           stop(
             "Internal error in out_res_sel, outproj_str or ",
             "full_ext. Aborting!"))
  } else {
    # on ENVI format, processing is identical, save for
    # not providing the "COMPRESSION" option to avoid
    # warnings
    switch(reproj_type,
           GdalTranslate = gdalUtils::gdal_translate(
             outfile_vrt,
             outrep_file_0,
             a_srs     = mod_proj_str,
             of        = out_format,
             ot        = datatype,
             a_nodata  = nodata_out,
             overwrite = TRUE
           ),
           Resample0_Resize0 = gdalUtils::gdalwarp(
             outfile_vrt,
             outrep_file_0,
             s_srs     = mod_proj_str,
             t_srs     = outproj_str,
             of        = out_format,
             r         = resampling,
             ot        = datatype,
             multi     = TRUE,
             wo        = c("INIT_DEST = NO_DATA",
                           paste0("NUM_THREADS=", ncores)),
             nomd      = TRUE,
             overwrite = TRUE
           ),
           Resample0_Resize1  = gdalUtils::gdalwarp(
             outfile_vrt,
             outrep_file_0,
             s_srs     = mod_proj_str,
             t_srs     = outproj_str,
             of        = out_format,
             r         = resampling,
             te        = bbox,
             ot        = datatype,
             multi     = TRUE,
             wo        = c("INIT_DEST = NO_DATA",
                           paste0("NUM_THREADS=", ncores)),
             nomd      = TRUE,
             overwrite = TRUE
           ),
           Resample1_Resize0  =  gdalUtils::gdalwarp(
             outfile_vrt,
             outrep_file_0,
             s_srs     = mod_proj_str,
             t_srs     = outproj_str,
             of        = out_format,
             r         = resampling,
             tr        = rep(out_res, 2),
             ot        = datatype,
             multi     = TRUE,
             wo        = c("INIT_DEST = NO_DATA",
                           paste0("NUM_THREADS=", ncores)),
             nomd      = TRUE,
             overwrite = TRUE
           ),
           Resample1_Resize1  =  gdalUtils::gdalwarp(
             outfile_vrt,
             outrep_file_0,
             s_srs     = mod_proj_str,
             t_srs     = outproj_str,
             of        = out_format,
             r         = resampling,
             te        = bbox,
             tr        = rep(out_res, 2),
             ot        = datatype,
             multi     = TRUE,
             wo        = c("INIT_DEST = NO_DATA",
                           paste0("NUM_THREADS=", ncores)),
             nomd      = TRUE,
             overwrite = TRUE
           ),
           stop("Internal error in out_res_sel, outproj_str ",
                "or full_ext. Aborting!"))
  }

  # If scale_factor="Yes", create final files by rescaling
  # values
  if (scale_val == "Yes"   &
      !(scale_factor == 1 & offset == 0)) {

    outrep_0 <- raster::raster(outrep_file_0)
    scl    <- as.numeric(scale_factor)
    off    <- as.numeric(offset)
    na     <- as.numeric(nodata_out)
    outrep <- raster::calc(x   = outrep_0,
                           fun = function(x) {
                             x * scl + off
                           }
                           , filename  = outrep_file,
                           format    = out_format,
                           datatype  = "FLT4S",
                           options   = ifelse(
                             out_format == "GTiff",
                             paste0("COMPRESS=", compress),
                             ""),
                           NAflag    = na,
                           overwrite = TRUE)
    rm(outrep, outrep_0)
  }

  # If output format is ENVI, add data ignore value to the
  # header file
  if (out_format == "ENVI") {
    fileConn_meta_hdr <- file(paste0(
      tools::file_path_sans_ext(outrep_file), ".hdr"), "a")
    writeLines(c("data ignore value = ",
                 nodata_out ),
               fileConn_meta_hdr, sep = " ")
    writeLines("", fileConn_meta_hdr)
    close(fileConn_meta_hdr)
  }
}
