#' @title MODIStsp helper for processing original HDF layers
#' @description Internal function used to perform the required spatial
#'  processing on MODIS original hdf layers (reprojection, resizing, resampling,
#'  mosaicing, computation of scaling factors). The function is based on the
#'  use of `gdal` routines.
#' @inheritParams MODIStsp
#' @param modislist `character array` List of MODIS images to be downloaded for
#'  the selected date (as returned from `get_mod_filenames`). Can be a single
#'  image, or a list of images in case different tiles are needed!
#' @param outproj_str `character` EPSG or WKT of output projection.
#' @param mod_proj_str `character` EPSG or WKT of MODIS projection.
#' @param sens_sel `character ["terra" | "aqua"]` Selected sensor.
#' @param band `numeric` band number corresponding to the HDF layer to be
#'  processed
#' @param bandname `character` Name of the HDF layer to be processed.
#' @param date_name `character` Date of acquisition of the images to be
#'  downloaded.
#' @param datatype `character` Datatype to the HDF layer to be processed.
#' @param nodata_in `numeric` Original nodata value to the HDF layer to be
#'  processed.
#' @param nodata_out `numeric` Output nodata value to the HDF layer to be
#'  processed.
#' @param full_ext `logical` If TRUE, process full tiles, if FALSE, process
#'  bbox
#' @param scale_factor `numeric` Scale factor to be applied to the HDF layer
#'  to be processed (Ignored if `scale_val` == FALSE).
#' @param offset `numeric` Offset to be applied to the HDF layer
#'  to be processed (Ignored if `scale_val` == FALSE).
#' @param outrep_file `character` Full path of the file where results of the
#'  processing are to be stored (created in `MODIStsp_process`)
#' @param verbose `logical` If FALSE, suppress processing messages, Default: TRUE
#' @return The function is called for its side effects
#' @rdname MODIStsp_process_bands
#' @author Lorenzo Busetto, phD (2014-2017) \email{lbusett@@gmail.com}
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @importFrom parallel detectCores
#' @importFrom sf gdal_utils gdal_subdatasets sf_extSoftVersion
#' @importFrom stats na.omit
#' @importFrom gdalUtilities gdal_translate gdalwarp gdalbuildvrt
#' @importFrom stringr str_sub
#' @importFrom raster raster res reclassify calc
#' @importFrom tools file_ext file_path_sans_ext
#'
MODIStsp_process_bands <- function(out_folder_mod, modislist,
                                   outproj_str, mod_proj_str, sens_sel,
                                   band, bandname, date_name, datatype,
                                   nodata_in, nodata_out,
                                   full_ext, bbox,
                                   scale_val, scale_factor, offset,
                                   out_format, outrep_file, compress,
                                   out_res_sel, out_res, resampling,
                                   nodata_change,
                                   gui, verbose) {

  tmpdir <- file.path(tempdir(), "mstp_temp")
  dir.create(tmpdir, showWarnings = FALSE)

  files_in <- file.path(out_folder_mod, modislist)

  #   __________________________________________________________________________
  #   Initialize number of cores for gdalwarp (equal to ncpus - 2 OR 8 if  ####
  #   number of available cpus > 8 (this to avoid overloading servers with high
  #   number of cpus)

  ncores <- min(c(8, parallel::detectCores() - 2))

  # check to see if the patch to correct wrong resolution/bbox in some HDF4 ----
  # original layers (e.g. albedo) is needed

  # Retrieve information from hdf4 with gdalinfo

  gdalinfo_raw <- suppressWarnings(suppressMessages(try(
    trimws(unlist(strsplit(sf::gdal_utils("info", sf::gdal_subdatasets(files_in[1])[band][[1]], quiet = TRUE), "\n"))),
    silent = TRUE
  )))

  gdalinfo_hdf_resunit  <- gsub(
    "^ *NADIRDATARESOLUTION=[0-9.]+ ?", "",
    gdalinfo_raw[grep("^ *NADIRDATARESOLUTION",
                      gdalinfo_raw)]
  )

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

  process_message(mess_text, verbose)

  # filename of temporary vrt file

  outfile_vrt <- tempfile(fileext = ".vrt", tmpdir = tmpdir)

  # workaround to avoid gdalbuildvrt bug in creation of vrt
  # files for UInt32 data type -
  # create tiff files from the original hdfs, and
  # then use those to build the vrt, instead than directly
  # buildibng the vrt from HDFS

  if (datatype == "UInt32") {
    for (file in seq_along(files_in)) {
      file_out <- tempfile(fileext = ".tif", tmpdir = tmpdir)
      gdalUtilities::gdal_translate(sf::gdal_subdatasets(files_in[file])[band][[1]],
                                    file_out)
      files_in[file] <- file_out
    }

    outfile_vrt <- paste0(stringr::str_sub(outfile_vrt, 1, -5),
                          ".tif")

    if (length(split_nodata_values(nodata_in)[[1]] == 1)) {

      gdalUtilities::gdalwarp(files_in,
                              outfile_vrt,
                              multi     = TRUE,
                              wo        = paste0("NUM_THREADS=", ncores),
                              nomd      = TRUE,
                              overwrite = TRUE,
                              srcnodata = nodata_in,
                              dstnodata = nodata_out
      )
    } else {
      # If multiple NODATA, do not change the nodata value in the VRT, because
      # it is dealt with later
      gdalUtilities::gdalwarp(files_in,
                              outfile_vrt,
                              multi     = TRUE,
                              wo        = paste0("NUM_THREADS=", ncores),
                              nomd      = TRUE,
                              overwrite = TRUE
      )
    }
  } else {

    # Create a GDAL vrt file corresponding to the original
    # hdf4

    if (length(split_nodata_values(nodata_in)[[1]]) == 1) {

      gdalUtilities::gdalbuildvrt(files_in,
                                  sd = band,
                                  output.vrt = outfile_vrt,
                                  srcnodata = nodata_in,
                                  vrtnodata = nodata_out,
                                  a_srs = mod_proj_str)
    } else {
      # If multiple NODATA, do not change the nodata value in the VRT, because
      # it is dealt with later

      gdalUtilities::gdalbuildvrt(files_in,
                                  outfile_vrt,
                                  sd = band,
                                  a_srs = mod_proj_str,
                                  dryrun = F)

    }

  }

  # if !nodata_change and multiple nodata, set nodata values to dummy,
  # to allow later also to correct in case scale is applied
  if (length(split_nodata_values(nodata_in)[[1]]) > 1 & !nodata_change) {
    nodata_in_tmp <- nodata_in
    nodata_in <- nodata_out <- 99999
  }

  # apply the patch if an error in the original hdf4 file at
  # step 0 was detected (and only if outfile_vrt is still a vrt)
  # TODO check if input file with geographic coordinates and type "UInt32"
  # are correctly generated.
  if (correct_hdf & gsub("^.+\\.(.+)$", "\\1", outfile_vrt) == "vrt") {

    outfile_vrt_or        <- outfile_vrt
    # filename of new temporary vrt file
    outfile_vrt           <- tempfile(fileext = ".vrt", tmpdir = tmpdir)
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

  # Fix needed to avoid that scale and offset are automatically "applied"
  # when working with GDAL > 2.3.x
  # workaround on gdal >= 2.3

  gdal_ver <- sf::sf_extSoftVersion()[["GDAL"]]
  gdal_ver <- as.numeric(substring(gdal_ver, 1,3))

  if (gdal_ver >= 2.3 & tools::file_ext(outfile_vrt) == "vrt") {
    vrt_in      <- readLines(outfile_vrt)

    scale_line <- grep("Scale", vrt_in)
    if (length(scale_line) != 0) {
      vrt_in[scale_line] <- "<Scale>1</Scale>"
    }

    offset_line <- grep("Offset", vrt_in)
    if (length(offset_line) != 0) {
      vrt_in[offset_line] <- "<Offset>0</Offset>"
    }

    writeLines(vrt_in, outfile_vrt)
  }



  if (length(split_nodata_values(nodata_in)[[1]]) > 1) {

    if (nodata_change) {
      # if there are more than one nodata_in values, and nodata_change is on
      # reclassify the input raster to use only one value

      outfile_vrt_prev <- outfile_vrt
      outfile_vrt <- paste0(stringr::str_sub(outfile_vrt, 1, -5),"_recl.tif")

      # if full_ext == "Resized", clip before reclassifying (to speed up)
      if (full_ext == FALSE) {
        outfile_vrt_prev2 <- outfile_vrt_prev
        outfile_vrt_prev <- paste0(stringr::str_sub(outfile_vrt, 1, -5),".vrt")

        outfile_prev_bbox <- do.call(
          function(x,y) {
            c(max(x[1],y[1]), max(x[2],y[2]), min(x[3],y[3]), min(x[4],y[4]))
          },
          list(
            reproj_bbox(bbox, outproj_str, mod_proj_str, enlarge = TRUE),
            raster::bbox(suppressWarnings(raster::raster(outfile_vrt_prev2)))
          )
        )
        gdalUtilities::gdalbuildvrt(
          outfile_vrt_prev2,
          outfile_vrt_prev,
          te = outfile_prev_bbox,
          tap = TRUE,
          tr = raster::res(suppressWarnings(raster::raster(outfile_vrt_prev2))), #nolint
          overwrite = TRUE
        )
      }

      # Ugly workaround to avoid crash on Windows - I do not understand why, but
      # using vrts fails in this case so we have to save a temporary tif
      if (Sys.info()['sysname'] == "Windows") {
        temptif <- tempfile(fileext = ".tif", tmpdir = tmpdir)
        gdalUtilities::gdal_translate(outfile_vrt_prev, temptif)
        outfile_vrt_prev <- temptif
      }

      recl_in  <- suppressWarnings(raster::stack(outfile_vrt_prev))
      recl_out <- suppressWarnings(raster::reclassify(
        recl_in,
        rcl = create_nodata_rcl(nodata_in, nodata_out)[[1]],
        filename = outfile_vrt,
        right = NA,
        NAflag = as.numeric(nodata_out)
      ))
      # this because nodata value has already been changed;
      # in this way, next gdal commands with srcnodata and dstnodata do not
      # change it again (providing error, since nodata_in is a string)
      nodata_in <- nodata_out
    }
  }

  # If resize required,  convert bbox coordinates from t_srs
  # to modis_srs, to get the correct extent, then build a new
  # vrt file subsetting the previous vrt file

  if (full_ext == FALSE) {

    outfile_vrt_or <- outfile_vrt
    # filename of new temporary vrt file
    outfile_vrt    <- tempfile(fileext = ".vrt", tmpdir = tmpdir)
    # for resizing BEFORE reprojecting
    bbox_mod <- reproj_bbox(bbox,
                            outproj_str,
                            mod_proj_str,
                            enlarge = TRUE)
    # Create a resized and (if needed) mosaiced GDAL vrt file

    if (datatype == "UInt32") {
      # fix to avoid bug on gdalbuildvrt for UInt32 datasets;
      # create a tif instead than a vrt
      outfile_vrt <- paste0(str_sub(outfile_vrt, 1, -5), ".tif")
      gdalUtilities::gdalwarp(outfile_vrt_or,
                              outfile_vrt,
                              te        = c(bbox_mod),
                              tap       = TRUE,
                              tr        = raster::res(suppressWarnings(raster::raster(outfile_vrt_or))), #nolint
                              # sd        = band,
                              srcnodata = nodata_in,
                              dstnodata = nodata_out,
                              ot        = as.character(datatype),
                              multi     = TRUE,
                              wo        = c("INIT_DEST = NO_DATA",
                                            paste0("NUM_THREADS=", ncores)),
                              nomd      = TRUE,
                              overwrite = TRUE)
    } else {

      gdalUtilities::gdalbuildvrt(outfile_vrt_or,
                                  outfile_vrt,
                                  te        = c(bbox_mod),
                                  tap       = TRUE,
                                  tr        = raster::res(suppressWarnings(raster::raster(outfile_vrt_or))), #nolint
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
  outrep_file_0 <- if (scale_val &
                       !(scale_factor == 1     &
                         offset       == 0)) {
    tempfile(fileext = ifelse(out_format == "GTiff",
                              ".tif", ".dat"), tmpdir = tmpdir)
  } else {
    outrep_file
  }


  # Launch the spatial processing -
  # operations to be done depend on whether resize and/or
  # reprojection and/or resampling are necessary. Operations
  # are done on the temporary vrt file built above!

  # Identify which processing is needed
  reproj_type <- if (out_res_sel == "Native" &
                     outproj_str$wkt  == mod_proj_str$wkt) {
    "GdalTranslate" #only save to new format and mosaic

  } else if (out_res_sel == "Resampled" &
             outproj_str$wkt == mod_proj_str$wkt) {
    "Resample1_Resize0" #Change of resolution

  } else if (out_res_sel == "Native"     &
             outproj_str$wkt != mod_proj_str$wkt &
             full_ext    == TRUE) {
    "Resample0_Resize0" #Change of projection

  } else if (out_res_sel == "Native"     &
             outproj_str$wkt != mod_proj_str$wkt &
             full_ext    == FALSE) {
    "Resample0_Resize1" #Change of projection and extent

  } else if (out_res_sel == "Resampled"  &
             outproj_str$wkt != mod_proj_str$wkt &
             full_ext    == TRUE) {
    "Resample1_Resize0" #Change of resolution and
    #projection

  } else if (out_res_sel == "Resampled"  &
             outproj_str$wkt != mod_proj_str$wkt &
             full_ext    == FALSE) {
    "Resample1_Resize1"  #Change of resolution,
    #projection and extent
  } else {
    "Error"
  }

  if (out_format == "GTiff") {

    switch(reproj_type,
           GdalTranslate = gdalUtilities::gdal_translate(
             outfile_vrt,
             outrep_file_0,
             a_srs    = mod_proj_str,
             of       = out_format,
             ot       = as.character(datatype),
             a_nodata = nodata_out,
             co = paste("COMPRESS", compress, sep = "=")
             # ,
             #   verbose = FALSE
           ),
           Resample0_Resize0 = gdalUtilities::gdalwarp(
             outfile_vrt, outrep_file_0,
             s_srs = mod_proj_str,
             t_srs = outproj_str,
             of    = out_format,
             r     = resampling,
             co    = paste("COMPRESS", compress, sep = "="),
             ot    = as.character(datatype),
             multi = TRUE,
             wo    = c("INIT_DEST = NO_DATA",
                       paste0("NUM_THREADS=", ncores)),
             nomd  = TRUE,
             overwrite = TRUE
           ),
           Resample0_Resize1 = gdalUtilities::gdalwarp(
             outfile_vrt, outrep_file_0,
             s_srs  = mod_proj_str,
             t_srs  = outproj_str,
             of     = out_format,
             r      = resampling,
             te     = bbox,
             co     = paste("COMPRESS", compress, sep = "="),
             ot     = as.character(datatype),
             multi  = TRUE,
             wo     = c("INIT_DEST = NO_DATA",
                        paste0("NUM_THREADS=", ncores)),
             nomd   = TRUE,
             overwrite  = TRUE
           ),
           Resample1_Resize0 = gdalUtilities::gdalwarp(
             outfile_vrt, outrep_file_0,
             s_srs  = mod_proj_str,
             t_srs  = outproj_str,
             of     = out_format,
             r      = resampling,
             tr     = rep(out_res, 2),
             co     = paste("COMPRESS", compress, sep = "="),
             ot     = as.character(datatype),
             multi  = TRUE,
             wo     = c("INIT_DEST = NO_DATA",
                        paste0("NUM_THREADS=", ncores)),
             nomd   = TRUE,
             overwrite = TRUE
           ),
           Resample1_Resize1 = gdalUtilities::gdalwarp(
             outfile_vrt, outrep_file_0,
             s_srs     = mod_proj_str, t_srs = outproj_str,
             of        = out_format,
             r         = resampling,
             te        = bbox,
             tr        = rep(out_res, 2),
             co        = paste("COMPRESS", compress, sep = "="),
             ot        = as.character(datatype),
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
           GdalTranslate = gdalUtilities::gdal_translate(
             outfile_vrt,
             outrep_file_0,
             a_srs     = mod_proj_str,
             of        = out_format,
             ot        = as.character(datatype),
             a_nodata  = nodata_out
           ),
           Resample0_Resize0 = gdalUtilities::gdalwarp(
             outfile_vrt,
             outrep_file_0,
             s_srs     = mod_proj_str,
             t_srs     = outproj_str,
             of        = out_format,
             r         = resampling,
             ot        = as.character(datatype),
             multi     = TRUE,
             nomd      = TRUE,
             overwrite = TRUE
           ),
           Resample0_Resize1  = gdalUtilities::gdalwarp(
             outfile_vrt,
             outrep_file_0,
             s_srs     = mod_proj_str,
             t_srs     = outproj_str,
             of        = out_format,
             r         = resampling,
             te        = bbox,
             ot        = as.character(datatype),
             multi     = TRUE,
             nomd      = TRUE,
             overwrite = TRUE
           ),
           Resample1_Resize0  =  gdalUtilities::gdalwarp(
             outfile_vrt,
             outrep_file_0,
             s_srs     = mod_proj_str,
             t_srs     = outproj_str,
             of        = out_format,
             r         = resampling,
             tr        = rep(out_res, 2),
             ot        = as.character(datatype),
             multi     = TRUE,
             nomd      = TRUE,
             overwrite = TRUE
           ),
           Resample1_Resize1  =  gdalUtilities::gdalwarp(
             outfile_vrt,
             outrep_file_0,
             s_srs     = mod_proj_str,
             t_srs     = outproj_str,
             of        = out_format,
             r         = resampling,
             te        = bbox,
             tr        = rep(out_res, 2),
             ot        = as.character(datatype),
             multi     = TRUE,
             nomd      = TRUE,
             overwrite = TRUE
           ),
           stop("Internal error in out_res_sel, outproj_str ",
                "or full_ext. Aborting!"))
  }

  # If scale_factor="Yes", create final files by rescaling
  # values

  if (scale_val &
      !(scale_factor == 1 & offset == 0)) {

    outrep_0 <- suppressWarnings(raster::raster(outrep_file_0))
    scl      <- as.numeric(scale_factor)
    off      <- as.numeric(offset)
    na       <- as.numeric(nodata_out)

    outrep   <- suppressWarnings(raster::calc(x   = outrep_0,
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
                             overwrite = TRUE))

    # workasround to avoid applying scale offset to nodata values if nodata_change is
    # FALSE and multiple nodata are present
    if (nodata_in == 99999) {

      if (!nodata_change) {
        nodata_in_min <- split_nodata_values(nodata_in_tmp)[[1]][1]
        correct_nodata <- function(x) {
          which_nodata <- which(x >=  (nodata_in_min * scl) + off)
          if (length(which_nodata) != 0) {
            x[which_nodata] <- (x[which_nodata] / scl) - off
          }
          x
        }

        outrep   <- suppressWarnings(raster::calc(x   = outrep,
                                 fun = function(x) {
                                   correct_nodata(x)
                                 }
                                 , filename  = outrep_file,
                                 format    = out_format,
                                 datatype  = "FLT4S",
                                 options   = ifelse(
                                   out_format == "GTiff",
                                   paste0("COMPRESS=", compress),
                                   ""),
                                 NAflag    = na,
                                 overwrite = TRUE))
      }
    }

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

  unlink(tmpdir, recursive = TRUE)
}
