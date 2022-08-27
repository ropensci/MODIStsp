#' @title check_proc_opts
#' @description helper function used to check consistency of processing
#' parameters
#' @param proc_opts data frame of parameters passed by `MODIStsp`
#' @return NULL - processing interrupted if any condition is not met
#' @importFrom assertthat is.dir see_if is.string is.number
#' @keywords internal
#'
check_proc_opts <- function(proc_opts) {

  o <- proc_opts
  assertthat::is.dir(o$out_folder)

  assertthat::is.dir(o$out_folder_mod)

  prod_opts <- load_prodopts()
  assertthat::see_if(o$selprod %in% names(prod_opts),
                     msg = "`selprod` is not a valid MODIS product name!")

  prod_layers <- MODIStsp_get_prodlayers(o$selprod)
  assertthat::see_if(all(o$bandsel %in% prod_layers$bandnames),
                     msg = "Some of `bandsel` are not a valid bandname of the selected MODIS product!")

  assertthat::see_if(all(o$indexes_bandsel %in% prod_layers$indexes_bandnames),
                     msg = "Some of `indexes_bandsel` are not valid index names of the selected MODIS product!")

  assertthat::see_if(all(o$quality_bandsel %in% prod_layers$quality_bandnames),
                     msg = "Some of `quality_bandsel` are not valid quality names of the selected MODIS product!")

  assertthat::see_if(o$sensor %in% c("Terra", "Aqua", "Both"),
                     msg = "`sensor` must be 'Terra', 'Aqua' or 'Both'")

  assertthat::see_if(o$download_server %in% c("http", "offline"),
                     msg = "`download_server` must be 'http' or 'offline'")

  assertthat::see_if(o$downloader %in% c("http", "aria2"),
                     msg = "`download_server` must be 'http' or 'aria2'")

  assertthat::see_if(assertthat::is.string(o$user))
  assertthat::see_if(assertthat::is.string(o$password))

  assertthat::see_if(o$download_range %in% c("Full", "Seasonal"),
                     msg = "`download_range` must be 'Full' or 'Seasonal'")

  assertthat::see_if(!is.na(as.Date(o$start_date, format = "%Y.%m.%d")),
                     msg = "`start_date is not a date in format yyyy.mm.dd")
  assertthat::see_if(!is.na(as.Date(o$end_date, format = "%Y.%m.%d")),
                     msg = "`end_date is not a date in format yyyy.mm.dd")
  assertthat::see_if((as.Date(o$start_date, format = "%Y.%m.%d") <= as.Date(o$end_date, format = "%Y.%m.%d")),
                     msg = "`start_date` must be lower than `end_date`")

  assertthat::see_if(o$spatmeth %in% c("tiles", "bbox", "File", "map"),
                     msg = "`spatmeth` must be 'tiles', 'bbox' or 'File'")

  assertthat::see_if(assertthat::is.number(o$start_x))
  assertthat::see_if(assertthat::is.number(o$start_y))
  assertthat::see_if(assertthat::is.number(o$end_x))
  assertthat::see_if(assertthat::is.number(o$end_y))

  if (o$spatmeth == "bbox") {
    assertthat::see_if(all(is.numeric(o$bbox)))
    assertthat::see_if(o$bbox[3] > o$bbox[1],
                       msg = "Incorrect bbox")
    assertthat::see_if(o$bbox[4] > o$bbox[2],
                       msg = "Incorrect bbox")
  }

  if (o$spatmeth == "File") {
    assertthat::see_if(!is.null(o$spafile),
                       msg = "`spafile` must be specified if `spatmeth` is \"File\"")
    assertthat::see_if(file.exists(o$spafile),
                       msg = "`spafile` is not a valid path to a spatial")
  }

  assertthat::see_if(o$out_projsel %in% c("Native", "User Defined"),
                     msg = "`out_projsel` must be 'Native' or 'User Defined'")

  assertthat::see_if(assertthat::is.string(o$output_proj))

  if (!o$out_projsel == "Native") {
    if (o$output_proj == "MODIS Sinusoidal") {
      checkproj = TRUE
    } else {
      checkproj <- check_projection(o$output_proj)
    }
    assertthat::see_if(checkproj == TRUE,
                       msg = "`output_proj` must be 'MODIS Sinusoidal' or a valid
                     EPSG code or WKT string")
  }

  assertthat::see_if(o$out_res_sel %in% c("Native", "User Defined"),
                     msg = "`out_res_sel` must be 'Native' or 'User Defined'")

  assertthat::see_if(assertthat::is.number(o$out_res))

  assertthat::see_if(o$resampling %in% c("near", "bilinear", "cubic",
                                         "cubicspline", "lanczos", "average",
                                         "mode", "max", "min",
                                         "q1", "q3", "sum"),
                     msg = "`resampling` must one of \"near\", \"bilinear\", \"cubic\",
                                         \"cubicspline\", \"lanczos\", \"average\",
                                         \"mode\", \"max\", \"min\",
                                         \"q1\", \"q3\", \"sum\"")

  assertthat::see_if(is.logical(o$reprocess))
  assertthat::see_if(is.logical(o$delete_hdf))
  assertthat::see_if(is.logical(o$nodata_change))
  assertthat::see_if(is.logical(o$scale_val))

  assertthat::see_if(all(o$ts_format %in% c("R RasterStack", "ENVI Meta Files", "GDAL VRT")),
                     msg = "`ts_format` must be (one or more of) 'R RasterStack', 'ENVI Meta Files' or 'GDAL VRT'")

  assertthat::see_if(o$compress %in% c("None", "PACKBITS", "LZW", "DEFLATE"),
                     msg = "`spatmeth` must be 'None', 'PACKBITS', 'LZW' or 'DEFLATE'")


}
