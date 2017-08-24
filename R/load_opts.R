#' @title Load MODIStsp processing options
#' @description Load MODIStsp processing option from `previous_jsfile` if
#'  it already exist, otherwise initialise processing options to default and
#'  save them to `previous_jsfile`. Send warnings if options file is from an
#'  old version
#' @param previous_jsfile Expected file name of the JSON file containing
#'  processing options
#' @param mod_proj_str poj4string defining MODIS standard sinusoidal projection
#' @return `data frame` general_opts, containing the processing options
#'   retrieved from the JSON file (or the defaults set at first execution).
#'   See also `MODIStsp_GUI` and `MODIStsp_process`
#' @rdname load_opts
#' @author Lorenzo Busetto, phD (2014-2015) \email{busetto.l@@irea.cnr.it}
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom RJSONIO fromJSON toJSON
#' @importFrom utils packageVersion
load_opts <- function(previous_jsfile, mod_proj_str) {
  if (file.exists(previous_jsfile)) {
    general_opts <- RJSONIO::fromJSON(previous_jsfile)
    if (is.null(general_opts$MODIStspVersion)) {
      stop("The option file in use (", previous_jsfile, ") was created with a ",
           "too old MODIStsp version (<=1.2.2).\n It can not be used with the ",
           "current version.\n",
           " Please delete it or specify a different value for the ",
           "`options_file` argument.")
    } else {
      if (general_opts$MODIStspVersion < utils::packageVersion("MODIStsp")) {
        message(
          "The option file in use (", previous_jsfile, ") was created ",
          "with an old MODIStsp version (", general_opts$MODIStspVersion, ")",
          "\n Although this should still work, please consider reopening",
          "the JSON file from the GUI\n and resaving it."
        )
      }
    }
  } else {
    # If previous file not found (i.e., at first execution), create the
    # "general_opts" structure used to  communicate with the GUI, set default
    # values and save it as a JSON file

    general_opts <- list(
      sel_prod        = "Surf_Ref_8Days_500m (M*D09A1)",
      sensor          = "Terra",
      prod_version    = "6",
      start_date      = strftime(Sys.Date(), "%Y-01-01"),
      end_date        = as.character(Sys.Date()),
      # lenghts refearred to "Surf_Ref_8Days_500m (M*D09A1)" v6! (used as
      # default product at first execution)
      bandsel         = rep(0, 13),
      indexes_bandsel = rep(0, 11),
      quality_bandsel = rep(0, 21),
      start_x         = 18,
      end_x           = 18,
      start_y         = 4,
      end_y           = 4,
      user            = "",
      password        = "",
      use_aria        = FALSE,
      download_server = "http",
      download_range  = "full",
      proj            = "Sinusoidal",
      user_proj4      = mod_proj_str,
      out_res_sel     = "Native",
      out_res         = "",
      full_ext        = "Full Tiles Extent",
      resampling      = "near",
      out_format      = "ENVI",
      ts_format       = "ENVI Meta Files",
      rts             = "Yes",
      compress        = "None",
      nodata_change   = "No",
      scale_val       = "No",
      delete_hdf      = "No",
      reprocess       = "No",
      bbox            = c("", "", "", ""),
      out_folder      = "",
      out_folder_mod  = "",
      MODIStspVersion = as.character(utils::packageVersion("MODIStsp")),
      custom_indexes  = list()
    )
    write(RJSONIO::toJSON(general_opts), previous_jsfile)
  }
  return(general_opts)
}