#' @title Load MODIStsp processing options from a JSON file
#' @description Load MODIStsp processing option from `opts_jsfile` if
#'  it already exist, otherwise initialize processing options to default and
#'  save them to `opts_jsfile` (typically done at first execution, or if the
#'  MODIStsp_previous.json is deleted). Sends warnings if options file is from an
#'  old version. Aborts if the json file is not a valid MODIStsp options file
#' @param opts_jsfile Expected file name of the JSON file containing
#'  processing options
#' @return `data frame` general_opts, containing the processing options
#'   retrieved from the JSON file (or the defaults set at first execution).
#'   See also `MODIStsp_GUI` and `MODIStsp_process`
#' @rdname load_opts
#' @author Lorenzo Busetto, phD (2014-2017) \email{lbusett@@gmail.com}
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @importFrom jsonlite fromJSON write_json
#' @importFrom utils packageVersion
load_opts <- function(opts_jsfile) {
  
  if (file.exists(opts_jsfile)) {
    
    general_opts <- try(jsonlite::fromJSON(opts_jsfile), silent = TRUE)
    
    # stop on errors
    if (class(general_opts) == "try-error" ) {
      stop(strwrap("Unable to read the provided JSON options file. Please check
                   your inputs!"))
    }
    
    if (!("MODIStspVersion" %in% names(general_opts))) {
      stop(
        strwrap("The specified json file does not appear to be a valid MODIStsp
           options file. Please check your inputs!")
      )
    }
    
    if (is.null(general_opts$MODIStspVersion)) {
      stop("The option file in use (", opts_jsfile, ") was created with a ",
           "too old MODIStsp version (<=1.2.2).\n It can not be used with the ",
           "current version.\n",
           " Please delete it or specify a different value for the ",
           "`options_file` argument.")
    } else {
      if (general_opts$MODIStspVersion < utils::packageVersion("MODIStsp")) {
        message(
          "The option file in use (", opts_jsfile, ") was created ",
          "with an old MODIStsp version (", general_opts$MODIStspVersion, ")",
          "\n Although this should still work, please consider reopening ",
          "the JSON file from the GUI\n and resaving it."
        )
      }
    }
  } else {
    # If previous file not found (i.e., at first execution), create the
    # "general_opts" structure used to  communicate with the GUI, set default
    # values and save it as a JSON file

    
    #nocov start (this is only executed at first ever execution)
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
      user_proj4      = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs", #nolint
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
    jsonlite::write_json(general_opts, opts_jsfile, pretty = TRUE,
                         auto_unbox = TRUE)
    #nocov end (this is only executed at first ever execution)
  }
  return(general_opts)
}
