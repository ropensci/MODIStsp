#' @title shinygui_saveopts
#' @description Helper function used to retrieve options from shiny widgets
#' @noRd

# retrieve inputs from shiny widgets ----
shinygui_saveopts <- function(input, prod_opt_list, rv) {
  #nocov start
  opts <- list()
  opts$selcat  <- input$selcat
  opts$selprod <- input$selprod
  # retrieve product options
  opts$prod_version <- "6"

  opts$sensor  <- input$selplat
  opts$bandsel <- if(is.null(input$sel_layers)) {
    NA
  } else {
    input$sel_layers
  }
  opts$quality_bandsel <- if(is.null(input$sel_qual)) {
    NA
  } else {
    input$sel_qual
  }

  opts$indexes_bandsel <- if(is.null(input$sel_ind)) {
    NA
  } else {
    input$sel_ind
  }

  if (all(is.na(c(opts$bandsel, opts$quality_bandsel ,opts$indexes_bandsel)))) {
    return("Please select at least one product layer, quality layer or spectral index!")
  }
  # Retrieve download method and authentication ----
  opts$download_server <- input$down_meth
  opts$user            <- input$user
  opts$password        <- input$password

  if (opts$download_server == "http" & any((c(opts$user, opts$password) == ""))) {
    return("Please provide your earthdata username and password!")
  }
  opts$downloader      <- input$downloader
  opts$download_range  <- input$download_range
  #
  # # Retrieve dates ----
  opts$start_date <- gsub("-", ".", input$proc_dates[1])
  opts$end_date   <- gsub("-", ".", input$proc_dates[2])
  opts$spatmeth  <- input$spatmeth

  # # Retrieve Tiles options ----
  if (input$spatmeth == "tiles") {
    opts$start_x <- input$xmintile
    opts$end_x   <- input$xmaxtile
    opts$start_y <- input$ymintile
    opts$end_y   <- input$ymaxtile
    opts$bbox    <- c(NA, NA, NA, NA)
  } else {
    tiles <- tiles_from_bbox(rv$outbbox, input$outprojtxt)
    opts$start_x <- tiles["xmintile"]
    opts$end_x   <- tiles["xmaxtile"]
    opts$start_y <- tiles["ymintile"]
    opts$end_y   <- tiles["ymaxtile"]
    opts$bbox    <- as.numeric(rv$outbbox)
  }

  # Retrieve spatial file ----
  if (input$spatmeth == "file") {
    opts$spafile <- input$spafiletxt
  } else {
    opts$spafile <- NA
  }

  # Retrieve drawn extent ----
  if (input$spatmeth == "map") {
    opts$drawnext <- geojsonio::geojson_json(
      st_transform(rv$drawn_extent, 4326),
      pretty = TRUE
    )
  } else {
    opts$drawnext <- NA
  }

  # # Retrieve projection options ----
  opts$out_projsel     <- input$out_projsel
  opts$output_proj <- input$outprojtxt
  opts$out_res_sel <- input$outressel
  opts$out_res     <- input$outres
  opts$resampling  <- input$resampmeth

  # # Retrieve reprocess, delete and nodata ----
  opts$reprocess     <- ifelse(input$reprocess  == "No", FALSE, TRUE)
  opts$delete_hdf    <- ifelse(input$delete_hdf == "No", FALSE, TRUE)
  opts$nodata_change <- ifelse(input$nodata    == "No", FALSE, TRUE)
  opts$scale_val     <- ifelse(input$scaleoff  == "No", FALSE, TRUE)

  # Retrieve format, virtual and compression

  opts$out_format <- input$out_format
  opts$ts_format  <- input$time_series
  opts$compress   <- input$compress

  # Retrieve Folder options
  opts$out_folder     <- input$out_folder_txt
  opts$out_folder_mod <- input$out_hdffolder_txt

  if (any(c(opts$out_folder, opts$out_folder_mod) == "")) {
    return("Please provide output folder names")
  }

  if (opts$out_folder == "$tempdir"){
    opts$out_folder <- file.path(tempdir(), "MODIStsp/HDF")
    dir.create(opts$out_folder)
  }

  if (opts$out_folder == "$tempdir"){
    opts$out_folder_mod <- file.path(tempdir(), "MODIStsp/HDF")
    dir.create(dirname(opts$out_folder_mod))
    dir.create(opts$out_folder_mod)
  }

  # #- Perform checks on options consistency ---------------

  #
  # # check that the selected product is available on the selected server
  # #

  http <- prod_opt_list[[opts$selprod]][["6"]]$http
  #
  if (opts$sensor == "Both") {
    http <- c(http["Terra"][[1]], http["Aqua"][[1]])
  } else {
    if (opts$sensor == "Combined") {
      http <- http[[1]]
    } else {
      http <- http[opts$sensor][[1]]
      if (opts$download_server == "http" &
          http == "Not Available" &
          opts$sensor == "Aqua") {
        return("The selected product is only available for the Terra sensor. Please switch sensor!")
      }
    }
  }
  opts$MODIStspVersion <- as.character(packageVersion("MODIStsp"))
  return(opts)
  #nocov end
}
