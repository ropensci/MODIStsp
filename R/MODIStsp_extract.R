#' @title Extract data from MODIStsp time series
#' @description
#' function used to extract time series data from rts files created by MODIStsp
#' on spatial locations provided in the form of "R" spatial objects (SpatialPoints,
#' SpatialPolygons, etc.)
#' @details
#' The function takes as input a RasterStack object containing time information
#' in the "z" attribute (set by `raster::setZ`), a starting and ending date
#' and a standard "R" spatial object, and returns the time series for the spatial locations
#' specified in the spatial object in the form of a "R" xts object OR a plain data.frame
#' with a "date" column in first position.
#' If the input spatial object is a "point" or "line" one, the  output object
#' contains one column for each specified point, or for each cell intersecting
#' the line, and one line for each date. If the input spatial object is a "polygon"
#' one, the output object contains one column for each polygon, containing values
#' obtained applying the function specified as the FUN argument over all pixels
#' belonging to the polygon, and one line for each date.
#'
#' @param in_rts A `RasterStack` bject created by MODIStsp
#'  (it MUST contain acquisition dates in the "Z" attribute)
#' @param sf_object "sf" object OR name of an GDAL-readable vector file specifying the
#'  "area" from which data has to be extracted.
#'  - If `sf_object` represents lines, the output object contains one column for
#'    each line, containing values obtained applying the function specified
#'    as the FUN argument over all pixels touched by the line, and one line for
#'     each date.
#'  - If `sf_object` represents points, the output object contains one column
#'    for each point, containing values of the cells corresponding to the point,
#'    and one line for each date.
#'  - If `sf_object` represents polygons, the output object contains one column
#'    for each polygon, containing values obtained applying the function
#'    specified as the FUN argument over all pixels belonging to the polygon,
#'    and one line for each date
#' @param start_date object of class `Date`, `POSIXct` or `POSIXlt` OR `character`
#'   coercible to Date class (format = "yyyy-mm-dd")Starting date of the period
#'   to be considered for data extraction . If not provided, the first date of
#'   the RasterStack is used.
#' @param end_date object of class `Date`, `POSIXct` or `POSIXlt` OR `character`
#'   coercible to Date class (format = "yyyy-mm-dd"). Ending date of the period
#'   to be considered for data extraction . If not provided, the last date of
#'   the RasterStack is used.
#' @param id_field `character` name of the column of the input sp object or
#'   shapefile to be used in the data extraction. Values contained in the column
#'   MUST be unique. The names of the columns of the output are taken from this
#'   column. If not provided, or an invalid value is provided, then the names
#'   of the columns of the output reflect the number of the feature in
#'   `sf_object`.
#' @param FUN function to summarize the values (e.g. mean) on polygon data frames.
#'  The function should take a single numeric vector as argument and return a
#'  single value (e.g. mean, min or max), and accept a na.rm argument. Thus,
#'  standard R functions not including an na.rm argument must  be wrapped as in
#'  this example: fun=function(x,...)length(x). Defaults to "mean"
#' @param out_format `character ["xts" | "dframe"]` If dframe, the output is a
#'  data frame with dates in the first column and extracted data in the others,
#'  otherwise it is a `xts` object, Default: "xts"
#' @param small `logical` If TRUE, and input is polygons, then values are
#'  returned also for polygons not covering at least one raster cell. "Included"
#' cells in this case depend on the values of the "small_method" parameter.
#' @param small_method `character ["centroids" | "full"]` If small == TRUE and
#'  input is polygons, controls which cells are "extracted" for small polygons.
#'  If set to "centroids" (default), then only the cells corresponding to polygon
#'  centroid are considered (faster, may have problems on strangely shaped
#'  polygons). If set to "full", then all cells intersected by the small polygon
#'  are extracted and used in calculations, Default: "centroids"
#' @param na.rm	`logical` If TRUE, and sf_object is a polygon, then na.rm = TRUE
#'  is used when applying FUN to the different pixels of the polygon, Default = TRUE.
#' @param verbose `logical` If TRUE, messages on processing status are sent
#'  to the console. Default = TRUE.
#' @return data.frame or xts object. Each column of data corresponds to one
#'  point or one polygon, each row to a date.
#'
#' @author Lorenzo Busetto, phD (2015 - 2017)
#' email: busetto.l@@irea.cnr.it
#'
#' @note License: GPL 3.0
#' @export
#' @rdname MODIStsp_extract
#' @importFrom raster getZ extent extract res raster getValues
#' @importFrom sf st_as_sf st_read st_crs st_transform st_as_text st_crop st_geometry st_write st_coordinates
#' @importFrom gdalUtilities gdal_rasterize
#' @importFrom data.table data.table setkey
#' @importFrom xts as.xts
#' @examples
#' \dontrun{
#' # Extract average and standard deviation values from a rts object created by
#' # MODIStsp for each polygon of a shapefile, for each date in the period
#' # between 2001-01-01 and 2014-12-31
#'
#' # The example uses tif files in testdata/VI_16Days_500m_v6 to build
#' # a MODIStsp rasterStack corresponding to the 2016 time series of the NDVI index
#' # over the Como Lake (Italy). It then extracts data on polygons corresponding
#' # to different land cover classes saved in testdata/extract_polys.shp
#'
#' # First, prepare the test dataset.
#' # __NOTE__ To avoid redownloading, here we copy some test data from MODIStsp
#' # installation folder to tempdir and use it to create a test time series.
#'
#' test_zip <-  system.file("testdata/VI_16Days_500m_v6/NDVI.zip",
#'                          package = "MODIStsp")
#' dir.create(file.path(tempdir(), "MODIStsp/VI_16Days_500m_v6"),
#'            showWarnings = FALSE, recursive = TRUE)
#' utils::unzip(test_zip, 
#'              exdir = file.path(tempdir(), "MODIStsp/VI_16Days_500m_v6"))
#'
#' opts_file <- system.file("testdata/test_extract.json", package = "MODIStsp")
#' MODIStsp(opts_file = opts_file, gui = FALSE, verbose = FALSE)
#'
#' # Now load the MODIStsp stack: This is a MODIS NDVI time series ranging between
#' # 2016-01-01 and 2016-12-18
#' # __NOTE__: MODIStsp rasterStack files are always saved in the "Time_Series\/RData"
#' # subfolder of your main output folder - see
#' # "https://docs.ropensci.org/MODIStsp/articles/output.html")
#'
#' # Specify the filename of the RData RasterStack of interest
#' stack_file  <- file.path(tempdir(),
#'  "MODIStsp/VI_16Days_500m_v6/Time_Series/RData/Terra/NDVI",
#'   "MOD13A1_NDVI_1_2016_353_2016_RData.RData")
#' basename(stack_file)
#'
#' ts_data <- get(load(stack_file))
#' ts_data
#'
#' # Now load a shapefile containing polygons from which we want to extract data
#'
#' polygons <- sf::st_read(system.file("testdata/extract_polys.shp",
#'                            package = "MODIStsp"), quiet = TRUE)
#' polygons
#'
#' # Finally, extract the average values for each polygon and date and plot the
#' # results
#'
#' out_dataavg <- suppressMessages(MODIStsp_extract(ts_data, polygons, id_field = "lc_type",
#'                              small = FALSE))
#' head(out_dataavg)
#'
#' plot(out_dataavg, legend.loc = "topleft")
#'
#' # use a different summarization function
#'
#' out_datasd <- MODIStsp_extract(ts_data, polygons, id_field = "lc_type",
#'                               FUN = "sd", small = FALSE)
#' head(out_datasd)
#'
#' # (See also https://docs.ropensci.org/MODIStsp/articles/Analyze.html for a
#' # worked-out example)
#' }


MODIStsp_extract <- function(in_rts, sf_object,
                             start_date = NULL,  end_date = NULL,
                             id_field   = NULL,  FUN      = "mean",
                             out_format = "xts", small    = TRUE,
                             small_method = "centroids",
                             na.rm      = TRUE, verbose = FALSE) {

  .SD <- NULL # Workaround to avoid note on package check


  if (!inherits(in_rts, c("RasterStack", "RasterBrick"))) {
    stop("Input is not a RasterStack or RasterBrick object")
  }
  if (!inherits(raster::getZ(in_rts), "Date")) {
    stop("Input does not contain valid dates in its 'Z' attribute !")
  }
  if (length(start_date) == 0) {
    start_date <- min(raster::getZ(in_rts))
    if (verbose)
      message("Starting date not provided - Using the first date in the stack") #nocov #nolint
  }
  if (length(end_date) == 0) {
    end_date <- max(raster::getZ(in_rts))
    if (verbose)
      message("Ending date not provided - Using the last date in the stack") #nocov #nolint
  }
  if (!inherits(start_date, c("Date", "POSIXct", "POSIXlt"))) {
    start_date <- try(as.Date(start_date), silent = TRUE)
    if (inherits(start_date, "try-error")) {
      stop("start_date is not a Date object or string coercible to date")
    }
  }
  if (!inherits(end_date, c("Date", "POSIXct", "POSIXlt"))) {
    end_date <- try(as.Date(end_date), silent = TRUE)
    if (inherits(end_date, "try-error")) {
      stop("end_date is not a Date object or string coercible to date")
    }
  }
  if (start_date > end_date) {
    stop("start_date larger than end_date")
  }
  if (!small_method %in% c("centroids", "full")) {
    warning("Unknown 'small_method' value - resetting to 'centroids'")
  }
  if (!out_format %in% c("xts", "dframe")) {
    message("Unknown 'out_format' value - resetting to 'xts'")
    out_format <- "xts"
  }

  if (length(attributes(attributes(sf_object)[["class"]])[["package"]]) != 0 &&
      attributes(attributes(sf_object)[["class"]])[["package"]] == "sp") {
    sf_object <- sf::st_as_sf(sf_object)
  }

  if (!inherits(sf_object, "sf")) {
    if (inherits(sf_object, "character")) {
      sf_object <- try(
        sf::st_read(sf_object))
      if (inherits(sf_object, "try-error")) {
        stop("sf_object is not a valid Spatial object or Shapefile")
      }
    }
  }
  if (length(id_field) != 0) {
    if (!id_field %in% names(sf_object)) {
      warning("Invalid 'id_field' value - names of output columns will be the ",
              "record number of the shapefile feature")
      id_field <- NULL
    }
  }
  dates     <- raster::getZ(in_rts)
  sel_dates <- which(dates >= start_date & dates <= end_date)

  if (length(sel_dates) > 0) {
    if (sf::st_crs(sf_object) != sf::st_crs(in_rts[[1]])) {
      sf_object <- sf::st_transform(sf_object,
                                    sf::st_crs(in_rts[[1]]))
    }

    sf_object$mdxtnq <- seq_along(1:dim(sf_object)[1])
    # shape <- raster::crop(as(sf_object, "Spatial"),  raster::extent(in_rts[[1]]))
    # shape <- sf::st_as_sf(shape)
    shape <- sf::st_crop(sf_object, in_rts[[1]])
    if (!isTRUE(all.equal(raster::extent(shape),
                          (raster::extent(sf_object)), scale = 100))) {
      warning("Some features of the spatial object are outside or partially ",
              "outside\n the extent of the input RasterStack ! Output for ",
              "features outside RasterStack extent\n will be set to NA. ",
              "Outputs for features only partially inside will be retrieved\n ",
              "using only the available pixels !")
      if (!setequal(sf_object$mdxtnq, shape$mdxtnq)) {

        outside_feat <- setdiff(sf_object$mdxtnq, shape$mdxtnq)
      }
    }
    if (inherits(sf::st_geometry(shape), c("sfc_POINT", "sfc_MULTIPOINT",
                            "sfc_LINESTRING", "sfc_MULTILINESTRING"))) {

      ts <- matrix(nrow = length(sel_dates), ncol = dim(shape)[1])
      for (f in seq_along(sel_dates)) {
        if (verbose == TRUE) {
          message("Extracting data from date: ", dates[sel_dates[f]]) #nocov
        }
        ts[f, ] <- raster::extract(in_rts[[sel_dates[f]]], shape,
                                   fun = FUN)
      }

      ts <- as.data.frame(ts)
      if (length(id_field) == 1) {
        feat_names <- as.character(shape[[eval(id_field)]])
        names(ts) <- c(feat_names)
      } else {
        names(ts) <- seq_along(shape[[1]])
      }

      if (out_format == "dframe") {
        ts <- cbind(date = dates[sel_dates], ts)
      }
      # On polygons, extract by rasterization !
    } else {
      if (verbose) message("Rasterizing shape") #nocov

      tempshape <- tempfile(tmpdir = tempdir(), fileext = ".shp")
      if (verbose) {
        message("Writing temporary rasterized shapefile") #nocov
      }
      sf::st_write(shape, tempshape, delete_dsn = TRUE, quiet = TRUE)
      tempraster <- tempfile(tmpdir = tempdir(), fileext = ".tiff")
      ext_conv <- function(x) {
        ext <- raster::extent(x)
        c(ext[1], ext[3], ext[2], ext[4])
      }
      if (max(shape$mdxtnq) <= 255) {
        ot <- "Byte"
      } else {
        #nocov start
        if (max(shape$mdxtnq) <= 65536) {

          ot <- "Int16"
        }
        else {
          ot <- "Int32"
        }
        #nocov end
      }
      gdalUtilities::gdal_rasterize(tempshape,
                                    tempraster,
                                    tr = raster::res(in_rts),
                                    te = ext_conv(in_rts[[1]]),
                                    a = "mdxtnq",
                                    ot = ot)
      zone_raster <- raster::raster(tempraster)
      zones       <- raster::getValues(zone_raster)
      ok_zones    <- which(is.finite(zones) & zones != 0)
      zones       <- zones[ok_zones]
      ncols       <- length(unique(zones))
      ts          <- matrix(nrow = length(sel_dates), ncol = ncols)

      for (f in seq_along(sel_dates)) {
        if (verbose) {
          message(paste0("Extracting data from date: ", dates[sel_dates[f]])) #nocov #nolint
        }

        value <- raster::getValues(in_rts[[sel_dates[f]]])[ok_zones]
        rDT   <- data.table::data.table(value, zones)
        data.table::setkey(rDT, zones)
        ts[f, 1:ncols] <- rDT[, lapply(.SD, match.fun(FUN), na.rm = na.rm),
                              by = zones]$value

      }
      ts <- as.data.frame(ts)
      if (length(id_field) == 1) {
        feat_names <- as.character(
          sf_object[[eval(id_field)]][sort(unique(zones))]
        )
        names(ts) <- feat_names
      }
      else {
        feat_names <- as.character(sf_object[["mdxtnq"]])[sort(unique(zones))]
        names(ts) <- feat_names
      }
      if (out_format == "dframe") {
        ts <- cbind(date = dates[sel_dates], ts)
      }

      if (small & ncols != dim(shape)[1]) {

        if (length(id_field) == 1) {
          miss_feat   <- setdiff(as.character(shape[[eval(id_field)]]),
                                 names(ts))
          pos_missing <- which(
            as.character(shape[[eval(id_field)]]) %in% miss_feat
          )
        } else {
          miss_feat <- setdiff(as.character(shape[["mdxtnq"]]), names(ts))
          pos_missing <- which(
            as.character(shape[["mdxtnq"]]) %in% miss_feat
          )
        }

        shpsub <- shape[pos_missing, ]
        ts_mis <- matrix(nrow = length(sel_dates), ncol = length(pos_missing))
        for (f in seq_along(sel_dates)) {
          if (verbose) {
            message("Extracting data from date: ", dates[sel_dates[f]]) #nocov

          }
          if (small_method == "centroids") {
            ts_mis[f, ] <- raster::extract(in_rts[[sel_dates[f]]],
                                           sf::st_coordinates(shpsub),
                                           fun = mean)

          } else {

            ts_mis[f, ] <- raster::extract(in_rts[[sel_dates[f]]],
                                           shpsub, fun = mean)

          }

        }
        colnames(ts_mis) <- miss_feat
        ts <- cbind(ts, ts_mis)
      }
      file.remove(tempraster)
      file.remove(tempshape)
    }

    if (exists("outside_feat")) {
      if (length(id_field) == 1) {
        feat_names_outside <- as.character(
          sf_object[[eval(id_field)]])[outside_feat]

      } else {
        feat_names_outside <- as.character(
          sf_object[["mdxtnq"]])[outside_feat]
      }

      ts_outside <- matrix(nrow = length(sel_dates),
                           ncol = length(feat_names_outside))
      ts_outside <- data.frame(ts_outside)
      names(ts_outside) <- feat_names_outside
      ts <- cbind(ts, ts_outside)
      if (length(id_field) == 1) {
        sortindex <- which(!is.na(match(sf_object[[eval(id_field)]],
                                        names(ts))))
      } else {
        sortindex <- which(!is.na(match(sf_object[["mdxtnq"]],
                                        names(ts))))
      }
      ts <- ts[, sortindex]
    }
    if (out_format == "xts") {
      ts <- xts::as.xts(ts, order.by = dates[sel_dates])
    }
    return(ts)
  } else {
    warning("Selected time range does not overlap with the one of the ",
            "RasterStack input dataset !")
  }

}
