#' MODIStsp_extract
#' @description
#' function used to extract time series data from rts files created by MODIStsp
#' on spatial locations provided in the form of "R" spatial objects (spatialPoints,
#' spatialPolygons, etc.)
#' @details
#' The function takes as input a rasterStack object containing time information in the "z" attribute (set by "raster" function "SetZ"),
#' a starting and ending date  and a standard "R" spatial object, and returns the time series for the spatial locations
#' specified in the spatial object in the form of a "R" xts object ORr a plain data.frame with a "date" column in first position.
#' If the input spatial object is a "point" or "line" one, the  output object contains one column for each specified point, or for each cell
#' intersecting the line, and one line for each date. If the input spatial object is a "polygon" one, the output object contains one column
#' for each polygon, containing values obtained applying the function specified as the FUN
#' argument over all pixels belonging to the polygon, and one line for each date.
#'
#' @param in_rts input "rasterStack" or "rasterBrick" object created by MODIStsp (it MUST contain acquisition dates in the "Z" attribute " )
#' @param sp_object "sp" object OR name of an ESRI shapefile specifying the "positions" from which data has to be extracted
#' 	If sp_object represents lines, the output object contains one column for each polygon, containing values obtained applying the function
#'  specified as the FUN argument over all pixels touched by the line, and one line for each
#'  If sp_object represents points, the output object contains one column for each point, containing values of the cells corresponding to the pint,
#'  and one line for each date.
#'  If sp_object represents polygons, the output object contains one column for each polygon, containing values obtained applying the function
#'  specified as the FUN argument over all pixels belonging to the polygon, and one line for each date
#' @param start_date "Date", "POSIXct" or "POSIXlt" starting date of the period to be considered for data extraction OR character string cohercible to
#' Date class (format = "yyyy-mm-dd"). If not provided, the starting date of the rasterstack is used.
#' @param end_date "Date", "POSIXct" or "POSIXlt" ending date of the period to be considered for data extraction OR character string cohercible to
#' Date class (format = "yyyy-mm-dd").If not provided, the ending date of the rasterstack is used.
#' @param id_field character name of the column of the input sp object or shapefile to be used in the data extraction. Values conatined in the column MUST be unique
#' The names of the columns of the output are taken from this column. If not provided, or an invalid value is provided, then the names of the columns of the output
#' reflect the number of the feature in the original sp object or shapefile.
#' @param FUN function to summarize the values (e.g. mean) on polygon data frames. The function should take a single numeric vector as argument and return a
#' single value (e.g. mean, min or max), and accept a na.rm argument. Thus, standard R functions not including an na.rm argument must
#' be wrapped as in this example: fun=function(x,...)length(x). Defaults to "mean"
#' @param out_format character "xts" or "dframe". default to 'xts'. If dframe, the output is a data frame with dates in the first column and extracted data in the others
#' @param small logical If set to TRUE, and input is polygons, then values are returned also for polygons not covering at least one raster cell. "Included"
#' cells in this case depend on the values of the "small_method" parameter.
#' @param small_method string 'centroids' or 'full'. if small == T and input is polygons, controls which cells are "extracted" for small polygons. If set to
#' centroids (default), then only the cells corresponding to polygon centroid are considered (faster, may have problems on strangely shaped polygons). If set to
#' "full", then all cells intersected by the small polygon are extracted and used in calculation.
#' @param na.rm	Logical If TRUE, and sp_object is a polygon, then na.rm = T is used when applying the function
#' 	to the different pixels of the polygon. Default = T.
#' @param verbose Logical If TRUE, messages on extraction completion are sent out. Default = T.
#'
#' @return data.frame or xts object. Each column of data corresponds to one point or one polygon
#'
#' @author Lorenzo Busetto, phD (2015)
#' email: busetto.l@@irea.cnr.it
#'
#' @note License: GPL 3.0
#' @export
#' @importFrom xts as.xts
#' @import data.table
#' @examples
#' # Extract average and standard deviation values from a rts object created by MODIStsp
#' # for each polygon of a shapefile, for each date in the period between 2001-01-01 and 2014-12-31
#' \dontrun{
#' #Set the inputs
#' infile = "in_path/MOD13Q1_MYD13Q1_NDVI_49_2000_353_2015_RData.RData"  # Input rts file
#' shpname = "path_to_file/rois.shp"  # Path to Polygon Shapefile
#' startdate = as.Date("2010-01-01")  # Start date for extraction
#' enddate = as.Date("2014-12-31")    # End date for extraction
#' #Load Data
#' inrts = get(load(infile))          # Load the rts file
#' # Compute average and St.dev
#' dataavg = MODIStsp_extract(inrts,shpname, startdate, enddate, FUN = 'mean', na.rm = T)
#' datasd = MODIStsp_extract(inrts,shpname,  startdate,  enddate,  FUN = 'sd', na.rm = T)
#' plot(dataavg)
#' }


MODIStsp_extract <- function(in_rts, sp_object, start_date = NULL, end_date = NULL,
                             id_field = NULL, FUN = "mean", out_format = "xts", small = T, small_method = "centroids", na.rm = T, verbose = F){

  # checks on inputs ----

  if (!class(in_rts) %in% c("RasterStack","RasterBrick")) {
    stop("in_rts is not a RasterStack or RasterBrick object")
  }

  if (!class(getZ(in_rts)) == "Date") {

    stop("in_rts doesn't contain valid dates in its 'Z' attribute !")
  }
  if (length(start_date) == 0) {

    start_date <- min(getZ(in_ras))
    if (verbose) cat("Starting date not provided - Using the first date in the stack\n")
  }

  if (length(end_date) == 0) {

    end_date <- max(getZ(in_ras))
    if (verbose) cat("Starting date not provided - Using the first date in the stack\n")
  }

  if (!class(start_date) %in% c("Date","POSIXct","POSIXlt")) {
    start_date = try(as.Date(start_date), silent = T)
    if (class(start_date) == "try-error") {
      stop("start_date is not a Date object or string cohercible to date")
    }
  }

  if (!class(end_date) %in% c("Date","POSIXct","POSIXlt")) {
    end_date = try(as.Date(end_date), silent = T)
    if (class(end_date) == "try-error") {
      stop("end_date is not a Date object or string cohercible to date")
    }
  }

  if (start_date > end_date) {

    stop("start_date larger than end_date")
  }

  if (!small_method %in% c("centroids","full")) {

    if (verbose) cat("Unknown 'small_method' value - resetting to 'centroids'\n")
  }

  if (!out_format %in% c("xts","dframe")) {

    if (verbose) cat("Unknown 'out_format' value - resetting to 'xts'\n")
    out_format = "xts"
  }

  # CHeck if sp_object is valid spatial object or ESRI shapefile name. If not, exit.
  if (!class(sp_object) %in% c("SpatialPolygonsDataFrame","SpatialPolygons", "SpatialPointsDataFrame",
                               "SpatialPoints","SpatialLines","SpatialLinesDataFrame")) {
    if (class(sp_object) == "character") {
      sp_object <- try(readOGR(dirname(sp_object),basename(file_path_sans_ext(sp_object))))
      if (class(sp_object) == "try-error") {
        stop("sp_object is not a valid Spatial object or Shapefile")
      }
    }
  }
  if (length(id_field) != 0) {
    if (!id_field %in% names(sp_object)) {
      if (verbose) cat("Invalid 'id_field' value - names of output columns will be the record number of the shapefile feature\n")
      id_field <- NULL
    }
  }

  # start processing ----
  dates <- getZ(in_rts)
  sel_dates <- which(dates >= start_date & dates <= end_date)

  if (length(sel_dates) > 1) {
    shape <- spTransform(sp_object, CRS(proj4string(in_rts[[1]])))
    in_rts <- crop(in_rts, extent(shape))
    # If object is points or lines, find the pixels intersected by points or lines, then extract the data for each date
    if (class(sp_object) %in% c("SpatialPointsDataFrame",
                                "SpatialPoints","SpatialLines","SpatialLinesDataFrame")) {

      # cells <- extract(in_rts[[1]], shape, cellnumbers <- T, fun = mean)[,1]
      ts <- matrix(nrow = length(sel_dates), ncol = length(shape[,1]))
      for (f in 1:length(sel_dates)) {
        if (verbose == T) {
          print(paste0("Extracting data from date: ", dates[sel_dates[f]]))
        }
        ts[f,] <- extract(in_rts[[sel_dates[f]]], sp_object, fun = FUN)
      }

      ts <- as.data.frame(ts)
      if (length(id_field) == 1) {
        feat_names <- as.character(shape@data[,eval(id_field)]) # get the "id_field" names for the identified zones
        names(ts) <- c(feat_names)
      } else {
        names(ts) <- 1:length(shape[,1])
      }
      if (out_format == "dframe") { # add the date column if outpit is dframe
        ts <- cbind(date =  dates[sel_dates], ts)
      }
    } else {

      # If object is polygons, rasterize the shape, then extract the data for each date using data.table

      zone_raster <- rasterize(shape, in_rts[[1]])
      zones <- getValues(zone_raster)
      ok_zones <- which(is.finite(zones))
      zones <- zones[ok_zones]
      sel_dates <- which(dates >= start_date & dates <= end_date)
      ncols <- length(unique(zones))
      ts <- matrix(nrow = length(sel_dates), ncol = ncols)
      for (f in 1:length(sel_dates)) {
        if (verbose == T) {
          print(paste0("Extracting data from date: ", dates[sel_dates[f]]))
        }
        value <- getValues(in_rts[[sel_dates[f]]]) [ok_zones]
        rDT <- data.table(value, zones)
        setkey(rDT, zones)
        ts[f,1:ncols] <- rDT[, lapply(.SD, match.fun(FUN), na.rm = na.rm), by = zones]$value
      }

      ts <- as.data.frame(ts)
      if (length(id_field) == 1) {
        feat_names <- as.character(shape@data[,eval(id_field)])[zones] # get the "id_field" names for the identified zones
        names(ts) <- c(feat_names)
      } else {
        names(ts) <- c(zones)
      }
      if (out_format == "dframe") { # add the date column if outpit is dframe
        ts <- cbind(date =  dates[sel_dates], ts)
      }

      # if small = T and not all the polygons had at least one point, then extract the data for the small polygons
      # using the standard "extract" function, with small == T and method defined by small_method
      if (small & ncols != length(shape@data[,1])) {

        if (length(id_field) == 1) {
          miss_feat <- setdiff(as.character(shape@data[,eval(id_field)]),as.character(shape@data[,eval(id_field)])[zones] )
          pos_missing <- which(shape@data[,eval(id_field)] %in% miss_feat)
        } else {
          pos_missing <- miss_feat <- setdiff(1:length(shape@data[,1]), zones)
        }
        # browser()
        shpsub <- shape[pos_missing,]
        ts_mis <- matrix(nrow = length(sel_dates), ncol = length(pos_missing))
        # cells <- extract(in_rts[[1]], shpsub, cellnumbers = TRUE)
        for (f in 1:length(sel_dates)) {
          if (verbose == T) {
            print(paste0("Extracting data from date: ", dates[sel_dates[f]]))
          }
          if (small_method == "centroids") {
            ts_mis[f,] <- extract(in_rts[[sel_dates[f]]], coordinates(shpsub), fun = mean)
          } else {
            ts_mis[f,] <- extract(in_rts[[sel_dates[f]]], shpsub, fun = mean)
          }
        }

        colnames(ts_mis) <- miss_feat
        ts <- cbind(ts, ts_mis)
      }
    }
    if (out_format == "xts") {  # If out_format is xts, convert the df to xts object
      ts <- as.xts(ts, order.by = dates[sel_dates])
    }
    ts
  } else {
    warning("Selected time range does not overlap with the one of the rasterstack input dataset !")
  }
}
