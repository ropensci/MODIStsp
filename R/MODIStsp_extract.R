

#' MODIStsp_extract
#' @description
#' function used to extract time series data from rts files created by MODIStsp
#' on spatial locations provided in the form of "R" spatial objects (spatialPoints,
#' spatialPolygons, etc.)
#' @details
#' The function takes as input an rasterStackTS object created by MODIStsp, a starting and ending date
#' and a standard "R" spatial object, and provides as time series for the spatial locations
#' specified in the spatial object in the form of a "R" xts object .
#' If the input spatial object is a "point" or "line" one, the xts output object contains one column
#' for each specified point, or for each cell intersecting the line, and one line for each date.
#' If the input spatial object is a "polygon" one, the xts output object contains one column
#' for each polygon, containing values obtained applying the function specified as the FUN
#' argument over all pixels belonging to the polygon, and one line for each date.
#'
#' @param in_rts "rasterStackTS" or "rasterBrickTS" object created by MODIStsp
#' @param start_date "Date", "POSIXct" or "POSIXlt" starting date of the period to be considered for data extraction
#' @param end_date "Date", "POSIXct" or "POSIXlt" ending date of the period to be considered for data extraction
#' @param sp_object "sp" object specifying the "positions" from which data has to be extracted
#' 	If sp_object represents lines, the function returns the values of the cells of a in_rts object that are touched by the line.
#'  If y represents polygons, the function  returns the values of the cells of a in_rts object that are covered by a polygon. A cell
#'  is covered if its center is inside the polygon .
#' @param FUN function to summarize the values (e.g. mean) on polygon data frames. The function should take a single numeric vector as argument and return a
#' single value (e.g. mean, min or max), and accept a na.rm argument. Thus, standard R functions not including an na.rm argument must
#' be wrapped as in this example: fun=function(x,...)length(x).
#'
#' @param na.rm	Logical If TRUE, and sp_object is a polygon, then na.rm = T is used when applying the function
#' 	to the different pixels of the polygon. Default = T.
#' @param verbose Logical If TRUE, messages on extraction completion are sent out. Default = T.
#'
#' @return xts object. Each column of data corresponds to one poin or one polygon
#'
#' @author Lorenzo Busetto, phD (2015)
#' email: busetto.l@@irea.cnr.it
#'
#' @license GPL(>2)
#' @export
#' @import rts
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
#' shape = readOGR(dirname(shpname), 'rois') # Read shapefile to SpatialPolygons
#' # Compute average and St.dev
#' dataavg = MODIStsp_extract(inrts, startdate, enddate, shape, FUN = 'mean', na.rm = T)
#' datasd = MODIStsp_extract(inrts, startdate, enddate, shape, FUN = 'sd', na.rm = T)
#' plot(dataavg)
#' }


MODIStsp_extract = function (in_rts, start_date, end_date, sp_object, FUN = 'mean', na.rm = T, verbose = F){

	if (!class(in_rts) %in% c('RasterStackTS','RasterBrickTS')){
		stop('in_rts is not a RasterStackTS or RasterBrickTS object')
	}

	if (!class(start_date) %in% c('Date','POSIXct','POSIXlt')) {

		stop('start_date is not a Date object')
	}

	if (!class(end_date) %in% c('Date','POSIXct','POSIXlt')) {

		stop('end_date is not a Date object')
	}

	if (!class(sp_object) %in% c('SpatialPolygonsDataFrame','SpatialPolygons', 'SpatialPointsDataFrame',
			'SpatialPoints','SpatialLines','SpatialLinesDataFrame')) {
		stop('sp_object is not a valid Spatial object ')
	}

#	if (!FUN %in% c('mean','sd', 'min',
#			'max','length')) {
#		stop('Invalid function - valid ones are : mean, sd, min, max')
#	}

	sel_indexes = which(index(in_rts) >= start_date & index(in_rts) <= end_date)
	if (length (sel_indexes)>1){
		shape = spTransform(sp_object, CRS(proj4string(in_rts@raster[[1]])))

		if (class(sp_object) %in% c('SpatialPointsDataFrame',
				'SpatialPoints','SpatialLines','SpatialLinesDataFrame')) {

			cells = extract(in_rts@raster[[1]], shape, cellnumbers = T)[,1]
			ts = matrix(nrow = length(sel_indexes), ncol = length(cells))
			for (f in 1:length(sel_indexes)) {
				if (verbose == T) {print(paste0('Extracting data from date: ', index(in_rts)[sel_indexes[f]]))}
				ts[f,] = extract(in_rts@raster[[sel_indexes[f]]], cells)
				}
		}else {
			zone_raster = rasterize(shape, in_rts@raster[[1]])
			zones = getValues(zone_raster)
			ok_zones = which(is.finite(zones))
			zones = zones [ok_zones]

			sel_indexes = which(index(in_rts) >= start_date & index(in_rts) <= end_date)

			ts = matrix(nrow = length(sel_indexes), ncol = nrow(sp_object))
			for (f in 1:length(sel_indexes)) {
				if (verbose == T) {print(paste0('Extracting data from date: ', index(in_rts)[sel_indexes[f]]))}
				value = getValues(in_rts@raster[[sel_indexes[f]]]) [ok_zones]
				rDT <- data.table(value, zones)
				setkey(rDT, zones)
				ts[f,] = rDT[, lapply(.SD, match.fun(FUN), na.rm = na.rm), by=zones]$value
			}
		}
		xtsdata = xts(ts, index(in_rts)[sel_indexes])
	} else {
		warning('Selected time range does not overlap with the one of the rts input dataset !')
	}
}

