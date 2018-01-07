#' MODIStsp: a package to automatize the creation of time series of raster
#'  images derived from MODIS Land Products
#'
#' MODIStsp allows automating the creation of time series of rasters derived
#' from MODIS Satellite Land Products data. It performs several typical
#' preprocessing steps such as download, mosaicking, reprojection and resize
#' of data acquired on a specified time period. All processing parameters
#' can be set using a user-friendly GUI. Users can select which layers of
#' the original MODIS HDF files they want to process, which additional
#' Quality Indicators should be extracted from aggregated MODIS Quality
#' Assurance layers and, in the case of Surface Reflectance products
#' , which Spectral Indexes should be computed from the original reflectance
#' bands. For each output layer, outputs are saved as single-band raster
#' files corresponding to each available acquisition date. Virtual files
#' allowing access to the entire time series as a single file are also created.
#' Command-line execution exploiting a previously saved processing options
#' file is also possible, allowing to automatically update time series
#' related to a MODIS product whenever a new image is available.
#' 
#' @docType package
#' @name MODIStsp-package
#' @author Lorenzo Busetto, phD (2014-2017) \email{lbusett@@gmail.com}
#' @author Luigi Ranghetti, phD (2015-2017) \email{ranghetti.l@@irea.cnr.it}
#' @seealso [http://lbusett.github.io/MODIStsp/](http://lbusett.github.io/MODIStsp/)
#' @seealso [https://github.com/lbusett/MODIStsp](https://github.com/lbusett/MODIStsp)
#' 
NULL