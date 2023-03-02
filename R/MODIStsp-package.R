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
#' @author Lorenzo Busetto, phD (2014-2017)
#' @author Luigi Ranghetti, phD (2015-2017)
#' @seealso [https://docs.ropensci.org/MODIStsp/](https://docs.ropensci.org/MODIStsp/)
#' @seealso [https://github.com/ropensci/MODIStsp](https://github.com/ropensci/MODIStsp)
#'
NULL

values <- c(3.4, 2.9, 1.7, 8.8, -12.7, 100.4, 8.4, 11.3)
weights <- c(1.0, 0.1, 1.0, 0.2,  0.44,   0.3, 0.3, 0.83)

wvar <- (sum(weights)/(sum(weights)^2 - sum(weights^2))) * sum(weights*(values - mean(values))^2)
