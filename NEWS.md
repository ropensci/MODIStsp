# MODIStsp v 1.1.0 Release NOtes

v1.1.0 was released on 20/04/2016
 
## Major Changes

* Modified format of "R" output time series from _rts_ objects to _RasterStack_ objects with temporal information added in the 
"z" attribute via setZ()

* Major changes/improvements in _MODIStsp\_extract_ function:
 ** Use of plain rasterstack with "z" attribute instead than rasterstackts
 ** Use of gdal\_rasterize (_gdalUtils_) instead of rasterize (_rgdal_) to improve speed. Temporary shapes and rasters necessay are saved in "R" temporary folder and removed automatically
 ** Fixed bugs on functionality for point/lines shapefiles, according to what specified by the "small" and "small_method" parameters
 ** Added functionality for retrieving data for small polygons
 ** Added out_format selection - xts or plain data.frame
 ** Added possibility to use a shp filename as input directly
 ** Added conformity checks on inputs
 ** Added functionaluity to run without specifying start and end dates
 ** Added id_field parameter for choosing which column of the input SP object should be used for "naming" the columns of the output
* Removed possibility to use "complex" resampling methods when reprojecting (e.g., bilinear, cubic, etc.) to avoid incorrect resampling on categorical variables and "contamination" of good pixels data. 

## Minor Changes

* Changed the input method for starting and ending dates selection in the GUI. Now a text field is used 
* Added functionaluty for writing data ignore value on ENVI files
* Removed automatic deletion of XML files created by writeRaster to keep metadata information
* Changed names of products in the GUI for products with both TERRA and AQUA dataset to M\*D09A1, M\*D13Q1, etc...
* Modified code syntax to satisfy R coe styling guidelines
* Modified roxygen parameters so that only required functions are imported from dependent packages
* Updated and corrected the list of dependencies
* Updated required "R" version to 3.2, and minimum versions for dependent packages to current versions.
* Added Welcome message
* Added "About" panel

## Bug Fixes

* Corrected a bug that threw an error in case incorrect bounding box specified

# MODIStsp v 1.0.0 Release Notes

v1.1.0 was released on xx/yy/zzzz

## Major changes

First stable release of MODIStsp





