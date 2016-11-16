# MODIStsp v 1.3.0.9000  

Added functionality for accelerating download using aria2c (Issue [#55](https://github.com/lbusett/MODIStsp/issues/55))

Fixed bug on insertion of custom projection (Issue [#57](https://github.com/lbusett/MODIStsp/issues/57))

Fixed bug on selection of custom index (Issue [#53](https://github.com/lbusett/MODIStsp/issues/53))



# MODIStsp v 1.3.0  Release NOtes

v1.3.0 was released on 11/05/2016

## Major Changes

1. Added functionality for downloading and preprocessing MODIS collection 006 datasets. For products with both 005 and 006 collections, the user can select the version using a new droplist in the GUI.

2. Added functionality for off-line processing. This allows both to _i)_ reprocessing already downloaded data (for example, to create time series for an additional layer) without the need to connect to NASA servers, and _ii)_ process HDF files downloaded outside _MODIStsp_ (e.g., directly from NASA ftp) and stored on the user's PC, without the need of an active internet connection. 

3. Improved the way in which options are saved. Much more readable .JSON files are now used instead than .RData. User options are no longer saved alongside products characteristics. This will allow to re-use an "old" options file even if changes are made on the XML file descriving the products.

4. Improved the GUI inteface for specifying additional Spectral Indexes. Hints are now showed to the user, and multiple indexes can be added in the same session.

## Minor Changes

1. General improvements in the GUI inteface. Products are now grouped by categories, to allow easier identification and selection.

2. Improvements in the README file and vignettes, providing more instructions on package use.

3. Improved functionality for checking for "complete" download, by comparing the size of the downloaded files with that of files on the server. 

4. Added "configure" file for Linux installation.

4. Temporary files necessary for processing (e.g., vrt files) are now created (and destroyed) within the "R" temporary folder.

5. Miscellaneous bug-fixing.


# MODIStsp v 1.2.1 Release NOtes

v1.2.1 was released on 04/20/2016
 
## Major Changes

1. Modified format of "R" output time series from _rts_ objects to _RasterStack_ objects with temporal information added in the 
"z" attribute via setZ()

2. Major changes/improvements in _MODIStsp\_extract_ function:
    * Use of plain rasterstack with "z" attribute instead than rasterstackts
    * Use of gdal\_rasterize (_gdalUtils_) instead of rasterize (_rgdal_) to improve speed. Temporary shapes and rasters necessay are saved in "R" temporary folder and removed automatically
    * Fixed bugs on functionality for point/lines shapefiles, according to what specified by the "small" and "small_method" parameters
    * Added functionality for retrieving data for small polygons
    * Added out_format selection - xts or plain data.frame
    * Added possibility to use a shp filename as input directly
    * Added conformity checks on inputs
    * Added functionaluity to run without specifying start and end dates
    * Added id_field parameter for choosing which column of the input SP object should be used for "naming" the columns of the output
  
3. Removed possibility to use "complex" resampling methods when reprojecting (e.g., bilinear, cubic, etc.) to avoid incorrect resampling on categorical variables and "contamination" of good pixels data. 

## Minor Changes

* Changed the input method for starting and ending dates selection in the GUI. Now a text field is used 
* Added functionaluty for writing data ignore value on ENVI files
* Removed automatic deletion of XML files created by writeRaster to keep metadata information
* Changed names of products in the GUI for products with both TERRA and AQUA dataset to M\*D09A1, M\*D13Q1, etc...
* Modified code syntax to satisfy R code styling guidelines
* Modified roxygen parameters so that only required functions are imported from dependent packages
* Updated and corrected the list of dependencies
* Updated required "R" version to 3.2, and minimum versions for dependent packages to current versions.
* Added Welcome message
* Updated links to LPDAAC product description pages
* Changed all "print" and "cat" calls to show messages/warnings to "message" or "warning" to allow easy disabling MODIStsp verbose messages
* Using "R" tempfile/tempdir to save vrt files

## Bug Fixes

* Corrected a bug that threw an error in case incorrect bounding box specified

# MODIStsp v 1.2.0 Release Notes

v1.2.0 was released on 29/07/2015


## Major changes

First stable release of advanced implementation of MODIStsp !
We know it should be 1.0.0, but thats'it !





