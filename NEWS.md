# MODIStsp 1.3.2.9000 Bug fixes on development version

## Bug fixing

-   We recently fixed a bug affecting MODIS layers coded as Unsigned Integer - 32 bit (Thanks to Rob Critchlow for signaling this). The bug was due to improper handling of UInt32 data in `gdalbuildvrt`, causing sometimes an incorrect translation from HDF to output formats ([#72](https://github.com/lbusett/MODIStsp/issues/72)).

     **M\*D09A1** - 500m Reflectance Band Quality (V005 and V006); **M\*DO9CMG** - Coarse Resolution QA (V005 and V006); **M\*D09CMG** - Coarse Resolution Number Mapping (V006); **M\*D09GA** - 500m Reflectance Band Quality (V005 and V006); **M\*DOCGA** - Band quality for MODIS bands 8-15 (V006); **M\*D11C3** - Days with clear-sky conditions and validated LSTs; Nights with clear-sky conditions and validated LSTs (V005 and V006); **MCD43A2** - BRDF\_Albedo\_Band\_Quality (V005 and V006).

    The bug in now fixed in the `github` repo (devel and master). Bug fix will be included in CRAN in v 1.3.3, to be released soon. *If you rely on those layers, we suggest you to reprocess your time series !*

- Fixed a bug affecting creation of time series files (RData and virtual rasters) on all MCD products ([#77](https://github.com/lbusett/MODIStsp/issues/77))

- Fixed a bug a error on creation of "burn_date" layers for MCD45A1 product ([#77](https://github.com/lbusett/MODIStsp/issues/77))

- Fixed bugs on specifying spatial extent files on non-interactive execution ([#75](https://github.com/lbusett/MODIStsp/issues/75))


### 17/04/2017 - MODIStsp is now on CRAN !

MODIStsp was recently accepted on CRAM. From now on, you can install it simply using

`install.packages("MODIStsp")`

You'll however still be able to install the `development` version from github,
containing the last improvements and bug fixing using:

`install_github("lbusett/MODIStsp", ref = "master")`

# MODIStsp 1.3.2 Release Notes

v1.3.2 was released on 22/03/2017

## Major Changes:
Added functionality to apply scale and offset coeeficients on MODIS original values according with the specifications of single MODIS products.

### Details:
NASA provides outputs as integer values, indicating a potential scale factor and/or an offset to apply in order to obtain values in the indicated measure units.
- Leaving the "Scale output values" option to "No", output files are left as provided by NASA, and additional indices are produced as integer values with a 10000 factor scale; 
- Setting the "Scale output values" option to "Yes", scale factor and offsets are applied if existing (for example, in this case Land Surface Temperature values in the output raster will be in °K), and  spectral indices are floating point values (for example, NDVI is between -1 and 1, etcetera).

## Minor Changes:

- Some product names and output layer names were modified to reduce the length of output file names, homogenize the names of the outputs and correct some errors.
__For compatibility with already created output files__ (versions up to 1.3.1), the old "XML" file specifying output files format is still available in `inst/ExtData/MODIStsp_ProdOpts_old_v1.3.1.xml`. To use the old file naming conventions, users have to 
1. delete `inst/ExtData/MODIStsp_ProdOpts.xml` and rename `MODIStsp_ProdOpts_old_v1.3.1.xml` to `MODIStsp_ProdOpts.xml`.
2. delete `MODIStsp_ProdOpts.RData` from the `Previous` folder within `your_R-library_path/MODIStsp/Previous`
3. Restart `MODIStsp`

- Timeouts of httr/ftp requests were increased to prevent problems on download on slow connections

## Bug fixing:

- Fixed bug on FTP download speed (Issue [#65](https://github.com/lbusett/MODIStsp/issues/65)) 
- Fixed bug on download of tile 0, preventing download of images with DOY 001 and of all "yearly based" products (e.g., MOD17)(Issue [#64](https://github.com/lbusett/MODIStsp/issues/64)) 
- Fixed other bugs affecting FTP download (https://github.com/lbusett/MODIStsp/commit/efbf1b469e7518ffc8a7ec6d9922242d6a5c228f, https://github.com/lbusett/MODIStsp/commit/1dc53a5ff5b355965acec86678a3104bd2d27fd9, https://github.com/lbusett/MODIStsp/commit/fa6c7b42eadce516a2f781604c9db28418120f36)

# MODIStsp 1.3.1 Release Notes

v1.3.1 was released on 13/02/2017

## Major Changes

- Added functionality for processing of Snow Cover datasets: MOD10A1, MOD10A2, MOD10C1, MOD10C2, MOD10CM (Issue
[#55](https://github.com/lbusett/MODIStsp/issues/55)) on devel

- Added functionality for downloading "partial" years(Issue [#54](https://github.com/lbusett/MODIStsp/issues/54)) on devel

- Added functionality for computing vegetation indexes on MCD43A4 (v5-v6), MCD43B4 (v5), MCD43C4 (v5-v6) (Issue [#59](https://github.com/lbusett/MODIStsp/issues/59)) on master/devel

- Added functionality for accelerating download using aria2c (Issue [#55](https://github.com/lbusett/MODIStsp/issues/55)) on devel

## Bug fixing

- Fixed bug on download with aria, throwing an error on partial download on http downlaod with aria ([6fbc875](https://github.com/lbusett/MODIStsp/commit/6fbc87547b6214b500afc0291c02166c0b855c78))

- Fixed bug on M*D15A2 processing (Issue [#60](https://github.com/lbusett/MODIStsp/issues/60)) on devel/master

- Fixed bug on MCD12Q1 processing (Issue [#58](https://github.com/lbusett/MODIStsp/issues/58)) on devel/master

- Fixed bug on MOD13C2 processing (Issue [#52](https://github.com/lbusett/MODIStsp/issues/52)) on devel/master

- Fixed bug on insertion of custom projection (Issue [#57](https://github.com/lbusett/MODIStsp/issues/57)) on devel/master

- Fixed bug on selection of custom index (Issue [#53](https://github.com/lbusett/MODIStsp/issues/53)) on devel/master


# MODIStsp 1.3.0  Release Notes

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





