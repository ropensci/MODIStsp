# MODIStsp 2.0.8

## Minor changes
- Reducing package size
- State that GDAL with support for HDF4 format is needed (in vignette and 
    package description), and exit gracefully if HDF4 is not supported.


# MODIStsp 2.0.7

## Minor changes
- Add product version 061 for existing products (#238)
- Closing the support on GitHub issues, due to career change of the maintainer.

## Bug fixes 
- Fix #245


# MODIStsp 2.0.6

## Minor changes
- Replace `M*D17A3H` with `M*D17A3HGF` and add `M*D17A2HGF` (#237)
- Avoid errors in case of missing internet connection

## Bug fixes 
- Patch for bbox loaded from json in case of drawn extent (#228)
- Fix #226
- Fix #232 
- Fix #234
- Fix CRAN notes


# MODIStsp 2.0.5

## Main changes

- Edit documentation related to the change of maintainer
    (see https://docs.ropensci.org/MODIStsp/articles/lorenzo).
    
- Add the argument `parallel` to function `MODIStsp()` and `MODSIStsp_process()`
    to allow running the processing in single core modality.
    
## Minor changes

- Fix Travis tests

- Bug fix (#222)


# MODIStsp 2.0.3

## Main changes

- This submission should fix errors on Debian CRAN builds, due to improper 
trigger of an internal function leading to writing in the user's 
lib folder. 

- Fixes a bug leading to crash when using scale_val = TRUE and change_no_data = FALSE

- Fixes a bug leading to the GUI crashing rather than giving info messages in 
  case not all input parameters are specified

- Implements redirection to MODIS products web pages when pressing 
 the corresponding button
 
- Modifies slightly the Shiny GUI


# MODIStsp 2.0.0

## Main changes

- Replace the old gWidgets-based GUI with a new one based on Shiny;

- Enhances support for CLI usage. Now all parameters can be passed to 
the `MODIStsp` function. If also a opts_file is passed, values specified
explicitly in the call override those in the options file;

- Fixes problems in retrieval of corners for MODIS products in 4008 projection (fixes #204);

- Fixes problems/improves support for datasets with multiple NoData values.
Now, all NoData values are kept to original values if NoData change is set
to FALSE. Also, Scale/Offset are no longer wrongly applied also to NoData values when scaleval = TRUE;


# MODIStsp 1.4.0

## Main changes

- switch to use of GDAL3/PROJ6 WKTs for projection representation, using sf::gdal_utils to perform gdalwarp/gdaltranslate instead of gdalUtils on external GDAL.

- switch to sf for all internal work on vector data.

- Remove sp, rgdal, rgeos, pacman, gdalUtils dependencies

- Adds support for products MCD19A1 and MCD19A2 products

# MODIStsp 1.3.9

## Main changes

- Fixes a bug causing crashes on MOD14A1 product

- Adds support for product MCD12Q2 and removes support for no longer available version 5 of some products.

- Updates web links to MODIS products description pages

# MODIStsp 1.3.8

## Main changes

- Fixed an issue causing incorrect application of scale/offset values on GDAL 
versions > 2.3 (introduced by change of behaviour of gdal_buildvrt - https://trac.osgeo.org/gdal/ticket/3221#comment:5) - see https://github.com/ropensci/MODIStsp/issues/163

- Added support for the following products: MOD21A1D.006 MOD21A1N.006 MOD21A2.006/

## Bug fixing

- Fixed an issue preventing correct processing of some products in offline mode (
https://github.com/ropensci/MODIStsp/issues/142)


# MODIStsp 1.3.7

## Main changes

- Fixed a bug leading to incorrect computation of custom spectral indices containing "additive"
parameters (e.g., (NIR+0.1)/(Red-0.2)) when scale_val == FALSE

## Bug fixing

- Fixed a bug leading to not properly reassigning NoData values when processing 
  full tiles and using change_nodata = TRUE

- Fixed inconsistencies in definition of characteristics of products
  MOD/MYD13C2/C1 and MOD/MYD13A3 (erroneous layers in xml)

- Fixed a bug leading to help messages in the select layers portion of the GUI 
to not render

- Updated MOD44B specifications to allow download of v006 version

# MODIStsp 1.3.6

## Main changes

Maintenance release to solve CRAN build errors on debian, due to the test_resetindexes
test. The test is now skipped on CRAN. Additionally, the MODIStsp_addindex 
function was modified to require explicit permission by the user to write on 
the MODIStsp_previous.json file

## Bug fixing

- Fixed a bug leading to errors if only "Aqua" platform was selected for download [#133](https://github.com/ropensci/MODIStsp/issues/133)

# MODIStsp 1.3.5

## Main changes

Maintenance release to solve CRAN build errors on debian, due to the test_addindex
test. The test is now skipped on CRAN. Additionally, the MODIStsp_addindex 
function was modified to require explicit permission by the user to write on 
the MODIStsp_previous.json file

## Bug fixing

- Fixed bug leading to errors in processing extent when switching products with different Native projection (4008 vs sinusoidal), the projection string was not properly updated. [77f5693e9](https://github.com/ropensci/MODIStsp/commit/77f5693e9e1e180f05efaa04fa031567e782ba89)

- Fixed warnings on check for uniqueness in http addresses

# MODIStsp 1.3.4

## Main changes

### Breaking changes

- Due to improvements and changes in the GUI (see below), `MODIStsp` .json options 
  files saved with older versions are no longer supported. Users will be informed of 
  this if trying to use an obsolete json file.
  
- **Removed support for FTP download** due to switch-off of NASA servers.

### Updates in supported products

- **Removed all v005 and earlier products**, due to discontinuation of their 
distribution by NASA servers

- **Added support** for the following products:

  MCD64A1; MCD12C1; MCD18A1; MCD18A2; MCD12Q1; MOD44B; MOD44W; MCD12C1; MCD12Q1;
  MOD12A2; MOD12A3

### Improvements in download functions

- **Improvements in GUI**. It is now possible to set the processing extent interactively
  using the "Select on Map" button. This opens a browser window allowing to select
  and extent. 

- Use of `httr::RETRY` to improve behavior while navigating the servers to
  retrieve available files and while downloading hdf file (when use_aria == FALSE), 
  thus removing dependency to RCurl. 
  
### Improvements in processing functions

 - **Improved functionality for dealing with NoData** for products with multiple 
   fill-values. If "Change NoData" is set to "Yes", then in case a layer 
   has multiple Nodata values all those values are set to NA in the output 
   (see github.com/ropensci/MODIStsp#113)

### Extensive code refactoring for submission to ropensci. 

- Long functions were simplified/split into smaller functions to allow for 
  easier maintenance
- GUI event handlers were moved into dedicated "R" files
- Extensive code linting to abide to ropensci standards
- Switch to jsonlite/xml2 to abide to ropensci standards
- Removal of some less-used dependencies (e.g., hash)

### Improvements in documentation and website

- More detailed documentation for the main functions
- Improvements in pkgdown articles to include some "worked examples" (e.g., 
  MODIStsp_Extract)
- New article in pkgdown showing a list of supported products

### Improvements in test coverage

- Several new tests added, bringing coverage above 90%

### New functions

- Added `MODIStsp_resetindexes` to remove all custom indexes from a MODIStsp 
  json options file and `MODIStsp_reset_options` to reset MODIStsp options to 
  default.
  
### Bug fixing

- Fixed bug affecting extent selection when working with non-tiled (MCD) products
https://github.com/ropensci/MODIStsp/issues/122

- Fixed bugs affecting the "Seasonal" time series download 
  
________________________________________________________________________________

  
# MODIStsp 1.3.3.1 Release Notes

This was mostly a maintenance release - fixing a bug in 1.3.3 submission related
to a missing import in NAMESPACE

## Minor Changes

Improved organization of Virtual Raster files and RData files in the "Time_Series"
output subfolder. Now virtual files and RData files are organized by sensor and 
layer to facilitate access. 

____________________________________________________________________________________

# MODIStsp 1.3.3 Release Notes

v1.3.3 was released on 10/08/2017

## Major Changes

-  Improved speed in computation of spectral indexes, quality indicators and in 
   computation of scaled variables by using `raster::calc()` and `raster::overlay`
   (commits [0f5d76d](https://github.com/ropensci/MODIStsp/commit/0f5d76de1661958cd5cbaa79f8115035cb9c348e),     [0f5d76d](https://github.com/ropensci/MODIStsp/commit/0f5d76de1661958cd5cbaa79f8115035cb9c348e), [e462721](https://github.com/ropensci/MODIStsp/commit/e462721a06a079185ec5a84270ea0c8bd8edf54d))
   
-  Added functionality for unit testing using `testthat` and codecov integration.
   (commit [0c00fc6](https://github.com/ropensci/MODIStsp/commit/0c00fc6bf07aed046b2b198e0278ab3264e5298a)
   and others)
   
-  Added "testing mode" to allow users to test proper functioning. Now, running 
   `MODIStsp(test = X)` (with X in (0,6)) runs the processing using default processing
   parameters  (commit [0c00fc6](https://github.com/ropensci/MODIStsp/commit/0c00fc6bf07aed046b2b198e0278ab3264e5298a) and others)

## Minor Changes

-  Suppression of verbose messages and (useless) warning messages while parsing the NASA
servers and downloading data using "ftp" ( [3775d60](https://github.com/ropensci/MODIStsp/commit/3775d6099bc359925d3dcbd96c2ffe8455502648));

## Bug fixing

-   Fixed a bug preventing the "last" choice (or that present in the json file) from 
    correctly showing in the GUI upon launch/restore of a saved json file (commit
    [633c2dd](https://github.com/ropensci/MODIStsp/commit/633c2dddd29d45c618e4ca121112000ceefe91e3))

-   Fixed a bug affecting MODIS layers coded as Unsigned Integer - 32 bit (Thanks
    to Rob Critchlow for signaling this). The bug was due to improper handling of 
    UInt32 data in `gdalbuildvrt`, causing sometimes an incorrect translation from 
    HDF to output formats ([#72](https://github.com/ropensci/MODIStsp/issues/72)).

     **M\*D09A1** - 500m Reflectance Band Quality (V005 and V006); 
     **M\*DO9CMG** - Coarse Resolution QA (V005 and V006);
     **M\*D09CMG** - Coarse Resolution Number Mapping (V006); 
     **M\*D09GA** - 500m Reflectance Band Quality (V005 and V006);
     **M\*DOCGA** - Band quality for MODIS bands 8-15 (V006);
     **M\*D11C3** - Days with clear-sky conditions and validated LSTs;
                    Nights with clear-sky conditions and validated LSTs (V005 and V006); 
     **MCD43A2** - BRDF\_Albedo\_Band\_Quality (V005 and V006).

- Fixed a bug affecting creation of time series files (RData and virtual rasters) 
  on all MCD products ([#77](https://github.com/ropensci/MODIStsp/issues/77))

- Fixed a bug a error on creation of "burn_date" layers for MCD45A1 product 
  ([#77](https://github.com/ropensci/MODIStsp/issues/77))

- Fixed bugs on specifying spatial extent files on non-interactive execution
  ([#75](https://github.com/ropensci/MODIStsp/issues/75))

____________________________________________________________________________________

## 17/04/2017 - MODIStsp is now on CRAN !

MODIStsp was recently accepted on CRAN. From now on, you can install it simply using

`install.packages("MODIStsp")`

You'll however still be able to install the `development` version from github,
containing the last improvements and bug fixing using:

`install_github("ropensci/MODIStsp", ref = "master")`

____________________________________________________________________________________

# MODIStsp 1.3.2 Release Notes

v1.3.2 was released on 22/03/2017

## Major Changes:

- Added functionality to apply scale and offset coefficients on MODIS original values
  according with the specifications of single MODIS products.

### Details:

- MODIS hdf datasets are always stored as integer values, with scales factor and/or
  offsets to apply in order to convert to the indicated measure units reported in
  the products' documentation.

- Starting from v1.3.2: 

    - Leaving the "Scale output values" option to "No", output files are left as
      provided by NASA, and additional indices are produced as integer values with
      a 10000 factor scale; 
    - Setting the "Scale output values" option to "Yes", scale factor and offsets 
      are applied if existing (for example, in this case Land Surface Temperature
      values in the output raster will be in °K), and  spectral indices are floating
      point values (for example, NDVI is between -1 and 1, etc.).

## Minor Changes:

- Some product names and output layer names were modified to reduce the length of
  output file names, homogenize the names of the outputs and correct some errors.
__For compatibility with already created output files__ (versions up to 1.3.1),
  the old "XML" file specifying output files format is still available in
  `inst/ExtData/MODIStsp_ProdOpts_old_v1.3.1.xml`. To use the old file naming
  conventions, users have to:

    1. delete `inst/ExtData/MODIStsp_ProdOpts.xml` and rename `MODIStsp_ProdOpts_old_v1.3.1.xml`
       to `MODIStsp_ProdOpts.xml`.
    2. delete `MODIStsp_ProdOpts.RData` from the `Previous` folder within 
       `your_R-library_path/MODIStsp/Previous`
    3. Restart `MODIStsp`  
    
    <br>
    
- Timeouts of httr/ftp requests were increased to prevent problems on download on
  slow connections

## Bug fixing:

- Fixed bug on FTP download speed (Issue [#65](https://github.com/ropensci/MODIStsp/issues/65)) 
- Fixed bug on download of tile 0, preventing download of images with DOY 001 and
  of all "yearly based" products (e.g., MOD17)(Issue [#64](https://github.com/ropensci/MODIStsp/issues/64)) 
- Fixed other bugs affecting FTP download (https://github.com/ropensci/MODIStsp/commit/efbf1b469e7518ffc8a7ec6d9922242d6a5c228f, https://github.com/ropensci/MODIStsp/commit/1dc53a5ff5b355965acec86678a3104bd2d27fd9, https://github.com/ropensci/MODIStsp/commit/fa6c7b42eadce516a2f781604c9db28418120f36)

____________________________________________________________________________________

# MODIStsp 1.3.1 Release Notes

v1.3.1 was released on 13/02/2017

## Major Changes

- Added functionality for processing of Snow Cover datasets: MOD10A1, MOD10A2, 
  MOD10C1, MOD10C2, MOD10CM (Issue
[#55](https://github.com/ropensci/MODIStsp/issues/55)) on devel

- Added functionality for downloading "partial" years (Issue
  [#54](https://github.com/ropensci/MODIStsp/issues/54)) on devel

- Added functionality for computing vegetation indexes on MCD43A4 (v5-v6),
  MCD43B4 (v5), MCD43C4 (v5-v6) (Issue [#59](https://github.com/ropensci/MODIStsp/issues/59))
  on master/devel

- Added functionality for accelerating download using aria2c (Issue 
  [#55](https://github.com/ropensci/MODIStsp/issues/55)) on devel

## Bug fixing

- Fixed bug on download with aria, throwing an error on partial download on http
  download with aria ([6fbc875](https://github.com/ropensci/MODIStsp/commit/6fbc87547b6214b500afc0291c02166c0b855c78))

- Fixed bug on M*D15A2 processing (Issue [#60](https://github.com/ropensci/MODIStsp/issues/60))
  on devel/master

- Fixed bug on MCD12Q1 processing (Issue [#58](https://github.com/ropensci/MODIStsp/issues/58))
  on devel/master

- Fixed bug on MOD13C2 processing (Issue [#52](https://github.com/ropensci/MODIStsp/issues/52))
  on devel/master

- Fixed bug on insertion of custom projection (Issue [#57](https://github.com/ropensci/MODIStsp/issues/57))
  on devel/master

- Fixed bug on selection of custom index (Issue [#53](https://github.com/ropensci/MODIStsp/issues/53))
  on devel/master

____________________________________________________________________________________

# MODIStsp 1.3.0  Release Notes

v1.3.0 was released on 11/05/2016

## Major Changes

- Added functionality for downloading and preprocessing MODIS collection 006 datasets.
  For products with both 005 and 006 collections, the user can select the version
  using a new droplist in the GUI.

- Added functionality for off-line processing. This allows both to _i)_ reprocessing
  already downloaded data (for example, to create time series for an additional
  layer) without the need to connect to NASA servers, and _ii)_ process HDF files 
  downloaded outside _MODIStsp_ (e.g., directly from NASA ftp) and stored on the
  user's PC, without the need of an active internet connection. 

- Improved the way in which options are saved. Much more readable. JSON files are
  now used instead than .RData. User options are no longer saved alongside products
  characteristics. This will allow to re-use an "old" options file even if changes 
  are made on the XML file describing the products.

- Improved the GUI interface for specifying additional Spectral Indexes. Hints are 
  now showed to the user, and multiple indexes can be added in the same session.

## Minor Changes

- General improvements in the GUI interface. Products are now grouped by categories,
  to allow easier identification and selection.

- Improvements in the README file and vignettes, providing more instructions on package
  use.

- Improved functionality for checking for "complete" download, by comparing the 
  size of the downloaded files with that of files on the server. 

- Added "configure" file for Linux installation.

- Temporary files necessary for processing (e.g., vrt files) are now created (and
  destroyed) within the "R" temporary folder.

- Miscellaneous bug-fixing


____________________________________________________________________________________

# MODIStsp 1.2.1 Release Notes

v1.2.1 was released on 11/05/2016
 
## Major Changes

1. Modified format of "R" output time series from _rts_ objects to _RasterStack_ 
   objects with temporal information added in the "z" attribute via setZ()

2. Major changes/improvements in _MODIStsp\_extract_ function:
    * Use of plain `RasterStack` with "z" attribute instead than `rasterstackts`
    * Use of gdal\_rasterize (_gdalUtils_) instead of rasterize (_rgdal_) to improve 
      speed. Temporary shapes and rasters necessary are saved in "R" temporary folder
      and removed automatically
    * Fixed bugs on functionality for point/lines shapefiles, according to what 
      specified by the "small" and "small_method" parameters
    * Added functionality for retrieving data for small polygons
    * Added out_format selection - xts or plain data.frame
    * Added possibility to use a shp filename as input directly
    * Added conformity checks on inputs
    * Added functionality to run without specifying start and end dates
    * Added id_field parameter for choosing which column of the input SP object
      should be used for "naming" the columns of the output
  
3. Removed possibility to use "complex" resampling methods when reprojecting (e.g.,
   bilinear, cubic, etc.) to avoid incorrect resampling on categorical variables
   and "contamination" of good pixels data. 

## Minor Changes

* Changed the input method for starting and ending dates selection in the GUI.
  Now a text field is used 
* Added functionality for writing data ignore value on ENVI files
* Removed automatic deletion of XML files created by writeRaster to keep metadata
  information
* Changed names of products in the GUI for products with both TERRA and AQUA
  dataset to M\*D09A1, M\*D13Q1, etc...
* Modified code syntax to satisfy R code styling guidelines
* Modified roxygen parameters so that only required functions are imported from 
  dependent packages
* Updated and corrected the list of dependencies
* Updated required "R" version to 3.2, and minimum versions for dependent packages 
  to current versions.
* Added Welcome message
* Updated links to LPDAAC product description pages
* Changed all "print" and "cat" calls to show messages/warnings to "message" or
  "warning" to allow easy disabling MODIStsp verbose messages
* Using "R" tempfile/tempdir to save vrt files

## Bug Fixes

* Corrected a bug that threw an error in case incorrect bounding box specified

________________________________________________________________________________


# MODIStsp 1.2.0 Release Notes

v1.2.0 was released on 29/07/2015

## Major changes

First stable release of advanced implementation of MODIStsp !
We know it should be 1.0.0, but that's it !
