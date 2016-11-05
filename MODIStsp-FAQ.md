#MODIStsp - Frequently Asked Questions

Here you can find possible **solutions for common MODIStsp problems**. We will add to this page as soon as additional problems are reported. If these solutions doesn't work for you, or you have a different problem, signal it at: https://github.com/lbusett/MODIStsp/issues

-   [Installation Problems](#Installation Problems)
    -   [On Linux, installation fails due to some packages missing](#Missing-packages)
    -   [On Windows, the program hangs at first interactive execution, while attemping to install gWisgets2RGtk2](#gWidgets-error)
    -   [MODIStsp hangs at first execution, while looking for `gdal` installation](#gdal-error)
    -   [   Installing from github fails due to missing dependencies](#github-error)

-   [Data download Problems](#download-errors)
    -   [On 'http' download, files are not getting downloaded, OR you get 'timeout' problems](#timeout-error)

-   [Processing Problems](#processing-errors)
    -   [MODIStsp fails while processing product **MxDyyy**](#product-error)
    -   [I can't find the processed files](#whereare-error)

- [Other Problems](#other-errors)

<a name="Installation Problems"></a>

## Installation Problems 

<a name="Missing-packages"></a>

#### - On Linux, installation fails due to some packages missing

Please Install the following required dependencies: 
    * Cairo >= 1.0.0, ATK >= 1.10.0, Pango >= 1.10.0, GTK+ >= 2.8.0, GLib >= 2.8.0 (required by package ```RGtk2```)
    * Curl (required by package ```curl```)
    * GDAL >= 1.6.3, PROJ.4 >= 4.4.9 (required by package ```rgdal```)
    
On **Debian and Ubuntu-based systems**, to install packages open a terminal and type:  
```
  sudo apt-get install r-cran-cairodevice r-cran-rgtk2 libcairo2-dev libatk1.0-dev libpango1.0-dev 
  libgtk2.0-dev libglib2.0-dev libcurl4-openssl-dev libgdal-dev libproj-dev
```

On **rpm-based systems**, to install packages open a terminal and type;  
```
   sudo yum install libcairo2-devel libatk1.0-devel libpango1.0-devel gtk2 gtk2-devel 
   glib2-devel libcurl4-devel gdal-devel proj-devel
```
On **others distros**: we didn't test installation on other distros yet - if you experience problems please contact us.

<a name="gWidgets-error"></a>

#### - On Windows, the program hangs at first interactive execution, while attemping to install gWisgets2RGtk2

 At first interactive execution (i.e., with "gui = TRUE) of `MODIStsp`, an error window will probably appear. **Don't worry!** This is just signaling that  _libatk-1.0-0.dll_ is missing from your system. This is due to the fact that library "GTK+"" is not yet installed on your system and needs to be installed. To do so, press "OK". A new window dialog window will appear, asking if you want to install "GTK+". Select *"Install GTK+"* and then *"OK"*. Windows will download and install the GTK+ library. When it finishes, the RSession should be restarted and next `MODISts` should go well ! In case 
RStudio doesn't automatically restart after installimg GTK+, simply kill it form "Task Manager" ad reload RStudio.

<a name="gdal-error"></a>

#### - MODIStsp hangs at first execution, while looking for `gdal` installation

At the first execution `MODIStsp` searches for a valid GDAL installation. If nothing happens for a long time (e.g., several minutes), `MODIStsp` (and in particular the gdalUtils package on which it relies) is not finding a valid GDAL installation in the more common locations. To solve the problem:

1. Ensure that GDAL is properly installed in your system. See the main MODIStsp gitHub page for simple instructions
2. (On Windows) If it is installed, verify that GDAL is in your system PATH, and that the _GDAL\_DATA_ environment variable is correctly set (You can find simple instructions [HERE](http://gisforthought.com/setting-up-your-gdal-and-ogr-environmental-variables/)) (If gdal is not correctly installed or the path not set, then opening a windows shell ("cmd") and issuing the "gdalinfo" command will result in an error !)

If nothing works, please report the issue here: https://github.com/lbusett/MODIStsp/issues

<a name="github-error"></a>

#### - Installing from github fails due to missing dependencies

There are currently some problems in installing MODIStsp **via install_github** on R >= 3.3.1 due to not correct installation of dependencies (related to a bug in CRAN version of `install_github`). Installing the development version of `devtools` should solve the issue. To do so, on a **clean** R/RStudio session do:

```{r, eval=FALSE, message=FALSE, warning=FALSE, include=TRUE}
  install.packages(c("devtools"))
  devtools::install_github("hadley/devtools")
  library(devtools)
```

, then continue with standard `MODIStsp` installation. 

If you have problems in installing the "devel" version of detools, manually installing all the dependencies should also solve the issue. To do so, please try doing: 
  
  ```
  install.packages(c("bitops", "data.table", "gdalUtils", "gWidgets", "hash", "plyr", "raster", "RCurl",
  "rgdal", "rgeos", "xts", "XML", "xml2", "sp", "stringr", "httr", "RJSONIO","chron","iterators", "digest"))
  ```

 , then continue with standard `MODIStsp` installation.
 
If you still don't succedd, please contact us ! 

<a name="download-errors"></a>

# Data download Problems <a name="Data download Problems"></a>

<a name="timeout-error"></a>

#### - On 'http' download, files are not getting downloaded, OR you get 'timeout' problems

1. _Is it wednesday ???_ If so, NASA http server may be down for maintainence. **Try switching to "ftp" download**;
2. Verify that the **username and password** you provided are correct (Those can be obtained by **registering an account** at: https://urs.earthdata.nasa.gov/profile.);
3. In some cases, access to the http server seems to be not allowed (we don't know why - maybe firewalling). **Try switching to ftp download**.

If nothing works, please report the issue here: https://github.com/lbusett/MODIStsp/issues

<a name="processing-errors"></a>

# Processing Problems

<a name="product-error"></a>

#### - MODIStsp fails while processing product **MxDyyy**

Although we tried to test functionality for all products, some bugs may be still present for specific products, due to the complexity and variability of MODIS hdfs structure. In that case, please report the issue here: https://github.com/lbusett/MODIStsp/issues 

<a name="whereare-error"></a>

#### - I can't find the processed files

Output raster files are saved **in a subfolder of the main output folder named after the selected product**. Within that, a _separate subfolder is created for each processed original MODIS layer, Quality Indicator or Spectral Index_. Each subfolder contains one image for each processed date, created according to the following naming conventions: 

"ProdCode"\_"Layer"\_"YYYY"\_"DOY"."ext"               _(e.g.,MOD13Q1_NDVI_2000_065.dat)_

**ProdCode** is the code name of the MODIS product from which the image was derived (e.g., MOD13Q1), **Layer** is a short name describing the dataset (e.g., b1_Red, NDII, UI), **YYYY** and **DOY** corresponds to the year and DOY (Day of the Year) of acquisition of the original MODIS image, and ext is the file extension (.tif for GTiff outputs, or .dat for ENVI outputs). 

ENVI and/or GDAL virtual time series files and _RasterStack_ RData objects are instead stored in the "Time\_Series" subfolder.

Naming convention for these files is as follow:

"ProdCode"\_"Layer"\_"StartDOY"\_"StartYear\_"EndDOY"\_"EndYear2\_"suffix".ext" 

_(MOD13Q1\_NDVI\_49\_2000\_17\_2015\_RData.dat)_
             
where _suffix_ indicates the type of virtual file (ENVI, GDAL or RData), while _StartDOY_, _StartYear_, _EndDOY_ and _EndYear_ indicate the temporal extent of the time serie created.

<a name="other-errors"></a>

# Other Problems

Please report any other issues at https://github.com/lbusett/MODIStsp/issues