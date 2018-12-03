
[![](https://www.r-pkg.org/badges/version-ago/MODIStsp)](https://cran.r-project.org/package=MODIStsp)
[![](http://cranlogs.r-pkg.org/badges/MODIStsp)](https://cran.r-project.org/package=MODIStsp)
[![Travis-CI Build
Status](https://travis-ci.org/ropensci/MODIStsp.svg?branch=master)](https://travis-ci.org/ropensci/MODIStsp)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1286981.svg)](https://doi.org/10.5281/zenodo.1286981)
[![Coverage
Status](http://img.shields.io/codecov/c/github/ropensci/MODIStsp/master.svg)](http://codecov.io/github/ropensci/MODIStsp?branch=master)
[![](http://badges.ropensci.org/184_status.svg)](https://github.com/ropensci/onboarding/issues/184)

# MODIStsp <img src="man/figures/logo.png" width="147" height="170" align="right" />

[MODIStsp](http://ropensci.github.io/MODIStsp/) is a “R” package devoted
to automatizing the creation of time series of rasters derived from
MODIS Land Products data. MODIStsp allows to perform several
preprocessing steps (e.g., download, mosaicing, reprojection and resize)
on MODIS data available within a given time period. Users have the
ability to select which specific layers of the original MODIS HDF files
they want to process. They also can select which additional Quality
Indicators should be extracted from the aggregated MODIS Quality
Assurance layers and, in the case of Surface Reflectance products, which
Spectral Indexes should be computed from the original reflectance bands.
For each output layer, outputs are saved as single-band raster
filescorresponding to each available acquisition date. Virtual files
allowing access to the entire time series as a single file can be also
created. All processing parameters can be easily selected with a
user-friendly GUI, although non-interactive execution exploiting a
previously created Options File is possible. Stand-alone execution
outside an “R” environment is also possible, allowing to use scheduled
execution of MODIStsp to automatically update time series related to a
MODIS product and extent whenever a new image is available.

**For more information, documentation and examples of use, please see
MODIStsp website at
[ropensci.github.io/MODIStsp](http://ropensci.github.io/MODIStsp/)**

<a href="http://www.irea.cnr.it/en/">
<img src="man/figures/logo_irea.png" height="40" align="left" /></a>

***`MODIStsp` is developed and maintained by Lorenzo Busetto and Luigi
Ranghetti, [Institute of Remote Sensing of
Environment](http://www.irea.cnr.it/en/) - National Research Council -
Italy (CNR-IREA)***

## Citation

To cite `MODIStsp` please use:

L. Busetto, L. Ranghetti (2016) MODIStsp: An R package for automatic
preprocessing of MODIS Land Products time series, Computers &
Geosciences, Volume 97, Pages 40-48, ISSN 0098-3004,
<http://dx.doi.org/10.1016/j.cageo.2016.08.020>, URL:
<https://github.com/ropensci/MODIStsp>.

## Important News \!

  - 29/11/2018 - We recently discovered a nasty bug in the computation
    of some custom spectral indices (those including additions /
    subtractions on reflectance values, such as in b1\_NIR+0.1) /
    b2\_Red. See [here](articles/discovered_bug.html) for further
    details\! The bug is fixed as of version 1.3.7.

  - 07/08/2018 - We are glad to report that MODIStsp is now included in
    the [rOpenSci](https://ropensci.org/about/) packages’ ecosystem. We
    thank reviewers Leah Wasser and Jeffrey Hanson for their valuable
    reviews, which helped us to further improve the package\!

  - 10/07/2018 - MODIStsp v. 1.3.6 is out. Check out the [Release
    Notes](https://github.com/ropensci/MODIStsp/releases/tag/1.3.6) for
    further details \!

  - 20/06/2018 - MODIStsp v. 1.3.5 is out. Check out the [Release
    Notes](https://github.com/ropensci/MODIStsp/releases/tag/v1.3.5) for
    further details \!

  - 11/04/2018 - Due to new NASA Policies the MODIS FTP servers were
    shut down starting, April 2, 2018. **FTP download is therefore no
    longer working** and will be removed in the next MODIStsp version\!

  - 11/04/2018 - [**Decommissioning of MODIS Version 5 Land Data
    Products**](https://lpdaac.usgs.gov/about/news_archive/decommissioning_modis_version_5_land_data_products_april_9_2018_second_notice).
    As per NASA notice above, MODIS v005 products are going to be
    decommisioned, and will soon be no longer available for download.
    Support for those products will be removed in the next MODIStsp
    version\!.

  - 11/08/2017 - MODIStp 1.3.3 was released today. It provides
    improvements in processing speed, as well as the usual bug fixes
    (thanks to all the users that signalled problems \!). Check the
    [Release
    Notes](https://github.com/ropensci/MODIStsp/releases/tag/v1.3.3) for
    further details \!

  - 25/07/2017 - As of today, **most of the content related to MODIStsp
    has been moved to our new website at
    [ropensci.github.io/MODIStsp](http://ropensci.github.io/MODIStsp/)
    **, which provides a much better user interface and ease of access
    to MODIStsp-related information. From now on, please **consult the
    new website for detailed and updated information on the package**.

  - Also our previous FAQ page on github containing info for solving
    common installation, downloading and processing problems and issues
    was discontinued and **migrated at
    [ropensci.github.io/MODIStsp/articles/faq.html](http://ropensci.github.io/MODIStsp/articles/faq.html)**.

## Problems and Issues

  - Please **report any issues** you may encounter in our [issues page
    on github
    <i class="fa fa-github-square" aria-hidden="true"></i>](https://github.com/ropensci/MODIStsp/issues).

## <i class="fa fa-desktop" aria-hidden="true"></i> System Requirements

`MODIStsp` requires [R](https://cran.r-project.org) v \>= 3.2.1 and
[GDAL](http://www.gdal.org) (Geospatial Data Abstraction Library) v \>=
1.11.1 **with support for HDF4 raster format** to be installed in your
system. Brief instructions for installing R and GDAL can be found
[HERE](http://ropensci.github.io/MODIStsp/articles/installation.html#installing-r-and-gdal).

-----

# Installation Instructions

## <i class="fa fa-windows" aria-hidden="true"></i> Installing on Windows

You can install the stable version of `MODIStsp` from CRAN:

`install.packages("MODIStsp")`

, or the development version (containing the latest improvements and bug
fixes) from github:

``` r
library(devtools)
install_github("ropensci/MODIStsp")
```

Note that **if the `GTK+` library is not already installed on your
system, installation may fail**. In that case, please install and load
the `gWidgetsRGtk2` library beforehand:

``` r
install.packages("gWidgetsRGtk2")
library(gWidgetsRGtk2)
```

Upon loading `gWidgetsRGtk2`, an error window will probably appear. This
signals that library “GTK+” is not yet installed on your system or is
not on your PATH. To install it press “OK”. A new window dialog window
will appear, asking if you. want to install “GTK+”. Select “Install GTK”
and then “OK” . Windows will download and install the GTK+ library. When
it finishes, the RSession should be restarted and you should be ready to
go \!

In case RStudio does not automatically restart or continuously asks to
install GTK+ again, kill it form “Task Manager” (or restart the R
session from RStudio “Session” menu), reload RStudio and the try to
reload `gWidgetsRGtk2`. If it loads correctly, you should be ready to
go.

If it still fails, try downloading the GTK+ bundle
from:

<http://ftp.gnome.org/pub/gnome/binaries/win64/gtk+/2.22/gtk+-bundle_2.22.1-20101229_win64.zip>
(OR
<http://ftp.gnome.org/pub/gnome/binaries/win32/gtk+/2.22/gtk+-bundle_2.22.1-20101227_win32.zip>
if on Win32)

, unzip the archive on a folder of your choice (e.g., `C:\\Program
Files\\GTK+`), then add the path to its “bin” subfolder (e.g.,
`C:\\Program Files\\GTK+\\bin\\` to your system PATH environment
variable.

Restart your system and try loading again `gWidgetsRGtk2`: if it loads
ok, you should be ready to install
`MODIStsp`

## <i class="fa fa-linux" aria-hidden="true"></i> Installing on Linux Systems

To install `MODIStsp` on Linux, you have to first install the following
required dependencies:

  - `Cairo` \>= 1.0.0, `ATK` \>= 1.10.0, `Pango` \>= 1.10.0, `GTK+` \>=
    2.8.0, `GLib` \>= 2.8.0 (required by package `RGtk2`)
  - `Curl` (required by package `curl`)
  - `GDAL` \>= 1.6.3, `PROJ.4` \>= 4.4.9 (required by package `rgdal`)

On *Debian and Ubuntu-based* systems, to install those packages open a
terminal and
type:

``` bash
sudo apt-get install r-cran-cairodevice r-cran-rgtk2 libcairo2-dev libatk1.0-dev libpango1.0-dev 
libgtk2.0-dev libglib2.0-dev libcurl4-openssl-dev libgdal-dev libproj-dev
```

On *rpm-base systems*, to install packages open a terminal and
type:

``` bash
sudo yum install libcairo2-devel libatk1.0-devel libpango1.0-devel gtk2 gtk2-devel 
glib2-devel libcurl-devel gdal-devel proj proj-devel proj-epsg proj-nad
```

Then, you can install the stable version of MODIStsp from CRAN:

``` r
install.packages("MODIStsp")
```

, or the development version (containing the latest improvements and bug
fixes) from github;

``` r
library(devtools)
install_github("ropensci/MODIStsp")
```

## <i class="fa fa-apple" aria-hidden="true"></i> Installing on Mac

**NOTE**: The following installation notes should be valid for MODIStsp
installation on R 3.4.0 and above with Mac OSX Sierra. They were mainly
taken (i.e., blatantly copied…) from:
<https://zhiyzuo.github.io/installation-rattle/>. Thanks to Zhiya Zuo
for providing this\!

To properly install `MODIStsp` you will need to first install package
`RGTk2`. This is a somehow difficult operation. The following
instructions should help: <br>

**1. Check your Mac OS X version and update if necessary: **

Enter the following command in terminal to check your macOS version.
Expected output is as below the dashed line —.

``` 
~$ sw_vers  
------------------------  
ProductName:    Mac OS X  
ProductVersion: 10.12.6  
BuildVersion:   16G29  
```

If your system is above 10.11, continue. Otherwise, upgrade it to
Sierra.

Install homebrew if you do not have it already installed. homebrew is a
very convenient package manager for macOS. To do so, open a terminal,
copy the following command in it and hit
Enter:

``` bash
~$ /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

Follow the instructions to get brew ready. When inserting your password,
nothing will show up for security reasons. Just hit Enter when you are
finished.

When brew is finished, copy the following command in terminal and hit
Enter:

``` bash
~$ touch ~/.bash_profile
~$ echo "export PATH=/usr/local/bin:$PATH
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/local/lib/pkgconfig/gtk+-2.0.pc:/opt/X11/lib/pkgconfig" >> ~/.bash_profile
~$ source ~/.bash_profile
```

<br>

**2. Install the `cairo` library with x11 support**. Enter the following
into your terminal:

``` bash
~$ brew uninstall --force cairo --ignore-dependencies
~$ brew cask install xquartz
~$ brew install --with-x11 cairo
```

<br>

**3. Install the `gtk+` library**:

To do so, you first have to change the way homebrew wants to install
gtk+. In an editor, write:

``` bash
~$ brew edit gtk+
```

A text editor will open. Look in the file, and find a section that
begins with “def install”. Substitute the current `args` section with
the following text:

``` bash
def install
 args = [
         "--disable-dependency-tracking",
         "--disable-silent-rules",
         "--prefix=#{prefix}",
         "--disable-glibtest",
         "--enable-introspection=yes",
         # "--disable-visibility",
         # "--with-gdktarget=quartz",
         "--with-gdktarget=x11",
         "--enable-x11-backend"
        ]
```

Save the modified file using `ctrl+x ctrl+c`, followed by `y` to quit
emacs. Now install the library using:

``` bash
~$ brew install --build-from-source --verbose gtk+
```

<br>

**4. Update your path** so that `gtk+` is recognized,
using:

``` bash
~$ export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/local/lib/pkgconfig/gtk+-2.0.pc:/opt/X11/lib/pkgconfig
```

<br>

**5. Install `RGtk2` from source**:

  - **Download the newest source file for RGtk2** from
    <https://CRAN.R-project.org/package=gWidgetsRGtk2>.

  - Assuming that the path to this file is ~/Downloads. Run the
    following in terminal (change the path if you did not download in
    ~/Downloads):

<!-- end list -->

``` bash
~$ cd ~/Downloads
~/Downloads$ R CMD INSTALL RGtk2_2.20.33.tar.gz
```

(Note that the name of the tar.gz file may vary depending on when you
downloaded the file).

**6. Open R and run**:

``` r
library(RGtk2)
```

hopefully, `RGtk2` will load without errors\! If so, you should be ready
to go, and you can:

**7. Install MODIStsp** from CRAN:

``` r
install.packages("MODIStsp")
MODIStsp()
```

or the development version from GitHub:

``` r
library(devtools) 
install_github("ropensci/MODIStsp", ref = "master")
MODIStsp()
```

Good luck\!

# Usage

The easiest way to use `MODIStsp` is to use its powerful GUI (Graphical
User Interface) for selection of processing options, and then run the
processing.

To open the GUI, load the package and launch the MODIStsp function, with
no parameters:

``` r
library(MODIStsp)
MODIStsp()
```

This **opens a GUI** from which processing options can be specified (and
eventually saved or loaded). After specifying all required parameters,
clicking on “Start” will start the processing (see
[HERE](http://ropensci.github.io/MODIStsp/articles/interactive_execution.html)
for more detailed instructions).

`MODIStsp` can also be launched in non-interactive mode within an `R`
session or script by setting the optional `GUI` parameter to FALSE, and
the `options_file` parameter to the path of a previously saved JSON
Options file. This allows to exploit `MODIStsp` functionalities within
generic “R” processing scripts.

``` r
library(MODIStsp) 
# --> Specify the path to a valid options file saved in advance from MODIStsp GUI 
options_file <- "X:/yourpath/youroptions.json" 
  
# --> Launch the processing
MODIStsp(gui = FALSE, options_file = options_file)
```

(see
[HERE](http://ropensci.github.io/MODIStsp/articles/noninteractive_execution.html)
for more detailed instructions and examples).

# Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its
terms.

[![ropensci\_footer](http://ropensci.org/public_images/github_footer.png)](https://ropensci.org)
