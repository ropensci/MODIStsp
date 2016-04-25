[![Travis-CI Build Status](https://travis-ci.org/lbusett/MODIStsp.svg?branch=devel)](https://travis-ci.org/lbusett/MODIStsp)

# Table of contents
1. [MODIStsp](#MODIStsp)
2. [Installation](#Installation)
3. [Dependencies](#Dependencies)
3. [Running the Tools](#Running)

# MODIStsp <a name="MODIStsp"></a>

MODIStsp is a "R" package devoted to automatizing the creation of time series of rasters derived from MODIS Land Products data.  MODIStsp allows to perform several preprocessing steps (e.g., download, mosaicking, reprojection and resize) on MODIS data available within a given time period. Users have the ability to select which specific layers of the original MODIS HDF files they want to process. They also can select which additional Quality Indicators should be extracted from the aggregated MODIS Quality Assurance layers and, in the case of Surface Reflectance products, which Spectral Indexes should be computed from the original reflectance bands. For each output layer, outputs are saved as single-band raster files corresponding to each available acquisition date. Virtual files allowing access to the entire time series as a single file can be also created. All processing parameters can be easily selected with a user-friendly GUI, although non-interactive execution exploiting a previously created Options File is possible. Stand-alone execution outside an "R" environment is also possible, allowing to use scheduled execution of MODIStsp to automatically update time series related to a MODIS product and extent whenever a new image is available. 

An extended description of MODIStsp functionalities is provided in the [package vignette](https://github.com/lbusett/MODIStsp/blob/devel/inst/doc/MODIStsp.pdf) (_Click on "Raw" at the beginning of the document to download the pdf file_) 

## Installation <a name="Installation"></a> 

**IMPORTANT:** _MODIStsp_ requires [R](http://cran.r-project.org) v ≥ 3.2.1 and [GDAL](http://www.gdal.org) (Geospatial Data Abstraction Library) v ≥ 1.11.1 To be installed in your system. Brief instructions for installing R and GDAL can be found [HERE](#gdal_inst).

1. Install and load the ```gWidgetsRGtk2``` package:
    ```r
    install.packages("gWidgetsRGtk2")
    library(gWidgetsRGtk2)
    ```
    
    On Windows, upon loading the package, an error window will probably appear. **Don’t worry!** This is just signaling that  _libatk-1.0-0.dll_ is missing from your system. This is due to the fact that library “GTK+” is not yet installed on your system and needs to be installed. To do so, press “OK”. A new window dialog window will appear, asking if you want to install “GTK+”. Select “Install GTK+” and then “OK”. Windows will download and install the GTK+ library. When it finishes, the RSession will be restarted and you should be ready to go!<sup id="a1">[1](#f1)</sup>
    
2. **Only for Linux users:** install the dependencies requested by MODIStsp package and relative package dependencies: 
    * Cairo ≥ 1.0.0, ATK ≥ 1.10.0, Pango ≥ 1.10.0, GTK+ ≥ 2.8.0, GLib ≥ 2.8.0 (requested from library ```RGtk2```)
    * Curl (requested from library ```curl```)
    * GDAL ≥ 1.6.3, PROJ.4  ≥ 4.4.9 (requested from library ```rgdal```)
    
    On Debian-based systems, to install packages open a terminal and type  
    ```bash
    sudo apt-get install r-cran-cairodevice r-cran-rgtk2 libcurl4-openssl-dev libgdal-dev libproj-dev
    ```
    
3. Install MODIStsp package from GitHub. (You'll need to have the "devtools" package installed and loaded)
    ```r 
    install.packages("devtools")
    library(devtools)
    install_github("lbusett/MODIStsp")
    ```

## Dependencies <a name="Dependencies"></a> 

MODIStsp exploits functionalities of several other "R" packages. In particular, the following packages are imported:  
``` bitops``` (≥ 1.9.6), ``` data.table``` (≥ 1.9.6), ``` gdalUtils``` (≥ 2.0.1.7), ```gWidgetsRGtk2``` (≥ 0.0-54), ```hash``` (≥ 2.2.6), ```plyr``` (≥ 1.8.3), ```raster``` (≥ 2.5-2), ```RCurl``` (≥ 1.95-4.8), ```rgdal``` (≥ 1.1-8), ```rgeos``` (≥ 0.3-8), ```xts``` (≥ 1.0-10), ```sp``` (≥ 1.2-2), ```stringr``` (≥ 1.0.0), ```XML``` (≥ 3.98-1.1).


## Running the tool <a name="Running"></a> 

### Interactive mode
To run the tool in interactive mode, load the package and launch the MODIS_tsp function, with no parameters
```r
library(MODIStsp)
MODIStsp()
```
This will open a GUI from which processing options can be specified and eventually saved (or loaded) (see the package PDF vignette for details)

### Non Interactive mode
(Exploiting a previously saved options file)

MODIStsp can be also launched in non-interactive mode by setting the optional "GUI" parameter to FALSE, and the "Options_File" parameter to the path of a previously saved Options file.  This allows to exploit MODIStsp functionalities within generic "R" processing scripts

```r
library(MODIStsp) 
options_file = "X:/yourpath/youroptions.RData"  # --> Specify the path to a valid options file saved in advance from the GUI
MODIStsp(gui = FALSE, options_File = options_file)
```

Specifying also the "spatial_file_path" optional parameter overrides the output extent of the selected Options File. This allows to perform the same preprocessing on different extents using a single Options File, by looping on an array of spatial files representing the desired output extents.

For example:
```r
# Create a character array containing a list of shapefiles (or other spatial files)
extent_list = list.files("X:/path/containing/some/shapefiles/", "\\.shp$")  

# loop on the list of spatial files and run MODIStsp using each of them to automatically 
# define the output extent (A separate output folder is created for each input spatial file).

for (single_shape in extent_list) {
  MODIStsp(gui = FALSE, options_File = "X:/yourpath/youroptions.RData", spatial_file_path = single_shape )
}
```

### Standalone execution and scheduled processing 

MODIStsp can be also executed as a standalone application ; to do it, from R launch the function `MODIStsp_install_launcher()`.
In a Linux operating system this function creates a desktop entry (accessible from the menu in the sections "Science" and "Geography")
and a symbolic link in a known path (default: /usr/bin/MODIStsp).
In Windows, a link in the Start Menu and optionally a desktop shortcut are created.
See `?install_MODIStsp_launcher` for details and path customisations.

Double-clicking the files or launching them from a shell without parameters launches MODIStsp in interactive mode. Non-interactive mode is triggered by adding the "-g"  argument to the call, and specifying the path to a valid Options File as "-s" argument:

* Linux: `MODIStsp -g -s "/yourpath/youroptions.RData"`
  (see `MODIStsp -h` for details).
* Windows:`C:\Users\you\your_r_library\MODIStsp\ExtData\Launcher\MODIStsp.bat -g -s "X:/yourpath/youroptions.RData"`
  (see `C:\Users\you\Desktop\MODIStsp -h` for details).

If you do not want to install any link, launchers can be found in the subdirectory "MODIStsp/ExtData/Launcher" of your library path.


Standalone non-interactive execution can be used to periodically and automatically update the time series of a selected product over a given study area. To do that, the user should simply:
1)	Open the MODIStsp GUI, define the parameters of the processing specifying a date in the future as the "Ending Date" and save the processing options. Then quit the program
 
2. Schedule non-interactive execution of the launcher installed as seen before (or located in the subdirectory "MODIStsp/ExtData/Launcher" of your library path) as windows scheduled task (or linux "cron" job) according to a specified time schedule, specifying the path of a previously saved Options file as additional argument
:
    * Linux: edit your crontab by opening a terminal and typing

        ```bash
        crontab -e
        ```
        
        Then add an entry for the launcher. For example, if you have installed it in /usr/bin and you want to run the tool every day at 23.00, add the following row:
        
        ```bash
        0 23 * * * /bin/bash /usr/bin/MODIStsp -g -s "/yourpath/youroptions.RData"
        ```
        
    * Windows: create a Task following [these instructions](https://technet.microsoft.com/en-us/library/cc748993.aspx); add the path of the MODIStsp.bat launcher as Action (point 6), and specify  `-g -s "X:/yourpath/youroptions.RData"` as argument.
    
<a name="gdal_inst"/>

## Installing R and GDAL

### Installing R

#### Windows

Download and install the latest version of R which can be found [here](https://cran.r-project.org/bin/windows/base).

#### Linux

Please refer to the documentation which can be found [here](https://cran.r-project.org/bin/linux), opening the directory relative to the user Linux distribution. The documentation provides instruction to add CRAN repositories and to install the latest R version.
With Ubuntu 15.10 Wily (and newer) this step is not mandatory (altough recommended), since packaged version of R is ≥ 3.2.1 (although not the latest); in this case, user can insall R by simply typing in a terminal
```bash
sudo apt-get install r-base
```

### Installing GDAL ≥ 1.11.1

#### Windows

The easiest way to install GDAL on Windows is from the [OSGeo4W Website](https://trac.osgeo.org/osgeo4w/)

1. Open the [OSGeo4W Website](https://trac.osgeo.org/osgeo4w/)
2. In the **Quick Start for OSGeo4W Users** section, select the download of 32bit or 64bit of OSGeo4W network installer
3. Run the installer
  + _Easiest Option_: 
    +Select **Express Desktop Install**, then proceed with the installation. This will install GDAL and also other useful Spatial Processing softwares like [QGIS](http://www.qgis.org/) and [GRASS GIS](https://grass.osgeo.org/)
  + _Advanded Option_: 
    +  Select **Advanced Install**, then click on "Next" a few times until you reach the "Select Packages" screen. 
    +  Click on "Commandline_Utilities_", and on the list look for "_gdal: The GDAL/OGR library..." entry
    +  Click on "Skip": the word "skip" will be replaced by the current GDAL version number
    +  Click on "Next" a few times to install GDAL
    
#### Debiam and Ubuntu-based systems

1. Ensure that your repositories contains a version of ```gdal-bin``` ≥ 1.11.1. 
    In particular, official repositories of Ubuntu 15.04 Vivid (or older) and Debian Jessie (or older) provide older versions of GDAL, so it is necessary to add UbuntuGIS-unstable repository before installing. To do it, follow instructions [here](https://launchpad.net/~ubuntugis/+archive/ubuntu/ubuntugis-unstable)). 
    With Ubuntu 15.10 Wily (and newer) this step is not mandatory, altough recommended in order to have updated version of GDAL installed.
    
2. To install GDAL a terminal and type  
    ```bash
    sudo apt-get install gdal-bin
    ```
    
#### ArchLinux

GDAL is maintained updated to the latest version as binary package within the community repository; although that, the support for HDF4 format is not included. 
To bypass this problem, ArchLinux users can install ```gdal-hdf4``` package from AUR. 
This package is updated manually after each release of ```gdal``` on the community repository, a temporal shift between a new ```gdal``` release and the update of ```gdal-hdf4``` could happen.
The user which want to manually add the support for HDF4 in case ```gdal-hdf4``` is out-of-date can do it following [these instructions](https://notehub.org/fctdn).

#### Other Linux systems

Install the packaged binary of GDAL included in your specific distribution; if the version is older than 1.11.1, or if the support for HDF4 format is not included, the user probably must manually compile the source code. 
To do it, add the parameter ```--with-hdf4``` to ```configure``` instruction.


<b id="f1">1</b> If you encounter problems installing the ``` gWidgetsRgtk2 ``` library, please signal it in the [issues](https://github.com/lbusett/MODIStsp/issues) GitHub page of MODIStsp and we'll try to help you! [↩](#a1)

