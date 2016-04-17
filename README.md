# Table of contents
1. [MODIStsp](#Installation)
2. [Some paragraph](#paragraph1)
    1. [Sub paragraph](#subparagraph1)
3. [Another paragraph](#paragraph2)

# MODIStsp

MODIStsp is a "R" package devoted to automatizing the creation of time series of rasters derived from MODIS Land Products data.  MODIStsp allows to perform several preprocessing steps (e.g., download, mosaicking, reprojection and resize) on MODIS data available within a given time period. Users have the ability to select which specific layers of the original MODIS HDF files they want to process. They also can select which additional Quality Indicators should be extracted from the aggregated MODIS Quality Assurance layers and, in the case of Surface Reflectance products, which Spectral Indexes should be computed from the original reflectance bands. For each output layer, outputs are saved as single-band raster files corresponding to each available acquisition date. Virtual files allowing access to the entire time series as a single file can be also created. All processing parameters can be easily selected with a user-friendly GUI, although non-interactive execution exploiting a previously created Options File is possible. Stand-alone execution outside an "R" environment is also possible, allowing to use scheduled execution of MODIStsp to automatically update time series related to a MODIS product and extent whenever a new image is available. 
`?/
An extended description of MODIStsp functionalities is provided in the [package vignette](https://github.com/lbusett/MODIStsp/blob/devel/inst/doc/MODIStsp.pdf) (_Click on "Raw" at the beginning of the document to download the pdf file_) 

## Installation <a name="Installation"></a> 

1. Within "R", install the "gWidgetsRGtk2" package

    ```r
    install.packages('gWidgetsRGtk2')
    ```
    when asked, request to install GTK

2. Install the package from GitHub. (You'll need to have the "devtools" package installed and loaded)

    ```r
    install.packages('devtools')
    require('devtools')
    install_github('lbusett/MODIStsp', ref = 'master')
    ```
    
## Dependencies

## Running the tool

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

[![Travis-CI Build Status](https://travis-ci.org/lbusett/MODIStsp.svg?branch=master)](https://travis-ci.org/lbusett/MODIStsp)
