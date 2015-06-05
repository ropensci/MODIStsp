# MODIStsp

MODIStsp is devoted to automatize the creation of time series of rasters derived from MODIS Land Products data.  MODISTSP allows to perform several preprocessing steps (e.g., download, mosaicking, reprojection and resize) on MODIS data available within a given time period. Users have the ability to select which specific layers of the original MODIS HDF files they want to process. They also can select which additional Quality Indicators should be extracted from the aggregated MODIS Quality Assurance layers and, in the case of Surface Reflectance products, which Spectral Indexes should be computed from the original reflectance bands. For each output layer, outputs are saved as single-band raster files corresponding to the different available acquisition dates. Virtual files allowing access to the entire time series as a single file are also created. All processing parameters can be easily selected with a user-friendly GUI. Command-line execution is also possible, allowing to use MODISTSP to automatically update time series related to a MODIS product whenever a new image is made available.^

## Installation

1. Within "R", install the "gWidgetsRGtk2" package 
```r
install.packages('gWidgetsRGtk2')
```
   when asked, request to install GTK
2. Install the package from GitHub. (You'll need to have the "devtools" package installed and loaded)
```r
install.packages('devtools')
require('devtools')
install_github('lbusett/LB_MOD_DWL',ref = 'devel')
```

## Running the tool

### Interactive mode
To run the tool in interactive mode, load the package and launch the MODIS_tsp function, with no parameters
```r
library(MODIStsp)
MODIStsp()
```

### Non Interactive mode
(Exploiting a previously saved options file)

To run the tool without GUI interaction: 
```r
library(MODIStsp) 
options_file = "X:/yourpath/youroptions.RData"  # --> Specify the path to a valid options file saved in advance from the GUI
MODIStsp(gui = FALSE, options_File = options_file)
```

### Standalone tool
The tool can be also launched as a standalone application; to do it, from R launch the function `MODIStsp_install_launcher()`.
In a Linux operating system this function creates a desktop entry (accessible from the menu in the sections "Science" and "Geography")
and a symbolic link in a known path (default: /usr/bin/MODIStsp).
In Windows, a link in the Start Menu and optionally a desktop shortcut are created.
See `?MODIStsp_install_launcher` for details and path customisations.

Also these links can be launched in interactive mode launching them without parameters (or double-clicking them);
to run in non-interactive mode, use the following syntax:
* Linux: `MODIStsp -g -s "/yourpath/youroptions.RData"`
  (see `MODIStsp -h` for details).
* Windows:`C:\Users\you\Desktop\MODIStsp -g -s "X:/yourpath/youroptions.RData"`
  (see `C:\Users\you\Desktop\MODIStsp -h` for details).

If you do not want to install any link, launchers can be found in the subdirectory MODIStsp/ExtData/Launcher of your library path.

### Batch Processing
MODIStsp allows to automatically update the time series of a selected MODIS product whenever a new image is available. To periodically and automatically update time series of a selected product without GUI interaction, you shuold simply:

1)	Open the MODIStsp GUI, define the parameters of the processing specifying a date in the future as the “Ending Date” and save the processing options as an RData file. Then quit the program. 

2) Schedule the execution of the MODIStsp.bat (for Windows) or MODIStsp.sh (for Linux) batch execution scripts available in the “Inst/batch” subfolder of the package installation as a windows scheduled task or linux “cron” job, specifying the path of the previously saved Options file as additional "-s" argument, such as in: 

  > MODIStsp.sh -g –s “c:/Temp/Test_Options.RData”.
  



 
