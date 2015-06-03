# LB_MOD_DWL
Proj for MOD dwl and preprocess

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

### Non Interactive mode (Exploiting a previously saved options file)
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

Also these links can be launched in interactive mode calling launching them without parameters (or double-clicking them);
to run in non-interactive mode, use the following syntax:
* Linux:
```console
MODIStsp -g -s "/yourpath/youroptions.RData"
```
* Windows:
```bat
C:\Users\you\Desktop\MODIStsp -g -s "X:/yourpath/youroptions.RData"
```
(see `C:\Users\you\Desktop\MODIStsp -h` for details).