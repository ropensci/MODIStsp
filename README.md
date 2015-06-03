# LB_MOD_DWL
Proj for MOD dwl and preprocess

## Installation

1. Within "R", install the "gWidgetsRGtk2" package 
   ```r
   install.packages('gWidgetsRGtk2')
   ```
   when asked, request to install GTK
2. Install the package from GitHub. (You'll need to have the "devtools" package installed and loaded=
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

### Non Interactive mode (Exploiting a previosly saved options file)
To run the tool without GUI interaction: 
```r
library(MODIStsp) 
options_file = 'X:/yourpath/youroptions.RData"  # --> Specify the path to a valid options file saved in advance from the GUI
MODIStsp(gui = FALSE, options_File = options_file)
```
