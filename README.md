# LB_MOD_DWL
Proj for MOD dwl and preprocess

## Installation

1. Within "R", install the "gWidgetsRGtk2" package 

   install.packages('gWidgetsRGtk2')

, when asked, request to install GTK

2. Install the package from GitHub. (You'll need to have the "devtools" package installed and loaded=
 
  install.packages('devtools')
  require('devtools')
  install_github('lbusett/LB_MOD_DWL',ref = 'devel')

## Running the tool

### Interactive mode

To run the tool in interactive mode, lload the package and  launch the MODIS_tsp function, with no parameters

  library(MODIStsp)
  MODIStsp()

### Non Interactive mode (Exploiting a previosly saved options file)

To run the tool without GUI interaction: 

  library(MODIStsp) 
  Options_file = 'X:/yourpath/youroptions.RData"      --> Specify the path to a valid options file saved in advance from the GUI
  MODIStsp(GUI = FALSE, Options_File = Options_file)

