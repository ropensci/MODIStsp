#!/bin/bash

usage="
$(basename "$0") [options] 

DESCRIPTION:
MODIStsp is devoted to automatize the creation of time series of rasters derived from MODIS Land Products data.  MODISTSP allows to perform several preprocessing steps (e.g., download, mosaicking, reprojection and resize) on MODIS data available within a given time period. Users have the ability to select which specific layers of the original MODIS HDF files they want to process. They also can select which additional Quality Indicators should be extracted from the aggregated MODIS Quality Assurance layers and, in the case of Surface Reflectance products, which Spectral Indexes should be computed from the original reflectance bands. For each output layer, outputs are saved as single-band raster files corresponding to the different available acquisition dates. Virtual files allowing access to the entire time series as a single file are also created. All processing parameters can be easily selected with a user-friendly GUI. Command-line execution is also possible, allowing to use MODISTSP to automatically update time series related to a MODIS product whenever a new image is made available.
    
LICENSE: GPL-3

OPTIONS:
    -h  show this help text
    -g  run without opening the GUI
    -s  settings: full path of the RData file containing the processing options (default: Previous.RData in subdir Previous)
    -e  extent: full path of a spatial file to use as extent (default: NULL); if defined, the processing options which define the extent, the selected tiles and the \"Full Tile / Resized\" options are not considered; instead, new files are created on the extent of the provided spatial file.
    -r  Rscript dir: directory of the Rscript executable (optional; use it if your R installation is not in the PATH)
"

MODISTSP_BATCH_DIR=`dirname $(dirname $(realpath $0))`

gui=TRUE
options_file="NULL"
spatial_file_path="NULL"

while getopts 'hge:s:r:' option; do
  case "$option" in
    h) echo "$usage"
       exit
       ;;
    g) gui=FALSE
       ;;
    e) spatial_file_path=$OPTARG
       ;;
    s) options_file=$OPTARG
       ;;
    r) set PATH=$OPTARG:$PATH
       ;;
   \?) echo "Invalid option: -$OPTARG" >&2
       exit
       ;;
  esac
done
shift $((OPTIND - 1))

echo $MODISTSP_BATCH_DIR
Rscript "${MODISTSP_BATCH_DIR}/MODIStsp_launcher.R" $gui $options_file $spatial_file_path
