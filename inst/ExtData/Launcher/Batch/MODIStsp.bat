@echo off

:: Path of R installation: you can modify this value to save it permanently or specify it as parameter
SET Rscript_dir="C:\Progra~1\R\R-3.4.3\bin\x64"
:: Do not modify below here!

setlocal EnableDelayedExpansion

set batch_path=%~dp0
set usage=^
MODIStsp.sh [options]^

DESCRIPTION:^

MODIStsp is devoted to automatize the creation of time series of rasters derived from MODIS Land Products data.  MODISTSP allows to perform several preprocessing steps (e.g., download, mosaicking, reprojection and resize) on MODIS data available within a given time period. Users have the ability to select which specific layers of the original MODIS HDF files they want to process. They also can select which additional Quality Indicators should be extracted from the aggregated MODIS Quality Assurance layers and, in the case of Surface Reflectance products, which Spectral Indexes should be computed from the original reflectance bands. For each output layer, outputs are saved as single-band raster files corresponding to the different available acquisition dates. Virtual files allowing access to the entire time series as a single file are also created. All processing parameters can be easily selected with a user-friendly GUI. Command-line execution is also possible, allowing to use MODISTSP to automatically update time series related to a MODIS product whenever a new image is made available.^

LICENSE: GPL-3^

OPTIONS:^

    -h  show this help text^

    -g  run without opening the GUI^

    -s  settings: full path of the RData file containing the processing options (default: Previous.RData in subdir Previous)^

    -e  extent: full path of a spatial file to use as extent (default: NULL); if defined, the processing options which define the extent, the selected tiles and the "Full Tile / Resized" options are not considered; instead, new files are created on the extent of the provided spatial file.^

    -r  Rscript dir: directory of the Rscript executable

:GETOPTS
 SET gui="TRUE"
 SET options_File="NULL"
 SET spatial_file_path="NULL"
if /I "%1" == "" goto Run
 if /I %1 == -h goto Help
 if /I %1 == -g set gui="FALSE"& shift
 if /I %1 == -s set options_file=%2& shift
 if /I %1 == -e set spatial_file_path=%2& shift
 if /I %1 == -r set Rscript_dir=%2& shift
 shift
if not (%1)==() goto GETOPTS

:Run
%Rscript_dir%\Rscript.exe %batch_path%..\MODIStsp_launcher.R %gui% %options_File% %spatial_file_path%
goto End

:Help
echo !usage!

:End
