#'@title EF_MOD_Preproc_Launcher
#' @author Lorenzo Busetto  
#' 
#'@description This function allows to automatically download, mosaic,  reproject and resize MOD09 and MOD11 data
#' @param Start_Year: Starting year.  
#' @param End_Year: Ending Year. (All images available between start and end year will be processed)
#' @param OutPath: Output folder. Images will be organized in subfolders of this one
#' @param ovr: Overwrite flag. If TRUE existing files will be replaced/rewritten
#' @param del: Deletion flag. If TRUE (default), then original HDF files will be deleted after processing
#' @param reproj: Reprojection flag (deprecated - always TRUE - to be removed)  
#' @param opts : options for processing. Data frame with the following fields
#'   						FTP = Set LDAAC ftp site name)
#' 							MRTpath= Path to MRT bin folder (Needed for mosaicing)
#' 							MOD_prj_str = 	# proj4 string for MODIS ISIN projection
#' 							out_prj_str = Proj4 string for output projection
#'					 		pixel_size =  Desired output pixel size
#' 							bbox =    # Bounding box for output mosaic (In Out_Proj coordinates; Xmin, Ymin, Xmax, Ymax)
#'
#' @return 
#' NONE
#'
#' @author Lorenzo Busetto (2013)
#' @email: lorenzo.busetto@@jrc.ec.europa.eu
#' @Created_Date: Mar 19, 2013
#' @export

# Set general processing options

OutPath = 'Z:/0-Project/SPACE4AGRI/Users/Daniela/Da_Lorenzo/MOD09'    			# Set output Path (main folder for procesed images - Subfolders are created as needed)
dir.create(OutPath, recursive = T)
memory.limit(6000)												# Increase maximum allocable memory
setwd(OutPath)														# Change working dir, so to allow easy assignation of filename

#- ------------------------------------------------------------------------------- -#
#  Check if needed packages are present. Install them otherwise
#- ------------------------------------------------------------------------------- -#
pkgTest <- function(x)
{if (!require(x,character.only = TRUE))
{install.packages(x,dep=TRUE)
 if(!require(x,character.only = TRUE)) stop("Package not found")
}
}

#pkgList = c('gWidgets','gWidgetsRGtk2','RCurl','rgdal','tcltk', 'raster','rJava')
pkgList = c('gWidgets','gWidgetsRGtk2','RCurl','rgdal','tcltk', 'raster')
for (pkg in pkgList) {pkgTest(pkg)	}
options("guiToolkit"="RGtk2")

# Start_Year = 2002
Start_Date = '2014.06.01'			; 		End_Date = '2014.06.20'			#Set processing years 
h=c(18)  	;  	v=c(4)    											# Set tiles to be downloaded (All combinations will be downloaded)
del = TRUE																	# Set deletion option. If TRUE, then original HDFs downloaded and used for mosaicing are deleted after completion
ovr = FALSE


# Options for bands to be processed and nodata values passed to gdal - MOD09GQ

bandnames = 	c('num_observations','Red',	'NIR',	'QA',	'grid_area')					#Band names (from https://lpdaac.usgs.gov/products/modis_products_table)
bands_subset = c(       0,           1,     1,     1,     0)							#Selection of desired bands
nodatas =         c('-1','-28672','-28672','2995','-1')	# Nodata values in input MODIS Science DataSets
nodata_out =  c('-999','-999','-999','255','255')

# Set options  on projection, pixel size, bounding box for reprojected images, 


opts = data.frame(FTP =  			"http://e4ftl01.cr.usgs.gov/MOLT/MOD09GQ.005/",   	 # Set LDAAC ftp site name)
							MRTpath=			  'C:/MRT/bin',		# Set Path to MRT bin folder (Needed for mosaicing)
							MOD_prj_str = 	'+proj=sinu +R=6371007.181 +nadgrids=@null +wktext',		# MODIS ISIN projection
							out_prj_str = 	"+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",	
							pixel_size = 250,					    		   # Desired output pixel size
              bbox = c(460707,4947603,691571,5077057)				   # Bounding box for output mosaic (Out_Proj coordinates; Xmin, Ymin, Xmax, Ymax) lorenz: bbox = c(-5.000,32.000,-1.000,35.000)
		, stringsAsFactors = F)									

out_format = 'GTiff'													# Set output format (Only"ENVI" allowed for now)

path_to_script = 'D:/Documents/Source_Code/R/MOD09_DWL/MOD09QC_Download.R'    # Modify as needed
source(path_to_script)

# Launch the processing
# res = try(MOD09QC_Download(Start_Date=Start_Date,End_Date = End_Date, OutPath = OutPath,   ovr = ovr, del = del,  reproj = T, opts = opts,
# 				bandnames = bandnames,bands_subset=bands_subset,nodatas=nodatas,nodata_out=nodata_out ))		# Download the images for the year

# Options for bands to be processed and nodata values passed to gdal - MOD11A1
OutPath = 'Z:/0-Project/SPACE4AGRI/Users/Daniela/Da_Lorenzo/MOD11'
dir.create(OutPath, recursive = T)
setwd(OutPath)
bandnames =   c('LST','LST_QA',	'LST_Time',	'',	'','','','','','','Cloud_cover','')					#Band names (from https://lpdaac.usgs.gov/products/modis_products_table)
bands_subset = c( 1,   1,        1,          0, 0,  0,0,  0,0,0,0,0)							#Selection of desired bands
nodatas =         c('0','-999','0','255','0','-999','0','255','0','0','0','0')	# Nodata values in input MODIS Science DataSets
nodata_out =  c('-999','255','255','255','255','255','255','255','255','255','255','255')

# Set options  on projection, pixel size, bounding box for reprojected images, 


opts = data.frame(FTP =  			"http://e4ftl01.cr.usgs.gov/MOLT/MOD11A1.005/",   	 # Set LDAAC ftp site name)
                  MRTpath=			  'C:/MRT/bin',		# Set Path to MRT bin folder (Needed for mosaicing)
                  MOD_prj_str = 	'+proj=sinu +R=6371007.181 +nadgrids=@null +wktext',		# MODIS ISIN projection
                  out_prj_str = 	"+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",	
                  pixel_size = 1000,					    		   # Desired output pixel size
                  bbox = c(460707,4947603,691571,5077057)				   # Bounding box for output mosaic (Out_Proj coordinates; Xmin, Ymin, Xmax, Ymax) lorenz: bbox = c(-5.000,32.000,-1.000,35.000)
                  , stringsAsFactors = F)									

out_format = 'GTiff'													# Set output format (Only"ENVI" allowed for now)

path_to_script = 'D:/Documents/Source_Code/R/MOD09_DWL/MOD11A1_Download.R'    # Modify as needed
source(path_to_script)

# Launch the processing
res = try(MOD11A1_Download(Start_Date=Start_Date,End_Date = End_Date, OutPath = OutPath,   ovr = ovr, del = del,  reproj = T, opts = opts,
                           bandnames = bandnames,bands_subset=bands_subset,nodatas=nodatas,nodata_out=nodata_out ))		# Download the images for the year



print('----------- Finished -------------')
