# Set options and launch MOD13Q1 download function

# Set general processing options

OutPath = 'X:/buttami'    			# Set output Path (main folder for procesed images - Subfolders are created as needed)
# Start_Year = 2002
Start_Year = 2013			; 		End_Year = 2013			#Set processing years 
del = TRUE																	# Set deletion option. If TRUE, then original HDFs downloaded and used for mosaicing are deleted after completion
ovr = FALSE
#h=c(17,18)		;  	v=c(5)    											# Set tiles to be downloaded (All combinations will be downloaded)
h=c(21,22)  	;  	v=c(8,9)    											# Set tiles to be downloaded (All combinations will be downloaded)

# Options for bands to be processed and nodata values passed to gdal - MOD13Q1

bandnames = 	c('NDVI','EVI',	'QA',	'Red',	'NIR',	'Blue','MIR','Zen',	'Azi',	'RelAz',	'DOY','Rely')					#Band names (from https://lpdaac.usgs.gov/products/modis_products_table)
bands_subset = c(1,       0,     1,     0,     0,     0,     0,     0,     0,     0,        1,      1)							#Selection of desired bands
nodatas =         c('-3000','-3000','65535','-1000','-1000','-1000','-1000','-10000','-10000','-4000','-1','-1')	# Nodata values in input MODIS Science DataSets
nodata_out = -999

# Set options  on projection, pixel size, bounding box for reprojected images, 

# opts = data.frame(FTP =				"http://e4ftl01.cr.usgs.gov/MOLT/MOD13Q1.005/",   	 # Set LDAAC ftp site name)
opts = data.frame(FTP =  			"http://e4ftl01.cr.usgs.gov/MOLA/MYD13Q1.005/",   	 # Set LDAAC ftp site name)
							MRTpath=			  'C:/MRT/bin',		# Set Path to MRT bin folder (Needed for mosaicing)
							MOD_prj_str = 	'+proj=sinu +R=6371007.181 +nadgrids=@null +wktext',		# MODIS ISIN projection
							out_prj_str = 	"+proj=longlat +ellps=WGS84 +datum=WGS84",	
							pixel_size = 0.00223214286,					    		   # Desired output pixel size
              bbox = c(33.500,-4.79798,42.000,5.600)				   # Bounding box for output mosaic (Out_Proj coordinates; Xmin, Ymin, Xmax, Ymax) lorenz: bbox = c(-5.000,32.000,-1.000,35.000)
		, stringsAsFactors = F)									

out_format = 'ENVI'													# Set output format (Only"ENVI" allowed for now)

path_to_script = 'd:/Users/meronmi/Documents/R_scripts/MODIS v2/MOD13Q1_Download.R'    # Modify as needed
source(path_to_script)

# Launch the processing
res = try(MOD13Q1_Download(Start_Year=Start_Year,End_Year = End_Year, OutPath = OutPath,   ovr = ovr, del = del,  reproj = T, opts = opts,
				bandnames = bandnames,bands_subset=bands_subset,nodatas=nodatas,nodata_out=nodata_out ))		# Download the images for the year
print('----------- Finished -------------')
