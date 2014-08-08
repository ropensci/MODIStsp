#' @Title moddwl_set_opts
#' @Description
#' Function used to define the structure of the "opts" variable used to store all processing options for the different
#' products and generic, and save it in the "previous" file. 
#' 
#' @param previous_file previous_file Name of file used intrnally by moddwl project to store previously used processing
#' options
#' @returnType 
#'
#' @return 
#' 
#' @author Lorenzo Busetto, phD (2014)
#' email: busetto.l@@irea.cnr.it
#'
#' @license GPL(>2)
#' @export

moddwl_set_opts = function(previous_file = previous_file) {

  # Define general options -----
 

  prod_opt_list = NULL   ; mod_prod_list = NULL
  
#- ------------------------------------------------------------------------------- -#
#  ---------------Define options for product MOD09GQ -----  
#- ------------------------------------------------------------------------------- -#

	prodopts = list()
	
	# General options-------
	prodopts$product = "Surf_Ref_Daily_250 (MOD09GQ)"    # Product Name
	prodopts$main_out_folder = "Surf_Ref_Daily_250"          # output folder Prefixes
	prodopts$native_res = 231.656358
	prodopts$file_prefix = hash("Terra" = "MOD09GQ","Aqua" = "MYD09GQ")     # Filename Prefixes
	prodopts$FTP = hash("Terra" = "http://e4ftl01.cr.usgs.gov/MOLT/MOD11A1.005/",  # http addresses for data download
											"Aqua" = "http://e4ftl01.cr.usgs.gov/MOLA/MYD11A1.005/")
	prodopts$multiband_bsq = T		# To create virtual multiband BSQ files for the different dates
		
	# Original bandnames and characteristics
  prodopts$bandnames =   c('Num_Obs','b1_Red',  'b2_NIR',	'BQ',	'obs_cov')					#Band names (from MRT + additional)
  prodopts$nicknames =   c('N° of Observations','Red',  'NIR',  'Band Quality',	'% grid cell covered')	               #Band nicknames (used for folder creation and band selection)
  prodopts$datatype =  	 c("Byte", "Int16", "Int16","UInt16","Byte")   		#Datatypes for bands
  prodopts$nodata_in = 	 c('-1',	'-28672','-28672','2995',	 '-1')						# Nodata values in input MODIS Science DataSets
  prodopts$nodata_out =  c('255',	'-32767','-32767','65535', '255')       # Nodata values to be used in produced output images
	prodopts$bandsel = rep(0, length(prodopts$bandnames))  					  #Initial Selection of desired bands (all zeroes)
	
	# Derived Indexes and Characteristics
	prodopts$indexes_bandnames =	c('NDVI')        #Names of possible indexes bands
	prodopts$indexes_nicknames =	prodopts$indexes_bandnames         #Names of possible indexes bands
	prodopts$indexes_formula = 		c('(b1-b2)/(b1+b2)')        #Formulas of different indexes
	prodopts$indexes_nodata_out = c('-32767')   #nodata for indexes bands
	prodopts$indexes_bandsel = 		rep(0, length(prodopts$indexes_bandnames))  	 #Selection of desired indexes bands(all zeroes)
	
	# Derived Quality and Characteristics
	
	prodopts$quality_bandnames = c('')        #Names of possible quality bands
	prodopts$quality_nicknames = c('')        #Names of possible quality bands
	prodopts$quality_source  =	 c('')        # original band containing the information
	prodopts$quality_bitN = 	  c('')	# Position of bits of the selected quality indicator in the total "bit word" of the original band
	prodopts$quality_nodata_in =  rep(255, length(prodopts$indexes_bandnames))  # nodata in for quality bands (dummy - always 255)
	prodopts$quality_nodata_out =  rep(255, length(prodopts$indexes_bandnames)) # nodata out for quality bands (always 255)
	prodopts$quality_bandsel = rep(0, length(prodopts$quality_bandnames))  	 #Selection of desired quality bands (all zeroes)

	# Put everything  together and update list of modis products
   
  mod_prod_list = c(mod_prod_list, prodopts$product )     # Add product to products list
	prod_opt_list [[prodopts$product ]] =prodopts
	
	
#	prod_opt_list = c(prod_opt_list, do.call("list", "pippo = prodopts"))     # Add options to options list
#
#	prod_opt_list = c(prod_opt_list, eval(list(eval(prodopts$product) = prodopts)))   
#- ------------------------------------------------------------------------------- -#
#  ---------------Define options for product MOD09GQ -----  
#- ------------------------------------------------------------------------------- -#

	prodopts = list()
	
	# General options-------

  prodopts$product = "Surf_Ref_Daily_500 (MOD09GA)"    # Product Name
	prodopts$main_out_folder = "Surf_Ref_Daily_500"           # output folder Prefixes
	prodopts$native_res = 463.3127
	prodopts$file_prefix = hash("Terra" = "MOD09GA","Aqua" = "MYD09GA")     # Filename Prefixes
	prodopts$FTP = hash("Terra" = "http://e4ftl01.cr.usgs.gov/MOLT/MOD09GA.005/",# http addresses
			"Aqua" = "http://e4ftl01.cr.usgs.gov/MOLA/MYD09GA.005/")
	prodopts$multiband_bsq = T
	
	# Original bandnames and characteristics
  prodopts$bandnames = c("NumObs_1Km","State_1Km","Sen_Zen","Sen_Azi","Range","Sun_Zen","Sun_Azi","GFlags","Pointer", #Band nicknames (used for folder creation and band selection)
                           "NumObs_500","b1_Red", "b2_NIR","b3_BLUE","b4_GREEN","b5_SWIR","b6_SWIR","b7_SWIR" ,"QC_500", "Obs_Cov","Obs_Num","Q_scan")
 	
  prodopts$nicknames = c("N° of Observations - 1Km","Quality State - 1Km ","Sensor Zenith","Sensor Azimuth","Range","Sun Zenith ","Sun Azimuth","Geolocation Flags","Orbit Pointer", #Band nicknames (used for folder creation and band selection)
		 										 "N° of Observations - 500m","Red Reflectance (b1)", "NIR Reflectance (b2)","Blue Reflectance (b3)","Green Reflectance (b4)","SWIR Reflectance 1 (b5)","SWIR Reflectance 1 (b6)","SWIR Reflectance 1 (b7)" ,
												 "Reflectance Band Quality - 500m", "Observation Coverage","Observation Numbers","Q Scan - 250m")
	
	prodopts$datatype =  c("Byte", "UInt16","Int16","Int16","UInt16","Int16","Int16","Byte","Byte","Byte",             #Datatypes for bands
                         "Int16","Int16","Int16","Int16","Int16","Int16","Int16","UInt32","Byte","Byte","Byte" )
										 
  prodopts$nodata_in = c('-1','65535','-32767','-32767','65535',"-32767","-32767","255","-1","-1",									 # Nodata values in input MODIS Science DataSets
												 "-28672","-28672","-28672","-28672","-28672","-28672","-28672","787410671","-1","255","255")
  
  prodopts$nodata_out =c('255','65535','-32767','-32767','65535',"-32767","-32767","255","255","255",    						# Nodata values in output images
			                   "-32767","-32767","-32767","-32767","-32767","-32767","-32767","787410671","255","255","255")
											 
	prodopts$bandsel =rep(0, length(prodopts$bandnames)) 					  #Initial Selection of desired bands (all zeroes)

	# Derived Indexes and Characteristics
	prodopts$indexes_bandnames =	c('NDVI')        #Names of possible indexes bands
	prodopts$indexes_nicknames =	prodopts$indexes_bandnames 
	prodopts$indexes_formula = 		c('(b1_Red-b2_NIR)/(b1_Red+b2_NIR)')        #Formulas of different indexes
	prodopts$indexes_nodata_out = c('-32767')   #nodata for indexes bands
	prodopts$indexes_bandsel = 		rep(0, length(prodopts$indexes_bandnames))  	 #Selection of desired indexes bands(all zeroes)
	
	# Derived Quality and Characteristics
	
	prodopts$quality_bandnames = c('Cl_St_1km','Cl_Sh_1km','lnd_wat','cirrus','MOD35_snow','snow','Quality_500')        #Names of possible quality bands
	prodopts$quality_nicknames = c('Cloud State 1 Km','Cloud Shadow 1Km','Land/Water Flag','Cirrus','MOD35 Snow Flaq','Internal Snow flag','Band Quality 500')        #Names of possible quality bands
	prodopts$quality_source  =	 c('State_1Km','State_1Km','State_1Km','State_1Km','State_1Km','State_1Km','QC_500')        # original band containing the information
	prodopts$quality_bitN = 	  c('0-1', '2', '3-5', '8-9', '12','15','0-1')	# Position of bits of the selected quality indicator in the total "bit word" of the original band
	prodopts$quality_nodata_in =  rep(255, length(prodopts$indexes_bandnames))  # nodata in for quality bands (dummy - always 255)
	prodopts$quality_nodata_out =  rep(255, length(prodopts$indexes_bandnames)) # nodata out for quality bands (always 255)
	prodopts$quality_bandsel = rep(0, length(prodopts$quality_bandnames))  	 #Selection of desired quality bands (all zeroes)
	
  mod_prod_list = c(mod_prod_list, prodopts$product )     # Add product to products list
	prod_opt_list [[prodopts$product ]] =prodopts
	
#- ------------------------------------------------------------------------------- -#
#  ---------------Define options for product MOD11A1 -----  
#- ------------------------------------------------------------------------------- -#
	prodopts = list()
	
	# General options -------
  prodopts$product = "Land_Surf_Temp_Daily_1Km (MOD11A1)"    # Product Name
	prodopts$main_out_folder = "Land_Surf_Temp_Daily_1Km"          # output folder Prefixes
	prodopts$native_res = 926.6254
	prodopts$file_prefix = hash("Terra" = "MOD11A1","Aqua" = "MYD11A1")     # Filename Prefixes
	prodopts$FTP = hash("Terra" = "http://e4ftl01.cr.usgs.gov/MOLT/MOD11A1.005/",  # http addresses
											"Aqua" = "http://e4ftl01.cr.usgs.gov/MOLA/MYD11A1.005/")
	
	# Original bandnames and characteristics ---
  prodopts$bandnames =  c('LST','LST_QC',  'LST_Time', 'Sen_Zen','LST_Night','LST_Night_QC','LST_Time_Night','Sen_Zen_Night','Emiss_B31','Emiss_B32','Day_Cloud_cov','Night_Cloud_cov')					#Band names (from MRT )
  prodopts$nicknames =  c('Surface Temperature','LST Quality',  'LST Time', 'Sensor Zenith','Surface Temperature - Night','LST Night Quality','LST Night Time','Sensor Zenith Night','Emissivity b31','Emissivity b32','Cloud Coverage Day','Cloud Coverage Night')  				#Band names (from MRT ) #Band nicknames (used for folder creation and band selection)
  prodopts$datatype =   c("UInt16", "Byte","Byte","Byte","UInt16","Byte","Byte","Byte","Byte","Byte","UInt16","UInt16")   #Datatypes for bands
  prodopts$nodata_in =  c('0','-999','0','255','0','-999','0','255','0','0','0','0')  # Nodata values in input MODIS Science DataSets	# Nodata values in input MODIS Science DataSets
  prodopts$nodata_out = c('0','255','255','255','0','255','255','255','0','0','0','0')          # Nodata values in output images
	prodopts$bandsel = rep(0, length(prodopts$bandnames))     			  #Selection of desired bands

	# Derived Indexes and Characteristics
	prodopts$indexes_bandnames =	c('')        #Names of possible indexes bands
	prodopts$indexes_nicknames =	prodopts$indexes_bandnames 
	prodopts$indexes_formula = 		c('')        #Formulas of different indexes
	prodopts$indexes_nodata_out = c('')			   #nodata for indexes bands
	prodopts$indexes_bandsel = 		rep(0, length(prodopts$indexes_bandnames))  	 #Selection of desired indexes bands(all zeroes)
	
	# Derived Quality and Characteristics
	
	prodopts$quality_bandnames = c('')        #Names of possible quality bands
	prodopts$quality_nicknames = c('')        #Names of possible quality bands
	prodopts$quality_source  =	 c('')        # original band containing the information
	prodopts$quality_bitN = 	  c('')	# Position of bits of the selected quality indicator in the total "bit word" of the original band
	prodopts$quality_nodata_in =  rep(255, length(prodopts$indexes_bandnames))  # nodata in for quality bands (dummy - always 255)
	prodopts$quality_nodata_out =  rep(255, length(prodopts$indexes_bandnames)) # nodata out for quality bands (always 255)
	prodopts$quality_bandsel = rep(0, length(prodopts$quality_bandnames))  	 #Selection of desired quality bands (all zeroes)
	
	# Put everything  together and update list of modis products
  mod_prod_list = c(mod_prod_list, prodopts$product )     # Add product to products list
	prod_opt_list [[prodopts$product ]] =prodopts
	
#- ------------------------------------------------------------------------------- -#
#  Define options for product MOD13Q1-----
#- ------------------------------------------------------------------------------- -#
	prodopts = list()
	
	# General options-------
  prodopts$product = "Vegetation Indexes_16Days_250 (MOD13Q1)"    # Product Name
	prodopts$main_out_folder = "VI_16Days_250"           # output folder Prefixes
	prodopts$native_res = 231.656358
	prodopts$file_prefix = hash("Terra" = "MOD13Q1","Aqua" = "MYD13Q1")     # Filename Prefixes
	prodopts$FTP = hash("Terra" = "http://e4ftl01.cr.usgs.gov/MOLT/MOD13Q1.005/",  # http addresses
			"Aqua" = "http://e4ftl01.cr.usgs.gov/MOLA/MYD13Q1.005/")
	
	# Original bandnames and characteristics
	prodopts$bandnames = c('NDVI','EVI',  'QA',	'b1_Red',	'b2_NIR',	'b3_Blue','b7_SWIR','View_Zen',	'Sun_Zen',	'Rel_Az',	'DOY','Rely')				#Band names (from MRT ) #Band nicknames (used for folder creation and band selection)
	
	prodopts$nicknames = c('NDVI','EVI',  'QA',	'Red Reflectance (b1)',	'NIR Reflectance (b2)',	'Blue Reflectance (b3)','SWIR Reflectance (b7)','Sensor Zenith',	'Sun Zenith',	'Relative Azimuth',	'DOY','Reliability')				#Band names (from MRT )
    
  prodopts$datatype =  c("Int16", "Int16","UInt16","Int16","Int16","Int16","Int16","Int16","Int16","Int16","Int16","Byte")   #Datatypes for bands
  
	prodopts$nodata_in = c('-3000','-3000','65535','-1000','-1000','-1000','-1000','-10000','-10000','-4000','-1','-1')  # Nodata values in input MODIS Science DataSets	# Nodata values in input MODIS Science DataSets
  
	prodopts$nodata_out =c('-32767','-32767','65535','-32767','-32767','-32767','-32767','-32767','-32767','-32767','-32767','255')          # Nodata values in output images
	
	prodopts$bandsel = rep(0, length(prodopts$bandnames))   			  #Selection of desired bands

  prodopts$derived_bandnames = c('')        #Names of possible derived bands
  prodopts$derived_bandsel = rep(0, length(prodopts$derived_bandnames))   #Selection of desired derived bands
  prodopts$derived_datatype =  c("")   #Datatypes for derived bands
  prodopts$derived_nodata_in =  c('')   #nodata for derived bands

	
	# Derived Indexes and Characteristics
	prodopts$indexes_bandnames =	c('')        #Names of possible indexes bands
	prodopts$indexes_nicknames =	prodopts$indexes_bandnames 
	prodopts$indexes_formula = 		c('')        #Formulas of different indexes
	prodopts$indexes_nodata_out = c('')			   #nodata for indexes bands
	prodopts$indexes_bandsel = 		rep(0, length(prodopts$indexes_bandnames))  	 #Selection of desired indexes bands(all zeroes)

	# Derived Quality and Characteristics
	
	prodopts$quality_bandnames = c('')        #Names of possible quality bands
	prodopts$quality_nicknames = c('')        #nickNames of possible quality bands
	prodopts$quality_source  =	 c('')        # original band containing the information
	prodopts$quality_bitN = 	  c('')	# Position of bits of the selected quality indicator in the total "bit word" of the original band
	prodopts$quality_nodata_in =  rep(255, length(prodopts$indexes_bandnames))  # nodata in for quality bands (dummy - always 255)
	prodopts$quality_nodata_out =  rep(255, length(prodopts$indexes_bandnames)) # nodata out for quality bands (always 255)
	prodopts$quality_bandsel = rep(0, length(prodopts$quality_bandnames))  	 #Selection of desired quality bands (all zeroes)
	
	# Put everything  together and update list of modis products
  mod_prod_list = c(mod_prod_list, prodopts$product )     # Add product to products list
	prod_opt_list [[prodopts$product ]] =prodopts

# Put all options in the "opts" list, add also "dummy" values for dates ,proj and other parameters and save to the "previous" file ----
  opts = list(MRTpath = MRTpath, out_proj_list = out_proj_list, MOD_proj_str = MOD_proj_str,
              mod_prod_list = mod_prod_list,sensor = 'Terra', prod_opt_list = prod_opt_list, modprod = 'MOD13Q1/MYD13Q1',                                                                                     start_day = 1,
              start_day = 1, start_month = 1,start_year = 2000,end_day = 1, end_month = 1, end_year = 2000,
              start_x = 1, end_x =1, start_y = 1, end_y = 1, proj = 'Sinusoidal', out_res = '',format = 'ENVI',
              reprocess ='No', bbox = c('','','',''), out_folder = '')

  save(prod_opt_list, mod_prod_list, file= previous_file)
}


