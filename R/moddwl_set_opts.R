#'@title Moddwl_prodop
#'@description Main function for the MODIS download and preprocessing functions
#'@details
#'  The function is used to initialize the processing (folder names, packages, etc.), to launch the GUI (Moddwl_GUI) and receive outputs,
#'  and to launch the required routines for downloading and processing the requested datasets.
#' @return
#' NULL
#'
#'
#' @author Lorenzo Busetto (2014)
#' email: lorenzo.busetto@@jrc.ec.europa.eu
#'

#' @export


moddwl_set_opts = function(previous_file = previous_file) {

  # Define general options -----
  MRTpath='C:/MRT/bin'
  out_proj_list = hash("Sinusoidal" = "",
                       "UTM 32N" = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                       "Latlon WGS84" = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
  MOD_prj_str = '+proj=sinu +R=6371007.181 +nadgrids=@null +wktext'

  prod_opt_list = NULL   ; mod_prod_list = NULL
  prodopts = list()

# Define options for product MOD09GQ -----

  prodopts$product = "MOD09GQ/MYD09GQ"    # Product Name
  prodopts$bandnames =   c('Num_Obs','b1_Red',  'b2_NIR',	'QA',	'grid_area')					#Band names (from MRT + additional)
  prodopts$bandnames =   c('Num_Obs','b1_Red',  'b2_NIR',  'QA',	'grid_area')	               #Band nicknames (used for folder creation and band selection)
  prodopts$bandsel = array(data = 1, dim =length(prodopts$bandnames))  					  #Selection of desired bands
  prodopts$datatype =  c("Byte", "Int16","Int16","UInt16","Byte")   #Datatypes for bands
  prodopts$nodata_in = c('-1','-28672','-28672','2995','-1')	# Nodata values in input MODIS Science DataSets
  prodopts$nodata_out =  c('255','-28672','-28672','255','255')          # Nodata values in output images

  prodopts$derived_bandnames = c('NDVI')        #Names of possible derived bands
  prodopts$derived_bandsel = array(data = 1, dim =length(prodopts$derived_bandnames)) #Selection of desired derived bands
  prodopts$derived_datatype =  c("Int16")   #Datatypes for derived bands
  prodopts$derived_nodata_in =  c('-28672')   #nodata for derived bands

  prodopts$FTP = hash("Terra" = "http://e4ftl01.cr.usgs.gov/MOLT/MOD11A1.005/",  # http addresses
                      "Aqua" = "http://e4ftl01.cr.usgs.gov/MOLT/MYD11A1.005/")
  prodopts$file_prefix = hash("Terra" = "MOD09GQ","Aqua" = "MYD09GQ")     # Filename Prefixes
  prodopts$main_out_folder = "Surf_Ref_250_Daily"           # output folder Prefixes

  prodopts$native_res = 231.656358
  mod_prod_list = c(mod_prod_list, prodopts$product )     # Add product to products list
  prod_opt_list = c(prod_opt_list, list("MOD09GQ/MYD09GQ"= prodopts))     # Add options to options list


# Define options for product MOD11A1 -----

  prodopts$product = "MOD09GA/MYD09GA"    # Product Name
  prodopts$bandnames =    c('num_observations_1km','state_1km_1','SensorZenith_1','SensorAzimuth_1',  #Band names (from MRT + additional)
                            'Range_1','SolarZenith_1','SolarAzimuth_1','gflags_1','orbit_pnt_1','num_observations_500m',
                            'sur_refl_b01_1','sur_refl_b02_1','sur_refl_b03_1','sur_refl_b04_1','sur_refl_b05_1',
                            'sur_refl_b06_1','sur_refl_b07_1','QC_500m_1','obscov_500m_1','iobs_res_1','q_scan_1')

  prodopts$bandnames =   c("NumObs_1Km","State_1Km","Sen_Zen","Sen_Azi","Range","Sun_Zen","Sun_Azi","GFlags","Pointer", #Band nicknames (used for folder creation and band selection)
                           "NumObs_500","b1_Red", "b2_NIR","b3_BLUE","b4_GREEN","b5_SWIR","b6_SWIR","b7_SWIR" ,"QA", "Obs_Cov","Obs_Num","Q_scan")
  prodopts$bandsel = array(data = 1, dim =length(prodopts$bandnames))  					  #Selection of desired bands

  prodopts$datatype =  c("Byte", "UInt16","Int16","Int16","UInt16","Int16","Int16","Byte","Byte",             #Datatypes for bands
                         "Byte","Int16","Int16","Int16","Int16","Int16","Int16","Int16","UInt32","Byte","Byte","Byte" )
  prodopts$nodata_in = c('-1','65535','-32767','-32767','65535',"-32767","-32767","255",                      # Nodata values in input MODIS Science DataSets
                         "-1","-1","-28672","-28762","-28762","-28762","-28762","-28762","-28762","787410671",
                         "-1","255","255")
  prodopts$nodata_out =  c('255','65535','-32767','-32767','65535',"-32767","-32767","255",    # Nodata values in output images
                           "255","255","-28672","-28762","-28762","-28762","-28762","-28762","-28762","787410671",
                           "255","255","255")

  prodopts$derived_bandnames = c('NDVI')        #Names of possible derived bands
  prodopts$derived_bandsel = array(data = 1, dim =length(prodopts$derived_bandnames)) #Selection of desired derived bands
  prodopts$derived_datatype =  c("Int16")   #Datatypes for derived bands
  prodopts$derived_nodata_in =  c('-28672')   #nodata for derived bands

  prodopts$FTP = hash("Terra" = "http://e4ftl01.cr.usgs.gov/MOLT/MOD09GA.005/",# http addresses
                      "Aqua" = "http://e4ftl01.cr.usgs.gov/MOLT/MYD09GA.005/")

  prodopts$file_prefix = hash("Terra" = "MOD09GA","Aqua" = "MYD09GA")     # Filename Prefixes
  prodopts$main_out_folder = "Surf_Ref_500_Daily"           # output folder Prefixes

  prodopts$multiband_bsq = T
  prodopts$native_res = 463.3127

  mod_prod_list = c(mod_prod_list, prodopts$product )     # Add product to products list
  prod_opt_list = c(prod_opt_list, list("MOD09GA/MYD09GA"= prodopts))     # Add options to options list


# Define options for product MOD11A1 -----

  prodopts$product = "MOD11A1/MYD11A1"    # Product Name
  prodopts$bandnames =   c('LST','LST_QC',  'LST_Time', 'Sen_Zen','LST_Night','LST_Night_QC','LST_Time_Night','Sen_Zen_Night','Emiss_B31','Emiss_B32','Day_Cloud_cov','Night_Cloud_cov')					#Band names (from MRT )
  prodopts$bandnames =    c('LST','LST_QC',  'LST_Time', 'Sen_Zen','LST_Night','LST_Night_QC','LST_Time_Night','Sen_Zen_Night','Emiss_B31','Emiss_B32','Day_Cloud_cov','Night_Cloud_cov')  				#Band names (from MRT ) #Band nicknames (used for folder creation and band selection)
  prodopts$bandsel = array(data = 1, dim =length(prodopts$bandnames))      			  #Selection of desired bands
  prodopts$datatype =  c("UInt16", "Byte","Byte","Byte","UInt16","Byte","Byte","Byte","Byte","Byte","UInt16","UInt16")   #Datatypes for bands
  prodopts$nodata_in = c('0','-999','0','255','0','-999','0','255','0','0','0','0')  # Nodata values in input MODIS Science DataSets	# Nodata values in input MODIS Science DataSets
  prodopts$nodata_out = c('0','-999','255','255','0','-999','255','255','0','0','0','0')          # Nodata values in output images


  prodopts$derived_bandnames = c('')        #Names of possible derived bands
  prodopts$derived_bandsel = array(data = 1, dim =length(prodopts$derived_bandnames)) #Selection of desired derived bands
  prodopts$derived_datatype =  c("Int16")   #Datatypes for derived bands
  prodopts$derived_nodata_in =  c('-28672')   #nodata for derived bands

  prodopts$FTP = hash("Terra" = "http://e4ftl01.cr.usgs.gov/MOLT/MOD11A1.005/",  # http addresses
                      "Aqua" = "http://e4ftl01.cr.usgs.gov/MOLT/MYD11A1.005/")
  prodopts$file_prefix = hash("Terra" = "MOD11A1","Aqua" = "MYD11A1")     # Filename Prefixes
  prodopts$main_out_folder = "Land_Surf_Temp"           # output folder Prefixes
  prodopts$native_res = 926.6254

  mod_prod_list = c(mod_prod_list, prodopts$product )     # Add product to products list
  prod_opt_list = c(prod_opt_list, list("MOD11A1/MYD11A1"= prodopts) )     # Add options to options list

# Define options for product MOD13Q1-----

  prodopts$product = "MOD13Q1/MYD13Q1"    # Product Name
  prodopts$bandnames =  c('NDVI','EVI',  'QA',	'Red',	'NIR',	'Blue','MIR','Zen',	'Azi',	'RelAz',	'DOY','Rely')				#Band names (from MRT )
  prodopts$bandnames =  c('NDVI','EVI',  'QA',	'b1_Red',	'b2_NIR',	'b3_Blue','b7_SWIR','View_Zen',	'Sun_Zen',	'Rel_Az',	'DOY','Rely')				#Band names (from MRT ) #Band nicknames (used for folder creation and band selection)
  prodopts$bandsel = array(data = 1, dim =length(prodopts$bandnames))      			  #Selection of desired bands
  prodopts$datatype =  c("Int16", "Int16","UInt16","Int16","Int16","Int16","Int16","Int16","Int16","Int16","Int16","Byte")   #Datatypes for bands
  prodopts$nodata_in =  c('-3000','-3000','65535','-1000','-1000','-1000','-1000','-10000','-10000','-4000','-1','-1')  # Nodata values in input MODIS Science DataSets	# Nodata values in input MODIS Science DataSets
  prodopts$nodata_out = c('32767','32767','65535','32767','32767','32767','32767','32767','32767','32767','32767','255')          # Nodata values in output images


  prodopts$derived_bandnames = c('')        #Names of possible derived bands
  prodopts$derived_bandsel = array(data = 1, dim =length(prodopts$derived_bandnames)) #Selection of desired derived bands
  prodopts$derived_datatype =  c("")   #Datatypes for derived bands
  prodopts$derived_nodata_in =  c('')   #nodata for derived bands

  prodopts$FTP = hash("Terra" = "http://e4ftl01.cr.usgs.gov/MOLT/MOD13Q1.005/",  # http addresses
                      "Aqua" = "http://e4ftl01.cr.usgs.gov/MOLT/MYD13Q1.005/")
  prodopts$file_prefix = hash("Terra" = "MOD13Q1","Aqua" = "MYD13Q1")     # Filename Prefixes
  prodopts$main_out_folder = "Veg_Indexes_250_16days"           # output folder Prefixes
  prodopts$native_res = 231.656358

  mod_prod_list = c(mod_prod_list, prodopts$product )     # Add product to products list
  prod_opt_list = c(prod_opt_list,list("MOD13Q1/MYD13Q1"= prodopts ))     # Add options to options list

# Put all options in the "opts" list, add also "dummy" values for dates ,proj and other parameters and save to the "previous" file ----
  opts = list(MRTpath = MRTpath, out_proj_list = out_proj_list, MOD_prj_str = MOD_prj_str,
              mod_prod_list = mod_prod_list,sensor = 'Terra', prod_opt_list = prod_opt_list, modprod = 'MOD13Q1/MYD13Q1',                                                                                     start_day = 1,
              start_day = 1, start_month = 1,start_year = 2000,end_day = 1, end_month = 1, end_year = 2000,
              start_x = 1, end_x =1, start_y = 1, end_y = 1, proj = 'Sinusoidal', out_res = '',format = 'ENVI',
              reprocess ='No', bbox = c('','','',''), out_folder = '')

  save(opts, file= previous_file)
}


