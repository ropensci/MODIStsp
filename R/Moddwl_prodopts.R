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


Moddwl_prodopts = function(product) {

  if(product == 'MOD09GK/MYD09GK') {
    bandnames =   c('num_observations','Red',  'NIR',	'QA',	'grid_area')					#Band names (from https://lpdaac.usgs.gov/products/modis_products_table)
    bands_subset = c(       0,           1,     1,     1,     0)							#Selection of desired bands
    nodata_in =         c('-1','-28672','-28672','2995','-1')	# Nodata values in input MODIS Science DataSets
    nodata_out =  c('-999','-999','-999','255','255')
    out_res = 250
    FTP = hash("Terra" = "http://e4ftl01.cr.usgs.gov/MOLT/MOD11A1.005/",
               "Aqua" = "http://e4ftl01.cr.usgs.gov/MOLT/MYD11A1.005/")
  }


  if(product == 'MOD09GA/MYD09GA') {      #Band names (from https://lpdaac.usgs.gov/products/modis_products_table)
    bandnames =   c('num_observations_1km',
                    'state_1km_1',
                    'SensorZenith_1',
                    'SensorAzimuth_1',
                    'Range_1',
                    'SolarZenith_1',
                    'SolarAzimuth_1',
                    'gflags_1',
                    'orbit_pnt_1',
                    'num_observations_500m',
                    'sur_refl_b01_1',
                    'sur_refl_b02_1',
                    'sur_refl_b03_1',
                    'sur_refl_b04_1',
                    'sur_refl_b05_1',
                    'sur_refl_b06_1',
                    'sur_refl_b07_1',
                    'QC_500m_1',
                    'obscov_500m_1',
                    'iobs_res_1',
                    'q_scan_1')


    bands_subset = c(       0,           1,     1,     1,     0)							#Selection of desired bands
    nodata_in =         c('-1','-28672','-28672','2995','-1')	# Nodata values in input MODIS Science DataSets
    nodata_out =  c('-999','-999','-999','255','255')
    out_res = 250
    FTP = hash("Terra" = "http://e4ftl01.cr.usgs.gov/MOLT/MOD11A1.005/",
               "Aqua" = "http://e4ftl01.cr.usgs.gov/MOLT/MYD11A1.005/")
  }

  if(product == 'MOD11/MYD11') {
    bandnames =   c('LST','LST_QA',  'LST_Time',	'',	'','','','','','','Cloud_cover','')					#Band names (from https://lpdaac.usgs.gov/products/modis_products_table)
    bands_subset = c( 1,   1,        1,          0, 0,  0,0,  0,0,0,0,0)							#Selection of desired bands
    nodata_in =         c('0','-999','0','255','0','-999','0','255','0','0','0','0')	# Nodata values in input MODIS Science DataSets
    nodata_out =  c('-999','255','255','255','255','255','255','255','255','255','255','255')
    out_res = 250
    FTP = hash("Terra" = "http://e4ftl01.cr.usgs.gov/MOLT/MOD11A1.005/",
               "Aqua" = "http://e4ftl01.cr.usgs.gov/MOLT/MYD11A1.005/")
  }

  prodopts = list(bandnames = bandnames, bands_subset = bands_subset, nodata_in = nodata_in, nodata_out = nodata_out, FTP = FTP , out_res = out_res)
  return(prodopts)
}
