create_help_messages <- function() {
  #nocov start
  help_messages <- NULL
  # Original layers help ----
  help_messages <- rbind(help_messages, data.frame(
    which_help = "layers_help",
    text = paste0("MODIS images store info inside different <span weight='bold'>HDF raster layers</span> ", #nolint
                  "each corresponding to a different varaible.\n\n",
                  "A separate image  is saved for selected layer and date. \n\n", #nolint
                  "Click on `Modify Selection` to change the selected layers"))
  )

  # QI help ----
  help_messages <- rbind(help_messages, data.frame(
    which_help = "qi_help",
    text = paste0(
      "MODIS images often use one or more <span weight='bold'>Quality Layers</span> ",#nolint
      "to store various information concerning pixel data quality.\n\nDifferent ", #nolint
      "<span weight='bold'>Quality Indicators </span> are stored within each \n",  #nolint
      "Quality Layer using a <span weight='bold'>bit-field representation</span> (see More info).\n\n", #nolint"
      "MODIStsp computes <span weight='bold'>Quality Indicators </span> from ",
      "MODIS Quality Layers by extracting the required information from the \n", #nolint
      "bit-field values\n\n",
      "A separate image is saved for each QI and date.\n\n",
      "Click on `Modify Selection` to change the selected layers"))
  )

  # SI help ----
  help_messages <- rbind(help_messages, data.frame(
    which_help = "si_help",
    text = paste0(
      "<span weight='bold'>Spectral Indices </span> (e.g., NDVI) are computed ",
      "from MODIS Surface Reflectance bands using simple mathematical operations.\n\n", #nolint
      "They are related to several bio-physical characteristics of the Earth ",
      "surface (e.g., LAI, soil water content, etc.).\n\n",
      "A separate image is saved for each selected SI and date.\n\n",
      "Click on `Modify Selection` to change the selected indices")
  )
  )

  # SI help ----
  help_messages <- rbind(help_messages, data.frame(
    which_help = "si_help_addindex",
    text = paste0(
      "<span weight='bold'>Spectral Indices </span> (e.g., NDVI) are computed ",
      "from MODIS Surface Reflectance bands using simple mathematical operations.\n\n", #nolint
      "They are related to several bio-physical characteristics of the Earth ",
      "surface (e.g., LAI, soil water content, etc.).\n\n",
      "MODIStsp allows automatic computation of some of the most commonly used",
      "Spectral Indices. You can however add new ones by clicking the ",
      "<span weight='bold'>Add New Indices</span> button and then following ",
      "instructions provided in the GUI.\n\n",
      "A separate image is saved for each selected SI and date.\n\n",
      "Click on `Modify Selection` to change the selected indices")
  )
  )

  # aria help ----
  help_messages <- rbind(help_messages, data.frame(
    which_help = "aria_help",
    text = paste0(
      "Select this option to use `aria2c` to accelerate download. \n\n",
      "Note that `aria2c` must be installed and that the binary executable ",
      "must be in your PATH, otherwise the selector will not be ",
      "available.\n\n (See https://aria2.github.io for installation info)")
  ))

  # season help ----
  help_messages <- rbind(help_messages, data.frame(
    which_help = "season_help",
    text = paste0(
      "<span weight='bold'>Full</span>: all the available images between the ", #nolint
      "starting and the ending dates will be downloaded;\n\n",
      "<span weight='bold'>`Seasonal`</span>: only the images included in \n",
      "the season will be downloaded (e.g: if the starting date is 2005-12-01 \n", #nolint
      "and the ending date is 2010-02-31, the images of December, January and \n",#nolint
      "February from 2005 to 2010 will be processed (excluding 2005-01, 2005-02 \n", #nolint
      "and 2010-12 - ).")
  ))

  # virtual help ----
  help_messages <- rbind(help_messages, data.frame(
    which_help = "timeseries_help",
    text = paste0(
      "MODIStsp can create <span weight='bold'>virtual files</span> allowing ",
      "access to the time series of the single-date rasters created for the ",
      "different processed layers as if they were a single multitemporal file.\n",#nolint
      "Available formats are: \n\n",
      "<span weight='bold'>R rasterStack</span>: extension `.RData`. Can be opened in R\n\n", #nolint
      "<span weight='bold'>ENVI Meta</span>: extension `.dat`. Can be opened in `ENVI`\n\n", #nolint
      "<span weight='bold'>GDAL vrt</span>: extension `.vrt`. Can be opened in `QGIS` and `R`\n\n", #nolint
      "Virtual time series files <span weight = 'bold'>are saved in the ",
      "`time_series` subfolder </span> of the main MODIStsp output folder."
    )
  ))

  # nodata_help ----
  help_messages <- rbind(help_messages, data.frame(
    which_help = "nodata_help",
    text = paste0(
      "NoData values of MODIS products are variable across layers. ",
      "This option allows you to modify them and use a common standard.\n\n",
      "<span weight='bold'>No</span>: Original MODIS NoData values are maintained; \n\n", #nolint
      "<span weight='bold'>Yes</span>: NoData values are replaced with default values equal",#nolint
      "to the maximum possible value of the data type of the output (e.g. 255 ",
      "for unsigned 8-bit integer, 32767 for signed 16-bit integer)."
    )
  ))

  # scale_help ----
  help_messages <- rbind(help_messages, data.frame(
    which_help = "scale_help",
    text = paste0(
      "MODIS data is always stored as Integer or Byte. A scale factor and/or ",
      "an offset can be applied in some cases to convert values to floating ",
      "point values in the indicated measure units (e.g., to obtain data in ",
      "Kelvin degrees for LST, or in reflectance for Surface Reflectance). \n\n " ,#nolint
      "<span weight='bold'>No</span>: scale and offset are NOT applied ",
      "Output values are kept as Integer and Spectral Indices stored ",
      "as integer values with a 10000 scale factor.\n\n",
      "<span weight='bold'>Yes</span>: scale factors and offsets are applied ",
      "Outputs are converted to their true values in the specified measure units",#nolint
      "(e.g., degrees Kelvin). Spectral Indices are also computed as floating ",
      "point values.\n",
      "(Notice that in this case the size on disk of the output images will be ", #nolint
      "larger.)"
    )
  ))

  # outfold_help ----
  help_messages <- rbind(help_messages, data.frame(
    which_help = "outfold_help",
    text = paste0(
      "MODIStsp outputs will be saved in the selected folder.\n\n ",
      "A separate subfolder will be created for each processed ",
      "original MODIS layer, Quality Indicator or Spectral Index.",
      "Each subfolder will contain one image for each processed date.\n\n ",
      "Virtual multitemporal files allowing access to the full time series ",
      "as if they were stored in a single file will be saved instead in the ",
      "`time_series` subfolder of the main output folder.\n\n",
      "<span weight='bold'>NOTE</span>: If you specify $tempdir as the output ",
      "folder outputs will be saved in the `R` temporary folder and ",
      "automatically removed when you exit `R`"
    )
  ))

  # reproc_help ----
  help_messages <- rbind(help_messages, data.frame(
    which_help = "reproc_help",
    text = paste0(
      "This option allows you to decide what to do with data (possibly) already ",#nolint
      "available in the main MODIStsp folder.\n\n",
      "<span weight = 'bold'>No</span>: MODIStsp will check the ",
      "main output folder and skip processing of dates/layers for which a ",
      "valid output is already available. This allows incrementally updating ",
      "time series when new imagery is acquired or you decide to extend the ",
      "time span of the analysis. \n\n",
      "<span weight = 'bold'>Yes</span>: all dates/layers will be ",
      "reprocessed and already available MODIStsp outputs overwritten."
    )
  ))

  # outhdffold_help ----
  help_messages <- rbind(help_messages, data.frame(
    which_help = "outhdffold_help",
    text = paste0(
      "The original MODIS HDF images downloaded from NASA servers will be ",
      "stored in the selected folder.\n\n",
      "Archiving images can be useful to avoid having to download them ",
      "again if you need to do process different subsets of a given MODIS ",
      "tile.\n\n",
      "Select 'Yes' on the 'Delete MODIS HDFs' on the right if you want the ",
      "HDFs to be automatically deleted from disk at the end of MODIStsp ",
      "processing.\n\n",
      "<span weight='bold'>NOTE</span>: If you specify $tempdir as the ",
      "output folder MODIS HDFs will be saved in the `R` temporary folder ",
      "and automatically removed when you exit `R`"
    )
  ))

  # delete_help ----
  help_messages <- rbind(help_messages, data.frame(
    which_help = "delete_help",
    text = paste0(
      "This option allows you to decide what to do with the original ",
      "MODIS HDF images downloaded from NASA at the end of MODIStsp ",
      "processing.\n\n",
      "<span weight = 'bold'>Yes</span>: HDF images will be deleted from ",
      "disk.\n\n",
      "<span weight = 'bold'>No</span>: HDF images will be stored in the ",
      "specified folder and be available for future use"
    )
  ))

  # delete_help ----
  help_messages <- rbind(help_messages, data.frame(
    which_help = "extent_help",
    text = paste0(
      "This frame allows specifying the spatial extent of MODIStsp outputs. ",
      "There are two main options: \n\n",
      "<span weight = 'bold'>Select MODIS Tiles</span>:\n\n",
      "MODIS data is organized in tiles of 10 degrees by 10 degrees at the equator. ", #nolint
      "Use this option if you wish to obtain outputs corresponding to one or more  ", #nolint
      "full MODIS tiles. ",
      "You can use the selector sliders to specify the extent of a rectangular ", #nolint
      "area to be processed, or click on 'Select From Map' ",
      "to interactively select the required tiles\n\n",
      "<span weight = 'bold'>Define Custom Area</span>:\n\n",
      "Use this option if you wish to obtain outputs resized to cover only a specific  ",#nolint
      "area (i.e., you do not need a full tile(s).\n  ",
      "*** Click on 'Select From Map' to interactively draw the desired output extent. ", #nolint
      "*** Click on 'Load Extent from Spatial File'",
      "to specify a valid spatial file (e.g., a shapefile or a raster file) ",
      "to be used to retrieve the extent.\n",
      "*** Fill in the boxes to manually set the corners of the bounding box of the ", #nolint
      "desired area\n\n",
      "<span weight = 'bold'>IMPORTANT NOTE</span>: Since MODIStsp automatically ", #nolint
      "mosaics outputs to create single raster output files the selected extent ", #nolint
      "MUST correspond to contiguous MODIS tiles! So, if you need for example ",
      "to process tile h18 v04 AND tile h20 v05 you will need to perform two ",
      "separate processings."
    )
  ))

  # delete_help ----
  help_messages <- rbind(help_messages, data.frame(
    which_help = "server_help",
    text = paste0(
      "This frame allows specifying the server used to download MODIS HDF images.", #nolint
      "There are three main options: \n\n",
      "<span weight = 'bold'>http</span>: downloads from NASA lpdaac server ",
      "(https://e4ftl01.cr.usgs.gov/). You will need to provide you 'earthdata' ",#nolint
      "credentials. You can create an earthdata account at: https://urs.earthdata.nasa.gov/home\n\n", #nolint
      "<span weight = 'bold'>offline</span>: process MODIS HDF files already  ",
      "available on disk (specify the folder where they are stored in the ",
      "'Folder for Storage of MODIS HDF files' input field."
    )
  ))

#nocov end
}
