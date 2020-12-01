# observers used to send help messages - approach recycled from sen2r
#
#### products panel ----
# product ----
shiny::observeEvent(input$help_product, {
  shiny::showModal(shiny::modalDialog(
    title = "MODIS Platforms",
    shiny::p(shiny::HTML(
      "This selector allows to choose which <strong> MODIS product</strong>
      is to be processed.")),
    easyClose = TRUE,
    footer = NULL
  ))
})

# platform ----
shiny::observeEvent(input$help_platform, {
  shiny::showModal(shiny::modalDialog(
    title = "MODIS Platforms",
    shiny::p(shiny::HTML(
      "This selector allows to choose if data from only one or both the <strong> MODIS platforms</strong>
      (i.e., Terra and Aqua) is to be processed.")),
    shiny::p(shiny::HTML("The selector is disabled/ineffective for <strong> Combined </strong>
                         MODIS products (i.e., MCDXXXX)")),
    easyClose = TRUE,
    footer = NULL
  ))
})


# layers ----
shiny::observeEvent(input$help_layers, {
  shiny::showModal(shiny::modalDialog(
    title = "MODIS Layers",
    shiny::p(shiny::HTML(
      "This selector allows to choose which of the <strong>original MODIS bands</strong> (SDS layers)",
      "are to be processed.")),
    shiny::p(shiny::HTML("A separate image will be saved for each selected layer
      and available date.")),
    easyClose = TRUE,
    footer = NULL
  ))
})

# quality ----
shiny::observeEvent(input$help_quality, {
  shiny::showModal(shiny::modalDialog(
    title = "Quality Indicators",
    shiny::p(shiny::HTML(
      "MODIS images often use one or more <strong>Quality Layers</strong>
      to store various information concerning pixel data quality. Different
      <strong>Quality Indicators</strong> are stored within each Quality Layer
      using a <strong>bit-field representation</strong>.")),
    shiny::p(shiny::HTML(
      "MODIStsp can automaticvally compute <strong>Quality Indicators</strong> from the different
      MODIS Quality Layers by extracting the required information from the bit-field values")),
    shiny::p(shiny::HTML("A separate image will be saved for each selected Quality Indicator
      and available date.")),
    easyClose = TRUE,
    footer = NULL
  ))
})

# indexes ----
shiny::observeEvent(input$help_indexes, {
  shiny::showModal(shiny::modalDialog(
    title = "Spectral Indexes",
    shiny::p(shiny::HTML(
      "<strong>Spectral Indexes </strong>(e.g., NDVI) can be computed
      from MODIS Surface Reflectance bands using simple mathematical operations.
      They are related to several bio-physical characteristics of the Earth
      surface (e.g., LAI, soil water content, etc.).")),
    shiny::p(shiny::HTML(
      "A separate image will be saved for each selected SI
      and available date.")),
    shiny::p(shiny::HTML(
      "It is possible to specify new indexes to be computed by
      clicking the <strong>\"Add New Spectral Index\"</strong>
      button at the bottom of the panel and following instructions
      provided in the modal dialog that will open.")),
    easyClose = TRUE,
    footer = NULL
  ))
})

# spatemp Panel ----

# date range ----
shiny::observeEvent(input$help_daterange, {
  shiny::showModal(shiny::modalDialog(
    title = "Date Range Type",
   shiny::p(shiny::HTML(
      "This selector allows specifying how the date range is interpreted.",
    )),
   shiny::p(shiny::HTML(
      "<strong>Full</strong>: all the available images between the starting and
      the ending dates will be downloaded;"
    )),
   shiny::p(shiny::HTML(
      "<strong>Seasonal</strong>:  only the images included \"in the season\"
      will be downloaded (e.g: if the starting date is 2005-12-01 and the ending
      date is 2010-02-31, the images of December, January and February from
      2005 to 2010 will be processed."
    )),
    easyClose = TRUE,
    footer = NULL
  ))
})

# dprojection ----
shiny::observeEvent(input$help_outproj, {
  shiny::showModal(shiny::modalDialog(
    title = "Output Projection",
   shiny::p(shiny::HTML(
      "These selectors allows specifying the output projection.",
    )),
   shiny::p(shiny::HTML(
      "<strong>Native</strong>: Native MODIS projection of the product
      (either MODIS Sinusoidal or EPSG 4008) will be kept;"
    )),
   shiny::p(shiny::HTML(
      "<strong>User Defined</strong>: The user can define the output projection
      by clicking the \"Change\" button and then specifying the desired projection
      as a EPSG code or WKT string"
    )),
    easyClose = TRUE,
    footer = NULL
  ))
})

# resolution ----
shiny::observeEvent(input$help_outres, {
  shiny::showModal(shiny::modalDialog(
    title = "Output Resolution",
   shiny::p(shiny::HTML(
      "These selectors allows specifying the output resolution",
    )),
   shiny::p(shiny::HTML(
      "<strong>Native</strong>: Native MODIS resolution of the product
      will be kept;"
    )),
   shiny::p(shiny::HTML(
      "<strong>Resampled</strong>: The user can define the output resolution
      by setting the desired value in the selector on the right"
    )),
   shiny::p(shiny::HTML(
      "<strong>NOTE</strong>: resolution MUST be set in the measure units
      of the output projection (e.g., decimal degrees for lat/lon projections"
    )),
    easyClose = TRUE,
    footer = NULL
  ))
})

# spatial selection methof ----
shiny::observeEvent(input$help_spameth, {
  shiny::showModal(shiny::modalDialog(
    title = "Spatial Selection Method",
   shiny::p(shiny::HTML(
      "These selectors allows specifying the spatial extent to be considered",
    )),
   shiny::p(shiny::HTML(
      "<strong>Select Tiles</strong>: User can specify the output extent by
      selecting the desired MODIS tiles, either manually or by selecting them
      an interactive map;"
    )),
   shiny::p(shiny::HTML(
      "<strong>Select Bounding Box</strong>: The user can define the output resolution
      by setting the desired output bounding box. Coordinates MUST be provided
      in the <strong>output projection</strong>;"
    )),
   shiny::p(shiny::HTML(
      "<strong>Load From Spatial File</strong>: The user can select a raster or
      vector file from which the extent has to be retrieved;"
    )),
    shiny::p(shiny::HTML(
      "<strong>Draw on Map</strong>: The user can select the extent by drawing
      it on an interactive map."
    )),
    easyClose = TRUE,
    footer = NULL
  ))
})

# resampling ----
shiny::observeEvent(input$help_resmeth, {
  shiny::showModal(shiny::modalDialog(
    title = "Resampling Method",
   shiny::p(shiny::HTML(
      "These selectors allows specifying the <strong>resampling method</strong> in case
      either the output projection or resolution is not set to \"Native\"",
    )),
   shiny::p(shiny::HTML(
      "All resampling methods available in <strong>gdalwarp</strong> can be used."
    )),
    easyClose = TRUE,
    footer = NULL
  ))
})

# Options panel ----

# online ----

shiny::observeEvent(input$help_downmeth, {
  shiny::showModal(shiny::modalDialog(
    title = "Download Server",
   shiny::p(shiny::HTML(
      "This selector allows specifying the server used to download MODIS HDF images.",
    )),
   shiny::p(shiny::HTML(
      "<strong>http</strong>: download data from NASA lpdaac server."
    )),
   shiny::p(shiny::HTML(
      "<strong>offline</strong>: process only MODIS HDF files already
      available on disk (specify the folder where they are stored in the
      'Folder for Storage of MODIS HDF files' input field.)"
    )),
    easyClose = TRUE,
    footer = NULL
  ))
})

# user ----
shiny::observeEvent(input$help_user, {
  shiny::showModal(shiny::modalDialog(
    title = "User",
   shiny::p(shiny::HTML(
      "Provide your <strong>earthdata</strong> user name." ,
    )),
   shiny::p(shiny::HTML(
     "You can create an earthdata account at:",
     "<a href='https://urs.earthdata.nasa.gov/home'",
     "target='_blank'>https://urs.earthdata.nasa.gov/home</a>."
   )),
    easyClose = TRUE,
    footer = NULL
  ))
})

# password ----
shiny::observeEvent(input$help_password, {
  shiny::showModal(shiny::modalDialog(
    title = "User",
   shiny::p(shiny::HTML(
      "Provide your <strong>earthdata</strong> password." ,
    )),
   shiny::p(shiny::HTML(
     "You can create an earthdata account at:",
     "<a href='https://urs.earthdata.nasa.gov/home'",
     "target='_blank'>https://urs.earthdata.nasa.gov/home</a>."
   )),
    easyClose = TRUE,
    footer = NULL
  ))
})

# downloader ----
shiny::observeEvent(input$help_downloader, {
  shiny::showModal(shiny::modalDialog(
    title = "Downloader",
   shiny::p(shiny::HTML(
      "This selector allows to choose which downloader will be used",
      "to download MODIS HDF files."
    )),
   shiny::p(shiny::HTML(
      "<strong>http</strong> is the downloader which is used by default",
      "through the package 'httr'."
    )),
   shiny::p(shiny::HTML(
      "<strong><a href=\"https://aria2.github.io\" target=\"_blank\">aria2</a></strong>",
      "is an alternative downloader which can be installed in Linux systems",
      "from the default install manager (in Ubuntu, install the package \"aria2\"),",
      "or in Windows from <a href='https://github.com/aria2/aria2/releases/tag/release-1.35.0'",
      "target='_blank'>here</a>."
    )),
   shiny::p(shiny::HTML(
     "This selector is active only if aria2 can be found using",
     "<span style='font-family: monospace;'>Sys.which(\"aria2c\")</span>."
   )),
   easyClose = TRUE,
   footer = NULL
  ))
})

# format ----
shiny::observeEvent(input$help_format, {
  shiny::showModal(shiny::modalDialog(
    title = "Output Format",
   shiny::p(shiny::HTML(
      "This selector allows specifying the server format used to save
      MODIStsp processed data.",
    )),
   shiny::p(shiny::HTML(
      "<strong>TIF</strong>: store processed data in GeoTiff format."
    )),
   shiny::p(shiny::HTML(
      "<strong>ENVI</strong>: store processed data in ENVI format."
    )),
    easyClose = TRUE,
    footer = NULL
  ))
})

# format ----
shiny::observeEvent(input$help_compression, {
  shiny::showModal(shiny::modalDialog(
    title = "Compression",
   shiny::p(shiny::HTML(
      "This selector allows specifying compression algorithm when
      output format is TIF."
    )),
    easyClose = TRUE,
    footer = NULL
  ))
})

# time series ----
shiny::observeEvent(input$help_time_series, {
  shiny::showModal(shiny::modalDialog(
    title = "Time series",
   shiny::p(shiny::HTML(
      "MODIStsp can create <strong>virtual files</strong> allowing access to
     the time series of the single-date rasters created for the different
     processed layers as if they were a single multitemporal file."
    )),
   shiny::p(shiny::HTML("Possible choices are: ")),
   shiny::p(shiny::HTML("<strong>R RasterStack</strong>:",
                        "extension <span style='font-family: monospace;'>.RData</span>.",
                        "Can be opened in R.")),
   shiny::p(shiny::HTML("<strong>ENVI Meta Files</strong>:",
                        "extension <span style='font-family: monospace;'>.dat</span>.",
                        "Can be opened in R.")),
   shiny::p(shiny::HTML("<strong>GDAL VRT</strong>:",
                        "extension <span style='font-family: monospace;'>.vrt</span>.",
                        "Can be opened in QGIS and R.")),

   shiny::p(shiny::HTML("Time series files are saved in the <em>\"time_series\"</em> subfolder of the main
    output folder")),
    easyClose = TRUE,
    footer = NULL
  ))
})

# scale ----
shiny::observeEvent(input$help_scaleoff, {
  shiny::showModal(shiny::modalDialog(
    title = "Apply Scale and Offset",
   shiny::p(shiny::HTML(
      "MODIS data is always stored as <strong>Integer</strong> or <strong>Byte</strong>.
      A <strong>scale factor</strong> and/or an <strong>offset</strong> can be applied
      in some cases to convert values to floating point values in the indicated measure units (e.g., to obtain data in
      Kelvin degrees for LST, or in reflectance for Surface Reflectance)."
    )),
   shiny::p(shiny::HTML("Possible choices are: ")),
   shiny::p(shiny::HTML("<strong>No</strong>: scale and offset are NOT applied
           Output values are kept as Integer and Spectral Indices stored as
           integer values with a 10000 scale factor;")),
   shiny::p(shiny::HTML("<strong>Yes</strong>:  scale factors and offsets are applied .
           Outputs are converted to their true values in the specified measure units
           (e.g., degrees Kelvin). Spectral Indices are also computed as floating
           point values.")),
    easyClose = TRUE,
    footer = NULL
  ))
})

# nodata ----
shiny::observeEvent(input$help_nodata, {
  shiny::showModal(shiny::modalDialog(
    title = "Modify NoData values",
   shiny::p(shiny::HTML(
      "<strong>NoData values</strong> of MODIS products are variable across layers.
      This option allows you to modify them and use a common standard"
    )),
   shiny::p(shiny::HTML("Possible choices are: ")),
   shiny::p(shiny::HTML("<strong>No</strong>: Original MODIS NoData values are maintained;")),
   shiny::p(shiny::HTML("<strong>Yes</strong>: NoData values are replaced with default values equal
           to the maximum possible value of the data type of the output (e.g. 255
           for unsigned 8-bit integer, 32767 for signed 16-bit integer).")),
    easyClose = TRUE,
    footer = NULL
  ))
})

# mainoutpfold ----
shiny::observeEvent(input$help_outfolder, {
  shiny::showModal(shiny::modalDialog(
    title = "Main Output Folder",
   shiny::p(shiny::HTML(
      "<strong>MODIStsp outputs</strong> will be saved in the selected folder.
     A separate subfolder will be created for each processed original MODIS
     layer, Quality Indicator or Spectral Index. Each subfolder will contain one
     image for each processed date."
    )),
   shiny::p(shiny::HTML("Virtual multitemporal files allowing access to the full time series
           as if they were stored in a single file will be saved instead in the
           <em>`time_series` </em> subfolder of the main output folder.")),
   shiny::p(shiny::HTML("<strong>NOTE</strong>: If you specify <strong>$tempdir</strong> as
           the output folder outputs will be saved in the `R` temporary folder and
           automatically removed when you exit `R`")),
    easyClose = TRUE,
    footer = NULL
  ))
})

# hdfoutpfold ----
shiny::observeEvent(input$help_outfolderhdf, {
  shiny::showModal(shiny::modalDialog(
    title = "HDF Output Folder",
   shiny::p(shiny::HTML(
      "The <strong>original MODIS HDF images</strong> downloaded from NASA servers
     will be stored in the selected folder. Archiving images can be useful to avoid
     having to download them again if you need to do process different subsets of
     a given MODIS tile."
    )),
   shiny::p(shiny::HTML("Select 'Yes' on the 'Delete MODIS HDFs' on the right if you want the
           HDFs to be automatically deleted from disk at the end of MODIStsp
           processing")),
   shiny::p(shiny::HTML("<strong>NOTE</strong>: If you specify <strong>$tempdir</strong> as the output
           folder outputs will be saved in the `R` temporary folder and
           automatically removed when you exit `R`")),
    easyClose = TRUE,
    footer = NULL
  ))
})
