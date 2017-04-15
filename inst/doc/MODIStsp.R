## ---- eval=FALSE, message=FALSE, warning=FALSE, include=TRUE-------------
#  install.packages("gWidgetsRGtk2")
#  library(gWidgetsRGtk2)

## ---- eval=FALSE, message=FALSE, warning=FALSE, include=TRUE-------------
#  install.packages("lbusett/MODIStsp")

## ---- eval=FALSE, message=FALSE, warning=FALSE, include=TRUE-------------
#  library(devtools)
#  install_github("lbusett/MODIStsp", ref = "master")

## ---- eval=FALSE, message=FALSE, warning=FALSE, include=TRUE-------------
#  library(MODIStsp)
#  MODIStsp()

## ----GUIfig, echo=FALSE, fig.cap="The MODIStsp main GUI", message=FALSE, warning=FALSE----
  library(png)
  library(grid)
  library(knitr)
  img <- readPNG("Figure_1_Vignette.png")
  grid.raster(img)

## ----proc_layers, echo=FALSE, fig.cap='The "Select Processing Layers" GUI', fig.height=3.5, fig.width=6, message=FALSE, warning=FALSE----
  library(png)
  library(grid)
  img <- readPNG('Figure_3.png')
  grid.raster(img)

## ----indexfig, echo=FALSE, fig.cap='The GUI for insertion of additional Spectral Indexes', fig.height=1.5, fig.width=5, message=FALSE, warning=FALSE----
  library(png)
  library(grid)
  img <- readPNG('figure_4.png')
  grid.raster(img)

## ---- eval=FALSE---------------------------------------------------------
#    #Set the input paths to raster and shape file
#    infile = 'in_path/MOD13Q1_MYD13Q1_NDVI_49_2000_353_2015_RData.RData'
#    shpname = 'path_to_file/rois.shp'
#    #Set the start/end dates for extraction
#    startdate = as.Date("2010-01-01")
#    enddate = as.Date("2014-12-31")
#    #Load the RasterStack
#    inrts = get(load(infile))
#    # Compute average and St.dev
#    dataavg = MODIStsp_extract(inrts, shpname, startdate, enddate, FUN = 'mean', na.rm = T)
#    datasd = MODIStsp_extract (inrts, shpname, startdate, enddate, FUN = 'sd', na.rm = T)
#    # Plot average time series for the polygons
#    plot.xts(dataavg)

