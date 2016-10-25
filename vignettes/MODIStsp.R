## ----fig.width=7, fig.height=6.8, fig.cap = "The MODIStsp main GUI", echo=FALSE----
  library(png)
  library(grid)
  library(knitr)
  img <- readPNG("Figure_2_new.png")
  grid.raster(img)

## ----fig.width = 7,  fig.height = 2.8, fig.align = "center", fig.cap = 'The "Select Processing Layers" GUI',echo=FALSE----
  library(png)
  library(grid)
  img <- readPNG('Figure_3.png')
  grid.raster(img)

## ----fig.width = 5, fig.height = 1.5, fig.cap = 'The GUI for insertion of additional Spectral Indexes',echo=FALSE----
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

