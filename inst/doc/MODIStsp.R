## ----eval=FALSE----------------------------------------------------------
#    install.packages("devtools", repos = "http://cran.us.r-project.org")
#    library(devtools)
#    install_github("lbusett/MODIStsp")

## ----eval=FALSE, echo = F------------------------------------------------
#    library(gWidgetsRGtk2)

## ----echo = F------------------------------------------------------------
  warning("Failed to load RGtk2 dynamic library, attempting to install it")

## ----fig.width=7, fig.height=7, fig.cap = "The MODIStsp main GUI", echo=FALSE----
  library(png)
  library(grid)
  img <- readPNG("D:/Documents/GDrive/My Files/MODIS_Paper/Submission_CAGEO/second_submission/latex_source/Figure_2.png")
  grid.raster(img)

## ----fig.width = 7,  fig.height = 3, fig.cap = 'The "Select Processing Layers" GUI',echo=FALSE----
  library(png)
  library(grid)
  img <- readPNG('D:/Documents/GDrive/My Files/MODIS_Paper/Submission_CAGEO/second_submission/latex_source/Figure_3.png')
  grid.raster(img)

## ----fig.width = 5,  fig.height = 2, fig.cap = 'The GUI for insertion of additional Spectral Indexes',echo=FALSE----
  library(png)
  library(grid)
  img <- readPNG('D:/Documents/GDrive/My Files/MODIS_Paper/Submission_CAGEO/second_submission/Figures/Figure_SI.png')
  grid.raster(img)

