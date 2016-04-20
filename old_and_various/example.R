#Set the inputs
infile = 'D:/Documents/GDrive/My Files/MODIS_Paper/Various/NDVI_ts_example.RData'  # Path to nput rts file
shpname = 'D:/Documents/GDrive/My Files/MODIS_Paper/Variosu/rois.shp'  # Path to Polygon Shapefile
startdate = as.Date("2010-01-01")  # Start date for extraction
enddate = as.Date("2010-12-31")    # End date for extraction
#Load Data
inrts = get(load(infile))          # Load the rts file

inrts2 = inrts@raster
inrts2 = setZ(inrts2, index(inrts), name = 'time')
inrts_mean = zApply(inrts2, by=as.yearmon, fun=mean, name='months')
dd = index(inrts)
inrts = setZ(inrts@raster, dd, name = 'time')
# Compute average and St.dev
dataavg = MODIStsp_extract(inrts, shpname, startdate, enddate, FUN = 'mean', na.rm = T, verbose = T)
datasd = MODIStsp_extract (inrts, shpname, startdate, enddate, FUN = 'sd', na.rm = T, verbose = T)
dataavg = extract(inrts,)

plot.xts(dataavg) # Plot average time series for the polygons
