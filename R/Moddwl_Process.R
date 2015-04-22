#' @Title moddwl_process
#' @Description
#' 
#' @details
#'
#' @param sel_prod selected MODIS product
#' @param start_date start_date for images download and preproc
#' @param end_date start_date start_date for images download and preproc
#' @param out_folder main ouput folder 
#' @param MRTpath path to MRT executable
#' @param reproj Always T
#' @param reprocess T = Redownload/reprocess existing dates
#' @param FTP http site for MODIS download
#' @param sensor Terra or Aqua
#' @param start_x start horiz. tile
#' @param start_y start vert. tile
#' @param end_x end horiz. tile
#' @param end_y end vert. tile
#' @param bbox output bounding box (xmin, xmax, ymin, ymax ) in out proj coords
#' @param out_format ENVI or GTiff
#' @param out_res Output resolution (Resampling in always NN - to be changed !)
#' @param MOD_proj_str Proj4 string for MODIS SIN proj
#' @param outproj_str  Proj4 string for output proj
#' @param nodata_in  array of input nodata for MODIS bands
#' @param nodata_out array of ouput nodata for MODIS bands
#' @param datatype array of datatypes for MODIS bands
#' @param bandsel array. 1 = band to be processed
#' @param bandnames Abbreviated Names of MODIS bands
#' @param indexes_bandsel array. 1 = index to be processed
#' @param indexes_bandnames Abbreviated Names of MODIS Indexes
#' @param indexes_formula Formulas for Indexes
#' @param indexes_nodata_out Nodata values for indexes 
#' @param quality_bandnames Abbreviated Names of MODIS Quality Indicators
#' @param quality_bitN Position of bits corresponding to quality indicators
#' @param quality_source source original MODIS band for quality indicators
#' @param quality_nodata_in Always 255
#' @param quality_nodata_out Always 255
#' @param file_prefix output file prefix according to sel_prod (e.g., MOD13Q1)
#' @param main_out_folder Suffix to add to the overall out_folder to create the out dir (corresponds to an abbreviation of the selected product)
#' @param multiband_bsq If T, and sel_prod has reflectance bands, create virtual BSQ file for each DOY
#' @returnType 
#'
#' @return 
#' 
#' @author Lorenzo Busetto, phD (2014)
#' email: busetto.l@@irea.cnr.it
#'
#' @license GPL(>2)
#' @export
moddwl_process <- function(sel_prod, start_date,end_date ,out_folder, out_folder_mod, MRTpath ,reproj , reprocess , sensor, https ,
                           start_x , start_y ,	end_x , end_y ,   bbox , out_format, out_res, MOD_proj_str , outproj_str, nodata_in , nodata_out, datatype, 
                           bandsel , bandnames , reflbands, reflorder, indexes_bandsel,  indexes_bandnames, indexes_formula, indexes_nodata_out, 
                           quality_bandnames, quality_bandsel, quality_bitN ,quality_source, quality_nodata_in , full_ext,
                           quality_nodata_out,	file_prefixes , main_out_folder , multiband_bsq, resampling, out_res_sel, ts_format) {
  			   
  modis_folder = out_folder_mod
	dir.create(modis_folder, recursive = T, showWarnings=FALSE)
  
  mess = gwindow(title = 'Processing Status', container = TRUE, width = 400, height = 40)
  size(mess) <- c(100,8)		;	addHandlerUnrealize(mess, handler = function(h,...) {return(TRUE)})
  mess_lab = glabel(text =paste('--- Processing ---'), editable = FALSE, container = mess)
  if (sensor == 'Both') {senslist = c('Terra','Aqua')} else {senslist = sensor}# selected sensors
  for (sens_sel in senslist) {		# cycle on selected sensors
    
    # get http site addresses and file prefixes
    if (sens_sel == "Terra") {FTP = https[["Terra"]]} else {FTP =https[["Aqua"]]}
    if (sens_sel == "Terra") {file_prefix = file_prefixes[["Terra"]]} else {file_prefix = file_prefixes[["Aqua"]]}
    
    out_prod_folder = file.path(out_folder,main_out_folder)
    dir.create(out_prod_folder, showWarnings = F, recursive = T)
	tmp_prod_folder = file.path(out_prod_folder,'tmp') # directory to store temporary [virtual] rasters 
    start_year = unlist(strsplit(start_date, '[.]'))[1]    ;  end_year = unlist(strsplit(end_date, '[.]'))[1]    ;  del = F
    
    #- ------------------------------------------------------------------------------- -#
    #  Verify if bands needed for indexes and/or quality computation are already selected
    #  if not, select them and set the "delete" option for them to 1 
    #- ------------------------------------------------------------------------------- -#
    
	delbands = rep(0, length(bandnames))    # dummy array set to 0 - will contain info on wether orignal downloaded bands has to be deleted
	bands_indexes = matrix(0, nrow=length(bandsel), ncol=length(indexes_bandsel)+length(quality_bandsel),
			dimnames=list(bandnames,c(indexes_bandnames,quality_bandnames)))
		# dummy matrix which associate, to each couple of index or quality band (col) - original band (row),
		# info on wether that band is required to build that index 
	bandsel_orig_choice = bandsel						# Save original choice of bands in bandsel_orig_choice (bandsel is later modified to set to 1 all bands needed for indexes and quality 
	for (band in seq(along = indexes_bandnames)) {
      if (indexes_bandsel[band] == 1) {			# If on selection of an index
        formula = indexes_formula[band]					# retrieve the formula
        
        for (bandorig in seq(along = bandnames)) {		# cycle on original bands
          if (length(grep(bandnames[bandorig],formula)) > 0) {			# check if the original band is needed for the index
            if (bandsel[bandorig] == 0) {
				bands_indexes[bandorig,band] = 1
			}
#            bandsel[bandorig] = 1														# Set original band to be downloaded
          }
        } #End Cycle on bandorig
      } #End If on bandsel[band] == 1
    } #End Cycle on band
    
    for (band in seq(along = quality_bandnames)) {
      if (quality_bandsel[band] == 1) {
        bandorig = which(bandnames == quality_source[band]) 		# Identify source band for the quality indicator selected
        if (bandsel[bandorig] == 0) {
			bands_indexes[bandorig,length(indexes_bandsel)+band] = 1
		}
#        bandsel[bandorig] = 1																		# Set source band to be downloaded
      } #End If on bandsel[band] == 1
    } #End Cycle on band
    
	## Check 
	
	# ---------------------------------- #
    # Start Cycle on selected years
    # ---------------------------------- #
    
    for (yy in start_year:end_year) {
      
      # Create string representing the dates to be processed 
      if (yy == start_year & yy == end_year) {dates = c(start_date,end_date)}
      if (yy == start_year & yy != end_year) {dates = c(start_date,paste(as.character(yy),'.12.31', sep = ''))}
      if (yy != start_year & yy == end_year) {dates = c(paste(as.character(yy),'.1.1', sep = ''),end_date)}
      if (yy != start_year & yy != end_year) {dates = c(paste(as.character(yy),'.1.1', sep = ''),paste(as.character(yy),'.12.31', sep = ''))}
      
      # Processing status message
      
      svalue(mess_lab) = paste('--- Retrieving Files for Year ',as.character(yy),' ---')
   
      # Create a list of the images to be downloaded (Corresponding to the selected dates, year and tiles)
      
      dirs = getmod_dates(dates = dates, dirs =  getmod_dirs(FTP = FTP, .Platform = .Platform))  
      
      if (length(dirs) > 0) {
        modislist = NULL
        # Start Cycling on directories containing images to be downloaded
        for (i in 1:length(dirs)) {
          
          date_name <- sub(sub(pattern="\\.", replacement="_", dirs[i]), pattern="\\.", replacement="_", dirs[i])  #Create the date string
          DOY =strftime( as.Date(date_name,"%Y_%m_%d" ), format = "%j")
          check_files = F
          check_files = moddwl_check_files(out_prod_folder, file_prefix,bandnames,bandsel_orig_choice,yy,DOY,out_format,  indexes_bandnames, indexes_bandsel, quality_bandnames, quality_bandsel)
					if (check_files == F | reprocess == 'Yes') {  		# If not all output files are present, start downloading hdfs
            # Create vector of image names corresponding to the selected tiles
            
            # Create vector of image names corresponding to the selected tiles
            modislist = getmod_names(FTP = FTP, dirs = dirs, i = i, v = seq(from=start_y, to =  end_y), h = seq(from = start_x, to = end_x))
            # 				date_name <- sub(sub(pattern="\\.", replacement="_", dirs[i]), pattern="\\.", replacement="_", dirs[i])	#Create the date string
            # 				DOY = substr(modislist[1],14,16)
            
            # ---------------------------------- ----------------------------------------------#
            # Download and preprocess Imagesin modislist vector -----------
            # ---------------------------------- ----------------------------------------------#
            if (length(modislist) > 0) {
              
              # Check if all expected out files for the selected date are already present.
              # If so, check_files is set to TRUE, MODIS hdf is not downloaded and bands are not processed
              
              # 							check_files = F
              # 							check_files = moddwl_check_files(out_prod_folder, file_prefix,bandnames,bandsel_orig_choice,yy,DOY,out_format,  indexes_bandnames, indexes_bandsel, quality_bandnames, quality_bandsel)
              # 							
              # 							if (check_files == F | reprocess == T) {			# If not all output files are present, start downloading hdfs
              #- ------------------------------------------------------------------------------- -#
              #  Download images
              #- ------------------------------------------------------------------------------- -#	
              for (modisname in modislist) {	
				  if (file.exists(file.path(modis_folder,modisname)) == F ) {		# If HDF not existing, download. 
                  er <- 5		; 	class(er) <- "try-error" ;	ce <- 0
                  while(er != 0) {   # repeat until no error or > 21 tryyouts
                    print(paste('Downloading File: ', modisname ))
                    svalue(mess_lab) = paste('--- Downloading Files for date', date_name, ':' ,which(modislist == modisname),' of ', length(modislist),' ---')    # Update progress window
                    er <- tryCatch(download.file(url=paste(FTP,dirs[i], "/",modisname,sep=''),destfile=file.path(modis_folder,modisname),mode='wb',quiet=F, cacheOK=FALSE),
                                   warning=function(war) {print(war) ; return (1)}, error =function(err) {	print(err);	return (1)} )
                    if (er != 0) {	# Stop after 21 failed attempts
                      print('Download Error -Retrying') ; Sys.sleep(10)   ;	ce <- ce + 1 ; 	if (ce == 21) stop("Error: FTP server is down!!")	
                    }		
                  }
                } # end IF on hdf existence
              } # End cycle for downloading the images in modislist vector 
              
              print (paste(length(modislist)," files for date of ",dirs[i]," were successfully downloaded!",sep=''))
              
              # ---------------------------------- ----------------------------------------------#
              # Run the MRT tool to generate the mosaic HDFs. One separate HDF is generated for each selected band
              # Added by L.Busetto --- Instead of a single mosaic,  one different mosaic for each selected band will be created.
              # This is useful in the case of very large mosaics !
              # ---------------------------------- ----------------------------------------------#
              # Create the temporary parameter file for MRT mosaic function
#               dir.create(file.path(out_prod_folder,'Temp'), recursive = T, showWarnings = F)
#               mosaicname = file(file.path(out_prod_folder,'Temp', 'TmpMosaic.prm'), open="wt")
#               write(paste(modis_folder,"/",modislist[1], sep=""), mosaicname)
#               if (length(modislist) >1) {for (j in 2:length(modislist)) write(paste(modis_folder,"/",modislist[j], sep=""), mosaicname,append=T)}
#               close(mosaicname)
#        
	

# -------------------------------------------------------------------------
# STEP 1: choose the image (bands, indexes and quality bands) to be created
# -------------------------------------------------------------------------

# at the end of this step, "bandsel" is recreated as the union of the bands selected by the user and the bands required by indexes and quality bands,
# but only those ones which are not already present.

req_bands_indexes <- bands_indexes; for (i in 1:length(req_bands_indexes)) {req_bands_indexes[1]<-0}
# matrix similar to band_indexes, but specific for this year-doy process 
if (length(which(indexes_bandsel==1) >= 1)) {
	for (band in seq(along = indexes_bandsel)) {
		if (indexes_bandsel[band] ==1 ) {
			indexes_band =  indexes_bandnames[band]
			out_filename = file.path(out_prod_folder,indexes_band,paste(file_prefix,'_',indexes_band,'_',yy,'_', DOY, sep = ''))
			if (out_format =='GTiff') {out_filename = paste(out_filename, '.tif', sep = '')} else {out_filename = paste(out_filename, '.dat', sep = '')}
			if (file.exists(out_filename) == F | reprocess == T) {
				req_bands_indexes[band,] <- bands_indexes[band,] # if the index does not exists then consider the original bands required for it
			}
		}
	}
} #End If on length(which(indexes_bandsel ==1))

if (length(which(quality_bandsel==1) >= 1)) {
	for (band in seq(along = quality_bandnames)) {
		if (quality_bandsel[band] ==1 ) {
			quality_band =  quality_bandnames[band]
			out_filename = file.path(out_prod_folder,quality_band,paste(file_prefix,'_',quality_band,'_',yy,'_', DOY, sep = ''))
			if (out_format =='GTiff') {out_filename = paste(out_filename, '.tif', sep = '')} else {out_filename = paste(out_filename, '.dat', sep = '')}
			if (file.exists(out_filename) == F | reprocess == T) {
				req_bands_indexes[band+length(indexes_bandsel),] <- bands_indexes[band+length(indexes_bandsel),] # if the index does not exists then consider the original bands required for it
			}
		}
	}
} #End If on length(which(quality_bandsel ==1))

# Create the vector of bands required for processing (bands chosen by the user + bands required for indexes and quality bands)
bandsel <- as.integer(as.logical(bandsel_orig_choice + apply(req_bands_indexes,1,sum)))

# -----------------------------------
# STEP 2: process the required images
# -----------------------------------

for (band in 1:length(bandnames)) {														# Cycle on MODIS Bands
	
	bands = numeric(length(bandnames))													# Create vector with length = bands, filled with zeroes
	er_mos = 1    #; er_rep = 1																	# dummies for error state
	if (bandsel [band] == 1) {					# If band selected, process it
		svalue(mess_lab) =  (paste('--- Mosaicing ', bandnames[band],' files for date: ',date_name ,' ---'))
		bands[band]=1																			# IF band selected for processing, put its value to 1
		dir.create(file.path(out_prod_folder, bandnames[band]), showWarnings = F, recursive = T)
		bands = paste(as.character(bands), collapse = '', sep = ' ')					# Convert to character
		outfile = paste(tmp_prod_folder, '/',bandnames[band],'_',yy,'_',DOY,'.tif', sep = '')  	# Create name for the HDF mosaic
		# NOTE: Chanfe outrep_file to a list of rep files: only one for original bands, multiple for indexes and quality
		outrep_file = file.path(out_prod_folder, bandnames[band], paste(file_prefix,'_',sub("[.][^.]*$", "", basename(outfile), perl=TRUE),sep = ''))	# Create name for the TIFF reprojected  mosaic
		if (out_format =='GTiff') {outrep_file = paste(outrep_file, '.tif', sep = '')} else {outrep_file = paste(outrep_file, '.dat', sep = '')} 
		
		# integrate bandsel (bands directly selected) with band_indexes (bands needed by indexes or quality bands)
		
		
		outfile_vrt = paste(tmp_prod_folder, '/',bandnames[band],'_',yy,'_',DOY,'vrt.tif', sep = '')    # Create name for the HDF mosaic
		# Launch MRT to mosaic
		if (file.exists(outrep_file) == F | reprocess == 'Yes') {
#                     er_mos <- system(paste(MRTpath, '/mrtmosaic -i ',file.path(out_prod_folder,'Temp', 'TmpMosaic.prm') ,' -o ', outfile,' -s ',bands, sep=""), show.output.on.console = F)	# Launche MRT to create the mosaic
			files_in = file.path(modis_folder, modislist)
			
			dir.create(tmp_prod_folder, showWarnings = FALSE)
			
			if (full_ext == 'Resized') { 
				
				# convert bbox coordinates from t_srs to modis_srs
				N_dens = 1000 # densification ratio of the bounding box
				d_bbox_out <- data.frame(lon=c(bbox[1]+diff(bbox[1:2])*(0:N_dens)/N_dens, rep(bbox[2],N_dens-1), bbox[1]+diff(bbox[1:2])*(N_dens:0)/N_dens, rep(bbox[1],N_dens-1)),
						lat=c(rep(bbox[3],N_dens), bbox[3]+diff(bbox[3:4])*(0:N_dens)/N_dens, rep(bbox[4],N_dens-1), bbox[3]+diff(bbox[3:4])*(N_dens:1)/N_dens))
				d_bbox_out <- SpatialPolygons(list(Polygons(list(Polygon(d_bbox_out)),1)))
				proj4string(d_bbox_out) <- CRS(outproj_str)
				d_bbox_mod <- spTransform(d_bbox_out, CRS(MOD_proj_str))
				gdalbuildvrt(files_in, outfile_vrt, te = c(d_bbox_mod@bbox),  sd = band) 
			} else {gdalbuildvrt(files_in, outfile_vrt,  sd = band) }
			er_mos = gdal_translate(outfile_vrt, outfile)
			if (is.null(er_mos) == FALSE)  {stop()}   # exit on error
			
			
			# ---------------------------------- ----------------------------------------------#
			# Convert to output projection, extent and format using gdalwarp ----
			# ---------------------------------- ----------------------------------------------#
			
			print (paste('Reprojecting ', bandnames[band],'files for date: ',date_name ))
			svalue(mess_lab) =  (paste('--- Reprojecting ', bandnames[band],'files for date: ',date_name,' ---'))
			
			# Launch the reprojection
			if (full_ext == 'Resized') {	# If bounding box was passed, the output reproject file will satisfy the bbox
				gdalwarp(outfile, outrep_file, s_srs=MOD_proj_str, t_srs=outproj_str, of=out_format, r=resampling, te=bbox[c(1,3,2,4)], tr=rep(out_res,2),
						wo="INIT_DEST=NO_DATA", wt=datatype[band], srcnodata=nodata_in[band], dstnodata=nodata_out[band], overwrite=TRUE)
			} else {						# If bounding box was not passed, keep the original extent when creating the File
				gdalwarp(outfile, outrep_file, s_srs=MOD_proj_str, t_srs=outproj_str, of=out_format, r=resampling, tr=rep(out_res,2),
						wo="INIT_DEST=NO_DATA", wt=datatype[band], srcnodata=nodata_in[band], dstnodata=nodata_out[band], overwrite=TRUE)                    
			}  
			gc()
			xml_file = paste(outrep_file,'.aux.xml',sep = '')		# Delete xml files created by gdalwarp
			unlink(xml_file)
			unlink(tmp_prod_folder, recursive=TRUE)					# Delete un-reprojected Mosaic HDF file
		}   
	}  # ENDIF band selected for processing
}	# END Cycle on available MODIS Bands
#               file.remove(file.path(out_prod_folder,'Temp', 'TmpMosaic.prm'))

# ---------------------------------- ----------------------------------------------#
# If Indexes selected, then start creating them 
# ---------------------------------- ----------------------------------------------#
if (length(which(indexes_bandsel==1) >= 1)) {
	for (band in seq(along = indexes_bandnames)) {
		if (indexes_bandsel[band] ==1 ) {
			indexes_band =  indexes_bandnames[band]		; 	formula = indexes_formula[band]
			svalue(mess_lab) = paste('--- Computing',  indexes_band,' for date: ',date_name,' ---')
			print (paste('Computing ', indexes_band,' for date: ',date_name ))
			out_filename = file.path(out_prod_folder,indexes_band,paste(file_prefix,'_',indexes_band,'_',yy,'_', DOY, sep = ''))
			if (out_format =='GTiff') {out_filename = paste(out_filename, '.tif', sep = '')} else {out_filename = paste(out_filename, '.dat', sep = '')}
			dir.create(file.path(out_prod_folder,indexes_band), showWarnings = F, recursive = T)
			if (file.exists(out_filename) == F | reprocess == T) {
				moddwl_process_indexes(out_filename = out_filename,indexes_band= indexes_band, formula = formula,bandnames=bandnames, nodata_out=nodata_out,
						indexes_nodata_out=indexes_nodata_out[band],out_prod_folder=out_prod_folder, file_prefix=file_prefix, yy=yy,out_format=out_format, DOY=DOY )
			}
		}
	}
} #End If on length(which(indexes_bandsel ==1))

# ---------------------------------- ----------------------------------------------#
# If Quality indicators selected , then start creating them 
# ---------------------------------- ----------------------------------------------#
if (length(which(quality_bandsel==1) >= 1)) {
	for (band in seq(along = quality_bandnames)) {
		if (quality_bandsel[band] ==1 ) {
			quality_band =  quality_bandnames[band]		; 	bitN =  quality_bitN[band]  ;	source = quality_source[band]
			quality_nodata_in = quality_nodata_in [band]   ; quality_nodata_out = quality_nodata_out [band]   
			svalue(mess_lab) = paste('--- Computing',  quality_band,' for date: ',date_name,' ---')
			print (paste('Computing ', quality_band,' for date: ',date_name ))
			out_filename = file.path(out_prod_folder,quality_band,paste(file_prefix,'_',quality_band,'_',yy,'_', DOY, sep = ''))
			if (out_format =='GTiff') {out_filename = paste(out_filename, '.tif', sep = '')} else {out_filename = paste(out_filename, '.dat', sep = '')}
			dir.create(file.path(out_prod_folder,quality_band), showWarnings = F, recursive = T)
			if (file.exists(out_filename) == F | reprocess == T) {
				moddwl_process_QA_bits(out_filename,in_raster_name = bandnames[grep(source,bandnames)], bitN, source, 
						out_prod_folder, file_prefix, yy, DOY, out_format, nodata_out = nodata_out [grep(source,bandnames)], quality_nodata_in , quality_nodata_out)
			}
		}
	}
} #End If on length(which(quality_bandsel ==1))


#- ------------------------------------------------------------------------------- -#
              #  Delete bands not needed (i.e., bands required for indexes or quality computation, 
              # but not requested by the user,
              #- ------------------------------------------------------------------------------- -#
              for (banddel in seq(along = delbands)) {
                
                if (delbands[banddel] == 1) {
                  out_filename = file.path(out_prod_folder,bandnames[banddel],paste(file_prefix,'_',bandnames[banddel],'_',yy,'_', DOY, sep = ''))
                  if (out_format =='ENVI') {
                    out_filename_dat = paste(out_filename, '.dat', sep = '')
                    unlink(out_filename_dat)
                    out_filename_hdr = paste(out_filename, '.hdr', sep = '')
                    unlink(out_filename_hdr)
                    
                  }
                  if (out_format =='GTiff') {
                    out_filename_tif = paste(out_filename, '.tif', sep = '')
                    unlink(out_filename_tif)
                  }
                  unlink(dirname(out_filename),recursive = T)
                } #End If on delbands[banddel] == 1
              } #End Cycle on banddel
              
              #             } else {print (paste('All Required output files for date ',date_name, ' are already existing - Doing Nothing !', sep = ''))}
              # If multiband BSQ required, sel_prod has reflectances, and reflectances were selectd, create an ENVI metafile ----
              if (multiband_bsq == T) {moddwl_refl_bsq(sel_prod, out_prod_folder,bandnames, bandsel_orig_choice, reflbands, reflorder ,file_prefix, yy, DOY)} 
              if (del == T) {for (modisname in modislist) unlink(file.path(out_prod_folder,modisname))}		# Delete original downloaded HDFs
            } else {print(paste("No available image for selected Tiles in ",dirs[i], sep=""))}
          } else {print (paste('All Required output files for date ',date_name, ' are already existing - Doing Nothing !', sep = ''))}
        }   # End cycling on available dates for selected year
        
      } else print (paste("No available data for year:  ", yy, "in selected dates dates"))
      
    }	# End Cycling on selected years
    bandsel = bandsel_orig_choice  # reset bandsel to original user's choice
  } # End cycling on sensors
  #- ------------------------------------------------------------------------------- -#
  #  Create META files of time series - original and derived
  #- ------------------------------------------------------------------------------- -#
  
  
  if (sensor == 'Both') {senslist = c('Terra','Aqua','Mixed')} # selected sensors
  
  for (sens_sel in senslist) {		# cycle on selected sensors
    
    
    for (band in seq(along = bandnames)) {
      
      if (bandsel[band] == 1) {
        
        meta_band = bandnames[band]		
        moddwl_meta_create(out_prod_folder = out_prod_folder, meta_band = meta_band, 
                           file_prefix = file_prefixes, sens_sel, ts_format = ts_format,  nodata_value = nodata_out[band])
        
      } #End If on bandsel[band] == 1
    } #End Cycle on band
    
    for (band in seq(along = indexes_bandnames)) {
      
      if (indexes_bandsel[band] == 1) {
        
        meta_band = indexes_bandnames[band]				
        moddwl_meta_create(out_prod_folder = out_prod_folder, meta_band = meta_band, 
                           file_prefixes = file_prefixes, sens_sel = sens_sel, ts_format = ts_format, nodata_value = indexes_nodata_out[band])
        
      } #End If on bandsel[band] == 1
    } #End Cycle on band
    
    for (band in seq(along = quality_bandnames)) {
      
      if (quality_bandsel[band] == 1) {
        
        meta_band = quality_bandnames[band]				
        moddwl_meta_create(out_prod_folder = out_prod_folder, meta_band = meta_band, 
                           file_prefixes = file_prefixes, sens_sel= sens_sel, ts_format = ts_format, nodata_value = quality_nodata_out[band])
        
      } #End If on bandsel[band] == 1
    } #End Cycle on band
    
  }
  #- ------------------------------------------------------------------------------- -#	
  # Close GUI and clean
  #- ------------------------------------------------------------------------------- -#
  gc()
  addHandlerUnrealize(mess_lab, handler = function(h,...) {return(FALSE)})		# Allow message lab to be closed since processing ended .
  dispose(mess_lab)
  unlink(file.path(out_prod_folder,'Temp'),recursive = TRUE)
  return('DONE')
}
