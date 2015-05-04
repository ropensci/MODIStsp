#' 
#' moddwl_process
#' @description main function of moddwl tool. Takes as input processing parameters specified by the user using moddl_GUI and saved in 
#' moddwl_Previous.RData (Interactive use), or a user specified RData file (batch use) (See moddwl_main for details ) and performs all required 
#' processing. 
#' @details After retrieving the input processing options, the function accesses lpdaac htttp archive to determine the list of dates 
#' to be processed. It then perform all required processing steps on each date (download, reprojection, resize, mosaicing, indexes computation, 
#' quality indicators computation), and finally performs virtual files creation. Checks are done in order to not re-download already existing HDF 
#' images, and not reprocess already processed dates (if the user didn'specify that)
#' @param sel_prod string selected MODIS product
#' @param start_date string start_date for images download and preproc (yyyy.mm.dd)
#' @param end_date string end_date for images download and preproc (yyyy.mm.dd)
#' @param out_folder  main ouput folder 
#' @param out_folder_mod  ouput folder for original HDF storage
#' @param reprocess string string ("Yes"/"No") If Yes, reprocess data for already existing dates (Default = 'Yes')
#' @param delete_hdf string ("Yes"/"No") If Yes, delete original hdf after completion
#' @param sensor string ("Terra" or "Aqua" or "Both")
#' @param https hash https site for download of hdf of selected product
#' @param start_x int start horiz. tile
#' @param start_y int start vertical. tile
#' @param end_x int end horiz. tile
#' @param end_y int end vertical. tile
#' @param bbox array output bounding box (xmin, xmax, ymin, ymax ) in out proj coords
#' @param out_format string output raster format (ENVI or GTiff)
#' @param compress string compression for GTiff outputs (None, LZW, DEFLATE)
#' @param out_res float Output resolution (in output projection measurement unit)
#' @param native_res float Native resolution of MODIS product
#' @param MOD_proj_str string proj4 string for MODIS product native projection (? Check ! for product in geographic !) 
#' @param outproj_str string proj4 string of selected output projection
#' @param nodata_in array Original nodata for MODIS bands
#' @param nodata_out Target nodata for MODIS bands
#' @param nodata_change string (Yes/No) if Yes, nodata are set to nodata_out in output rasters
#' @param datatype string array datatypes of MODIS bands
#' @param bandsel  array of lenght equal to number of original modis layers. set to 1 for bands to be processed
#' @param bandnames array of Abbreviated Names of MODIS bands
#' @param indexes_bandsel array of lenght equal to number of available spectral indexes, set to  1 for indexes to be processed
#' @param indexes_bandnames array of Abbreviated Names of MODIS indexes
#' @param indexes_formula  array of indexes formulas
#' @param indexes_nodata_out Nodata values for indexes 
#' @param quality_bandnames array of  Names of MODIS quality indicators
#' @param quality_bandsel array of lenght equal to number of available quality indicators, set to  1 for indicators to be processed
#' @param quality_bitN list of strings with number of entries equal to number of quality indicators. each entry caontains position of bits corresponding to a QI (e.g., 0-1)
#' @param quality_source list of strings which connects each quality indicator to its source aggregated quality assurance layer
#' @param quality_nodata_in Always 255
#' @param full_ext string ("Full_Ext" or "Resized")
#' @param quality_nodata_out Always 255
#' @param file_prefixes output file prefix according to selelected product (e.g., MOD13Q1)
#' @param main_out_folder Suffix to add to the overall out_folder to create the out dir for the product (corresponds to an abbreviation of the selected product)
#' @param resampling string resampling method (near, bilinear, etc.)
#' @param ts_format string format of virtual files (None, ENVI Meta Files, GDAL vrt files, ENVI and GDAL)
#' @return NULL
#' 
#' @author Lorenzo Busetto, phD (2014-2015)
#' email: busetto.l@@irea.cnr.it
#' Luigi Ranghetti, phD (2015)
#' license CC BY-NC 3.0
#' @export
moddwl_process <- function(sel_prod, start_date, end_date ,out_folder, out_folder_mod, reprocess = 'Yes', delete_hdf = 'No', sensor, https,
		start_x, start_y, end_x, end_y, bbox, out_format, compress,  out_res, native_res, MOD_proj_str, outproj_str, nodata_in, nodata_out,nodata_change, datatype, 
		bandsel, bandnames, indexes_bandsel, indexes_bandnames, indexes_formula, indexes_nodata_out, 
		quality_bandnames, quality_bandsel, quality_bitN ,quality_source, quality_nodata_in, full_ext,
		quality_nodata_out, file_prefixes, main_out_folder, resampling, ts_format) {
#	browser()
	if(nodata_change == 'No') {nodata_out = nodata_in}  # if nodata chande set to no, set ou_nodata to in_nodata
	dir.create(out_folder_mod, recursive = T, showWarnings=FALSE) # create out folder if not existing
	
	#Initialize some variables
	out_prod_folder = file.path(out_folder,main_out_folder)  # main output folder --> define on the basis of product name and create if necessary
	dir.create(out_prod_folder, showWarnings = F, recursive = T)
	tmp_prod_folder = file.path(out_prod_folder,'tmp') # directory to store temporary [virtual] rasters 
	start_year = unlist(strsplit(start_date, '[.]'))[1]    ;  end_year = unlist(strsplit(end_date, '[.]'))[1]    ;  del = F
	
	
	mess = gwindow(title = 'Processing Status', container = TRUE, width = 400, height = 40)
	size(mess) <- c(100,8)		;	addHandlerUnrealize(mess, handler = function(h,...) {return(TRUE)})
	mess_lab = glabel(text =paste('--- Processing ---'), editable = FALSE, container = mess)
	if (sensor == 'Both') {senslist = c('Terra','Aqua')} else {senslist = sensor}		# If both sensor selected, do a cycle. Process first Terra then Aqua
	for (sens_sel in senslist) {		# cycle on selected sensors
		
		# get http site addresses and file prefixes
		if (sens_sel == "Terra") {http = https[["Terra"]]} else {http =https[["Aqua"]]}
		if (sens_sel == "Terra") {file_prefix = file_prefixes[["Terra"]]} else {file_prefix = file_prefixes[["Aqua"]]}
		
		#- ------------------------------------------------------------------------------- -#
		#  Verify if bands needed for computing spectal indexes and/or quality indicators are already selected
		#  if not, select them and set the "delete" option for them to 1 
		#- ------------------------------------------------------------------------------- -#
		
		bands_indexes = matrix(0, nrow=length(bandsel), ncol=length(indexes_bandsel)+length(quality_bandsel),# dummy matrix which associate, to each couple of index or quality band (col) - original band (row),
				dimnames=list(bandnames,c(indexes_bandnames,quality_bandnames)))								# info on wether that band is required to build that index 
		
		bandsel_orig_choice = bandsel						# Save original choice of bands in bandsel_orig_choice (bandsel is later modified to set to 1 all bands needed for indexes and quality 

		
		for (band in which(indexes_bandsel == 1)) {  # cycle on selected indexes
			formula = indexes_formula[band]	  # If an index is selected retrieve its formula
			for (bandorig in seq(along = bandnames)) {		# cycle on original bands
				if (length(grep(bandnames[bandorig],formula)) > 0) {			# check if the original band is needed for the index
					if (bandsel[bandorig] == 0) {        # if yes and band not set to be processed, set it to be processed
						bands_indexes[bandorig,band] = 1
					}
				}
			} #End Cycle on bandorig
		} #End If on bandsel[band] == 1

		for (band in which(quality_bandsel == 1)) {  # cycle on selected QIs
			bandorig = which(bandnames == quality_source[band]) 		# Identify source band for the quality indicator selected
			if (bandsel[bandorig] == 0) {							# if ource not already selected to be processed, select it 
				bands_indexes[bandorig,length(indexes_bandsel)+band] = 1
			}
		} #End If on bandsel[band] == 1

		
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
			
			# Get a list of the folders containing hdf images required (Corresponding to the subfolders in lpdaac corresponding to 
			# selected product, dates, and current year under processing)
			
			date_dirs = lpdaac_getmod_dates(dates = dates, date_dirs =  lpdaac_getmod_dirs(http = http, .Platform = .Platform))  # First, find the folders in lpdaac corresponding to the required dates
			
			if (length(date_dirs) > 0) {
				modislist = NULL
				# Start Cycling on directories containing images to be downloaded and identify the required ones (i.e., the ones corresponding to selected tiles)
				for (date in 1:length(date_dirs)) {
					
					date_name <- sub(sub(pattern="\\.", replacement="_", date_dirs[date]), pattern="\\.", replacement="_", date_dirs[date])  #Create the date string
					DOY =strftime( as.Date(date_name,"%Y_%m_%d" ), format = "%j")  # transform date to DOY
					
					# check if all foreseen output rasters already exist. If so, skip the date. Otherwise start proecssing
					check_files = F
					check_files = moddwl_check_files(out_prod_folder, file_prefix,bandnames,bandsel_orig_choice,yy,DOY,out_format,  indexes_bandnames, indexes_bandsel, quality_bandnames, quality_bandsel)
					if (check_files == F | reprocess == 'Yes') {  		# If not all output files are already present or reprocess = "Yes", start downloading hdfs
						
						# Create vector of image names required (corresponding to the selected tiles, within current dir)
						modislist = lpdaac_getmod_names(http = http, date_dirs = date_dirs, date = date, v = seq(from=start_y, to =  end_y), h = seq(from = start_x, to = end_x))
						
						# ---------------------------------- ----------------------------------------------#
						# Download and preprocess Imagesin modislist vector -----------
						# ---------------------------------- ----------------------------------------------#
						if (length(modislist) > 0) {
							
							#- ------------------------------------------------------------------------------- -#
							#  Download images (If HDF file already in out_mod_folder, it is not redownloaded !!!!
							#- ------------------------------------------------------------------------------- -#	
							for (modisname in modislist) {	
								if (file.exists(file.path(out_folder_mod,modisname)) == F ) {		# If HDF not existing, download. 
									er <- 5		; 	class(er) <- "try-error" ;	ce <- 0
									while(er != 0) {   # repeat until no error or > 21 tryyouts
										print(paste('Downloading File: ', modisname ))
										svalue(mess_lab) = paste('--- Downloading Files for date', date_name, ':' ,which(modislist == modisname),' of ', length(modislist),' ---')    # Update progress window
										er <- tryCatch(download.file(url=paste(http,date_dirs[date], "/",modisname,sep=''),destfile=file.path(out_folder_mod,modisname),mode='wb',quiet=F, cacheOK=FALSE),
												warning=function(war) {print(war) ; return (1)}, error =function(err) {	print(err);	return (1)} )
										if (er != 0) {	# Stop after 21 failed attempts
											print('Download Error -Retrying') 
											Sys.sleep(10)   
											ce <- ce + 1 
											if (ce == 21) {
												unlink(file.path(out_folder_mod,modisname))  # on error, delete last hdf file (to be sure no incomplete files are left behind)
												stop("Error: http server is down ! Please Retry Later !")	
											}	
										}		
									}
								} # end IF on hdf existence
							} # End cycle for downloading the images in modislist vector 
							
							print (paste(length(modislist)," files for date of ",date_dirs[date]," were successfully downloaded!",sep=''))
							
							# -------------------------------------------------------------------------
							# After all required tiles for the date are downloaded, start geoprocessing 
							# -------------------------------------------------------------------------
							
							# -------------------------------------------------------------------------
							# STEP 1: choose the layers (original, indexes and quality bands) to be created
							# -------------------------------------------------------------------------
							
							# at the end of this step, "bandsel" is recreated as the union of the bands selected by the user and the bands required 
							# by indexes and quality bands, but only those ones which are not already present.
							
							req_bands_indexes <- bands_indexes
							for (i in 1:length(req_bands_indexes)) {req_bands_indexes[i]<-0}	# matrix similar to band_indexes, but specific for this year-doy process 
							
#							if (length(which(indexes_bandsel==1) >= 1)) {
#								for (band in seq(along = indexes_bandsel)) {
#									if (indexes_bandsel[band] ==1 ) {
							for (band in which(indexes_bandsel== 1)) {
								indexes_band =  indexes_bandnames[band]
								out_filename = file.path(out_prod_folder,indexes_band,paste(file_prefix,'_',indexes_band,'_',yy,'_', DOY, sep = ''))
								if (out_format =='GTiff') {out_filename = paste(out_filename, '.tif', sep = '')} else {out_filename = paste(out_filename, '.dat', sep = '')}
								if (file.exists(out_filename) == F | reprocess == "Yes") {
									req_bands_indexes[,band] <- bands_indexes[,band] # if the index does not exists then consider the original bands required for it
								}
							}
#								}
#							} #End If on length(which(indexes_bandsel ==1))
							
#							if (length(which(quality_bandsel==1) >= 1)) {
#								for (band in seq(along = quality_bandnames)) {
#									if (quality_bandsel[band] ==1 ) {
							for (band in which(quality_bandsel == 1)) {
								quality_band =  quality_bandnames[band]
								out_filename = file.path(out_prod_folder,quality_band,paste(file_prefix,'_',quality_band,'_',yy,'_', DOY, sep = ''))
								if (out_format =='GTiff') {out_filename = paste(out_filename, '.tif', sep = '')} else {out_filename = paste(out_filename, '.dat', sep = '')}
								if (file.exists(out_filename) == F | reprocess == "Yes") {
									req_bands_indexes[,band+length(indexes_bandsel)] <- bands_indexes[,band+length(indexes_bandsel)] # if the index does not exists then consider the original bands required for it
								}
							}
#								}
#							} #End If on length(which(quality_bandsel ==1))
							
							# Create the final vector of bands required for processing (bands chosen by the user + bands required for indexes and quality bands)
							bandsel <- as.integer(as.logical(bandsel_orig_choice + apply(req_bands_indexes,1,sum)))
							delbands = bandsel - bandsel_orig_choice    # dummy array set to 0 - will contain info on wether orignal downloaded bands has to be deleted
							
							# -----------------------------------
							# STEP 2: process the required original MODIS layers 
							# -----------------------------------
							
							for (band in 1:length(bandnames)) {														# Cycle on MODIS original layers
								
								bands = numeric(length(bandnames))													# Create vector with length = bands, filled with zeroes
								er_mos = 1  														# dummies for error state
								if (bandsel [band] == 1) {					# If band selected, process it
									svalue(mess_lab) =  (paste('--- Mosaicing ', bandnames[band],' files for date: ',date_name ,' ---'))
									bands[band]=1																			# IF band selected for processing, put its value to 1
									dir.create(file.path(out_prod_folder, bandnames[band]), showWarnings = F, recursive = T)
									bands = paste(as.character(bands), collapse = '', sep = ' ')					# Convert to character
									outfile = paste(tmp_prod_folder, '/',bandnames[band],'_',yy,'_',DOY,'.tif', sep = '')  	# Create name for the temporary tif mosaic
									outfile = paste(bandnames[band],'_',yy,'_',DOY,'.tif', sep = '')  	# Create name for the temporary tif mosaic
									# NOTE: Chanfe outrep_file to a list of rep files: only one for original bands, multiple for indexes and quality
									outrep_file = file.path(out_prod_folder, bandnames[band], paste(file_prefix,'_',sub("[.][^.]*$", "", basename(outfile), perl=TRUE),sep = ''))	# Create name for the TIFF reprojected  mosaic
									if (out_format =='GTiff') {outrep_file = paste(outrep_file, '.tif', sep = '')} else {outrep_file = paste(outrep_file, '.dat', sep = '')} 
									
									# integrate bandsel (bands directly selected) with band_indexes (bands needed by indexes or quality bands)
									
									outfile_vrt = paste(tmp_prod_folder, '/',bandnames[band],'_',yy,'_',DOY,'vrt.tif', sep = '')    # Create name for the vrt mosaic
									
									if (file.exists(outrep_file) == F | reprocess == 'Yes') {
										
										files_in = file.path(out_folder_mod, modislist)
										dir.create(tmp_prod_folder, showWarnings = FALSE)
										
										if (full_ext == 'Resized') { #If resize required,  convert bbox coordinates from t_srs to modis_srs, to get the correct extent
											# for resizing BEFORE reprojecting
											
											N_dens = 1000 # densification ratio of the bounding box
											d_bbox_out <- data.frame(lon=c(bbox[1]+diff(bbox[1:2])*(0:N_dens)/N_dens, rep(bbox[2],N_dens-1), bbox[1]+diff(bbox[1:2])*(N_dens:0)/N_dens, rep(bbox[1],N_dens-1)),
													lat=c(rep(bbox[3],N_dens), bbox[3]+diff(bbox[3:4])*(0:N_dens)/N_dens, rep(bbox[4],N_dens-1), bbox[3]+diff(bbox[3:4])*(N_dens:1)/N_dens))
											d_bbox_out <- SpatialPolygons(list(Polygons(list(Polygon(d_bbox_out)),1)))
											proj4string(d_bbox_out) <- CRS(outproj_str)
											d_bbox_mod <- spTransform(d_bbox_out, CRS(MOD_proj_str))
											# Create a resized and eventually mosaiced GDAL vrt file
											gdalbuildvrt(files_in, outfile_vrt, te = c(d_bbox_mod@bbox), tap = TRUE, tr = paste(rep(native_res,2),collapse=' '), sd = band) 
										} else {gdalbuildvrt(files_in, outfile_vrt,  sd = band) }  # Create a resized and eventually mosaiced GDAL vrt file
										# check if this also need to add tap (it should not)
										
										#Tranform the vrt to a physical resized TIFF file. 
#										er_mos = gdal_translate(outfile_vrt, outfile)
										er_mos = 1
#										if (is.null(er_mos) == FALSE)  {stop()}   # exit on error
										
										# ---------------------------------- ----------------------------------------------#
										# Convert to output projection, extent and format using gdalwarp ----
										# ---------------------------------- ----------------------------------------------#
										
										print (paste('Reprojecting ', bandnames[band],'files for date: ',date_name ))
										svalue(mess_lab) =  (paste('--- Reprojecting ', bandnames[band],'files for date: ',date_name,' ---'))
										
										# Launch the reprojection
										
										if (all.equal(as.numeric(native_res),out_res)==TRUE & outproj_str==MOD_proj_str) {	
											# If both IN/OUT resolution and projection are the same, run gdal_translate only to convert the vrt to the output format
											gdal_translate(outfile_vrt, outrep_file, a_srs=MOD_proj_str, of=out_format, ot=datatype[band], a_nodata=nodata_out[band], 
													co=paste('COMPRESS',compress,sep='='), overwrite=TRUE)
										} else if (full_ext == 'Full Tiles Extent' | outproj_str==MOD_proj_str) {	
											# If bounding box was not passed keep the original extent of th vrt when creating the File;
											# also in the case the output proj is in the MODIS sinusoidal, but a bbox was passed, 
											# don't pass again the bbox in order to keep the pixel alignment (vrt is already resized !) 
											gdalwarp(outfile_vrt, outrep_file, s_srs=MOD_proj_str, t_srs=outproj_str, of=out_format, r=resampling, tr=rep(out_res,2),
													co=paste('COMPRESS',compress,sep='='), wo="INIT_DEST=NO_DATA", wt=datatype[band], srcnodata=nodata_in[band], dstnodata=nodata_out[band], overwrite=TRUE)
										} else {						
											# If bounding box was passed, and projection is not the native one, the output reproject file will satisfy the bbox
											gdalwarp(outfile_vrt, outrep_file, s_srs=MOD_proj_str, t_srs=outproj_str, of=out_format, r=resampling, te=bbox[c(1,3,2,4)], tr=rep(out_res,2),
													co=paste('COMPRESS',compress,sep='='), wo="INIT_DEST=NO_DATA", wt=datatype[band], srcnodata=nodata_in[band], dstnodata=nodata_out[band], overwrite=TRUE)                    
										}  
										
										gc()
										xml_file = paste(outrep_file,'.aux.xml',sep = '')		# Delete xml files created by gdalwarp
										unlink(xml_file)
										unlink(tmp_prod_folder, recursive=TRUE)					# Delete temporary files in temp folder
									}   
								}  # ENDIF band selected for processing
							}	# END Cycle on available MODIS Bands
							
							# ---------------------------------- ----------------------------------------------#
							# If Indexes selected, then start creating them 
							# ---------------------------------- ----------------------------------------------#

							for (band in which(indexes_bandsel== 1)) {
								indexes_band =  indexes_bandnames[band] 	# index name
								formula = indexes_formula[band]				#index formula
								svalue(mess_lab) = paste('--- Computing',  indexes_band,' for date: ',date_name,' ---')
								print (paste('Computing ', indexes_band,' for date: ',date_name ))
								out_filename = file.path(out_prod_folder,indexes_band,paste(file_prefix,'_',indexes_band,'_',yy,'_', DOY, sep = ''))
								if (out_format =='GTiff') {out_filename = paste(out_filename, '.tif', sep = '')} else {out_filename = paste(out_filename, '.dat', sep = '')}
								dir.create(file.path(out_prod_folder,indexes_band), showWarnings = F, recursive = T) # create folder for index
								if (file.exists(out_filename) == F | reprocess == 'Yes') { #If file not existing and reprocess = No, compute the index and save it
									moddwl_process_indexes(out_filename = out_filename, formula = formula,bandnames=bandnames, nodata_out=nodata_out,
											indexes_nodata_out=indexes_nodata_out[band],out_prod_folder = out_prod_folder,  file_prefix=file_prefix, yy=yy,out_format=out_format, DOY=DOY )
								}
							}
							
							# ---------------------------------- ----------------------------------------------#
							# If Quality indicators selected , then start creating them 
							# ---------------------------------- ----------------------------------------------#

							for (band in which(quality_bandsel== 1)) {
								quality_band =  quality_bandnames[band]		 # indicatori name
								source = quality_source[band]  #  Original MODIS layer containing data of the indicator 
								bitN =  quality_bitN[band]  #  bitfields corresponding to indicator within source
								nodata_qa_in = quality_nodata_in [band]   
								nodata_qa_out = quality_nodata_out [band]  
								svalue(mess_lab) = paste('--- Computing',  quality_band,' for date: ',date_name,' ---')
								print (paste('Computing ', quality_band,' for date: ',date_name ))
								out_filename = file.path(out_prod_folder,quality_band,paste(file_prefix,'_',quality_band,'_',yy,'_', DOY, sep = ''))
								if (out_format =='GTiff') {out_filename = paste(out_filename, '.tif', sep = '')} else {out_filename = paste(out_filename, '.dat', sep = '')}
								dir.create(file.path(out_prod_folder,quality_band), showWarnings = F, recursive = T)
								if (file.exists(out_filename) == F | reprocess == 'Yes') { #If file not existing and reprocess = No, compute the indicator and save it
									moddwl_process_QA_bits(out_filename,in_raster_name = bandnames[grep(source,bandnames)], bitN, source, 
											out_prod_folder, file_prefix, yy, DOY, out_format, nodata_source = nodata_out [grep(source,bandnames)],
											nodata_qa_in , nodata_qa_out  )
								}
							}
						
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
							
							# If multiband BSQ required, sel_prod has reflectances, and reflectances were selectd, create an ENVI metafile ----
							# Removed for now - TBC  in next versions
							# if (multiband_bsq == T) {moddwl_refl_bsq(sel_prod, out_prod_folder,bandnames, bandsel_orig_choice, reflbands, reflorder ,file_prefix, yy, DOY)} 
							
						} else {print(paste("No available image for selected Tiles in ",date_dirs[dir], sep=""))} # End check on at least one image available
						
					} else {print (paste('All Required output files for date ',date_name, ' are already existing - Doing Nothing !', sep = ''))} # End check on all data already processed for date or reprocees = Yes
					
				}   # End cycling on available dates for selected year
				
			} else print (paste("No available data for year:  ", yy, "for Sensor",sens_sel," in selected dates"))
			
			#- ------------------------------------------------------------------------------- -#
			# If deletion selected, delete the HDF files in folder_mod directory 
			#- ------------------------------------------------------------------------------- -#
			if (delete_hdf == 'Yes') {  
				
				for (dir in 1:length(date_dirs)) {
					modislist = getmod_names(http = http, date_dirs = date_dirs, dir = dir, v = seq(from=start_y, to =  end_y), h = seq(from = start_x, to = end_x))
					for (modisname in modislist) {unlink(file.path(out_folder_mod,modisname))}
				}
			} #end if on Delete original downloaded HDFs	
			
		}	# End Cycling on selected years
		
		bandsel = bandsel_orig_choice  # reset bandsel to original user's choice
		
	} # End cycling on sensors
	
	#- ------------------------------------------------------------------------------- -#
	#  Create META files of time series - original and derived
	#- ------------------------------------------------------------------------------- -#
	
	
	if (sensor == 'Both') {senslist = c('Terra','Aqua','Mixed')} # selected sensors
	
	for (sens_sel in senslist) {		# cycle on selected sensors
		
		for (band in which(bandsel== 1)) {
			moddwl_meta_create(out_prod_folder = out_prod_folder, meta_band = bandnames[band], 
					file_prefix = file_prefixes, sens_sel, ts_format = ts_format,  nodata_value = nodata_out[band],out_format = out_format)
		} #End Cycle on bandsel
		
		for (band in which(indexes_bandsel== 1)) {
			moddwl_meta_create(out_prod_folder = out_prod_folder, meta_band = indexes_bandnames[band], 
					file_prefixes = file_prefixes, sens_sel = sens_sel, ts_format = ts_format, nodata_value = indexes_nodata_out[band],out_format = out_format)
		} #End Cycle on indexes_bandsel
		
		for (band in which(quality_bandsel== 1)) {
			
			moddwl_meta_create(out_prod_folder = out_prod_folder, meta_band = quality_bandnames[band]		, 
					file_prefixes = file_prefixes, sens_sel= sens_sel, ts_format = ts_format, nodata_value = quality_nodata_out[band],out_format = out_format)
		} #End Cycle on quality_bandsel
		
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
