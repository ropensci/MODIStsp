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
moddwl_process <- function(sel_prod, start_date,end_date ,out_folder, MRTpath ,reproj , reprocess , FTP , sensor,
		start_x , start_y ,	end_x , end_y , bbox , out_format, out_res, MOD_proj_str , outproj_str, nodata_in , nodata_out, datatype, 
		bandsel , bandnames , indexes_bandsel,  indexes_bandnames, indexes_formula, indexes_nodata_out, 
		quality_bandnames, quality_bandsel, quality_bitN ,quality_source, quality_nodata_in ,
		quality_nodata_out,	file_prefix , main_out_folder , multiband_bsq) {
	
	out_prod_folder = file.path(out_folder,main_out_folder)
	dir.create(out_prod_folder, showWarnings = F, recursive = T)
	
	start_year = unlist(strsplit(start_date, '[.]'))[1]    ;  end_year = unlist(strsplit(end_date, '[.]'))[1]    ;  del = F
	# ---------------------------------- #
	# Start Cycle on selected years
	# ---------------------------------- #
	for (yy in start_year:end_year) {
		
		# Create string representing the dates to be processed 
		if (yy == start_year & yy == end_year) {dates = c(start_date,end_date)}
		if (yy == start_year & yy != end_year) {dates = c(start_date,paste(as.character(yy),'.12.31', sep = ''))}
		if (yy != start_year & yy == end_year) {dates = c(paste(as.character(yy),'.12.31',end_date, sep = ''))}
		
		# Processing status message
		mess = gwindow(title = 'Processing Status', container = TRUE, width = 400, height = 40)
		size(mess) <- c(100,8)		;	addHandlerUnrealize(mess, handler = function(h,...) {return(TRUE)})
		mess_lab = glabel(text =paste('--- Retrieving Files for Year ',as.character(yy),' ---'), editable = FALSE, container = mess)
		
		# Create a list of the folders containing images to be downloaded (Corresponding to the selected dates)
		
		dirs = getmod_dates(dates = dates, dirs =  getmod_dirs(FTP = FTP, .Platform = .Platform))  ; if (length(dirs) < 1) stop("No available data for selected dates")
		
		# Start Cycling on directories containing images to be downloaded
		for (i in 1:length(dirs)) {
			
			# Create vector of image names corresponding to the selected tiles
			modislist = getmod_names(FTP = FTP, dirs = dirs, i = i, v = seq(from=start_y, to =  end_y), h = seq(from = start_x, to = end_x))
			date_name <- sub(sub(pattern="\\.", replacement="_", dirs[i]), pattern="\\.", replacement="_", dirs[i])	#Create the date string
			DOY = substr(modislist[1],14,16)
			
# ---------------------------------- ----------------------------------------------#
# Download and preprocess Imagesin modislist vector -----------
# ---------------------------------- ----------------------------------------------#
			
			if (length(modislist) > 0) {
				
				# Check if all expected out files for the selected date are already present.
				# If so, check_files is set to TRUE, MODIS hdf is not downloaded and bands are not processed
				
				check_files = F
#				check_files = moddwl_check_files(out_prod_folder, file_prefix,bandnames,bandsel,yy,DOY,out_format,  indexes_bandnames, indexes_bandsel, quality_bandnames, quality_bandsel)
				
				#- ------------------------------------------------------------------------------- -#
				#  Download images
				#- ------------------------------------------------------------------------------- -#
				if (check_files == F | reprocess == T) {			# If not all output files are present, start downloading hdfs
					
					for (modisname in modislist) {	
						if (file.exists(file.path(out_prod_folder,modisname)) == F ) {		# If HDF not existing, download. 
							er <- 5		; 	class(er) <- "try-error" ;	ce <- 0
							while(er != 0) {   # repeat until no error or > 21 tryyouts
								print(paste('Downloading File: ', modisname ))
								svalue(mess_lab) = paste('--- Downloading Files for date', date_name, ':' ,which(modislist == modisname),' of ', length(modislist),' ---')    # Update progress window
#								er <- try(download.file(url=paste(FTP,dirs[i], "/",modisname,sep=''),destfile=file.path(out_prod_folder,modisname),
#												mode='wb',quiet=F, cacheOK=FALSE),silent=FALSE)   # Start download
								
								er <- tryCatch(download.file(url=paste(FTP,dirs[i], "/",modisname,sep=''),destfile=file.path(out_prod_folder,modisname),mode='wb',quiet=T, cacheOK=FALSE),
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
					mosaicname = file(paste(MRTpath, "/TmpMosaic.prm", sep=""), open="wt")
					write(paste(out_prod_folder,"/",modislist[1], sep=""), mosaicname)
					if (length(modislist) >1) {for (j in 2:length(modislist)) write(paste(out_prod_folder,"/",modislist[j], sep=""),mosaicname,append=T)}
					close(mosaicname)
					
					# Cycle on bands to mosaic and reproject them them 
					
					for (band in 1:length(bandnames)) {														# Cycle on MODIS Bands
						
						bands = numeric(length(bandnames))													# Create vector with length = bands, filled with zeroes
						er_mos = 1    ; er_rep = 1																	# dummies for error state
						if (bandsel [band] == 1) {					# If band selected, process it
							svalue(mess_lab) =  (paste('--- Mosaicing ', bandnames[band],' files for date: ',date_name ,' ---'))
							bands[band]=1																			# IF band selected for processing, put its value to 1
							dir.create(file.path(out_prod_folder, bandnames[band]), showWarnings = F, recursive = T)
							bands = paste(as.character(bands), collapse = '', sep = ' ')					# Convert to character
							outfile = paste(out_prod_folder, '/',bandnames[band],'_',yy,'_',DOY,'.hdf', sep = '')  	# Create name for the HDF mosaic
							# Launch MRT to mosaic
							if (file.exists(outfile) == F | reprocess == T) {
								er_mos <- system(paste(MRTpath, '/mrtmosaic -i ', MRTpath, '/TmpMosaic.prm' ,' -o ', outfile,' -s ',bands, sep=""), show.output.on.console = F)	# Launche MRT to create the mosaic
								if (er_mos != 0)  {stop()}   # exit on error
							}
							# ---------------------------------- ----------------------------------------------#
							# Convert to output projection, extent and format using gdalwarp ----
							# ---------------------------------- ----------------------------------------------#
							
							print (paste('Reprojecting ', bandnames[band],'files for date: ',date_name ))
							svalue(mess_lab) =  (paste('--- Reprojecting ', bandnames[band],'files for date: ',date_name,' ---'))
							outrep_file = file.path(out_prod_folder, bandnames[band], paste(file_prefix,'_',sub("[.][^.]*$", "", basename(outfile), perl=TRUE),sep = ''))	# Create name for the TIFF reprojected  mosaic
							
							if (out_format =='GTiff') {outrep_file = paste(outrep_file, '.tif', sep = '')} else {outrep_file = paste(outrep_file, '.dat', sep = '')} 
							if (file.exists(outrep_file) == F | reprocess == T) {
								
								
								# Create the string for gdal and launch the reprojection in a command shell ----
								
								if (length(which(is.finite(as.numeric(bbox)))) == 4) {	# If bounding box was passed, the output reproject file will satisfy the bbox
#									browser()
									er_rep = 	system(paste('gdalwarp -s_srs "',MOD_proj_str, 	# Launch GDAL to crete the reprojected File
													'" -t_srs "', outproj_str, '" -of ',out_format,' -r near -co compress=lzw',
													'-te',bbox[1],bbox[3],bbox[2],bbox[4], '-tr ',out_res, out_res,
													'-wo "INIT_DEST=NO_DATA"', '-wt ', datatype[band],'-srcnodata ', nodata_in[band],'-dstnodata ', nodata_out[band], '-overwrite',
													outfile,  outrep_file, #'-overwrite',outfile,  outrep_file,
													sep = ' '), show.output.on.console = F)
								} else {						# If bounding box was not passed, keep the original extent when creating the File
									er_rep = 	system(paste('gdalwarp -s_srs "',MOD_proj_str,
													'" -t_srs "', outproj_str,'" -of ',out_format,' -r near -co compress=lzw',
													'-tr ',out_res,out_res,
													'-wo "INIT_DEST=NO_DATA"','-wt ', datatype[band],'-srcnodata ', nodata_in[band],'-dstnodata ',nodata_out[band],
													'-overwrite', outfile,  outrep_file, #'-overwrite',outfile,  outrep_file,
													sep = ' '), show.output.on.console = F)
									
								}  
								
								if (er_rep != 0)  {stop()}
							}  # End if on file existence
							gc()
							unlink(outfile)																			# Delete un-reprojected Mosaic HDF file
							xml_file = paste(outrep_file,'.aux.xml',sep = '')		# Delete xml files created by gdalwarp
							unlink(xml_file)
							
						}  # ENDIF band selected for processing
					}	# END Cycle on available MODIS Bands
					
					# ---------------------------------- ----------------------------------------------#
					# If Indexes selected, then start creating them 
					# ---------------------------------- ----------------------------------------------#
					
#					
					if (length(which(indexes_bandsel==1) >= 1)) {
						for (band in seq(along = indexes_bandnames)) {
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
						
					} #End If on length(which(indexes_bandsel ==1))
					
					# ---------------------------------- ----------------------------------------------#
					# If Quality indicators, then start creating them 
					# ---------------------------------- ----------------------------------------------#
					
					if (length(which(quality_bandsel==1) >= 1)) {
						for (band in seq(along = quality_bandnames)) {
							browser()
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
						
					} #End If on length(which(quality_bandsel ==1))
					
				} else {print (paste('All Required output files for date ',date_name, ' are already existing - Doing Nothing !', sep = ''))}
				
				# If multiband BSQ required and sel_prod has reflectances, create an ENVI metafile ----
				if (multiband_bsq == T) {moddwl_refl_bsq(sel_prod, out_prod_folder,bandnames, bandsel ,file_prefix, yy, DOY )} 
				
				
				
				if (del == T) {for (modisname in modislist) unlink(file.path(out_prod_folder,modisname))}		# Delete original downloaded HDFs
			} else {print(paste("No available image for selected Tiles in ",dirs[i], sep=""))}
			
		} # End Cycling on dates in selected year
	}	# End Cycling on selected years
	
#- ------------------------------------------------------------------------------- -#
#  Create META files of time series - original and derived
#- ------------------------------------------------------------------------------- -#
	for (band in seq(along = bandnames)) {
		
		if (bandsel[band] == 1) {
			
			meta_band = bandnames[band]				
			moddwl_meta_create(out_prod_folder = out_prod_folder, meta_band = meta_band, file_prefix = file_prefix)
			
		} #End If on bandsel[band] == 1
	} #End Cycle on band
	
	for (band in seq(along = indexes_bandnames)) {
		
		if (indexes_bandsel[band] == 1) {
			
			meta_band = indexes_bandnames[band]				
			moddwl_meta_create(out_prod_folder = out_prod_folder, meta_band = meta_band, file_prefix = file_prefix)
			
		} #End If on bandsel[band] == 1
	} #End Cycle on band
	
#- ------------------------------------------------------------------------------- -#	
# Close GUI and clean
#- ------------------------------------------------------------------------------- -#
	gc()
	addHandlerUnrealize(mess_lab, handler = function(h,...) {return(FALSE)})		# Allow message lab to be closed since processing ended .
	dispose(mess_lab)
	return('DONE')
}
