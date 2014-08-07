#' @Title moddwl_process
#' @Description Function used to automatically download, mosaic, reproject and resize, 
#' preprocess MODIS images of the selected MODIS product 
#' 
#' @details
#'
#' @param product selected MODIS product
#' @param start_date 
#' @param end_date 
#' @param out_folder 
#' @param MRTpath Path of MRT executable
#' @param reproj Always T
#' @param reprocess T = Redownload/reprocess existing dates
#' @param FTP http site for MODIS download
#' @param bbox output bounding box (xmin, xmax, ymin, ymax ) in out proj coords
#' @param format ENVI or GTiff
#' @param start_x start horiz. tile
#' @param start_y start vert. tile
#' @param end_x end horiz. tile
#' @param end_y end vert. tile
#' @param nodata_in array of input nodata for MODIS bands
#' @param nodata_out array of ouput nodata for MODIS bands
#' @param bandsel 0-1 array. 1 = band to be processed
#' @param bandnames Names of MODIS bands
#' @param derived_bandsel 0-1 array. 1 = derived band to be created
#' @param derived_bandnames Names of MODIS derived bands 
#' @param datatype array of datatypes for MODIS bands
#' @param MOD_prj_str Proj4 string for MODIS SIN proj.
#' @param outproj_str Proj4 string for output proj
#' @param out_res Output resolution (Resampling in always NN - to be changed !)
#' @param sensor Terra or Aqua
#' @param file_prefix output file prefix according to product
#' @param main_out_folder folder file prefix according to product
#' @param multiband_bsq If T, and product has reflectance bands, create virtual BSQ file for each DOY
#'
#' @return 
#' NULL
#' 
#' @author Lorenzo Busetto, phD (2014). Derived from Original functions by Babak Naimi (naimi@r-gis.net) - Modified by Lorenzo Busetto for producing separated mosaics
#' for the different SDS,and for converting Quality data bits into meaningful QC and UI information (Thanks Tomislav Hengl as his script made the main core of this function [spatial-analyst.net])
#' (Thanks Tomislav Hengl as his script made the main core of this function [spatial-analyst.net] Reference: http://www.r-gis.net)
#' email: busetto.l@@irea.cnr.it
#'
#' @license GPL(>2)
#' @export

moddwl_process <- function(product, start_date,end_date ,out_folder, MRTpath ,reproj , reprocess , FTP , sensor,
		start_x , start_y ,	end_x , end_y , bbox , format, out_res, MOD_prj_str , outproj_str, nodata_in , nodata_out,derived_nodata_out, datatype, 
		bandsel , bandnames , derived_bandsel,  derived_bandnames  ,
		file_prefix , main_out_folder , multiband_bsq) {
	
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
				
				# Accessory function used to see if all expected out files for the selected date are already present.
				# If so, check_files is set to TRUE, and MODIS hdf is not downloaded
				moddwl_check_files = function(out_prod_folder, bandnames) {check = T
					for (band in 1:length(bandnames)) {
						if (bandsel [band] == 1) {
							outfile = paste(out_prod_folder, '/',bandnames[band],'_',yy,'_',DOY,'.hdf', sep = '')    # Create name for the HDF mosaic
							outrep_file = file.path(out_prod_folder, bandnames[band], paste(file_prefix,'_',sub("[.][^.]*$", "", basename(outfile), perl=TRUE),sep = ''))  # Create name for the TIFF reprojected  mosaic
							if (format =='GTiff') {outrep_file = paste(outrep_file, '.tif', sep = '')}
							if (format =='ENVI') {outrep_file = paste(outrep_file, '.dat', sep = '')}
							if (file.exists(outrep_file) == F) {check = F}
						}
					}
					return(check)
				}
				check_files = moddwl_check_files(out_prod_folder, bandnames)
				
				
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
								er <- try(download.file(url=paste(FTP,dirs[i], "/",modisname,sep=''),destfile=file.path(out_prod_folder,modisname),
												mode='wb',quiet=T, cacheOK=FALSE),silent=FALSE)   # Start download
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
							
							if (format =='GTiff') {outrep_file = paste(outrep_file, '.tif', sep = '')} else {outrep_file = paste(outrep_file, '.dat', sep = '')} 
							if (file.exists(outrep_file) == F | reprocess == T) {
								

							# Create the string for gdal and launch the reprojection in a command shell ----

								if (length(which(is.finite(as.numeric(bbox)))) == 4) {	# If bounding box was passed, the output reproject file will satisfy the bbox
									
									er_rep = 	system(paste('gdalwarp -s_srs "',MOD_prj_str, 	# Launch GDAL to crete the reprojected File
													'" -t_srs "', outproj_str, '" -of ',format,' -r near -co compress=lzw',
													'-te',bbox[1],bbox[3],bbox[2],bbox[4], '-tr ',out_res, out_res,
													'-wo "INIT_DEST=NO_DATA"', '-wt ', datatype[band],'-srcnodata ', nodata_in[band],'-dstnodata ', nodata_out[band], '-overwrite',
													outfile,  outrep_file, #'-overwrite',outfile,  outrep_file,
													sep = ' '), show.output.on.console = F)
								} else {						# If bounding box was not passed, keep the original extent when creating the File
									er_rep = 	system(paste('gdalwarp -s_srs "',MOD_prj_str,
													'" -t_srs "', outproj_str,'" -of ',format,' -r near -co compress=lzw',
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
					
				} else {print (paste('All Required output files for date ',date_name, ' are already existing - Doing Nothing !', sep = ''))}
				
				# If multiband BSQ required and product has reflectances, create an ENVI metafile ----
				if (multiband_bsq == T) {moddwl_refl_bsq(product, out_prod_folder,bandnames, bandsel ,file_prefix, yy, DOY )} 
				
				# ---------------------------------- ----------------------------------------------#
				# If derived or quality bands selected, then start creating them 
				# ---------------------------------- ----------------------------------------------#
				
				if (length(which(derived_bandsel==1) >= 1)) {
					
					for (derived in seq(along = derived_bandnames)) {
						
						if(derived_bandsel[derived] == 1) {
							browser()
							derived_band =  derived_bandnames[derived]
							fun_name = paste('moddwl_process_',derived_band, sep = '')
							svalue(mess_lab) = paste('--- Computing',  derived_band,' for date: ',date_name,' ---')
							print (paste('Computing ', derived_band,' for date: ',date_name ))
							
							
							do.call(fun_name,list(derived_band, out_prod_folder,bandnames,file_prefix,yy, DOY, nodata_out, derived_nodata_out = derived_nodata_out[bandsel], format ))
							gc()
							
							
								moddwl_process_QA_bits <- function(out_prod_folder, derived_band, bandnames, file_prefix, yy, DOY, format, nodata_out) {
									out_dir = file.path(out_prod_folder,derived_band)
									dir.create(out_dir, recursive = T)
									rasterOptions(setfileext = F)
									
									in_raster = bandnames[grep('State_1Km',bandnames)]
									bit_file  =	file.path(out_prod_folder, in_raster,paste(file_prefix,'_',in_raster,'_',yy,'_', DOY, '.dat', sep = ''))
									in_raster = raster(bit_file, format = format)				# Open QA file
									NAvalue(in_raster)<- as.numeric(nodata_out [grep('State_1Km',bandnames)])
									in_values = getValues(in_raster)								# Get the values
									QC_vals = bitAnd(in_values,3)								# Get the QA (First 2 bits)
									
									# Save the QA to new file
									in_raster = setValues(in_raster, values=QC_vals)
									out_file = file.path(out_dir,paste(file_prefix,'_',derived_band,'_',yy,'_', DOY, sep = ''))
									if (format =='GTiff') {out_file = paste(out_file, '.tif', sep = '')} else {out_file = paste(out_file, '.dat', sep = '')}  
									writeRaster(in_raster,out_file, format = format ,overwrite = TRUE, datatype = 'INT1U')
								}
								moddwl_process_QA_bits(out_prod_folder = out_prod_folder, derived_band = derived_band, bandnames = bandnames, file_prefix = file_prefix, yy = yy, DOY = DOY, format = format, nodata_out = nodata_out)

							
							
							
							
							
							
							
						}
					}
				} #End If on length(which(derived_bandsel ==1))
				
				
				if (del == T) {for (modisname in modislist) unlink(file.path(out_prod_folder,modisname))}		# Delete original downloaded HDFs
			} else {print(paste("No available image for selected Tiles in ",dirs[i], sep=""))}
		} # End Cycling on directories containing images to be downloaded (i.e., dates)
		
		
	}	# End Cycling on years
	browser()
#- ------------------------------------------------------------------------------- -#
#  Create META files of time series
#- ------------------------------------------------------------------------------- -#
	for (band in seq(along = bandnames)) {
		if (bandsel[band] == 1) {
			meta_band = bandnames[band]				
			moddwl_meta_create(out_prod_folder = out_prod_folder, meta_band = meta_band)
			
		} #End If on bandsel[band] == 1
	} #End Cycle on band


	addHandlerUnrealize(mess_lab, handler = function(h,...) {return(FALSE)})		# Allow message lab to be closed since processing ended .
	dispose(mess_lab)
	
	return('DONE')
}

# ----- Accessory Functions -------------#


