LB_MOD_DWL_MOD11 = function ( ){
  
  
  
}


DWL_MOD11 = function(Start_Date=Start_Date,End_Date = End_Date, OutPath = OutPath,  ovr = ovr, del = del, reproj = reproj, opts = opts,
                     bandnames = bandnames,bands_subset=bands_subset,nodatas=nodatas,nodata_out=nodata_out, t_srs = t_srs) {
  
    Start_Year = unlist(strsplit(Start_Date, '[.]'))[1]
    End_Year = unlist(strsplit(End_Date, '[.]'))[1]
    dir.create(OutPath, recursive = TRUE)
    
    # ---------------------------------- #	
    # Start Cycle on selected years
    # ---------------------------------- #	
    for (yy in Start_Year:End_Year) {
      
      # Create string representing the dates to be processed 
      if (yy == Start_Year & yy == End_Year) {dates = c(Start_Date,End_Date, sep = '')}
      if (yy == Start_Year & yy != End_Year) {dates = c(Start_Date,paste(as.character(yy),'.12.31', sep = ''))}
      if (yy != Start_Year & yy == End_Year) {dates = c(paste(as.character(yy),'.12.31',End_Date, sep = ''))}
      
      # Processing status message
      mess = gwindow(title = 'Processing Status', container = TRUE, width = 400, height = 40)
      size(mess) <- c(100,8)		;	addHandlerUnrealize(mess, handler = function(h,...) {return(TRUE)})	
      mess_lab = glabel(text =paste('--- Retrieving Files for Year ',as.character(yy),' ---'), editable = FALSE, container = mess)
      
      # Create a list of the folders containing images to be downloaded (Corresponding to the selected dates) 
      
      dirs <- get_MOD_dirs(FTP = opts$FTP[1], .Platform = .Platform)
      dirs = get_MOD_dirs_dates(dates = dates, dirs = dirs)
      if (length(dirs) < 1) stop("No available data for selected dates")
      
      # Start Cycling on directories containing images to be downloaded
      for (i in 1:length(dirs)) {
        
        # Create vector of image names corresponding to the selected tiles
        Modislist = GET_MOD_NAMES(FTP = opts$FTP[1], dirs = dirs, i = i, v = v, h = h)
        date_name <- sub(sub(pattern="\\.", replacement="_", dirs[i]), pattern="\\.", replacement="_", dirs[i])	#Create the date string
        
        # ---------------------------------- ----------------------------------------------#	
        # Start cycle for downloading the images in Modislist vector  
        # ---------------------------------- ----------------------------------------------#	
        
        if (length(Modislist) > 0) {
          for (ModisName in Modislist) {
            if (file.exists(ModisName)  == FALSE | ovr == TRUE ) {
              er <- 5		; 	class(er) <- "try-error" ;	ce <- 0
              while(er != 0) {
                print(paste('Downloading File: ', ModisName ))
                svalue(mess_lab) = paste('--- Downloading Files for date', date_name, ':' ,which(Modislist == ModisName),' of ', length(Modislist),' ---')    # Update progress window
                #old version
                #               browser()
                er <- try(download.file(url=paste(opts$FTP[1],dirs[i], "/",ModisName,sep=''),destfile=ModisName,mode='wb',quiet=F, cacheOK=FALSE),silent=FALSE)   # Start download
                if (er != 0) {
                  print('Download Error -Retrying')
                  Sys.sleep(10)   ;	ce <- ce + 1 ; 	if (ce == 21) stop("Error: FTP server is down!!")	}		# Stop after 21 failed attempts
              }
            }
          }  
          
          # End cycle for downloading the images in Modislist vector	
          
          print (paste(length(Modislist)," files for date of ",dirs[i]," were successfully downloaded!",sep=''))
          
          # ---------------------------------- ----------------------------------------------#	
          # Create the temporary parameter file for MRT mosaic function
          # ---------------------------------- ----------------------------------------------#	
          mosaicname = file(paste(opts$MRTpath[1], "/TmpMosaic.prm", sep=""), open="wt")
          write(paste(OutPath,"/",Modislist[1], sep=""), mosaicname)
          if (length(Modislist) >1) {for (j in 2:length(Modislist)) write(paste(getwd(),"/",Modislist[j], sep=""),mosaicname,append=T)}
          close(mosaicname)
          
          # ---------------------------------- ----------------------------------------------#	
          # Run the MRT tool to generate the mosaic HDFs. One separate HDF is generated for each selected band
          # Added by L.Busetto --- Instead of a single mosaic,  one different mosaic for each selected band will be created.
          # This is useful in the case of very large mosaics !
          # ---------------------------------- ----------------------------------------------#	
          
          for (band in 1:length(bands_subset)) {														# Cycle on MODIS Bands
            bands = numeric(length(bands_subset))													# Create vector with length = bands, filled with zeroes
            er_mos = 1    ; er_rep = 1
            if (bands_subset[band] == 1) {	
              # 							while(er_mos != 0 &  er_rep != 0) {
              svalue(mess_lab) =  (paste('--- Mosaicing ', bandnames[band],'files for date: ',date_name ,' ---'))
              bands[band]=1																			# IF band selected for processing, put its value to 1
              
              dir.create(file.path(OutPath, bandnames[band]), showWarnings = F, recursive = T)
              bands = paste(as.character(bands), collapse = '', sep = ' ')					# Convert to character
              outfile = paste(OutPath, '/',bandnames[band],'_',substr(Modislist[1],10,13),'_',substr(Modislist[1],14,16),'.hdf', sep = '')		# Create name for the HDF mosaic
              # 								browser()
              er_mos <- system(paste(opts$MRTpath[1], '/mrtmosaic -i ', opts$MRTpath[1], '/TmpMosaic.prm' ,' -o ', outfile,' -s ',bands, sep=""))	# Launche MRT to create the mosaic
              if (er_mos != 0)  {stop()}
              # If reprojection requested, convert to output projection using gdalwarp
              if (reproj == T) {
                print (paste('Reprojecting ', bandnames[band],'files for date: ',date_name ))
                svalue(mess_lab) =  (paste('--- Reprojecting ', bandnames[band],'files for date: ',date_name,' ---'))
                outrep_file = file.path(OutPath, bandnames[band],paste(sub("[.][^.]*$", "", basename(outfile), perl=TRUE),'.tif',sep = ''))	# Create name for the TIFF reprojected  mosaic
                
                # ---------------------------------- ----------------------------------------------#	
                # Create the string for gdal and launch the reprojection in a command shell
                # ---------------------------------- ----------------------------------------------#	
                # 									browser()
                # If bounding box was passed, the output reproject file will satisfy the bbox
                if (length(opts$bbox) == 4) {
                  #browser()
                  er_rep = 	system(paste('gdalwarp -s_srs "', opts$MOD_prj_str[1], 	# Launch GDAL to crete the reprojected TIFF
                                         '" -t_srs "', opts$out_prj_str[1], '" -of ',out_format,' -r near -co compress=lzw',
                                         '-te',opts$bbox[1],opts$bbox[2],opts$bbox[3],opts$bbox[4], '-tr ',opts$pixel_size[1], opts$pixel_size[1],
                                         '-wo "INIT_DEST=NO_DATA"','-srcnodata ', nodatas[band],'-dstnodata ', nodata_out[band], '-overwrite',
                                         outfile,  outrep_file, #'-overwrite',outfile,  outrep_file,
                                         sep = ' '))	
                } 
                else {						# If bounding box was not passed, keep the original extent when creating the TIFF
                  er_rep = 	system(paste('gdalwarp -s_srs "',opts$MOD_prj_str[1], 
                                         '" -t_srs "', opts$out_prj_str[1],'" -of ',out_format,' -r near -co compress=lzw',
                                         '-tr ',opts$pixel_size[1], opts$pixel_size[1],
                                         '-wo "INIT_DEST=NO_DATA"','-srcnodata ',  nodata_out[band],'-dstnodata -999',
                                         '-overwrite', outfile,  outrep_file, #'-overwrite',outfile,  outrep_file,
                                         sep = ' '))			
                }
                if (er_rep != 0)  {stop()}
              } 
              
              if (bandnames [band] == 'LST_QA') {
                QA_File = outrep_file
                er = MOD11_QA_to_QC(OutPath, QA_File)
              }
              # 							}
              unlink(outfile)														# Delete un-reprojected Mosaic HDF file
              xml_file = paste(outrep_file,'.aux.xml',sep = '')		# Delete xml files created by gdalwarp
              unlink(xml_file)
            }  # ENDIF band selected for processing
          }	# END Cycle on selected MODIS Bands
          #}
          if (del == T) {for (ModisName in Modislist) unlink(paste(getwd(), '/', ModisName, sep=""))}		# Delete original downloaded HDFs
          
        } # ENDIF on lenght of Modislist
        
        else {print(paste("No available image for selected Tiles in ",dirs[i], sep=""))}
        
      }	# End Cycling on directories containing images to be downloaded (i.e., dates)
      
      addHandlerUnrealize(mess_lab, handler = function(h,...) {return(FALSE)})		# Allow message lab to be closed since processing ended .
      dispose(mess_lab)
    }
    return('DONE')
  }	
  
}

# ----- Accessory Functions -------------#

# ---------------------------------- ----------------------------------------------#  
# Accessory function to convert Quality data bits into QC and UI readable values (see https://lpdaac.usgs.gov/products/modis_products_table/mod13q1)
# ---------------------------------- ----------------------------------------------#	

MOD11_QA_TO_QC <- function(OutPath, QA_File) {
  
  # 	UI_Dir = file.path(OutPath,'UI','Single_Dates')			# Create folders for storing QC and UI images
  QC_Dir = file.path(OutPath,'LST_QC')
  # 	dir.create(UI_Dir, recursive = T)
  dir.create(QC_Dir, recursive = T)
  rasterOptions(setfileext = F)
  
  in_raster = raster(QA_File, format = 'GTiff')				# Open QA file
  in_values = getValues(in_raster)								# Get the values
  QC_vals = bitAnd (in_values,3)								# Get the QA (First 2 bits)
  # 	UI_vals <- bitAnd(bitShiftR(in_values,2),15)			# Get the UI (Bits 3-6)
  
  # Save the QA to new file
  out_qc_raster = raster(in_raster)
  out_qc_raster = setValues(out_qc_raster, values=QC_vals) 
  QC_File = gsub( 'QA','QC',QA_File)
  writeRaster(out_qc_raster,QC_File, format = 'ENVI' ,overwrite = TRUE, datatype='INT1U')
  
  # Save the UI to new file
  # 	out_ui_raster = raster(in_raster)
  # 	out_ui_raster=setValues(out_ui_raster, value = UI_vals) 
  # 	UI_File = gsub( 'QA','UI',QA_File)
  # 	writeRaster(out_ui_raster,UI_File, format = 'ENVI' ,overwrite = TRUE, datatype='INT1U')
  
  # Remove automatically created XML files
  # 	xml_file = paste(UI_File,'.aux.xml',sep = '')
  # 	unlink(xml_file)
  xml_file = paste(QC_File,'.aux.xml',sep = '')
  unlink(xml_file)
  
  #	}
}

  