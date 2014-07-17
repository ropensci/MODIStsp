
#'@title MOD09QC_Download
#' @author Lorenzo Busetto  - Derived from Original functions by Babak Naimi (naimi@r-gis.net) - Modified by Lorenzo Busetto for producing separated mosaics
#' for the different SDS,and for converting Quality data bits into meaningful QC and UI information (Thanks Tomislav Hengl as his script made the main core of this function [spatial-analyst.net])
#' (Thanks Tomislav Hengl as his script made the main core of this function [spatial-analyst.net] Reference: http://www.r-gis.net)
#'
#'@description This function allows to automatically download, mosaic,  reproject and resize MOD13Q1 images for selected MODIS tiles and years
#'
#' @param Start_Year: Starting year.
#' @param End_Year: Ending Year. (All images available between start and end year will be processed)
#' @param OutPath: Output folder. Images will be organized in subfolders of this one
#' @param ovr: Overwrite flag (deprecated)
#' @param del: Deletion flag. If TRUE (default), then original HDF files will be deleted after processing
#' @param reproj: Reprojection flag (deprecated - always TRUE - to be removed)
#' @param opts : options for processing. Data frame with the following fields
#' 							FTP = Set LDAAC ftp site name)
#' 							MRTpath= Path to MRT bin folder (Needed for mosaicing)
#' 							MOD_prj_str = 	# proj4 string for MODIS ISIN projection
#' 							out_prj_str = Proj4 string for output projectiongf
#'					 		pixel_size =  Desired output pixel size
#' 							bbox =    # Bounding box for output mosaic (In Out_Proj coordinates; Xmin, Ymin, Xmax, Ymax)
#'
#' @return
#' NONE
#'
#' @author Lorenzo Busetto (2013)
#' @email: lorenzo.busetto@@jrc.ec.europa.eu
#' @Created_Date: Mar 19, 2013
#' @export

# ----- Start of Main download and processing Function -------------#

Moddwl_Process <- function(product = product, Start_Date=Start_Date,End_Date = End_Date,out_folder = out_folder, MRTpath = MRTpath,
                           reproj = reproj, reprocess = reprocess, FTP = FTP,
                           bbox = bbox, format = format, start_x = start_x, start_y = start_y,
                           end_x = end_x, end_y = end_y, nodata_in = nodata_in, nodata_out= nodata_out,
                           bandnames = bandnames, bands_subset = bands_subset, MOD_prj_str = MOD_prj_str, outproj_str = outproj_str,
                           out_res = out_res,  sensor = sensor) {

  dir.create(out_folder, recursive = TRUE)
  Start_Year = unlist(strsplit(Start_Date, '[.]'))[1]    ;  End_Year = unlist(strsplit(End_Date, '[.]'))[1]    ;
  ovr = T; del = T
  # ---------------------------------- #
  # Start Cycle on selected years
  # ---------------------------------- #
  for (yy in Start_Year:End_Year) {

    # Create string representing the dates to be processed - Modify as needed
    if (yy == Start_Year & yy == End_Year) {dates = c(Start_Date,End_Date, sep = '')}
    if (yy == Start_Year & yy != End_Year) {dates = c(Start_Date,paste(as.character(yy),'.12.31', sep = ''))}
    if (yy != Start_Year & yy == End_Year) {dates = c(paste(as.character(yy),'.12.31',End_Date, sep = ''))}

    # Processing status message
    mess = gwindow(title = 'Processing Status', container = TRUE, width = 400, height = 40)
    size(mess) <- c(100,8)		;	addHandlerUnrealize(mess, handler = function(h,...) {return(TRUE)})
    mess_lab = glabel(text =paste('--- Retrieving Files for Year ',as.character(yy),' ---'), editable = FALSE, container = mess)

    # Create a list of the folders containing images to be downloaded (Corresponding to the selected dates)

    dirs <- getmod_dirs(FTP = FTP, .Platform = .Platform)
    dirs = getmod_dates(dates = dates, dirs = dirs)
    if (length(dirs) < 1) stop("No available data for selected dates")

    # Start Cycling on directories containing images to be downloaded
    for (i in 1:length(dirs)) {

      # Create vector of image names corresponding to the selected tiles
      Modislist = getmod_names(FTP = FTP, dirs = dirs, i = i, v = seq(from=start_y, to =  end_y), h = seq(from = start_x, to = end_x))
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
              er <- try(download.file(url=paste(FTP,dirs[i], "/",ModisName,sep=''),destfile=file.path(out_folder,ModisName),mode='wb',quiet=F, cacheOK=FALSE),silent=FALSE)   # Start download
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
        mosaicname = file(paste(MRTpath[1], "/TmpMosaic.prm", sep=""), open="wt")
        write(paste(out_folder,"/",Modislist[1], sep=""), mosaicname)
        if (length(Modislist) >1) {for (j in 2:length(Modislist)) write(paste(getwd(),"/",Modislist[j], sep=""),mosaicname,append=T)}
        close(mosaicname)

        # ---------------------------------- ----------------------------------------------#
        # Run the MRT tool to generate the mosaic HDFs. One separate HDF is generated for each selected band
        # Added by L.Busetto --- Instead of a single mosaic,  one different mosaic for each selected band will be created.
        # This is useful in the case of very large mosaics !
        # ---------------------------------- ----------------------------------------------#
        # 					browser()
        for (band in 1:length(bands_subset)) {														# Cycle on MODIS Bands
          bands = numeric(length(bands_subset))													# Create vector with length = bands, filled with zeroes
          er_mos = 1    ; er_rep = 1
          if (bands_subset[band] == 1) {
            svalue(mess_lab) =  (paste('--- Mosaicing ', bandnames[band],'files for date: ',date_name ,' ---'))
            bands[band]=1																			# IF band selected for processing, put its value to 1
            dir.create(file.path(out_folder, bandnames[band]), showWarnings = F, recursive = T)
            bands = paste(as.character(bands), collapse = '', sep = ' ')					# Convert to character
            outfile = paste(out_folder, '/',bandnames[band],'_',substr(Modislist[1],10,13),'_',substr(Modislist[1],14,16),'.hdf', sep = '')  	# Create name for the HDF mosaic
            er_mos <- system(paste(MRTpath, '/mrtmosaic -i ', MRTpath, '/TmpMosaic.prm' ,' -o ', outfile,' -s ',bands, sep=""))	# Launche MRT to create the mosaic
            if (er_mos != 0)  {stop()}
            # If reprojection requires (i.e., out proj not Sinusoidal), convert to output projection using gdalwarp
            if (reproj == T) {
              print (paste('Reprojecting ', bandnames[band],'files for date: ',date_name ))
              svalue(mess_lab) =  (paste('--- Reprojecting ', bandnames[band],'files for date: ',date_name,' ---'))
              outrep_file = file.path(out_folder, bandnames[band],paste(sub("[.][^.]*$", "", basename(outfile), perl=TRUE),sep = ''))	# Create name for the TIFF reprojected  mosaic

              if (file.exists(outrep_file) == F) {
                # ---------------------------------- ----------------------------------------------#
                # Create the string for gdal and launch the reprojection in a command shell
                # ---------------------------------- ----------------------------------------------#

                # If bounding box was passed, the output reproject file will satisfy the bbox
                if (length(which(is.finite(bbox))) == 4) {
                  #browser()
                  er_rep = 	system(paste('gdalwarp -s_srs "',MOD_prj_str, 	# Launch GDAL to crete the reprojected TIFF
                                         '" -t_srs "', outproj_str, '" -of ',format,' -r near -co compress=lzw',
                                         '-te',bbox[1],bbox[2],bbox[3],bbox[4], '-tr ',out_res, out_res,
                                         '-wo "INIT_DEST=NO_DATA"','-srcnodata ', nodata_in[band],'-dstnodata ', nodata_out[band], '-overwrite',
                                         outfile,  outrep_file, #'-overwrite',outfile,  outrep_file,
                                         sep = ' '))
                }
                else {						# If bounding box was not passed, keep the original extent when creating the TIFF
                  er_rep = 	system(paste('gdalwarp -s_srs "',MOD_prj_str,
                                         '" -t_srs "', outproj_str,'" -of ',format,' -r near -co compress=lzw',
                                         '-tr ',out_res,out_res,
                                         '-wo "INIT_DEST=NO_DATA"','-srcnodata ',  nodata_in[band],'-dstnodata ',nodata_out[band],
                                         '-overwrite', outfile,  outrep_file, #'-overwrite',outfile,  outrep_file,
                                         sep = ' '))
                }
                if (er_rep != 0)  {stop()}
              }
            } #End if on file existence
            if (bandnames [band] == 'QA') {
              QA_File = outrep_file
              Moddwl_QA_convert (out_folder = out_folder, QA_file = QA_file, product = product)
            }
            # 							}
            unlink(outfile)														# Delete un-reprojected Mosaic HDF file
            xml_file = paste(outrep_file,'.aux.xml',sep = '')		# Delete xml files created by gdalwarp
            unlink(xml_file)
          }  # ENDIF band selected for processing
        }	# END Cycle on selected MODIS Bands

        if (del == T) {for (ModisName in Modislist) unlink(file.path(out_folder,ModisName))}		# Delete original downloaded HDFs

      } # ENDIF on lenght of Modislist

      else {print(paste("No available image for selected Tiles in ",dirs[i], sep=""))}

    }	# End Cycling on directories containing images to be downloaded (i.e., dates)

    addHandlerUnrealize(mess_lab, handler = function(h,...) {return(FALSE)})		# Allow message lab to be closed since processing ended .
    dispose(mess_lab)
  }
  return('DONE')
}

# ----- Accessory Functions -------------#


