#'  MODIStsp_install_launcher
#' @description Function which allows to use MODIStsp in batch mode by creating links
#' @details MODIStsp tool can be used also as a standalone tool my launching a bash/batch script, which is stored in the function files.
#'  In order to simply retrieve it, this function will create a desktop entry and a symbolic link to the bash script (in Linux) 
#'  or a link in the Start Menu to the batch script (in Windows).
#' @param desktop_path path to the desktop entry (Linux; default: /usr/share/applications/MODIStsp.desktop) or to the link in the Start Menu 
#'  (Windows; default: Start Menu -> Programs -> MODIStsp). Note that the patch must contain also the file name.
#' @param bin_path (Linux only) path of the link to the bash script (default: /usr/bin/MODIStsp); a path included in the PATH environment variable
#'  is suggested.
#' @param sudo (Linux only) logical value which indicates if the creation of bin_path and desktop_path requires root permissions; in this case,
#'  the root password is requested when launching the function.
#' @param desktop_shortcut (Windows only) logical value which indicates if also a desktop shortcut should be created.
#' @return NULL
#'
#' @author Lorenzo Busetto, phD (2014-2015)
#' email: busetto.l@@irea.cnr.it
#' Luigi Ranghetti, phD (2015)
#' license GPL 3.0
#' @export
#' @importFrom hash hash

MODIStsp_install_launcher <- function( desktop_path=NA, bin_path=NA, sudo=FALSE, desktop_shortcut=FALSE) {
	
	MODIStsp_dir = system.file(package = "MODIStsp")
	running_os <- Sys.info()[['sysname']]

	if (running_os=='Linux') {
		if (is.na(bin_path)) {bin_path = '/usr/bin/MODIStsp'}
		if (is.na(desktop_path)) {desktop_path = '/usr/share/applications/MODIStsp.desktop'}
		# Create symbolic link to a directory in the path 
		if (sudo) {
			system(paste('sudo -S ln -fs', file.path(MODIStsp_dir,'ExtData/Batch/MODIStsp.sh'), bin_path), input=readline("Enter your password: "))
		} else {
			file.symlink(from=file.path(MODIStsp_dir,'ExtData/Batch/MODIStsp.sh'),to=bin_path,overwrite=TRUE)
		}
		# Create desktop entry
		desktopEntry = paste0("[Desktop Entry]\nName=MODIStsp\nComment=A tool for automatic download and preprocessing of time series of MODIS Land Products data\nExec=",MODIStsp_dir,"/ExtData/Batch/MODIStsp.sh\nTerminal=true\nType=Application\nCategories=Science;Geography;\nStartupNotify=true")
		fileConn <- file(file.path(MODIStsp_dir,'ExtData/Batch/MODIStsp.desktop'))
		writeLines(desktopEntry,fileConn)
		close(fileConn)
		if (sudo) {
			system(paste('sudo -S cp', file.path(MODIStsp_dir,'ExtData/Batch/MODIStsp.desktop'), desktop_path), input=readline("Enter your password: "))
		} else {
			file.copy(from=file.path(MODIStsp_dir,'ExtData/Batch/MODIStsp.desktop'),to=desktop_path,overwrite=TRUE)
		}
	}
	
	if (running_os=='Windows') {
		if (is.na(desktop_path)) {desktop_path = file.path(Sys.getenv('USERPROFILE'),'AppData/Roaming/Microsoft/Windows/Start Menu/Programs/MODIStsp')}
		# Create entry in the start menu
		file.symlink(from=file.path(MODIStsp_dir,'ExtData/Batch/MODIStsp.sh'),to=desktop_path)
		# Create desktop shortcut
		if (desktop_shorcut) {file.symlink(from=file.path(MODIStsp_dir,'ExtData/Batch/MODIStsp.sh'),to=file.path(Sys.getenv('USERPROFILE'),'Desktop/MODIStsp'))}
	}
	
}
