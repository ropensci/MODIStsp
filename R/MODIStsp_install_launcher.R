#'  install_MODIStsp_launcher
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
#' @author Luigi Ranghetti, phD (2015)
#' email: ranghetti.l@@irea.cnr.it
#'
#' license GPL 3.0
#' @export
#' @importFrom hash hash

install_MODIStsp_launcher <- function( desktop_path=NA, bin_path=NA, sudo=FALSE, desktop_shortcut=FALSE) {

	MODIStsp_dir = system.file(package = "MODIStsp")
	running_os <- Sys.info()[['sysname']]

	if (running_os=='Linux') {
		if (is.na(bin_path)) {bin_path = '/usr/bin/MODIStsp'}
		if (is.na(desktop_path)) {desktop_path = '/usr/share/applications/MODIStsp.desktop'}
		# Create symbolic link to a directory in the path
		if (sudo) {
			system(paste('sudo -S ln -fs', file.path(MODIStsp_dir,'ExtData/Launcher/Bash/MODIStsp.sh'), bin_path), input=readline("Enter your password: "))
		} else {
			file.symlink(from=file.path(MODIStsp_dir,'ExtData/Launcher/Bash/MODIStsp.sh'),to=bin_path,overwrite=TRUE)
		}
		# Create desktop entry
		desktopEntry = paste0("[Desktop Entry]\nName=MODIStsp\nComment=A tool for automatic download and preprocessing of time series of MODIS Land Products data\nExec=",MODIStsp_dir,"/ExtData/Launcher/Bash/MODIStsp.sh\nTerminal=true\nType=Application\nCategories=Science;Geography;\nStartupNotify=true")
		fileConn <- file(file.path(MODIStsp_dir,'ExtData/Launcher/Bash/MODIStsp.desktop'))
		writeLines(desktopEntry,fileConn)
		close(fileConn)
		if (sudo) {
			system(paste('sudo -S cp -f', file.path(MODIStsp_dir,'ExtData/Launcher/Bash/MODIStsp.desktop'), desktop_path), input=readline("Enter your password: "))
		} else {
			file.copy(from=file.path(MODIStsp_dir,'ExtData/Launcher/Bash/MODIStsp.desktop'),to=desktop_path,overwrite=TRUE)
		}
	}

	if (running_os=='Windows') {
		if (is.na(desktop_path)) {desktop_path = file.path(Sys.getenv('USERPROFILE'),'AppData/Roaming/Microsoft/Windows/Start Menu/Programs/MODIStsp/')}
		# Create entry in the start menu
		if (!file.exists(file.path(desktop_path,'/MODIStsp.bat'))) {
			Sys.junction(from=file.path(MODIStsp_dir,'ExtData/Launcher/Batch/'),to=file.path(desktop_path,'/'))
		} else warning('Link in Start Menu already exists!')
		# Create desktop shortcut
		if (desktop_shortcut) {
			if (!file.exists(file.path(Sys.getenv('USERPROFILE'),'Desktop/MODIStsp/MODIStsp.bat'))) {
				Sys.junction(from=file.path(MODIStsp_dir,'ExtData/Launcher/Batch/'),to=file.path(Sys.getenv('USERPROFILE'),'Desktop/MODIStsp/'))
			} else warning('Desktop shortcut already exists!')
		}
	}

}
