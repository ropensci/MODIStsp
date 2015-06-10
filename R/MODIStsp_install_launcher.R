#'  install_MODIStsp_launcher
#' @description Function which allows to use MODIStsp in batch mode by creating links
#' @details MODIStsp tool can be used also as a standalone tool my launching a bash/batch script, which is stored in the function files.
#'  In order to simply retrieve it, this function will create a desktop entry and a symbolic link to the bash script (in Linux)
#'  or a link in the Start Menu to the batch script (in Windows).
#' @param bin_path in Linux, directory in which the link to the bash script should be placed (default: /usr/bin; a path included in the PATH 
#'  environment variable is suggested); in Windows, directory where to place the menu entry in the Start Menu (default: Start Menu -> Programs -> MODIStsp).
#' @param desktop_shortcut logical value which indicates if the desktop entry or the desktop shortcut should be created (default: TRUE).
#' @param desktop_path if desktop_shortcut=TRUE: in Linux, directory in which the desktop entry should be placed (default: /usr/share/applications);
#'  in Windows, directory where to place the desktop entry (default: Desktop).
#' @param sudo (Linux only) logical value which indicates if administrator rights have to be used to write within bin_path and desktop_path (default: FALSE);
#'  in this case, the root password is requested when launching the function. Note that default values of bin_path and desktop_path requires to set this
#'  option to TRUE (or to lauch the script in a root session of R)!
#' @return NULL
#'
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#'
#' @export
#' @importFrom hash hash
#' @examples
#' # Linux: common installation (script in /usr/bin,
#' # desktop entry in /usr/share/applications)
#' # (requires administrator permissions)
#' \dontrun{
#' install_MODIStsp_launcher(sudo = TRUE)
#'   # the administrator password is asked interactively}
#' 
#' # Linux: installation in a directory which does 
#' # not require administrator permissions
#' \dontrun{
#' install_MODIStsp_launcher(bin_path = "~/bin/MODIStsp"), 
#'   desktop_path = "~/Desktop/MODIStsp.desktop"}
#' 
#' # Windows: common installation
#' # (script in the Start Menu and shortcut on the desktop)
#' \dontrun{
#' install_MODIStsp_launcher()}

install_MODIStsp_launcher <- function( bin_path=NA, desktop_path=NA, desktop_shortcut=TRUE, sudo=FALSE) {

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
			system(paste('sudo -S cp -f', file.path(MODIStsp_dir,'ExtData/Launcher/Bash/MODIStsp.desktop'), file.path(desktop_path,'MODIStsp.desktop')), input=readline("Enter your password: "))
		} else {
			file.copy(from=file.path(MODIStsp_dir,'ExtData/Launcher/Bash/MODIStsp.desktop'),to=file.path(desktop_path,'MODIStsp.desktop'),overwrite=TRUE)
		}
	}

	if (running_os=='Windows') {
		
		# Create entry in the start menu
		if (is.na(bin_path)) {bin_path = file.path(Sys.getenv('USERPROFILE'),'AppData/Roaming/Microsoft/Windows/Start Menu/Programs/MODIStsp')}
		if (!file.exists(file.path(bin_path,'/MODIStsp.lnk'))) {
			dir.create(bin_path,recursive=TRUE,showWarnings=FALSE)
			shell("set create_script=\"%TEMP%\\create_MODIStsp_shortcut.vbs\" >> \"%TEMP%\\create_MODIStsp_shortcut.vbs\"")
			shell("echo Set oWS = WScript.CreateObject(\"WScript.Shell\") >> \"%TEMP%\\create_MODIStsp_shortcut.vbs\"")
			shell(paste0("echo Set oLink = oWS.CreateShortcut(\"",bin_path,"\\MODIStsp.lnk\") >> \"%TEMP%\\create_MODIStsp_shortcut.vbs\""))
			shell(paste0("echo oLink.TargetPath = \"",MODIStsp_dir,"\\ExtData\\Launcher\\Batch\\MODIStsp.bat\" >> \"%TEMP%\\create_MODIStsp_shortcut.vbs\""))
			shell("echo oLink.Save >> \"%TEMP%\\create_MODIStsp_shortcut.vbs\"")
			shell("cscript /nologo \"%TEMP%\\create_MODIStsp_shortcut.vbs\"")
			shell("del \"%TEMP%\\create_MODIStsp_shortcut.vbs\"")
		} else warning('Link in Start Menu already exists!')
		
		# Create desktop shortcut
		if (desktop_shortcut) {
			if (!file.exists(file.path(desktop_path,'/MODIStsp.bat'))) {
				if (is.na(desktop_path)) {desktop_path = file.path(Sys.getenv('USERPROFILE'),'Desktop')}
				shell("set create_script=\"%TEMP%\\create_MODIStsp_shortcut.vbs\" >> \"%TEMP%\\create_MODIStsp_shortcut.vbs\"")
				shell("echo Set oWS = WScript.CreateObject(\"WScript.Shell\") >> \"%TEMP%\\create_MODIStsp_shortcut.vbs\"")
				shell(paste0("echo Set oLink = oWS.CreateShortcut(\"",bin_path,"\\MODIStsp.lnk\") >> \"%TEMP%\\create_MODIStsp_shortcut.vbs\""))
				shell(paste0("echo oLink.TargetPath = \"",MODIStsp_dir,"\\ExtData\\Launcher\\Batch\\MODIStsp.bat\" >> \"%TEMP%\\create_MODIStsp_shortcut.vbs\""))
				shell("echo oLink.Save >> \"%TEMP%\\create_MODIStsp_shortcut.vbs\"")
				shell("cscript /nologo \"%TEMP%\\create_MODIStsp_shortcut.vbs\"")
				shell("del \"%TEMP%\\create_MODIStsp_shortcut.vbs\"")
			} else warning('Desktop shortcut already exists!')
		}
		
	}
	
}
