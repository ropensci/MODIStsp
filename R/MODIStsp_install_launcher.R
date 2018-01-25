#' @title Install a launcher for MODIStsp
#' @description Function which allows to use MODIStsp in batch mode by creating links
#' @details MODIStsp can be used also as a stand-alone tool (i.e., without opening RStudio
#'  or R-GUI) by launching a bash/batch script, which is stored in the installation
#'  folder (/ExtData/Launcher)
#'  To allow to easily find it, this function creates a desktop entry and a symbolic link 
#'  to the bash script (on Linux) or a link in the Start Menu to the batch script and a 
#'  shortcut on the desktop (on Windows).
#'  __Note that__, if the packages MODIStsp is installed in a version-dependent directory
#'  (as the default one is), this function should be re-executed after an R upgrade,
#'  otherwise the links would continue to point to the old package version!
#' @param bin_dir 
#'  - on Linux, directory in which the link to the bash script should be
#'    placed, Default: /usr/bin - use of a path included in the PATH environment variable is
#'    suggested;
#'  - on Windows, directory where to place the menu entry in the Start Menu, 
#'    Default: Start Menu -> Programs -> MODIStsp.
#' @param rscript_dir 'character' in Windows only, the path of the directory in which
#'  Rscript is installed (default is "C:\Progra~1\R\R-<version>\bin\<arch>"). 
#'  Edit this parameter if R is installed in a custom directory.
#' @param desktop_shortcut `logical` indicates if the desktop entry or the
#'  desktop shortcut should be created, Default: TRUE.
#' @param desktop_dir `character` 
#'   - on Linux, directory in which the desktop entry should be placed, Default: /usr/share/applications;
#'   - on Windows, directory where to place the desktop entry, Default: "Desktop"
#'   (Ignored if desktop_shortcut = FALSE).
#' @param sudo (Linux only) `logical`  indicates if administrator rights have to
#'  be used to write within bin_dir and desktop_dir, If FALSE the root password is requested
#'  when launching the function. Note that using default values of bin_dir and desktop_dir
#'  requires to set this option to TRUE (or to launch the script in a root session of R),
#'  Default: FALSE
#' @return The function is called for its side effects.
#' @author Luigi Ranghetti, phD (2015) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @export
#' @rdname install_MODIStsp_launcher
#' @examples
#' # Linux: common installation (script in /usr/bin,
#' # desktop entry in /usr/share/applications)
#' # (requires administrator permissions)
#' \dontrun{
#' install_MODIStsp_launcher(sudo = TRUE)
#'   # the administrator password is asked interactively}
#'
#' # Linux: installation in a directory which does not require administrator 
#' # permissions
#' \dontrun{
#' install_MODIStsp_launcher(bin_dir = "~/bin"), desktop_dir = "~/Desktop"}
#'
#' # Windows: common installation
#' # (script in the Start Menu and shortcut on the desktop)
#' \dontrun{
#' install_MODIStsp_launcher()}

install_MODIStsp_launcher <- function(bin_dir          = NA,
                                      rscript_dir      = NA,
                                      desktop_dir      = NA,
                                      desktop_shortcut = TRUE,
                                      sudo             = FALSE) {
  
  #nocov start (no coverage test since this requires interactive execution)
  
  MODIStsp_dir <- system.file(package = "MODIStsp")
  running_os   <- Sys.info()[["sysname"]]
  
  if (running_os == "Linux") {
    if (is.na(bin_dir)) {
      bin_dir <- "usr/bin/MODIStsp"
    }
    if (is.na(desktop_dir)) {
      desktop_dir <- "usr/share/applications"
    }
    # Create symbolic link to a directory in the path
    if (sudo) {
      system(paste("sudo -S ln -fs",
                   file.path(MODIStsp_dir, "ExtData/Launcher/Bash/MODIStsp.sh"),
                   bin_dir),
             input = readline("Enter your password: "))
    } else {
      file.symlink(from = file.path(MODIStsp_dir,
                                    "ExtData/Launcher/Bash/MODIStsp.sh"),
                   to = bin_dir)
    }
    # Create desktop entry
    desktopEntry <- paste0(
      "[Desktop Entry]\nName=MODIStsp\nComment=Download and preprocessing of MODIS data\nExec=",#nolint
      MODIStsp_dir,
      "/ExtData/Launcher/Bash/MODIStsp.sh\nTerminal=true\nType=Application\nCategories=Science;Geography;\nStartupNotify=true")#nolint
    fileConn <- file(file.path(MODIStsp_dir,
                               "ExtData/Launcher/Bash/MODIStsp.desktop"))
    writeLines(desktopEntry, fileConn)
    close(fileConn)
    if (sudo) {
      system(paste("sudo -S cp -f",
                   file.path(MODIStsp_dir, "ExtData/Launcher/Bash/MODIStsp.desktop"),#nolint
                   file.path(desktop_dir, "MODIStsp.desktop")),
             input = readline("Enter your password: "))
    } else {
      file.copy(
        from = file.path(MODIStsp_dir, "ExtData/Launcher/Bash/MODIStsp.desktop"), #nolint
        to = file.path(desktop_dir, "MODIStsp.desktop"),
        overwrite = TRUE
      )
    }
  }

  if (running_os == "Windows") {
    
    # Edit the R version in the bat launcher
    rscript_path <- if (!is.na(rscript_dir)) {
      rscript_dir
    } else {
      paste0(
        "C:\\Progra~1\\R\\",
        "R-",R.version["major"],".",R.version["minor"],"\\",
        "bin\\",
        if(R.version["arch"]=="x86_64") {"x64"} else {"i386"}
      )
    }
    if (!file.exists(file.path(rscript_path,"Rscript.exe"))) {
      warning(paste0(
        "Rscript.exe was not found, so the launcher will not work properly. ",
        "Please specify it using the argument \"rscript_dir\"."
      ))
    }
    bat_launcher_path <- file(paste0(MODIStsp_dir,"\\ExtData\\Launcher\\Batch\\MODIStsp.bat"))
    bat_launcher <- readLines(bat_launcher_path)
    bat_launcher[ grep("^SET Rscript_dir=", bat_launcher)] <- paste0("SET Rscript_dir=\"", rscript_path, "\"")
    writeLines(bat_launcher, bat_launcher_path)

    # Create entry in the start menu
    if (is.na(bin_dir)) {
      bin_dir <- file.path(
        Sys.getenv("USERPROFILE"),
        "AppData/Roaming/Microsoft/Windows/Start Menu/Programs/MODIStsp"
      )
    }
    if (!file.exists(file.path(bin_dir, "MODIStsp.lnk"))) {
      dir.create(bin_dir, recursive = TRUE, showWarnings = FALSE)
      shell("set create_script=\"%TEMP%\\create_MODIStsp_shortcut.vbs\" >> \"%TEMP%\\create_MODIStsp_shortcut.vbs\"")#nolint
      shell("echo Set oWS = WScript.CreateObject(\"WScript.Shell\") >> \"%TEMP%\\create_MODIStsp_shortcut.vbs\"")#nolint
      shell(paste0(
        "echo Set oLink = oWS.CreateShortcut(\"", bin_dir,
        "\\MODIStsp.lnk\") >> \"%TEMP%\\create_MODIStsp_shortcut.vbs\"")
      )
      shell(paste0(
        "echo oLink.TargetPath = \"",
        MODIStsp_dir, "\\ExtData\\Launcher\\Batch\\MODIStsp.bat\" >> \"%TEMP%\\create_MODIStsp_shortcut.vbs\"")#nolint
      )
      shell("echo oLink.Save >> \"%TEMP%\\create_MODIStsp_shortcut.vbs\"")
      shell("cscript /nologo \"%TEMP%\\create_MODIStsp_shortcut.vbs\"")
      shell("del \"%TEMP%\\create_MODIStsp_shortcut.vbs\"")
    } else warning("Link in Start Menu already exists!")

    # Create desktop shortcut
    if (desktop_shortcut) {
      if (!file.exists(file.path(desktop_dir, "MODIStsp"))) {
        if (is.na(desktop_dir)) {
          desktop_dir <- file.path(Sys.getenv("USERPROFILE"), "Desktop")
        }
        shell("set create_script=\"%TEMP%\\create_MODIStsp_shortcut.vbs\" >> \"%TEMP%\\create_MODIStsp_shortcut.vbs\"")#nolint
        shell("echo Set oWS = WScript.CreateObject(\"WScript.Shell\") >> \"%TEMP%\\create_MODIStsp_shortcut.vbs\"")#nolint
        shell(paste0(
          "echo Set oLink = oWS.CreateShortcut(\"", desktop_dir,
          "\\MODIStsp.lnk\") >> \"%TEMP%\\create_MODIStsp_shortcut.vbs\"")
        )
        shell(paste0(
          "echo oLink.TargetPath = \"", MODIStsp_dir,
          "\\ExtData\\Launcher\\Batch\\MODIStsp.bat\" >> \"%TEMP%\\create_MODIStsp_shortcut.vbs\"")#nolint
        )
        shell("echo oLink.Save >> \"%TEMP%\\create_MODIStsp_shortcut.vbs\"")
        shell("cscript /nologo \"%TEMP%\\create_MODIStsp_shortcut.vbs\"")
        shell("del \"%TEMP%\\create_MODIStsp_shortcut.vbs\"")
      } else warning("Desktop shortcut already exists!")
    }
  }
  #nocov end
} 