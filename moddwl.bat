PATH "C:\Progra~1\R\R-3.0.3\bin\x64\";%path%
Rscript "%~dp0\Accessoires\RscriptEcho.R" %~dp0 %1 %2
:: This batch should be run with two arguments:
:: 1) a logical to start /not to start the gui;
:: 2) the path of the RData with the settings
:: (see Moddwl_Main.R for further details)

:: example of launcher:
:: C:\Users\ermes\Documents\R\workspace\LB_MOD_DWL\moddwl.bat FALSE C:\Users\ermes\Documents\R\workspace\WARM_synced_updates\Mod_dwl_settings\reg_it_opts.RData
