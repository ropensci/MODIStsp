PATH "C:\Program Files\R\R-3.1.3\bin\x64";%path%
Rscript "%~dp0\Accessoires\RscriptEcho.R" "%~dp0\R\Moddwl_Main.R" "%~dp0\Log" %~dp0 %1 %2
:: This batch should be run with two arguments:
:: 1) a logical to start /not to start the gui;
:: 2) the path of the RData with the settings
:: (see Moddwl_Main.R for further details)