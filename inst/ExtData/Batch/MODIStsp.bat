@echo off
SET Rscript_Path="C:\Progra~1\R\R-3.0.3\bin\x64\RScript"
SET MODIStsp_Path="D:\Documents\Source_Code\R\LB_MOD_DWL\Extdata\MODIStsp_launcher.R"
SET Options_File="D:\Documents\Source_Code\R\LB_MOD_DWL\Previous\Moddwl_Previous.RData"
SET GUI="FALSE"
SET Out= "> NUL"
%Rscript_Path% %MODIStsp_Path% %GUI% %Options_File%
