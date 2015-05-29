#!/bin/bash
MODISTSP_BATCH_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
MODISTSP_DIR=${MODISTSP_BATCH_DIR%/*/*}
# MODIStsp_Path="~/R/x86_64-unknown-linux-gnu-library/3.1/MODIStsp/Extdata/Batch/MODIStsp_launcher.R"
# Options_File="~/R/x86_64-unknown-linux-gnu-library/3.1/MODIStsp/Previous/MODIStsp_Previous.RData"
# GUI="FALSE"
Rscript "${MODISTSP_BATCH_DIR}/MODIStsp_launcher.R" "`echo $MODISTSP_DIR`"