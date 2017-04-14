## Test environments
* ubuntu 16.10 on local install, R 3.3.3
* ubuntu 14.04 on travis-ci, R 3.3.3 (https://travis-ci.org/lbusett/MODIStsp/builds/222174310)
* win-builder (R-devel)
* windows 10 on local install, R 3.3.3 (check passes if GTK+ library is
  properly installed and on Windows PATH, otherwise the check causes an endless
  GTK+ installation loop)
* local OS X install, R 3.3.3

## R CMD check results
There were no ERRORs, WARNINGs 

There was 1 NOTE, related to the fact that this is a first submission

## Downstream dependencies

This package has no downstream dependencies