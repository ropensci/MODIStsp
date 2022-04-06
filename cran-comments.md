MODIStsp 2.0.7
================

* Windows 10 on local install, R 4.1.3
* ArchLinux on local install, R 4.1.3
* win-builder (R-devel, R-release, R-oldrelease)

## R CMD check results

There were no ERRORs, WARNINGs nor NOTEs.


MODIStsp 2.0.6
================

* Windows 10 on local install, R 4.1.0
* ArchLinux on local install, R 4.1.0
* win-builder (R-devel, R-release, R-oldrelease)

## R CMD check results

There were no ERRORs, WARNINGs nor NOTEs.


MODIStsp 2.0.5
================

* Windows 10 on local install, R 4.0.3
* Ubuntu 18.04 on local install, R 4.0.3
* ArchLinux on local install, R 4.0.3
* win-builder (R-devel, R-release, R-oldrelease)

This submission should fix errors on Debian builds, due to improper 
trigger of an helper function leading to writing in the user's 
lib folder. 

Also fixes a couple of bugs.

## R CMD check results

There were no ERRORs nor WARNINGs.

NOTE:
```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Luigi Ranghetti <luigi@ranghetti.info>'

New submission

Package was archived on CRAN
```
Unfortunately Lorenzo Busetto, who maintained the package, suddently passed away
(https://docs.ropensci.org/MODIStsp/articles/lorenzo).
After that, package was automatically archived for policy violation, since 
communications sent to him were not read.
I now started maintaining MODIStsp (I already authored parts of code).

```
CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2020-10-31 for policy violation.

  Attempts writing to the user library.
```
In this submission I fixed a possible tentative of writing in the user library
(commit ebdfa2a6ebbadf1ab42bc4679b48d2224ddf333f).

```
Possibly mis-spelled words in DESCRIPTION:
  Busetto (40:5)
  HDF (29:24)
  MODIS (2:35, 25:10, 29:18, 30:60, 38:18)
  Ranghetti (40:17)
  mosaicking (26:43)
  rasters (24:63)
  reflectance (31:50, 32:65)
  reprojecting (26:55)
  resizing (26:72)
```
All these words are correctly spelled.

## CRAN resubmission review

Please find below the answers to the CRAN reviewer.

> Please reduce the length of the title to less than 65 characters.

The title was changed from "A Tool for Automating Download and Preprocessing of 
MODIS Land Products Data" to "Find, Download and Process MODIS Land Products".


> Please only capitalize sentence beginnings and names in the description text.
> e.g.  Quality Indicators --> quality indicators
>          Spectral Indexes --> spectral indexes
>          Surface Reflectance --> surface reflectance

All capitalized letters were changed (unless sentence beginnings).


> Please change:
> products ,
> -->
> products,

Done.


> Please provide a link to the used webservices to the description field
> of your DESCRIPTION file in the form
> <http:...> or <https:...>
> with angle brackets for auto-linking and no space after 'http:' and
> 'https:'.

No webservices were used (the MODIStsp Shiny GUI is launched from local PC).
I added a reference to the paper which describes MODIStsp:
Busetto and Ranghetti (2016) <doi:10.1016/j.cageo.2016.08.020>


> You have examples for unexported functions.
> Please either omit these examples or export these functions.
> Used ::: in documentation:
>       man/split_nodata_values.Rd:
>          MODIStsp:::split_nodata_values(c("255", "250,254:255"))
>       man/split_nodata_values.Rd:
>          MODIStsp:::split_nodata_values(c("255", "250,254:255"),
> take_all = FALSE)
>       man/split_nodata_values.Rd:
>          MODIStsp:::create_nodata_rcl(c("255", "250,254:255"), c("255",
> "255"))

Examples of unexported functions were removed.


> Some code lines in examples are commented out in MODIStsp.Rd.
> 
> Please never do that. Ideally find toy examples that can be regularly
> executed and checked. Lengthy examples (> 5 sec), can be wrapped in
> \donttest.

Examples were changed following these indications.
All lenghy examples were wrapped in \donttest{}.
\dontrun{} is no more used.


> Please ensure that you do not use more than 2 cores in your examples,
> vignettes, etc.

Argument `parallel` was added to functions `MODIStsp()`, MODIStsp_process()`
and `MODIStsp_process_bands()` (the only functions exploiting multicore 
computation): if `parallel = FALSE`, single core is used.
All examples and vignettes were edited setting `parallel = FALSE` everywhere.


> Please always add all authors, contributors and copyright holders in the
> Authors@R field with the appropriate roles.
> e.g.: Babak Naimi

Babak Naimi's contribution was added to the DESCRIPTION.


MODIStsp 2.0.5
================

* Windows 10 on local install, R 4.0
* Ubuntu 18.04 on local install, R 3.6.3
* win-builder (R-devel, R-release)

## R CMD check results

There were no ERRORs, WARNINGs and NOTES

MODIStsp 2.0.4
================

* Windows 10 on local install, R 4.0
* Ubuntu 18.04 on local install, R 3.6.3
* win-builder (R-devel, R-release)

## R CMD check results

There were no ERRORs, WARNINGs

There is one note related to nuw submission (package was archived before I had time to fix it)

This submission should fix errors due to trying accessing temporarily non available
urls when building vignettes, which led to CRAN archival. Sorry for that. 

Also limits number of retries on HTTR GET to 5 (previously 20) as suggested by
DR Riply in his mail

MODIStsp 2.0.3
================

* Windows 10 on local install, R 4.0
* Ubuntu 18.04 on local install, R 3.6.3
* win-builder (R-devel, R-release)

## R CMD check results

There were no ERRORs, WARNINGs and NOTES

This submission should fix errors on Debian builds, due to improper 
trigger of an helper function leading to writing in the user's 
lib folder. 

Also fixes a couple of bugs.


MODIStsp 2.0.1
================
* Windows 10 on local install, R 3.6.3
* Windows 10 on local install, R 4.0
* Ubuntu 18.04 on local install, R 4.0
* win-builder (R-devel, R-release)

## R CMD check results

There were no ERRORs, WARNINGs and NOTES

This is a resubmission, fixing notes related to redirected URLS. Sorry for that.

MODIStsp 2.0.0
================
* Windows 10 on local install, R 3.6.3
* Windows 10 on local install, R 4.0
* Ubuntu 18.04 on local install, R 4.0
* win-builder (R-devel, R-release)

## R CMD check results

There were no ERRORs, WARNINGs

There is a NOTE related to inability to retreive future timestamps.
There is a NOTE related to possible invalid urls on some builds, but the url is valid.

MODIStsp 1.4.0
================

## Test environments

* Windows 10 on local install, R 3.6.3
* Windows 10 on local install, R 4.0
* Ubuntu 16.04 on travis-ci (devel and release)
* win-builder (R-devel, R-release)

## R CMD check results

There were no ERRORs, WARNINGs

There is a NOTE related to suggesting a orphaned package (gWidgets)
I received a mail from Prof. Ripley about that recently. As suggested, I temporarily moved gWidgets to suggests and use it conditionally for the time being, with the intention of removing the dependency in the next MODIstsp release by switching to a Shiny-based GUI. 

## Note on installation on macos 

Build on macos currently fails (also for MODIStsp 1.3.8) with error

"ERROR Package required but not available: ‘gWidgetsRGtk2’"

due to erroring in build of package "RGtk2". 

MODIStsp can be however installed by installing RGtk2 beforehand, following
instructions reported here: 

http://lbusett.github.io/MODIStsp/articles/installation.html#installing-on-mac


MODIStsp 1.3.9
================

## Test environments

* ubuntu 18.04 on local install, R 3.6.0
* ubuntu 14.04 on travis-ci (devel and release)
* win-builder (R-devel, R-release)
* windows 10 on local install, R 3.6.0

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs

## Note on installation on macos 

Build on macos currently fails (also for MODIStsp 1.3.8) with error

"ERROR Package required but not available: ‘gWidgetsRGtk2’"

due to erroring in build of package "RGtk2". 

MODIStsp can be however installed by installing RGtk2 beforehand, following
instructions reported here: 

http://lbusett.github.io/MODIStsp/articles/installation.html#installing-on-mac



## Test environments

* ubuntu 18.04 on local install, R 3.5.2
* ubuntu 14.04 on travis-ci
* win-builder (R-devel, R-release)
* windows 10 on local install, R 3.5.2

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs

## Note on installation on macos 

Build on macos currently fails (also for MODIStsp 1.3.3.1) with error

"ERROR Package required but not available: ‘gWidgetsRGtk2’"

due to erroring in build of package "RGtk2". 

MODIStsp can be however installed by installing RGtk2 beforehand, following
instructions reported here: 

http://lbusett.github.io/MODIStsp/articles/installation.html#installing-on-mac


MODIStsp 1.3.7
================

## Test environments

* ubuntu 18.04 on local install, R 3.5.0
* ubuntu 14.04 on travis-ci (https://travis-ci.org/ropensci/MODIStsp/builds/462820855)
* win-builder (R-devel, R-release)
* windows 10 on local install, R 3.5.1

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs

## Note on installation on macos 

Build on macos currently fails (also for MODIStsp 1.3.3.1) with error

"ERROR Package required but not available: ‘gWidgetsRGtk2’"

due to erroring in build of package "RGtk2". 

MODIStsp can be however installed by installing RGtk2 beforehand, following
instructions reported here: 

http://lbusett.github.io/MODIStsp/articles/installation.html#installing-on-mac


MODIStsp 1.3.6
================

Maintenance release to solve CRAN build errors on debian, due to the test_resetindexes
test (I'm sorry I missed this in the previous resubmission). 
The test is now skipped on CRAN. Additionally, the MODIStsp_resetindexes
function was modified to require explicit permission by the user to write on 
the MODIStsp_previous.json file

## Test environments

* ubuntu 18.04 on local install, R 3.4.4
* ubuntu 14.04 on travis-ci (https://travis-ci.org/lbusett/MODIStsp/jobs/402298232)
  (R-devel, R-release)
* win-builder (R-devel, R-release) (r-release and devel)
* windows 10 on local install, R 3.5

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs

MODIStsp 1.3.5
================

Maintenance release to solve CRAN build errors on debian, due to the test_addindex
test. The test is now skipped on CRAN. Additionally, the MODIStsp_addindex 
function was modified to require explicit permission by the user to write on 
the MODIStsp_previous.json file

## Test environments

* ubuntu 18.04 on local install, R 3.4.4
* ubuntu 14.04 on travis-ci (https://travis-ci.org/lbusett/MODIStsp/builds/394612258)
  (R-devel, R-release)
* win-builder (R-devel, R-release)
* windows 10 on local install, R 3.5

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs


MODIStsp 1.3.4
================

## Resubmission 
 
This is a resubmission. In this version I have: 
 
* Re-added package "testthat" in Suggests and removed an unnecessary call to  
library(testthat) to avoid the following WARNING on Debian: 
 
  "* checking for unstated dependencies in ‘tests’ ... WARNING 
  '::' or ':::' import not declared from: ‘testthat’ 
  'library' or 'require' call not declared from: ‘testthat’" 
  
## Test environments

* ubuntu 18.04 on local install, R 3.4.4
* ubuntu 14.04 on travis-ci (https://travis-ci.org/lbusett/MODIStsp/jobs/383285994#L6)
* win-builder (R-devel, R-release)
* windows 10 on local install, R 3.5

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs

## Note on installation on macos 

Build on macos currently fails (also for MODIStsp 1.3.3.1) with error

"ERROR Package required but not available: ‘gWidgetsRGtk2’"

due to erroring in build of package "RGtk2". 

Note that MODIStsp can be however installed by installing RGtk2 beforehand, following
instructions reported here: 

http://lbusett.github.io/MODIStsp/articles/installation.html#installing-on-mac


MODIStsp 1.3.3.1
================

re-submission of v 1.3.3 patching an issue related to missing import of 
gWidgetsRGtk2, leading to NOTES on Solaris, Fedora and OSX builds (see this 
thread on r-package-devel for details: 
https://stat.ethz.ch/pipermail/r-package-devel/2017q3/001790.html)

There were no ERRORs or WARNING. There was 1 NOTE:

   * checking CRAN incoming feasibility ... NOTE
   Maintainer: 'Lorenzo Busetto <lbusett@gmail.com>'

   Days since last update: 4

due to close resubmission. Sorry for this.

For comments on build environments etc. see the notes below concerning v 1.3.3. 

MODIStsp 1.3.3
==============

## Test environments
* ubuntu 17.10 on local install, R 3.4.1
* ubuntu 16.04 on travis-ci (https://travis-ci.org/lbusett/MODIStsp/builds/263768847)
* win-builder (R-devel)
* windows 10 on local install, R 3.4.1 (As for the previous reelease, 
  R CMD check passes if GTK+ library is properly installed and on Windows PATH,
  otherwise the check causes an endless GTK+ installation loop. This seems a 
  common behaviour for packages relying on RGtk2/gWidgetsRGtk2)
* MacOs on rhub:
  
  Build on Maverick works (https://builder.r-hub.io/status/MODIStsp_1.3.3.tar.gz-3ec4e4680884dc9ca4ebca061af09da3)
  
  Build on El Capitain or Sierra is not possible at the moment, due to the lack
  of updated binaries for RGtk2 (https://builder.r-hub.io/status/MODIStsp_1.3.3.tar.gz-c0e71d5c8f0a42ef85157b346255b4bb)
  A workaround solution using oldrel (Maverick) binaries is provided in MODIStsp README and website


## R CMD check results

There were no ERRORs, WARNINGs or NOTEs


## Downstream dependencies

This package has no downstream dependencies.

--------------------------------------------------------------------------------
MODIStsp 1.3.2
==============

## Test environments
* ubuntu 16.10 on local install, R 3.3.3
* ubuntu 14.04 on travis-ci, R 3.3.3 (https://travis-ci.org/lbusett/MODIStsp/builds/222347443)
* win-builder (R-devel)
* windows 10 on local install, R 3.3.3 (R CMD check passes if GTK+ library is
  properly installed and on Windows PATH, otherwise the check causes an endless
  GTK+ installation loop. This seems a common behaviour for packages relying on 
  gWidgetsRGtk2)
* local OS X install, R 3.3.3

## R CMD check results

There were no ERRORs, WARNINGs 

There was 1 NOTE, related to the fact that this is a first submission.

There was a warning in win_builder about the following (possibly) invalid URL:

https://notehub.org/fctdn

I checked it, and it's working.

## Downstream dependencies

This package has no downstream dependencies.
