---
output: github_document
---
<!-- README.md is generated from README.Rmd. Please edit that file -->



[![](https://www.r-pkg.org/badges/version-ago/MODIStsp)](http://cran.rstudio.com/web/packages/MODIStsp/index.html)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.290683.svg)](https://doi.org/10.5281/zenodo.290683)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/MODIStsp?color=red)](http://cran.rstudio.com/web/packages/MODIStsp/index.html)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)
[![Travis-CI Build Status](https://travis-ci.org/lbusett/MODIStsp.svg?branch=master)](https://travis-ci.org/lbusett/MODIStsp)
[![Coverage Status](https://img.shields.io/codecov/c/github/lbusett/MODIStsp/master.svg)](https://codecov.io/github/lbusett/MODIStsp?branch=master)


# <i class="fa fa-globe" aria-hidden="true"></i> MODIStsp <img src="man/figures/logo.png" width="100" height="100" align="right"/>

**MODIStsp** is a "R" package devoted to **automatizing the creation of time series of raster images derived from MODIS Land Products data**. 

**MODIStsp** allows to perform several preprocessing steps _(e.g., download, mosaicing, 
reprojection, resize, data extraction)_ on MODIS data available within a given time period.
Users have the ability to select which specific layers of the original MODIS HDF files they 
want to process. They also can select which additional **Quality Indicators** should be 
extracted from the aggregated MODIS Quality Assurance layers and, in the case of Surface 
Reflectance products, which **Spectral Indexes** should be computed from the original reflectance 
bands. 

All processing parameters can be easily selected with a **powerful and user-friendly GUI**, 
although non-interactive execution exploiting a previously created Options File is possible. 
Stand-alone execution outside an "R" environment is also possible, allowing to use scheduled 
execution of MODIStsp to automatically update time series related to a MODIS product and extent 
whenever a new image is available. 

For each output layer, outputs are saved as **single-band raster** files corresponding to 
each available acquisition date. **Virtual files** allowing access to the entire time series 
as a single file can be also created. 


<a href="http://www.irea.cnr.it/en/"> <img src="man/figures/logo_irea.png" height="40" align="left" /></a>


___`MODIStsp` is developed and maintained by Lorenzo Busetto and Luigi Ranghetti,
[Institute of Remote Sensing of Environment](http://www.irea.cnr.it/en/) - National Research Council - Italy (CNR-IREA)___


____________________________________________________________________________________

## <i class="fa fa-cog" aria-hidden="true"></i> Getting Started

- __To install `MODIStsp`__, please follow instructions reported [here](articles/installation.html),
both for [<i class="fa fa-windows" aria-hidden="true"></i>](articles/installation.html#installing-on-windows) , [<i class="fa fa-linux" aria-hidden="true"></i>](articles/installation.html#installing-on-linux-systems) and [<i class="fa fa-apple" aria-hidden="true"></i>](articles/installation.html#installing-on-mac)

- `MODIStsp` can be used either in [interactive mode](articles/interactive_execution.html) 
exploiting its user-friendly GUI, or in [non-interactive mode](articles/noninteractive_execution.html) 
from within `R` scripts

- The list of __currently supported MODIS products and versions__ can be found [here](articles/Products_list.html) 

- [Scheduled Processing](articles/noninteractive_execution.html#scheduled-processing) allows 
automatic updating of MODIS time series through scheduled jobs, both on [<i class="fa fa-windows" aria-hidden="true"></i>](articles/standalone_execution.html#on-windows) and [<i class="fa fa-linux" aria-hidden="true"></i>](articles/standalone_execution.html#on-linux)

- Solutions to common **installation, data download and processing problems** can be found 
in our [<i class="fa fa-question-circle-o aria-hidden="true"></i> faq](articles/faq.html)

- Please **report any issues** you may encounter in our [issues page on github <i class="fa fa-github-square" aria-hidden="true"></i>](https://github.com/lbusett/MODIStsp/issues) .

____________________________________________________________________________________

## <i class="fa fa-newspaper-o" aria-hidden="true"></i> What's New 

- 10/07/2018 - MODIStsp v. 1.3.6 is out. Check out the [Release Notes](https://github.com/lbusett/MODIStsp/releases/tag/v1.3.5) for further details !

- 20/06/2018 - MODIStsp v. 1.3.5 is out. Check out the [Release Notes](https://github.com/lbusett/MODIStsp/releases/tag/v1.3.5) for further details !

- 11/04/2018 - Due to new NASA Policies the MODIS FTP servers were shut down 
starting, April 2, 2018. **FTP download is therefore no longer working** and will
be removed in the next MODIStsp version!

- 11/04/2018 - [**Decommissioning of MODIS Version 5 Land Data Products**]( https://lpdaac.usgs.gov/about/news_archive/decommissioning_modis_version_5_land_data_products_april_9_2018_second_notice). As per NASA notice above, MODIS v005 products are going to be 
decommisioned, and will soon be no longer available for download. Support for those
products will be removed in the next MODIStsp version!.

- 11/08/2017 - MODIStp 1.3.3 was released today. It provides improvements in processing speed, as well as the usual bug fixes.  See our [<i class="fa fa-newspaper-o aria-hidden="true"></i> news](news/index.html) page for a detailed changelog.
**We thank to all the users that signalled problems for their feedback for imporoving the package!**

____________________________________________________________________________________

## <i class="fa fa-pencil" aria-hidden="true"></i>Citation


To cite MODIStsp in publications, please use:

L. Busetto, L. Ranghetti (2016) MODIStsp: An R package for automatic preprocessing of MODIS
  Land Products time series, Computers & Geosciences, Volume 97, Pages
  40-48, ISSN 0098-3004, http://dx.doi.org/10.1016/j.cageo.2016.08.020, URL: https://github.com/lbusett/MODIStsp. 
  
A BibTeX entry for LaTeX users is:

```
  @Article{,
    author  = {Lorenzo Busetto and Luigi Ranghetti},
    title   = {MODIStsp: an R package for preprocessing of MODIS Land Products time series},
    journal = {Computers & Geosciences},
    year    = {2016},
    volume  = {97},
    pages   = {40-48},
    issn    = {0098-3004},
    doi     = {10.1016/j.cageo.2016.08.020},
    url     = {https://github.com/lbusett/MODIStsp},
  }
```
[![ropensci_footer](http://ropensci.org/public_images/github_footer.png)](https://ropensci.org)
