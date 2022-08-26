
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![](https://www.r-pkg.org/badges/version-ago/MODIStsp)](https://cran.rstudio.com/web/packages/MODIStsp/index.html)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.290683.svg)](https://doi.org/10.5281/zenodo.290683)
[![Downloads](https://cranlogs.r-pkg.org/badges/MODIStsp?color=orange)](https://cran.rstudio.com/web/packages/MODIStsp/index.html)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Coverage
Status](https://img.shields.io/codecov/c/github/ropensci/MODIStsp/master.svg)](https://codecov.io/github/ropensci/MODIStsp?branch=master)

# <i class="fa fa-globe" aria-hidden="true"></i> MODIStsp <img src="man/figures/logo.png" width="100" height="100" align="right"/>

<!-- # MODIStsp <img src='man/figures/logo.png' align="right" height="139" /> -->

**`{MODIStsp}`** is a `R` package devoted to automatizing the creation
of time series of raster images derived from MODIS Land Products data.

**`{MODIStsp}`** allows performing several preprocessing steps (e.g.,
download, mosaicing, reprojection, resize, data extraction) on MODIS
data available within a given time period. Users have the ability to
select which specific layers of the original MODIS HDF files they want
to process. They also can select which additional **Quality Indicators**
should be extracted from the aggregated MODIS Quality Assurance layers
and, in the case of Surface Reflectance products, which **Spectral
Indexes** should be computed from the original reflectance bands.

All processing parameters can be easily selected with a **powerful and
user-friendly GUI**, although non-interactive execution exploiting a
previously created Options File is possible. Stand-alone execution
outside an `R` environment is also possible, allowing using scheduled
execution of MODIStsp to automatically update time series related to a
MODIS product and extent whenever a new image is available.

For each output layer, outputs are saved as **single-band raster** files
corresponding to each available acquisition date. **Virtual files**,
allowing accessing to the entire time series as a single file, can be
also created.

<a href="http://www.irea.cnr.it/en/">
<img src="man/figures/irea_logo.png" height="40" align="left" /></a>

<span style="font-style:italic;font-weight:bold;">`{MODIStsp}` was
developed by Lorenzo Busetto and Luigi Ranghetti, [Institute of Remote
Sensing of Environment](http://www.irea.cnr.it/en/) - National Research
Council - Italy (CNR-IREA). [It is dedicated to the memory of
Lorenzo](https://docs.ropensci.org/MODIStsp/articles/lorenzo.html).</span>

------------------------------------------------------------------------

## <i class="fa fa-newspaper-o" aria-hidden="true"></i> What’s New

-   29/10/2021 - `{MODIStsp}` (GitHub version 2.0.6.9000) supports
    products with version 061. Version 006 will remain the default
    product version until its decommission will be announced. Version
    061 can be specified through the argument `prod_version` of function
    `MODIStsp()` or by selecting it in the GUI.

-   10/12/2020 - `{MODIStsp}` was resubmitted to CRAN after the
    maintainer’s death. Now `{MODIStsp}` is dedicated to Lorenzo Busetto
    (<https://docs.ropensci.org/MODIStsp/articles/lorenzo>).

-   01/09/2020 - `{MODIStsp}` 2.0.0 is out. Replaces the old gWidgets
    GUI with a new one based on Shiny, enhances support for CLI usage
    and enhances support/provides bug fixing for datasets with multiple
    NoData values when applying scale/offset.

-   09/05/2020 - `{MODIStsp}` 1.4.0 is out. Switches to use of
    GDAL3/PROJ6 WKTs for projection representation and usage of `{sf}`
    for all internal work on vector data. Adds support for products
    MCD19A1 and MCD19A2 products.

-   07/06/2019 - `{MODIStsp}` 1.3.9 is out. Fixes a bug causing crashes
    on MOD14A1 product, adds support for product MCD12Q2 and removes
    support for no longer available version 5 of some products.

-   05/03/2019 - `{MODIStsp}` 1.3.8 is out. Fixes an issue causing
    incorrect application of scale/offset values on GDAL versions \> 2.3
    (<https://github.com/ropensci/MODIStsp/issues/163>) and adds support
    for products `MOD21A1D.006 MOD21A1N.006 MOD21A2.006`.

-   29/11/2018 - We recently discovered a nasty bug in the computation
    of some custom spectral indices (those including additions /
    subtractions on reflectance values, such as in
    ![\\frac{(b1\_{NIR}+0.1)}{b2\_{Red}}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cfrac%7B%28b1_%7BNIR%7D%2B0.1%29%7D%7Bb2_%7BRed%7D%7D "\frac{(b1_{NIR}+0.1)}{b2_{Red}}")
    (with
    ![\\rho](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Crho "\rho")
    being a reflectance). See
    [here](https://docs.ropensci.org/MODIStsp/articles/discovered_bug.html)
    for further details. The bug is now fixed on the GitHub version. A
    patched release will be made available on CRAN as soon as possible.

-   07/08/2018 - We are glad to report that `{MODIStsp}` is now included
    in the [rOpenSci](https://ropensci.org/about/) packages ecosystem.
    We thank reviewers Leah Wasser and Jeffrey Hanson for their valuable
    reviews, which helped us  
    further improving the package.

-   10/07/2018 - `{MODIStsp}` v. 1.3.6 is out (check out the [Release
    Notes](https://github.com/ropensci/MODIStsp/releases/tag/1.3.6) for
    further details).

-   20/06/2018 - `{MODIStsp}` v. 1.3.5 is out (check out the [Release
    Notes](https://github.com/ropensci/MODIStsp/releases/tag/v1.3.5) for
    further details).

-   11/04/2018 - Due to new NASA Policies the MODIS FTP servers were
    shut down starting, April 2, 2018. **FTP download is therefore no
    longer working** and will be removed in the next MODIStsp version.

-   11/04/2018 - [**Decommissioning of MODIS Version 5 Land Data
    Products**](https://lpdaac.usgs.gov/news/decommissioning-modis-version-51-land-cover-type-data-products-january-7-2019/).
    As per NASA notice above, MODIS v005 products are going to be
    decommissioned, and will soon be no longer available for download.
    Support for those products will be removed in the next MODIStsp
    version.

-   11/08/2017 - `{MODIStsp}` 1.3.3 was released today. It provides
    improvements in processing speed, as well as the usual bug fixes.
    See our [\<i class=“fa fa-newspaper-o aria-hidden=”true”\></i>
    news](news/index.html) page for a detailed changelog.

------------------------------------------------------------------------

## <i class="fa fa-cog" aria-hidden="true"></i> Getting Started

-   **To install `{MODIStsp}`**, please follow instructions reported
    [here](articles/installation.html), both for
    [<i class="fa fa-windows" aria-hidden="true"></i>](articles/installation.html#installing-on-windows)
    ,
    [<i class="fa fa-linux" aria-hidden="true"></i>](articles/installation.html#installing-on-linux-systems)
    and
    [<i class="fa fa-apple" aria-hidden="true"></i>](articles/installation.html#installing-on-mac).

-   **`{MODIStsp}`** can be used either in [interactive
    mode](articles/interactive_execution.html) exploiting its
    user-friendly GUI, or in [non-interactive
    mode](articles/noninteractive_execution.html) from within `R`
    scripts.

-   The list of **currently supported MODIS products and versions** can
    be found [here](articles/products_list.html).

-   [Scheduled
    Processing](articles/noninteractive_execution.html#scheduled-processing)
    allows automatic updating of MODIS time series through scheduled
    jobs, both on
    [<i class="fa fa-windows" aria-hidden="true"></i>](articles/standalone_execution.html#on-windows)
    and
    [<i class="fa fa-linux" aria-hidden="true"></i>](articles/standalone_execution.html#on-linux).

-   Solutions to common **installation, data download and processing
    problems** can be found in our
    [<i class="fa fa-question-circle-o" aria-hidden="true"></i>
    faq](https://docs.ropensci.org/MODIStsp/articles/faq.html).

-   Please **report any issues** you may encounter in our [issues page
    on github
    <i class="fa fa-github-square" aria-hidden="true"></i>](https://github.com/ropensci/MODIStsp/issues).

------------------------------------------------------------------------

## <i class="fa fa-pencil" aria-hidden="true"></i>Citation

To cite MODIStsp in publications, please use:

L. Busetto, L. Ranghetti (2016) MODIStsp: An R package for automatic
preprocessing of MODIS Land Products time series, Computers &
Geosciences, Volume 97, Pages 40-48, ISSN 0098-3004,
<https://doi.org/10.1016/j.cageo.2016.08.020>, URL:
<https://github.com/ropensci/MODIStsp>.

A BibTeX entry for LaTeX users is:

      @Article{,
        author  = {Lorenzo Busetto and Luigi Ranghetti},
        title   = {MODIStsp: an R package for preprocessing of MODIS Land Products time series},
        journal = {Computers & Geosciences},
        year    = {2016},
        volume  = {97},
        pages   = {40-48},
        issn    = {0098-3004},
        doi     = {10.1016/j.cageo.2016.08.020},
        url     = {https://github.com/ropensci/MODIStsp},
      }
