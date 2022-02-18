
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rptnlmixr <img src="man/figures/rptnlmixr_hex.png" align="right" width="138.5" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/john-harrold/rptnlmixr/workflows/R-CMD-check/badge.svg)](https://github.com/john-harrold/rptnlmixr/actions)
<!---
[![CRAN checks](https://cranchecks.info/badges/summary/rptnlmixr)](https://cran.r-project.org/web/checks/check_results_rptnlmixr.html)
--> <!-- badges: end -->

Templated Word and PowerPoint reporting for `nlmixr`

## Overview

The purpose of this package is to automate the reporting of analyses
performed in the `nlmixr` package in both PowerPoint and Word. This is
accomplished with a yaml file that contains specifications for report
elements (figures and tables) as well as the contents of Word and
PowerPoint documents. Internal templates for both the documents and the
report yaml file are included. These can be customized: The format of
the document templates can be customized for your organization by using
the `onbrand` package. The report contents can be customized by creating
a copy of the included yaml file and modifying it to suite your needs
(see the vignette below).

## Installation

Then you can install `rptnlmixr` from CRAN

``` r
install.packages("rptnlmixr") 
```

Or install the development version from GitHub:

``` r
#install.packages("devtools") 
devtools::install_github("john-harrold/rptnlmixr")
```

## Getting Started

Browse through the [documentation](https://rptnlmixr.ubiquity.tools/)
and check out the vignette:

-   [Reporting `nlmixr` Fit
    Results](https://rptnlmixr.ubiquity.tools/articles/Reportin_nlmixr_Fit_Results.html)
-   [Accessing Figures and
    Tables](https://rptnmlixr.ubiquity.tools/articles/Accessing_Figures_and_Tables.html)
