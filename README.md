
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nlmixr2rpt <img src="man/figures/nlmixr2rpt_hex.png" align="right" width="138.5" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/john-harrold/nlmixr2rpt/workflows/R-CMD-check/badge.svg)](https://github.com/john-harrold/nlmixr2rpt/actions)
<!---
[![CRAN checks](https://cranchecks.info/badges/summary/nlmixr2rpt)](https://cran.r-project.org/web/checks/check_results_nlmixr2rpt.html)
--> <!-- badges: end -->

Templated Word and PowerPoint reporting for `nlmixr2`

## Overview

The purpose of this package is to automate the reporting of analyses
performed in the `nlmixr2` package in both PowerPoint and Word. This is
accomplished with a yaml file that contains specifications for report
elements (figures and tables) as well as the contents of Word and
PowerPoint documents. Internal templates for both the documents and the
report yaml file are included. These can be customized: The format of
the document templates can be customized for your organization by using
the `onbrand` package. The report contents can be customized by creating
a copy of the included yaml file and modifying it to suite your needs
(see the vignette below).

## Installation

Then you can install `nlmixr2rpt` from CRAN

``` r
install.packages("nlmixr2rpt") 
```

Or install the development version from GitHub:

``` r
#install.packages("devtools") 
devtools::install_github("john-harrold/nlmixr2rpt")
```

## Getting Started

Browse through the [documentation](https://nlmixr2rpt.ubiquity.tools/)
and check out the vignette:

-   [Reporting `nlmixr` Fit
    Results](https://nlmixr2rpt.ubiquity.tools/articles/Reportin_nlmixr_Fit_Results.html)
-   [Accessing Figures and
    Tables](https://nmlixr2rpt.ubiquity.tools/articles/Accessing_Figures_and_Tables.html)
