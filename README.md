
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nlmixr2rpt <img src="man/figures/nlmixr2rpt_hex.png" align="right" width="138.5" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/nlmixr2/nlmixr2rpt/workflows/R-CMD-check/badge.svg)](https://github.com/nlmixr2/nlmixr2rpt/actions)
[![CRAN
checks](https://badges.cranchecks.info/worst/nlmixr2rpt.svg)](https://cran.r-project.org/web/checks/check_results_nlmixr2rpt.html)
[![version](https://www.r-pkg.org/badges/version/nlmixr2rpt)](https://CRAN.R-project.org/package=nlmixr2rpt)
![cranlogs](https://cranlogs.r-pkg.org/badges/nlmixr2rpt)
![Active](https://www.repostatus.org/badges/latest/active.svg)
[![Lifecycle:
Experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
<!---
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

To install off of [CRAN](https://cran.r-project.org/package=nlmixr2rpt):

``` r
install.packages("nlmixr2rpt", dependencies = TRUE)
```

To install the development version from
[GitHub](https://github.com/nlmixr2/nlmixr2rpt/)

``` r
#install.packages("devtools") 
devtools::install_github("nlmixr2/nlmixr2rpt", dependencies=TRUE)
```

## Getting Started

### PowerPoint

Assuming you have the results of an nlmixr analysis in the object `fit`
you can dump those results into a PowerPoint document by doing the
following:

``` r
obnd_pptx = read_template(
  template = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.pptx"),
  mapping  = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.yaml"))

obnd_pptx = report_fit(
  fit     = fit, 
  obnd    = obnd_pptx)

save_report(obnd_pptx, "report.pptx")
```

This creates an onbrand reporting object `obnd_pptx`. You can add other
reporting elements using `onbrand` or `officer`. Next the command
`report_fit()` will append the results in the `fit` object to an onbrand
report. After calling `report_fit()` you can append other reporting
elements. Finally you just need to save the document using
`save_report()`.

### Word

The process for a Word document is the same. You simply need to use a
Word onbrand template instead of a PowerPoint.

``` r
obnd_docx = read_template(
  template = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.docx"),
  mapping  = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.yaml"))

obnd_docx = report_fit(
  fit     = fit, 
  obnd    = obnd_docx)

save_report(obnd_docx, "report.docx")
```

## Further reading

If you want to learn more about customizing the outputs or using your
own organizational templates in reporting, be sure to browse through the
[documentation](https://nlmixr2.github.io/nlmixr2rpt/) and check out the
vignettes:

- [Reporting nlmixr Fit
  Results](https://nlmixr2.github.io/nlmixr2rpt/articles/Reporting_nlmixr_Fit_Results.html)
- [Accessing Figures and
  Tables](https://nlmixr2.github.io/nlmixr2rpt/articles/Accessing_Figures_and_Tables.html)
