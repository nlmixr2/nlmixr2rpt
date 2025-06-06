% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rptnlmixr.R
\name{process_ph}
\alias{process_ph}
\title{Substitutes Placehodlers in Strings}
\usage{
process_ph(str, rptdetails)
}
\arguments{
\item{str}{String to process}

\item{rptdetails}{Object creating when reading in rptyaml file
(default: \code{NULL})}
}
\value{
processed string
}
\description{
Takes placeholder information from the rptyaml file and
applies it to strings.
}
\examples{
library(onbrand)
obnd = read_template(
 template = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.docx"),
 mapping  = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.yaml"))

# We also need an nlmixr fit object
fit = fetch_fit_example()

# This reads in the report details as well
rptdetails = yaml_read_fit(
 obnd    = obnd,
 rptyaml = system.file(package="nlmixr2rpt", "examples", "report_fit_test.yaml"),
 fit     = fit)$rptdetails

str = "This is ===CMPD==="

process_ph(str, rptdetails)
}
