% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rptnlmixr.R
\name{fetch_option}
\alias{fetch_option}
\title{Fetch Analysis Options}
\usage{
fetch_option(rptdetails, option = NULL, fit = NULL, verbose = TRUE)
}
\arguments{
\item{rptdetails}{Object creating when reading in rptyaml file}

\item{option}{String containing the option to fetch (see below)}

\item{fit}{nlmixr2 fit object to be reported}

\item{verbose}{Boolean variable when set to \code{TRUE} (default) messages will be
displayed on the terminal
following:}
}
\value{
List containing the following information about the output directory
\itemize{
\item \code{"isgood"} - Boolean variable indicating success or failure
\item \code{"msgs"} - Vector of messages
\item \code{"value"} - The value of the option or the default if not
specified
}
}
\description{
Fetches analysis options from the report yaml
applies it to strings.
}
\details{
The option can be one of the following (default: \code{NULL}):
\itemize{
\item \code{"output_dir"} - Directory to place figures that are generated (default: \code{tempdir()})
\item \code{"resolution"} - Resolution of figure files (default: \code{300})
}
}
\examples{
library(onbrand)
obnd = read_template(
 template = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.pptx"),
 mapping  = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.yaml"))

# This will create an example fit object to use in the examples below
fit = fetch_fit_example()

# This reads in the report details as well
rptdetails = yaml_read_fit(
 obnd    = obnd,
 rptyaml = system.file(package="nlmixr2rpt", "examples", "report_fit_test.yaml"),
 fit     = fit)$rptdetails


fetch_option(rptdetails, option="output_dir", fit=fit)
}
