% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_figures.R
\name{build_figures}
\alias{build_figures}
\title{Generates Figures for an \code{nlmixr2} Report}
\usage{
build_figures(
  obnd = NULL,
  fit = NULL,
  rptdetails = NULL,
  cat_covars = NULL,
  cont_covars = NULL,
  verbose = TRUE
)
}
\arguments{
\item{obnd}{\code{onbrand} report object to have report elements appended to}

\item{fit}{\code{nlmixr2} fit object to be reported}

\item{rptdetails}{Object created  when reading in rptyaml file}

\item{cat_covars}{character vector of categorical covariates to overwrite defaults in yaml file}

\item{cont_covars}{character vector of continuous covariates to overwrite defaults in yaml file}

\item{verbose}{Boolean variable when set to TRUE (default) messages will be
displayed on the terminal}
}
\value{
List containing the figures with the following structure:
\itemize{
\item \code{"rptfigs"} - List of figures with names corresponding to the
figure ids in the yaml file. Each figure ID contains the following elements:
\itemize{
\item \code{"figure"}       - list of figure file names for the current fid
\item \code{"orientation"}  - Figure orientation ("portrait" or "landscape")
\item \code{"isgood"}       - Boolean variable indicating success or failure
\item \code{"skip"}         - Boolean variable indicating whether the figure should be skipped during reporting
\item \code{"fmsgs"}        - Vector of messages
\item \code{"cmd"}          - Original plot generation command
\item \code{"cmd_proc"}     - Plot generation command after processing for placeholders
\item \code{"height"}       - Figure height
\item \code{"width"}        - Figure width
\item \code{"caption"}      - Caption for Word
\item \code{"caption_proc"} - Caption for Word after processing for placeholders
\item \code{"title"}        - Slide title for PowerPoint
\item \code{"title_proc"}   - Slide title for PowerPoint after processing for placeholders
}
\item \code{"isgood"} - Boolean variable indicating success or failure
\item \code{"msgs"} - Vector of messages
}
}
\description{
Creates figures specified in a rptyaml file
}
\examples{
# We need an onbrand object to use below
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

# Now we will build the figures
bfres = build_figures(obnd       = obnd,
                     fit        = fit,
                     rptdetails = rptdetails)
}
