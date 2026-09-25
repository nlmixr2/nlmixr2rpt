#'@importFrom equatags transform_mathjax
#'@export
#'@title Makes `nlmixr2` Model Equation Table for Reporting
#'@description Generates a flextable with one row per model equation rendered
#'as native Word/PowerPoint math. The equations are taken from the LaTeX
#'produced by the `knit_print()` method for `nlmixr2` fit objects (provided
#'by `nlmixr2extra`), split into individual rows, and converted with
#'`flextable::as_equation()`.
#'@details The math is rendered by `flextable::as_equation()`, which relies
#'on the `equatags` package. Line spacing and padding are adjusted based on the report
#'type in `obnd`: Word documents get extra line spacing so rows containing
#'fractions do not crowd each other, while PowerPoint sizes the math itself
#'and keeps the rows tight.
#'@param fit nlmixr2 fit object to be reported
#'@param obnd onbrand report object used to determine the report type
#'(`"Word"` or `"PowerPoint"`)
#'@return `flextable` object containing the model equations, one equation per
#'row, with no header and no borders
#'@examples
#'\donttest{
#'# We need an onbrand object to use below
#'library(onbrand)
#'obnd = read_template(
#'  template = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.docx"),
#'  mapping  = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.yaml"))
#'
#'# This will create a fit object to use in the examples below
#'fit = fetch_fit_example()
#'
#'ft = gen_eqn_table(fit = fit, obnd = obnd)
#'}
gen_eqn_table <- function(fit, obnd) {
  eq <- as.character(knitr::knit_print(fit))
  eq <- sub("^\\s*\\\\begin\\{align\\*\\}\\s*", "", eq)
  eq <- sub("\\s*\\\\end\\{align\\*\\}\\s*$", "", eq)
  eq <- strsplit(eq, " \\\\\n", fixed = TRUE)[[1]]
  eq <- gsub("(?<!\\\\)&", "", eq, perl = TRUE)
  ft <- flextable::flextable(data.frame(eq = eq))
  ft <- flextable::delete_part(ft, part = "header")
  ft <- flextable::border_remove(ft)
  ft <- flextable::compose(ft, j = "eq",
    value = flextable::as_paragraph(flextable::as_equation(eq)))
  # Word needs extra line spacing so rows with fractions don't crowd;
  # PowerPoint sizes the math itself, so keep its rows tight
  word <- identical(obnd[["rpttype"]], "Word")
  ft <- flextable::line_spacing(ft, space = if (word) 2 else 1)
  if (!word) ft <- flextable::padding(ft, padding.top = 1,
                                      padding.bottom = 1)
  flextable::width(ft, width = 6)
}
