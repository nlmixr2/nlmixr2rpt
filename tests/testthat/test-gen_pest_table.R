suppressPackageStartupMessages(library("onbrand"))
suppressPackageStartupMessages(library("nlmixr2rpt"))

obnd_pest = read_template(
  template = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.docx"),
  mapping  = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.yaml"))

# report names as a report yaml gives them (the parameters section)
rptdetails_pest = list(parameters = list(
  tcl = list(md = "CL~int~ (L/hr)"),
  tv  = list(md = "V (L)")))

# The text of each body cell of a flextable column, as rendered (markdown
# subscripts become separate chunks)
pest_rendered_text = function(ft, col){
  ic = flextable::information_data_chunk(ft)
  ic = ic[ic[[".part"]] == "body" & ic[[".col_id"]] == col, ]
  ic = ic[order(ic[[".row_id"]], ic[[".chunk_index"]]), ]
as.vector(tapply(ic[["txt"]], ic[[".row_id"]], paste, collapse = ""))}

test_that("gen_pest_table names a BSV correlation row's parameters with the report names", {
  # the parameter table nlmixr2est reports for a correlated block
  pf = data.frame(
    "Est."     = c("1.01", "3.46", ""),
    "BSV(CV%)" = c("24.3", "14.1", "0.725"),
    row.names  = c("tcl", "tv", "BSV correlation between tcl and tv"),
    check.names = FALSE)
  res = gen_pest_table(obnd = obnd_pest, fit = list(par.fixed = pf), rptdetails = rptdetails_pest)
  expect_true(res$isgood)
  expect_identical(
    res$df[[1]]$Parameter,
    c("CL~int~ (L/hr)", "V (L)", "BSV correlation between CL~int~ (L/hr) and V (L)"))
  expect_identical(
    pest_rendered_text(res$ft[[1]], "Parameter"),
    c("CLint (L/hr)", "V (L)", "BSV correlation between CLint (L/hr) and V (L)"))
  # the other cells of the row are as nlmixr2est gave them
  expect_identical(res$df[[1]][3, "BSV(CV%)"], "0.725")
  expect_identical(res$df[[1]][3, "Est."], "")

  # a name the report does not give stays the model's
  res = gen_pest_table(
    obnd       = obnd_pest,
    fit        = list(par.fixed = pf),
    rptdetails = list(parameters = list(tcl = list(md = "CL~int~ (L/hr)"))))
  expect_identical(res$df[[1]]$Parameter[3], "BSV correlation between CL~int~ (L/hr) and tv")
  res = gen_pest_table(obnd = obnd_pest, fit = list(par.fixed = pf), rptdetails = NULL)
  expect_identical(res$df[[1]]$Parameter, c("tcl", "tv", "BSV correlation between tcl and tv"))

  # a report name given for the whole row is used as it is
  rd_row = rptdetails_pest
  rd_row$parameters[["BSV correlation between tcl and tv"]] = list(md = "corr(CL, V)")
  res = gen_pest_table(obnd = obnd_pest, fit = list(par.fixed = pf), rptdetails = rd_row)
  expect_identical(res$df[[1]]$Parameter[3], "corr(CL, V)")
})

test_that("gen_pest_table reports the BSV correlation rows of a fit", {
  skip_on_cran()
  mod = function() {
    ini({
      tka <- 0.45
      tcl <- 1
      tv <- 3.45
      eta.ka ~ 0.6
      eta.cl + eta.v ~ c(0.3, 0.05, 0.1)
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      linCmt() ~ add(add.sd)
    })
  }
  fit = suppressMessages(suppressWarnings(nlmixr2est::nlmixr2(
    mod,
    nlmixr2data::theo_sd,
    "focei",
    control = nlmixr2est::foceiControl(print = 0, maxOuterIterations = 0, calcTables = FALSE, covMethod = ""))))
  # nlmixr2est versions before the BSV correlation rows report none
  skip_if_not(
    any(startsWith(rownames(fit$par.fixed), "BSV correlation between ")),
    "the installed nlmixr2est does not report BSV correlation rows")
  res = gen_pest_table(obnd = obnd_pest, fit = fit, rptdetails = rptdetails_pest)
  expect_true(res$isgood)
  expect_identical(
    res$df[[1]]$Parameter,
    c("tka", "CL~int~ (L/hr)", "V (L)", "add.sd", "BSV correlation between CL~int~ (L/hr) and V (L)"))
  expect_identical(
    pest_rendered_text(res$ft[[1]], "Parameter"),
    c("tka", "CLint (L/hr)", "V (L)", "add.sd", "BSV correlation between CLint (L/hr) and V (L)"))
  # maxOuterIterations = 0 keeps the omega at its ini() values: 0.05 / sqrt(0.3 * 0.1)
  expect_identical(res$df[[1]][5, "BSV(CV%)"], "0.289")
})

test_that("gen_pest_table is unchanged for a fit without BSV correlation rows", {
  fit = fetch_fit_example()
  pf  = fit$par.fixed
  expect_false(any(startsWith(rownames(pf), "BSV correlation between ")))
  res = gen_pest_table(obnd = obnd_pest, fit = fit, rptdetails = rptdetails_pest)
  expect_true(res$isgood)
  expect_identical(res$df[[1]]$Parameter, c("tka", "CL~int~ (L/hr)", "V (L)", "add.sd"))
  expect_identical(res$df[[1]][, names(pf)], pf)
})
