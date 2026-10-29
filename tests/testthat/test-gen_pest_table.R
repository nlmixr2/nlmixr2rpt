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

# The rendered chunks of one body cell: text and formatting flags
pest_rendered_chunks = function(ft, col, row){
  ic = flextable::information_data_chunk(ft)
  ic = ic[ic[[".part"]] == "body" & ic[[".col_id"]] == col & ic[[".row_id"]] == row, ]
  ic = ic[order(ic[[".chunk_index"]]), ]
  data.frame(
    txt            = ic[["txt"]],
    italic         = ic[["italic"]],
    vertical.align = ic[["vertical.align"]])}

# A one-row table holding the correlation row `rname`
pest_cor_table = function(rname){
  data.frame(
    "Est."     = "",
    "BSV(CV%)" = "0.725",
    row.names  = rname,
    check.names = FALSE)}

# The parameter table nlmixr2est reports for a correlated block: without
# parameter labels, and with them (the label column then describes the
# correlation row)
pest_pf = list(
  unlabelled = data.frame(
    "Est."     = c("1.01", "3.46", ""),
    "BSV(CV%)" = c("24.3", "14.1", "0.725"),
    row.names  = c("tcl", "tv", "bsvcorr(tcl,tv)"),
    check.names = FALSE),
  labelled = data.frame(
    "Parameter" = c("CL (L/hr)", "", "BSV correlation between tcl and tv"),
    "Est."      = c("1.01", "3.46", ""),
    "BSV(CV%)"  = c("24.3", "14.1", "0.725"),
    row.names   = c("tcl", "tv", "bsvcorr(tcl,tv)"),
    check.names = FALSE))

test_that("gen_pest_table labels a BSV correlation row with the report names", {
  for(pf in pest_pf){
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
    expect_identical(res$df[[1]]$Parameter[3], "BSV correlation between tcl and tv")

    # a report name given for the whole row is used as it is
    rd_row = rptdetails_pest
    rd_row$parameters[["bsvcorr(tcl,tv)"]] = list(md = "corr(CL, V)")
    res = gen_pest_table(obnd = obnd_pest, fit = list(par.fixed = pf), rptdetails = rd_row)
    expect_identical(res$df[[1]]$Parameter[3], "corr(CL, V)")
  }
})

test_that("gen_pest_table renders a correlation row's names as separate runs", {
  plain = function(txt){
    data.frame(txt = txt, italic = FALSE, vertical.align = "baseline")}
  # model names are plain text: an underscore is not markdown
  for(pair in list(c("eta_cl", "eta_v"), c("a_b_c", "d_e_f"))){
    rname = paste0("bsvcorr(", pair[1], ",", pair[2], ")")
    res = gen_pest_table(obnd = obnd_pest, fit = list(par.fixed = pest_cor_table(rname)), rptdetails = NULL)
    expect_identical(res$df[[1]]$Parameter, paste0("BSV correlation between ", pair[1], " and ", pair[2]))
    expect_identical(
      pest_rendered_chunks(res$ft[[1]], "Parameter", 1),
      plain(c("BSV correlation between ", pair[1], " and ", pair[2])))
  }
  # each report name is rendered on its own, so markdown in one does not
  # reach the other
  for(mds in list(c("CL_int", "V_c"), c("CL*", "V*"))){
    rd = list(parameters = list(tcl = list(md = mds[1]), tv = list(md = mds[2])))
    res = gen_pest_table(obnd = obnd_pest, fit = list(par.fixed = pest_cor_table("bsvcorr(tcl,tv)")), rptdetails = rd)
    expect_identical(
      pest_rendered_chunks(res$ft[[1]], "Parameter", 1),
      plain(c("BSV correlation between ", mds[1], " and ", mds[2])))
  }
  # a report name's own markdown still renders
  res = gen_pest_table(
    obnd       = obnd_pest,
    fit        = list(par.fixed = pest_cor_table("bsvcorr(tcl,tv)")),
    rptdetails = rptdetails_pest)
  expect_identical(
    pest_rendered_chunks(res$ft[[1]], "Parameter", 1),
    data.frame(
      txt            = c("BSV correlation between ", "CL", "int", " (L/hr)", " and ", "V (L)"),
      italic         = FALSE,
      vertical.align = c("baseline", "baseline", "subscript", "baseline", "baseline", "baseline")))
})

test_that("gen_pest_table renders every row of a label that repeats", {
  pf = data.frame(
    "Est."      = c("1.01", "3.46"),
    row.names   = c("tcl", "tv"),
    check.names = FALSE)
  rd = list(parameters = list(tcl = list(md = "k~a~"), tv = list(md = "k~a~")))
  res = gen_pest_table(obnd = obnd_pest, fit = list(par.fixed = pf), rptdetails = rd)
  for(ridx in 1:2){
    expect_identical(
      pest_rendered_chunks(res$ft[[1]], "Parameter", ridx),
      data.frame(txt = c("k", "a"), italic = FALSE, vertical.align = c("baseline", "subscript")))
  }
})

test_that("gen_pest_table reports the BSV correlation rows of a fit", {
  skip_on_cran()
  skip_if_not_installed("nlmixr2data")
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
    any(grepl("^bsvcorr\\(", rownames(fit$par.fixed))),
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
  expect_false(any(grepl("^bsvcorr\\(", rownames(pf))))
  res = gen_pest_table(obnd = obnd_pest, fit = fit, rptdetails = rptdetails_pest)
  expect_true(res$isgood)
  expect_identical(res$df[[1]]$Parameter, c("tka", "CL~int~ (L/hr)", "V (L)", "add.sd"))
  expect_identical(res$df[[1]][, names(pf)], pf)
})
