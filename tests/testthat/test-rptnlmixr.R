suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("xpose"))
suppressPackageStartupMessages(library("ggforce"))
suppressPackageStartupMessages(library("onbrand"))
suppressPackageStartupMessages(library("nlmixr2rpt"))

# Objects used in the tests below:
rptyaml    = system.file(package="nlmixr2rpt", "examples", "report_fit_test.yaml")
fit        = fetch_fit_example()
obnd_test  = read_template(
  template = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.docx"),
  mapping  = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.yaml"))


test_that("Reading yaml", {
  yaml_read_res = yaml_read_fit(
  obnd    = obnd_test,
  rptyaml = rptyaml,
  fit     = fit)
  expect_true(yaml_read_res$isgood)
})


test_that("build_figures", {

  rptdetails    = yaml_read_fit(
     obnd    = obnd_test,
     rptyaml = rptyaml,
     fit     = fit)$rptdetails
  
  bfres = 
    suppressWarnings(
    suppressMessages(
    invisible(
          build_figures(obnd       = obnd_test,
                        fit        = fit, 
                        verbose    = FALSE,
                        rptdetails = rptdetails)
    )
    )
    )
  
  # Tests the overall build process:
  expect_true(bfres$isgood)

  # This should be a good figure so skip should be false and 
  # isgood should be true:
  expect_false(bfres$rptfigs$dv_vs_pred$skip)
  expect_true(bfres$rptfigs$dv_vs_pred$isgood)

  # Catching broken figures:
  expect_false(bfres$rptfigs$bad_figure$isgood)

  # Catching skipped figures
  expect_true(bfres$rptfigs$skip_figure$skip)
})

test_that("build_tables", {

  rptdetails    = yaml_read_fit(
     obnd    = obnd_test,
     rptyaml = rptyaml,
     fit     = fit)$rptdetails
  
  btres = 
    suppressWarnings(
    suppressMessages(
    invisible(
          build_tables(obnd       = obnd_test,
                        fit        = fit, 
                        verbose    = FALSE,
                        rptdetails = rptdetails)
    )
    )
    )
  
  # Tests the overall build process:
  expect_true(btres$isgood)

  # This should be a good figure so skip should be false and 
  # isgood should be true:
  expect_false(btres$rpttabs$pest_table$skip)
  expect_true(btres$rpttabs$pest_table$isgood)

  # Catching broken figures:
  expect_false(btres$rpttabs$bad_table$isgood)

  # Catching skipped figures
  expect_true(btres$rpttabs$skip_table$skip)
})


test_that("PowerPoint Workflow", {
  obnd_pptx = read_template(
    template = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.pptx"),
    mapping  = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.yaml"))
  
  obnd_pptx      = 
    suppressMessages(
    suppressWarnings(
    invisible(
    report_fit(
    fit          = fit,
    obnd         = obnd_pptx,
    verbose      = FALSE, 
    parameters   = list(
      TV_Vc = list(
        md  = "V~c~ (L)",
        txt = "Vc (L)" ),
      TV_ka = list(
        md  = "k~a~ (1/hr stuff)",
        txt = "ka (1/hr stuff)" )),
    placeholders = list(RUN="10"),
    cat_covars   = NULL, #c("SEX","SUBTYPE"),
    cont_covars  = NULL #c("WT") 
    )
   )
   )
   )

  expect_true(obnd_pptx$isgood)
  
  save_res = save_report(obnd_pptx, tempfile(fileext=".pptx"))
  
  expect_true(save_res$isgood)

})

test_that("Word Workflow", {
  obnd_docx = read_template(
    template = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.docx"),
    mapping  = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.yaml"))
  
  obnd_docx      = 
    suppressMessages(
    suppressWarnings(
    invisible(
    report_fit(
    fit          = fit,
    obnd         = obnd_docx,
    verbose      = FALSE,
    parameters   = list(
      TV_Vc = list(
        md  = "V~c~ (L)",
        txt = "Vc (L)" ),
      TV_ka = list(
        md  = "k~a~ (1/hr stuff)",
        txt = "ka (1/hr stuff)" )),
    placeholders = list(RUN="10"),
    cat_covars   = c("SEX","SUBTYPE"),
    cont_covars  = c("WT") )
   )
   )
   )

  expect_true(obnd_docx$isgood)
  
  save_res = save_report(obnd_docx, tempfile(fileext=".docx"))
  
  expect_true(save_res$isgood)

})

test_that("gen_pest_table names a row without a label by its row name", {
  # nlmixr2est gives a non-mu-referenced eta's row an NA label in a model with
  # labels (later versions give ""); both are named by the row name
  for(eta_label in c(NA_character_, "")){
    pf = data.frame(
      "Parameter" = c("Ka (1/hr)", "", eta_label),
      "Est."      = c("0.476", "0.687", ""),
      "BSV(CV%)"  = c("", "", "69.5"),
      row.names   = c("tka", "add.sd", "eta.ka"),
      check.names = FALSE)
    res = gen_pest_table(obnd = obnd_test, fit = list(par.fixed = pf), rptdetails = NULL)
    expect_true(res$isgood)
    expect_identical(res$df[[1]]$Parameter, c("Ka (1/hr)", "add.sd", "eta.ka"))
  }
})

test_that("gen_pest_table reports a labelled fit with a non-mu-referenced eta", {
  skip_on_cran()
  mod = function() {
    ini({
      tka <- 0.45
      label("Ka (1/hr)")
      tcl <- 1
      label("CL (L/hr)")
      tv <- 3.45
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka) * exp(eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      linCmt() ~ add(add.sd)
    })
  }
  fit_labels = suppressMessages(suppressWarnings(nlmixr2est::nlmixr2(
    mod,
    nlmixr2data::theo_sd,
    "focei",
    control = nlmixr2est::foceiControl(print = 0, maxOuterIterations = 0, calcTables = FALSE, covMethod = ""))))
  expect_identical(rownames(fit_labels$par.fixed), c("tka", "tcl", "tv", "add.sd", "eta.ka"))
  res = gen_pest_table(obnd = obnd_test, fit = fit_labels, rptdetails = NULL)
  expect_true(res$isgood)
  expect_identical(res$df[[1]]$Parameter, c("Ka (1/hr)", "CL (L/hr)", "tv", "add.sd", "eta.ka"))
})
