suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("xpose"))
suppressPackageStartupMessages(library("ggforce"))
suppressPackageStartupMessages(library("ggPMX")   )

# Objects used in the tests below:
rptyaml    = system.file(package="nlmixr2rpt", "examples", "report_fit_test.yaml")
fit        = readRDS(system.file(package="nlmixr2rpt", "examples", "fit.rds"))
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


test_that("PowerPoint Workflow", {

  rptdetails    = yaml_read_fit(
     obnd    = obnd_test,
     rptyaml = rptyaml,
     fit     = fit)$rptdetails
  
  bfres = 
    #suppressPackageStartupMessages(
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
    #)
  
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

test_that("PowerPoint Workflow", {
  obnd_pptx = read_template(
    template = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.pptx"),
    mapping  = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.yaml"))
  
  obnd_pptx      = 
    #suppressPackageStartupMessages(
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
    cat_covars   = c("SEX","SUBTYPE"),
    cont_covars  = c("WT") )
   )
   )
   )
   #)

  expect_true(obnd_pptx$isgood)
  
  save_res = save_report(obnd_pptx, tempfile(fileext=".pptx"))
  
  expect_true(save_res$isgood)

})
