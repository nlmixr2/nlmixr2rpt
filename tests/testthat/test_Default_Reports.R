fit = readRDS(system.file(package="rptnlmixr", "examples", "fit.rds"))
#--------------
test_that("Default PowerPoint Report",{
  obnd_pptx = read_template(
    template = system.file(package="rptnlmixr", "templates","nlmixr_obnd_template.pptx"),
    mapping  = system.file(package="rptnlmixr", "templates","nlmixr_obnd_template.yaml"))


  invisible(capture.output(
  obnd_pptx = report_fit(
    fit     = fit,
    obnd    = obnd_pptx)))
  expect_true(obnd_pptx$isgood)})

test_that("Default Word Report",{
  obnd_docx = read_template(
    template = system.file(package="rptnlmixr", "templates","nlmixr_obnd_template.docx"),
    mapping  = system.file(package="rptnlmixr", "templates","nlmixr_obnd_template.yaml"))

  invisible(capture.output(
  obnd_docx = report_fit(
    fit     = fit,
    obnd    = obnd_docx)))
  expect_true(obnd_docx$isgood)})
