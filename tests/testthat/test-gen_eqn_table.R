suppressPackageStartupMessages(library("onbrand"))
suppressPackageStartupMessages(library("nlmixr2rpt"))

# Objects used in the tests below:
fit       = fetch_fit_example()
obnd_docx = read_template(
  template = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.docx"),
  mapping  = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.yaml"))
obnd_pptx = read_template(
  template = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.pptx"),
  mapping  = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.yaml"))


test_that("gen_eqn_table Word", {
  ft = gen_eqn_table(fit = fit, obnd = obnd_docx)

  # Should be a flextable with one column and one row per equation
  expect_s3_class(ft, "flextable")
  expect_equal(ft$col_keys, "eq")
  expect_gt(nrow(ft$body$dataset), 0)

  # Header should have been removed
  expect_equal(nrow(ft$header$dataset), 0)

  # The align* wrapper and alignment markers should be stripped
  expect_false(any(grepl("begin\\{align", ft$body$dataset$eq)))
  expect_false(any(grepl("end\\{align",   ft$body$dataset$eq)))
  expect_false(any(grepl("(?<!\\\\)&",    ft$body$dataset$eq, perl=TRUE)))

  # Word gets double line spacing
  expect_true(all(ft$body$styles$pars$line_spacing$data == 2))

  # Equations should render into a Word document
  save_res = flextable::save_as_docx(ft, path = tempfile(fileext=".docx"))
  expect_true(file.exists(save_res))
})

test_that("gen_eqn_table PowerPoint", {
  ft = gen_eqn_table(fit = fit, obnd = obnd_pptx)

  expect_s3_class(ft, "flextable")
  expect_gt(nrow(ft$body$dataset), 0)

  # PowerPoint keeps rows tight
  expect_true(all(ft$body$styles$pars$line_spacing$data == 1))
  expect_true(all(ft$body$styles$pars$padding.top$data    == 1))
  expect_true(all(ft$body$styles$pars$padding.bottom$data == 1))

  # Equations should render into a PowerPoint document
  save_res = flextable::save_as_pptx(ft, path = tempfile(fileext=".pptx"))
  expect_true(file.exists(save_res))
})
