context("summary lme output")


test_that("Knit rmd and compare with expected output",{
  outfile = tempfile(fileext = ".html")
  use_rmd = "knitprinter.Rmd"
  expect_true(file.exists(use_rmd))
  rmarkdown::render(use_rmd, output_file = outfile, quiet = TRUE)
  expect_zip = "knitprinter.zip"
  expect_true(file.exists(expect_zip))
  unzip(expect_zip, exdir = tempdir())
  expect_html = file.path(tempdir(), "knitprinter.html")
  expect_true(file.exists(expect_html))
  expect_true(all.equal(readLines(outfile), readLines(expect_html)))
})

