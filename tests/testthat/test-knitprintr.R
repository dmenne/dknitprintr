context("summary lme output")

core_html = function(file){
  f1 = readLines(file)
  from_i = which(str_detect(f1,"^\\<!-- code folding -->"))[1]
  to_i = which(str_detect(f1,"^\\<script>"))[2]
  ret = f1[(from_i + 1):(to_i + 1)]
  ret[str_length(ret)>9]
}

test_that("Knit rmd and compare with expected output",{
  #skip_on_travis() ## does not work yet on travis
  outfile = tempfile(fileext = ".html")
  use_rmd = "knitprinter.Rmd"
  expect_true(file.exists(use_rmd))
  rmarkdown::render(use_rmd, output_file = outfile, quiet = TRUE)
  expect_zip = "knitprinter.zip"
  expect_true(file.exists(expect_zip))
  unzip(expect_zip, exdir = tempdir())
  expect_html = file.path(tempdir(), "knitprinter.html")
  expect_true(file.exists(expect_html))

  f1 = core_html(outfile)
  f2 = core_html(expect_html)
  cat("\n outfile -----------------\n\n")
  cat(f1[1:2], sep = "\n")
  cat("\n expect_html -----------------\n\n")
  cat(f2[1:2], sep = "\n")
  expect_true(all.equal(f1,f2))
})

