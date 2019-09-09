context("opening spectra files")

test_that("open ASD spectra file", {
  
  file.dir <- system.file("extdata/PM01_TIAM_B_LC_REFL00005.asd",package="FieldSpectra")
  spec <- read.asd(file.dir, out.dir=file.path('~'), start.wave=350, end.wave=2500, step.size=1)
  expect_equal(round(sum(spec$Spectra)), 517)
  
})