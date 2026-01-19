# Create test
test_that("read.sig function works correctly with default parameter", {
  # Create a temporary directory and file for testing
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "test.sig")

  # Create a mock .sig file with some sample data
  writeLines(c(
    "name=test_file.sig",
    "data=",
    "400 1 2 10",
    "500 1 2 20",
    "600 1 2 30",
    "700 1 2 40"
  ), temp_file)

  filepath <- system.file("extdata/gr070214_003.sig",package="FieldSpectra")
  
  # Test with default = TRUE (all columns)
  data_all <- read.sig(filepath, default = TRUE)
  expect_equal(ncol(data_all), 5)  # Should have 5 columns
  expect_equal(colnames(data_all), c("Wavelengths(nm)", "Reference Values", "Target Values", "Reflectance(%)", "file_name"))

  # Test with default = FALSE (selected columns)
  data_selected <- read.sig(filepath, default = FALSE)
  expect_equal(ncol(data_selected), 3)  # Should have 3 columns: Wavelengths(nm), Reflectance(%), file_name
  expect_equal(colnames(data_selected), c("Wavelengths(nm)", "Reflectance(%)", "file_name"))

  # Clean up
  unlink(temp_file)
})


