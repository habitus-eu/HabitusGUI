library(GGIR)
context("create_test_file_myRtool")
test_that("test files are generated and myRtool is able to process them", {
  inputdir = "inputdir"
  outputdir = "outputdir"
  if (!dir.exists(inputdir)) dir.create(path = inputdir)
  if (!dir.exists(outputdir)) dir.create(path = outputdir)
  config = "configfile.csv"
  
  # does create_test_files generate files?
  create_test_files(dir=inputdir, Nfiles=10, Nobs = 10)
  expect_true(dir.exists(inputdir))
  expect_equal(length(dir(inputdir)), 10) 
  
  # does myRTool generate files?
  myRTool(inputdir, outputdir, config=c(), desiredtz = "Europe/Amsterdam")
  expect_true(dir.exists(outputdir))
  expect_equal(length(dir(outputdir)), 1)
  expect_equal(dir(outputdir,full.names = FALSE), "results.csv")

  # clean up folders and files
  if (dir.exists(inputdir)) unlink(inputdir, recursive=TRUE)
  if (dir.exists(outputdir)) unlink(outputdir, recursive=TRUE)
  
})