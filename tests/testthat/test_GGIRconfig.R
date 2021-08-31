library(GGIR)
context("GGIRconfig")
test_that("config file can be created and updated", {
  configfile = "config.csv"
  create_test_GGIRconfig(configfile=configfile, desiredtz = "Europe/Amsterdam")
  # has file been created?
  expect_true(file.exists(configfile))
  
  # does file have expected dimentions
  config = read.csv(configfile)
  expect_equal(nrow(config), 112)
  
  # check that it is a GGIR config file
  test_config = checkGGIRconfig(configfile)
  expect_equal(test_config, "Europe/Amsterdam")
  
  # check that we can update the desiredtz value
  updateGGIRconfig(configfile, new_desiredtz = "Europe/Berlin")
  test_update = checkGGIRconfig(configfile)
  expect_equal(test_update, "Europe/Berlin")
  
  # clean up folders and files
  if (file.exists(configfile)) rm(configfile)

})