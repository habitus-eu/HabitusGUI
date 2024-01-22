options("sp_evolution_status" = 2)
library(HabitusGUI)
context("Identify wrong files loaded as GGIR config files")
test_that("Wrong GGIR config files trigger a message and a sample config.csv file is created and used", {
  
  # Load a json file as a GGIR .csv file ----
  ggir_config_wrong_json = system.file("testfiles_palmspy/palmspy-params.json", package = "HabitusGUI")[1]
  check = checkConfigFile(file = ggir_config_wrong_json, tool = "GGIR")
  
  # test there is an error message
  expect_equal(length(check), 1)
  expect_equal(check, "The GGIR config file uploaded is not a csv file")
  
  # Load a csv file that is not a GGIR config file ----
  ggir_config_wrong_csv = system.file("testfiles_hbGIS/config_hbGIS.csv", package = "HabitusGUI")[1]
  check = checkConfigFile(file = ggir_config_wrong_csv, tool = "GGIR")
  
  # test there is an error message
  expect_equal(length(check), 1)
  expect_equal(check, "The csv file uploaded is not a GGIR config file")
})