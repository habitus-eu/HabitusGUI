library(HabitusGUI)
context("Loading of parameters from configuration files")
test_that("Paramters can be loaded from configuration files", {
  # GGIR .csv file
  ggir_config_csv  = system.file("testfiles_ggir/config.csv", package = "HabitusGUI")[1]
  params_ggir = load_params(file=ggir_config_csv, format="csv_ggir")
  expect_equal(ncol(params_ggir), 2) 
  
  # PALMSpy .json file
  palmspy_config_json = system.file("testfiles_palmspy/palmspy-params.json", package = "HabitusGUI")[1]
  params_palmspy = load_params(file = palmspy_config_json, format="json_palmspy")
  expect_equal(ncol(params_palmspy), 1) 
})