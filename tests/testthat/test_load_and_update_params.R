library(HabitusGUI)
context("Load and update parameters from configuration files")
test_that("Parameters can be loaded and updated from config files", {
  
  # Load GGIR .csv file
  ggir_config_csv  = system.file("testfiles_ggir/config.csv", package = "HabitusGUI")[1]
  params_ggir = load_params(file=ggir_config_csv, format="csv_ggir")
  expect_equal(ncol(params_ggir), 2) 
  
  # Load PALMSpy .json file
  palmspy_config_json = system.file("testfiles_palmspy/palmspy-params.json", package = "HabitusGUI")[1]
  params_palmspy = load_params(file = palmspy_config_json, format="json_palmspy")
  expect_equal(ncol(params_palmspy), 3) 
  
  # Update PALMSpy .json file
  params_palmspy$value[which(rownames(params_palmspy) == "interval")] = "35"
  update_params(new_params = params_palmspy, file = palmspy_config_json, format="json_palmspy")
  params_palmspy2 = load_params(file = palmspy_config_json, format="json_palmspy")
  expect_equal(ncol(params_palmspy2), 3) 
  expect_equal(params_palmspy2$value[which(rownames(params_palmspy2) == "interval")] , "35") 
})