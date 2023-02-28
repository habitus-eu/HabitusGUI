library(HabitusGUI)
context("Identify wrong files loaded as GGIR config files")
test_that("Wrong GGIR config files trigger a message and a sample config.csv file is created and used", {
  
  # Load a json file as a GGIR .csv file ----
  ggir_config_wrong_json = system.file("testfiles_palmspy/palmspy-params.json", package = "HabitusGUI")[1]
  params_ggir = load_params(file = ggir_config_wrong_json, format = "csv_ggir", homedir = getwd())
  
  # test there is an error message
  expect_equal(length(params_ggir$message), 1)
  expect_equal(params_ggir$message, "The GGIR config file uploaded is not a csv file. A sample GGIR config file is loaded instead.")
  
  # test a sample config file has been generated
  expect_equal(file.exists(paste0(getwd(), "/config.csv")), TRUE)
  
  # test params are loaded
  expect_equal(ncol(params_ggir$params), 9)
  
  # Load a csv file that is not a GGIR config file ----
  ggir_config_wrong_csv = system.file("testfiles_palmsplusr/config_palmsplusr.csv", package = "HabitusGUI")[1]
  params_ggir = load_params(file = ggir_config_wrong_csv, format = "csv_ggir", homedir = getwd())
  
  # test there is an error message
  expect_equal(length(params_ggir$message), 1)
  expect_equal(params_ggir$message, "The csv file uploaded is not a GGIR config file. A sample GGIR config file is loaded instead.")
  
  # test a sample config file has been generated
  expect_equal(file.exists(paste0(getwd(), "/config.csv")), TRUE)
  
  # test params are loaded
  expect_equal(ncol(params_ggir$params), 9)
  
  # remove generated config file ----
  if (file.exists(paste0(getwd(), "/config.csv"))) file.remove(paste0(getwd(), "/config.csv"))
})