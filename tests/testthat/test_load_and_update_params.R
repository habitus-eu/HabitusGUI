library(HabitusGUI)
context("Load and update parameters from configuration files")
test_that("Parameters can be loaded and updated from config files", {
 
  # Load GGIR .csv file
  ggir_config_csv = system.file("testfiles_ggir/config.csv", package = "HabitusGUI")[1]
  params_ggir = load_params(file = ggir_config_csv, format = "csv_ggir")
  expect_equal(ncol(params_ggir), 9)
  
  # Load PALMSpy .json file
  palmspy_config_json = system.file("testfiles_palmspy/palmspy-params.json", package = "HabitusGUI")[1]
  params_palmspy = load_params(file = palmspy_config_json, format = "json_palmspy")
  expect_equal(ncol(params_palmspy), 10)
  
  
  # Load palmsplusr .csv file
  palmsplusr_config_csv = system.file("testfiles_palmsplusr/config_palmsplusr.csv", package = "HabitusGUI")[1]
  params_palmsplusr = load_params(file = palmsplusr_config_csv, format = "csv_palmsplusr")
  expect_equal(ncol(params_palmsplusr), 10)
  
  # Update PALMSpy .json file
  params_palmspy$value[which(rownames(params_palmspy) == "interval")] = "25"
  update_params(new_params = params_palmspy, file = palmspy_config_json, format = "json_palmspy")
  params_palmspy2 = load_params(file = palmspy_config_json, format = "json_palmspy")
  expect_equal(ncol(params_palmspy2), 10)
  expect_equal(as.character(params_palmspy2$value[which(rownames(params_palmspy2) == "interval")]), "25")

  # Update GGIR .csv file
  params_ggir$value[which(rownames(params_ggir) == "idloc")] = "3"
  update_params(new_params = params_ggir, file = ggir_config_csv, format = "csv_ggir")
  params_ggir2 = load_params(file = ggir_config_csv, format = "csv_ggir")
  expect_equal(ncol(params_ggir2), 9)
  expect_equal(params_ggir2$value[which(rownames(params_ggir2) == "idloc")] , "3")

  # Update palmsplusr .csv file
  params_palmsplusr$value[which(rownames(params_palmsplusr) == "trajectory_field__sedentary")] = "sum(activityintensity == 1) * 15"
  # palmsplusr_config_csv_tmp = gsub(pattern = ".csv", replacement = "2.csv", x = palmsplusr_config_csv)
  # file.copy(from = palmsplusr_config_csv, to = palmsplusr_config_csv_tmp, overwrite = TRUE)
  update_params(new_params = params_palmsplusr, file = palmsplusr_config_csv, format = "csv_palmsplusr")
  params_palmsplusr2 = load_params(file = palmsplusr_config_csv, format = "csv_palmsplusr")
  expect_equal(ncol(params_palmsplusr2), 10)
  expect_equal(params_palmsplusr2$value[which(rownames(params_palmsplusr2) == "trajectory_field__sedentary")] ,
               "sum(activityintensity == 1) * 15")
  
})