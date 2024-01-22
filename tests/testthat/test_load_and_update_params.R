options("sp_evolution_status" = 2)
library(HabitusGUI)
context("Load and update parameters from configuration files")
test_that("Parameters can be loaded and updated from config files", {
 
  # Load GGIR .csv file
  ggir_config_zip = system.file("testfiles_ggir/example_config_files_GGIR.zip", package = "HabitusGUI")[1]
  ggir_config_csv = "config_GGIR_raw.csv"
  unzip(ggir_config_zip, ggir_config_csv)
  params_ggir = load_params(file = ggir_config_csv, format = "csv_ggir")
  expect_equal(ncol(params_ggir), 9)

  # Load hbGIS .csv file
  hbGIS_config_csv = system.file("testfiles_hbGIS/config_hbGIS.csv", package = "HabitusGUI")[1]
  params_hbGIS = load_params(file = hbGIS_config_csv, format = "csv_hbGIS")
  expect_equal(ncol(params_hbGIS), 10)
  
  # Load hbGPS .csv file
  hbGPS_config_csv = system.file("testfiles_hbGPS/config_hbGPS.csv", package = "HabitusGUI")[1]
  params_hbGPS = load_params(file = hbGPS_config_csv, format = "csv_hbGPS")
  expect_equal(ncol(params_hbGPS), 9)
  
  # Update GGIR .csv file
  params_ggir$value[which(rownames(params_ggir) == "idloc")] = "3"
  update_params(new_params = params_ggir, file = ggir_config_csv, format = "csv_ggir")
  params_ggir2 = load_params(file = ggir_config_csv, format = "csv_ggir")
  expect_equal(ncol(params_ggir2), 9)
  expect_equal(params_ggir2$value[which(rownames(params_ggir2) == "idloc")] , "3")

  # Update hbGIS .csv file
  params_hbGIS$value[which(rownames(params_hbGIS) == "trajectory_field__sedentary")] = "sum(activityintensity == 1) * 15"
  update_params(new_params = params_hbGIS, file = hbGIS_config_csv, format = "csv_hbGIS")
  params_hbGIS2 = load_params(file = hbGIS_config_csv, format = "csv_hbGIS")
  expect_equal(ncol(params_hbGIS2), 10)
  expect_equal(params_hbGIS2$value[which(rownames(params_hbGIS2) == "trajectory_field__sedentary")] ,
               "sum(activityintensity == 1) * 15")
  
})