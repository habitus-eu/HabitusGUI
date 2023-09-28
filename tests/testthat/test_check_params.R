options("sp_evolution_status" = 2)
library(HabitusGUI)
context("Check parameters during loading and before saving")
test_that("Parameters are checked", {
  
  params = data.frame(value = c("0", "1", "1.1", "A", "A", "A", "NA", "c(1,2,3)", "1,2,3"),
                      topic = rep("topic", 9),
                      description = rep("description", 9),
                      class = c("double", "double", "integer", 
                                "set", "set", "double", "double", "integer", "integer"),
                      minimum = rep("1", 9),
                      maximum = rep("10", 9),
                      set = c(NA, NA, NA, "A;B;C", "D;E;F", NA, NA, NA, NA))
  rownames(params) = c("a", "b", "c", "d", "e", "f", "g", "h", "i")
  paramcheck = check_params(params, tool = "GGIR")
  
  # Check that values are as expected
  expect_equal(paramcheck$blocked_params$name, c("a", "c", "f", "g", "e"))
  expect_equal(paramcheck$blocked_params$error, c("is not within expected range: 1 - 10",
                                                  "is not an integer", "is not numeric", "is not numeric",
                                                  "is not among expected values: D, E, F"))
  expect_equal(paramcheck$error_message, paste0("Error in parameter \" a \": Value 0 is",
                                                " not within expected range: 1 - 10<br/>Error",
                                                " in parameter \" c \": Value 1.1 is not an",
                                                " integer<br/>Error in parameter \" e \": ",
                                                "Value A is not among expected values: D, E,",
                                                " F<br/>Error in parameter \" f \": Value A ",
                                                "is not numeric<br/>Error in parameter \" g",
                                                " \": Value NA is not numeric<br/>", collapse = ""))
  #===================================
  # Check timeformat seperately
  params = data.frame(value = c("%Y-%m-%d %H:%M:%S", "%Y-typo-%d %H:%M:%S"),
                      topic = rep("topic", 2),
                      description = rep("description", 2),
                      class = c("timeformat", "timeformat"),
                      minimum = rep(NA, 2),
                      maximum = rep(NA, 2),
                      set = c(NA, NA))
  rownames(params) = c("j", "k")
  paramcheck = check_params(params, tool = "hbGPS")
  
  # Check that values are as expected
  expect_equal(nrow(paramcheck$blocked_params), 1)
  expect_equal(paramcheck$blocked_params$name, c("k"))
  expect_equal(paramcheck$blocked_params$error, "is not a valid R time format specification.")
  expect_equal(paramcheck$error_message, paste0("Error in parameter \" k \": Value",
                                                " %Y-typo-%d %H:%M:%S is not a",
                                                " valid R time format ",
                                                "specification.<br/>", collapse = ""))
  
})