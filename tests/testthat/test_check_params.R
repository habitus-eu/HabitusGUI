library(HabitusGUI)
context("Check parameters during loading and before saving")
test_that("Parameters are checked", {
  
  params = data.frame(value = c("0", "1", "1.1", "A", "A", "A", "NA", "c(1,2,3)"),
                      topic = rep("topic", 8),
                      description = rep("description", 8),
                      class = c("num_double", "num_double", "num_integer", 
                                "set", "set", "num_double", "num_double", "num_integer"),
                      minimum = rep("1", 8),
                      maximum = rep("10", 8),
                      set = c(NA, NA, NA, "A;B;C", "D;E;F", NA, NA, NA))
  rownames(params) = c("a", "b", "c", "d", "e", "f", "g", "h")
  paramcheck = check_params(params)
  
  # Check that values are as expected
  expect_equal(paramcheck$blocked_params$name, c("a", "c", "f", "g", "e"))
  expect_equal(paramcheck$blocked_params$error, c("is not within expected range: 1 - 10",
                                                  "is not an integer", "is not numeric", "is not numeric",
                                                   "is not among excpected values: D, E, F"))
  expect_equal(paramcheck$error_message, paste0("Error in parameter \" a \": Value 0 is not within expected",
                                                " range: 1 - 10<br/>Error in parameter \" c \": Value 1.1 is",
                                                " not an integer<br/>Error in parameter \" e \": Value A is",
                                                " not among excpected values: D, E, F<br/>Error in parameter",
                                                " \" f \": Value A is not numeric<br/>Error in parameter \" g",
                                                " \": Value NA is not numeric<br/>", collapse = ""))
})