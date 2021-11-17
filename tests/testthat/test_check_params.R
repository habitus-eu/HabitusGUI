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
  expect_equal(paramcheck$blocked_params$error, c("Number is not within expected range", 
                                   "Number is not an integer",
                                   "Value is not numeric",
                                   "Value is not numeric", 
                                   "Value is not part of expected set"))
  expect_equal(paramcheck$error_message, paste0("Error in parameter \" a = 0 \": Number is not within",
                                                " expected range<br/>Error in parameter \" c = 1.1 \": ",
                                                "Number is not an integer<br/>Error in parameter \" e = A ",
                                                "\": Value is not part of expected set<br/>Error in parameter",
                                                " \" f = A \": Value is not numeric<br/>Error in parameter \" ",
                                                "g = NA \": Value is not numeric<br/>", collapse = ""))
})