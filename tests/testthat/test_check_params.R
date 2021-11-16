library(HabitusGUI)
context("Check parameters during loading and before saving")
test_that("Parameters are checked", {
  
  params = data.frame(value = c("0", "1", "1.1", "A", "A", "", "NA"),
                      topic = rep("topic", 7),
                      description = rep("description", 7),
                      class = c("num_double", "num_double", "num_integer", 
                                "set", "set", "num_double", "num_double"),
                      minimum = rep("1", 7),
                      maximum = rep("10", 7),
                      set = c(NA, NA, NA, "A;B;C", "D;E;F", NA, NA))
  rownames(params) = c("a", "b", "c", "d", "e", "f", "g")
  paramcheck = check_params(params)
  
  # Check that values are as expected
  expect_equal(paramcheck$name, c("a", "c", "f", "g", "e"))
  expect_equal(paramcheck$error, c("Number is not within expected range", 
                                   "Number is not an integer",
                                   "Value is not numeric",
                                   "Value is not numeric", 
                                   "Value is not part of expected set"))
})