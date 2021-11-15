library(HabitusGUI)
context("Check parameters during loading and before saving")
test_that("Parameters are checked", {
  params = data.frame(value = c("0", "1", "A", "", "NA"),
                      topic = rep("topic", 5),
                      description = rep("description", 5),
                      class = rep("num", 5),
                      minimum = rep("1", 5),
                      maximum = rep("10", 5))
  rownames(params) = c("a", "b", "c", "d", "e")
  paramcheck = check_params(params)
  # Check that values are as expected
  expect_equal(paramcheck$name, c("a", "c", "d", "e"))
  expect_equal(paramcheck$error, c("Value not within expected range",
                                   "Value is not numeric",
                                   "Value is not numeric",
                                   "Value is not numeric"))
})