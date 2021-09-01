library(HabitusGUI)
context("Identify tools needed to process data")
test_that("Correct tools are proposed by test_identify_tools", {
  # Scenario 1: All tools needed
  sce1 = identify_tools(datatypes = c("AccRaw", "AccCount", "GPS", "GIS"),
                            goals = c("PA", "Sleep", "QC", "Trips", "Environment"),
                            available_tools = c("GGIR", "PALMS", "PALMSplus"))
  expect_equal(length(sce1$tools_needed), 3) 
  expect_equal(sce1$tools_needed[3], "PALMSplus") 
  expect_equal(sce1$iotools[[1]]@output, c("GGIR_out", "AccCount"))
  
  # Scenario 2: GIS missing
  sce2 = identify_tools(datatypes = c("AccRaw", "AccCount", "GPS"),
                        goals = c("PA", "Sleep", "QC", "Trips", "Environment"),
                        available_tools = c("GGIR", "PALMS", "PALMSplus"))
  expect_equal(length(sce2$tools_needed), 2) 
  expect_equal(sce2$tools_needed, c("GGIR", "PALMS"))
  expect_equal(sce2$iotools[[2]]@output, "PALMS_out")
  
  # Scenario 3: AccRaw missing
  sce3 = identify_tools(datatypes = c("AccCount", "GPS", "GIS"),
                        goals = c("PA", "Sleep", "QC", "Trips", "Environment"),
                        available_tools = c("GGIR", "PALMS", "PALMSplus"))
  expect_equal(length(sce3$tools_needed), 2) 
  expect_equal(sce3$tools_needed, c("PALMS", "PALMSplus"))
  expect_equal(sce3$iotools[[2]]@output, "PALMSplus_out")
  expect_equal(sce3$iotools[[2]]@usecases, c("Environment", "QC"))
})