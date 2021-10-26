library(HabitusGUI)
context("Identify tools needed to process data")
test_that("Correct tools are proposed by test_identify_tools", {
  # Scenario 1: All tools needed
  sce1 = identify_tools(datatypes = c("AccRaw", "ACount", "GPS", "GIS"),
                            goals = c("PA", "Sleep", "QC", "Trips", "Environment"),
                            available_tools = c("GGIR", "PALMSpy", "PALMSplus"))
  expect_equal(length(sce1$tools_needed), 3) 
  expect_equal(sce1$tools_needed[3], "PALMSplus") 
  expect_equal(sce1$iotools[[1]]@output, c("GGIR_out", "ACount"))
  
  # Scenario 2: GIS missing
  sce2 = identify_tools(datatypes = c("AccRaw", "ACount", "GPS"),
                        goals = c("PA", "Sleep", "QC", "Trips", "Environment"),
                        available_tools = c("GGIR", "PALMSpy", "PALMSplus"))
  expect_equal(length(sce2$tools_needed), 2) 
  expect_equal(sce2$tools_needed, c("GGIR", "PALMSpy"))
  expect_equal(sce2$iotools[[2]]@output, "PALMSpy_out")
  
  # Scenario 3: AccRaw missing
  sce3 = identify_tools(datatypes = c("ACount", "GPS", "GIS"),
                        goals = c("PA", "Sleep", "QC", "Trips", "Environment"),
                        available_tools = c("GGIR", "PALMSpy", "PALMSplus"))
  expect_equal(length(sce3$tools_needed), 2) 
  expect_equal(sce3$tools_needed, c("PALMSpy", "PALMSplus"))
  expect_equal(sce3$iotools[[2]]@output, "PALMSplus_out")
  expect_equal(sce3$iotools[[2]]@usecases, c("Environment", "QC"))
  
  # Scenario 4: ACount missing
  sce4 = identify_tools(datatypes = c("AccRaw", "GPS", "GIS"),
                        goals = c("PA", "Sleep", "QC", "Trips", "Environment"),
                        available_tools = c("GGIR", "PALMSpy", "PALMSplus"))
  expect_equal(length(sce4$tools_needed), 3) 
  expect_equal(sce4$tools_needed, c("GGIR", "PALMSpy", "PALMSplus"))
  expect_equal(sce4$iotools[[2]]@output, "PALMSpy_out")
  expect_equal(sce4$iotools[[2]]@usecases, c("Trips", "QC", "Environment"))
  
  
  # Scenario 5: All data vailable, but only interest in Environment
  sce5 = identify_tools(datatypes = c("AccRaw", "ACount", "GPS", "GIS"),
                        goals = c("Environment"),
                        available_tools = c("GGIR", "PALMSpy", "PALMSplus"))
  expect_equal(length(sce5$tools_needed), 3) 
  expect_equal(sce5$tools_needed, c("GGIR", "PALMSpy", "PALMSplus"))
  expect_equal(sce5$iotools[[2]]@output, "PALMSpy_out")
  expect_equal(sce5$iotools[[2]]@usecases, c("Trips", "QC", "Environment"))
})