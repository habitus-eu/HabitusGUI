options("sp_evolution_status" = 2)
library(HabitusGUI)
context("Identify tools needed to process data")
test_that("Correct tools are proposed by test_identify_tools", {
  available_tools = c("GGIR", "PALMSpy", "hbGPS", "palmsplusr", "CountConverter")
  # Scenario 1: All tools needed
  sce1 = identify_tools(datatypes = c("AccRaw", "ACount", "GPS", "GIS", "GGIR_out"),
                            goals = c("PA", "Sleep", "QC", "Trips", "Environment"),
                            available_tools = available_tools)
  expect_equal(length(sce1$tools_needed), 3) 
  expect_equal(sce1$tools_needed, c("GGIR", "palmsplusr", "hbGPS")) 
  expect_equal(sce1$iotools[[1]]@output, c("GGIR_out", "ACount"))
  expect_equal(sce1$iotools[[3]]@output, "palmsplusr_out")
  expect_equal(sce1$iotools[[4]]@output, "hbGPS_out")
  
  # Scenario 2: GIS missing
  sce2 = identify_tools(datatypes = c("AccRaw", "ACount", "GPS", "GGIR_out"),
                        goals = c("PA", "Sleep", "QC", "Trips", "Environment"),
                        available_tools = available_tools)
  expect_equal(length(sce2$tools_needed), 2) 
  expect_equal(sce2$tools_needed, c("GGIR", "hbGPS"))
  expect_equal(sce2$iotools[[2]]@output, "hbGPS_out")
  
  # Scenario 3: AccRaw missing
  sce3 = identify_tools(datatypes = c("ACount", "GPS", "GIS", "GGIR_out"),
                        goals = c("PA", "Sleep", "QC", "Trips", "Environment"),
                        available_tools = available_tools)
  expect_equal(length(sce3$tools_needed), 2) 
  expect_equal(sce3$tools_needed, c("palmsplusr", "hbGPS"))
  expect_equal(sce3$iotools[[2]]@output, "palmsplusr_out")
  expect_equal(sce3$iotools[[2]]@usecases, c("Environment", "QC"))
  
  # Scenario 4: ACount missing
  sce4 = identify_tools(datatypes = c("AccRaw", "GPS", "GIS", "GGIR_out"),
                        goals = c("PA", "Sleep", "QC", "Trips", "Environment"),
                        available_tools = available_tools)
  expect_equal(length(sce4$tools_needed), 3) 
  expect_equal(sce4$tools_needed, c("GGIR", "palmsplusr", "hbGPS"))
  expect_equal(sce4$iotools[[2]]@output, "palmsplusr_out")
  expect_equal(sce4$iotools[[2]]@usecases, c("Environment", "QC"))
  expect_equal(sce4$iotools[[4]]@output, "hbGPS_out")
  expect_equal(sce4$iotools[[4]]@usecases, c("Trips", "QC", "Environment"))
  
  # Scenario 5: All data vailable, but only interest in Environment
  sce5 = identify_tools(datatypes = c("AccRaw", "ACount", "GPS", "GIS"),
                        goals = c("Environment"),
                        available_tools = available_tools)
  expect_equal(length(sce5$tools_needed), 3) 
  expect_equal(sce5$tools_needed, c("GGIR", "palmsplusr", "hbGPS"))
  expect_equal(sce5$iotools[[2]]@output, "palmsplusr_out")
  expect_equal(sce5$iotools[[2]]@usecases, c("Environment", "QC"))
  
  # Scenario 6: hbGPS_out and GIS available
  sce6 = identify_tools(datatypes = c("hbGPS_out", "GIS"),
                        goals = c("QC"),
                        available_tools = available_tools)
  expect_equal(length(sce6$tools_needed), 1) 
  expect_equal(sce6$tools_needed, "palmsplusr")
  expect_equal(sce6$iotools[[1]]@output, "palmsplusr_out")
  expect_equal(sce6$iotools[[1]]@usecases, c("Environment", "QC"))
  
})