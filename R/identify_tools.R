#' identify_tools 
#'
#' @param datatypes Character vector with available data types
#' @param goals Character vector with research goals
#' @param available_tools Character vector with available tools/tools to consider
#' @return List with proposed pipeline and tool input/ouptut properties
#' @importFrom methods new setClass
#' @export
#' 

# S4 class needs to be defined outside function
setClass(Class = "toolio", slots = list(input = "character", output = "character", usecases = "character"))

identify_tools = function(datatypes = c("AccRaw", "GPS", "GIS", 
                                        "GGIR_out", "hbGPS_out"),
                          goals = c("PA", "QC", "Trips", "Environment"),
                          available_tools = c("GGIR", "hbGIS", "hbGPS")) {
  iotools = list(GGIR = new("toolio",
                            input = "AccRaw",
                            output = "GGIR_out",
                            usecases = c("PA", "QC", "Trips", "Environment")), 
                 hbGIS = new("toolio",
                                   input = c("hbGPS_out", "GIS"),
                                   output = c("hbGIS_out"),
                                   usecases = c("Environment", "QC")),
                 hbGPS = new("toolio",
                             input = c("GGIR_out","GPS"),
                             output = c("hbGPS_out"),
                             usecases = c("Trips", "QC", "Environment")))
  iotools = iotools[which(names(iotools) %in% available_tools)] # only look at available tools
  tools_needed = outputs = c()
  # loop over tools and select the ones that generate the output users needs and is able to generate
  for (j in 1:length(available_tools)) { # assumption is that pipeline is never longer then number of tools
    for (i in 1:length(iotools)) {
      if (all(iotools[[i]]@input %in% datatypes | iotools[[i]]@input %in% outputs) &
          any(iotools[[i]]@usecases %in% goals)) {
        outputs = c(outputs, iotools[[i]]@output)
        tools_needed = c(tools_needed, names(iotools[i]))
      }
    }
  }
  tools_needed = tools_needed[!duplicated(tools_needed)]
  if ("AccRaw" %in% datatypes == FALSE & "GGIR" %in% tools_needed) {
    tools_needed = tools_needed[-which(tools_needed == "GGIR")]
  }
  invisible(list(tools_needed = tools_needed, iotools = iotools[which(names(iotools) %in% tools_needed)]))
}