#' Counts2csv
#'
#' @param outputdir Path to output directory
#' @param configfile Path to configuration file
#' @return no object is returned, only a new file is created in the output directory
#' @importFrom stats aggregate
#' @importFrom utils write.table
#' @export
#' 
Counts2csv = function(outputdir = c(), configfile = c()) {
  # Create imitated Actigraph file from counts in GGIR output
  #------------------------------------------------------------------------
  # Add folder for simulated actigraph output:
  tmp = unlist(strsplit(outputdir, "/"))
  actigraphdir = paste0(paste0(tmp[1:(length(tmp) - 1)], collapse = "/"), "/actigraph")
  if (dir.exists(actigraphdir) == FALSE) {
    dir.create(actigraphdir)
  }
  # Load GGIR part 1 milestone files
  rdafolder = paste0(outputdir,"/meta/basic")
  rdafiles = dir(rdafolder, full.names = T)
  config = read.csv(file = configfile)
  desiredtz = config$value[which(config$argument == "desiredtz")]
  for (i in 1:length(rdafiles)) {
    M = c()
    load(rdafiles[i])
    if (length(M) > 0) {
      # Extract windowsize
      tmp = as.POSIXlt(M$metashort$timestamp[1:2], format = "%Y-%m-%dT%H:%M:%S%z", desiredtz)
      ws3 = as.numeric(difftime(tmp[2], tmp[1], units = "secs"))
      # Create file header for the Actigraph file
      start = as.POSIXlt(M$metashort$timestamp[1], format = "%Y-%m-%dT%H:%M:%S%z", desiredtz)
      starttime = strftime(start, format="%H:%M:%S")
      startdate = paste0(start$mon + 1, "/", start$mday, "/", start$year + 1900) #month day year 
      SN = which(rownames(I$header) == "Serial Number:")
      DT = which(rownames(I$header) == "Download Date")
      if (length(SN) > 0) {  
        serialnumber = unlist(strsplit(as.character(I$header[SN, 1]), " "))
        if (length(serialnumber) > 0) {
          serialnumber = serialnumber[which(nchar(serialnumber) == max(nchar(serialnumber)))]
        }
      } else {
        serialnumber = "notidentified"
      }
      if (length(DT) > 0) {  
        downloaddate = unlist(strsplit(as.character(I$header[DT,1]), " "))
        if (length(downloaddate) > 0) {
          downloaddate = downloaddate[which(nchar(downloaddate) == max(nchar(downloaddate)))]
          splitdt = unlist(strsplit(downloaddate, "-"))
          if (length(splitdt) > 0) { # change - to / seperator
            downloaddate = paste0(splitdt[c(3:1)], collapse = "/") 
          }
        }
      } else {
        downloaddate = "notidentified"
      }
      header = c("------------ Data File Created By ActiGraph wGT3XPlus ActiLife v6.10.2 Firmware v2.2.1 date format M/d/yyyy Filter Normal -----------",
                 paste0("Serial Number: ",serialnumber),
                 paste0("Start Time ", starttime),
                 paste0("Start Date ",startdate),
                 "Epoch Period (hh:mm:ss) 00:00:15",
                 paste0("Download Time ",starttime),
                 paste0("Download Date ",downloaddate),
                 "Current Memory Address: 0",
                 "Current Battery Voltage: 4.03     Mode = 13",
                 "--------------------------------------------------")
      # Aggregate GGIR counts per 15 minutes
      colsofinterest = c("NeishabouriCount_x", "NeishabouriCount_y", "NeishabouriCount_z")
      counts5sec = M$metashort[, colsofinterest]
      counts5sec$timenum = floor(seq(0, nrow(counts5sec) - 1, 1) / ((15)/ws3))
      # Assuming that count unit is counts/min, so we have sum the epochs per 15 minutes and then devide by 15
      counts15sec = round( as.matrix(aggregate(counts5sec, by = list(counts5sec$timenum), sum)[, colsofinterest]))
      # Add dummy fourth column for which we do not have an estimate
      counts15sec = cbind(counts15sec,ifelse(test = counts15sec[,2] > 200, yes = 1,no = 0))
      colnames(counts15sec) = NULL
      # Store to csv file
      fname = unlist(strsplit(rdafiles[i], "eta_|[.]RD"))
      fname = fname[length(fname) - 1]
      fname_full = paste0(actigraphdir,"/",fname, ".csv")
      way_to_save = "ruben" #"original" # to aid investigating whether way to save file matters
      if (way_to_save == "original") {
        cat(paste(header, "\n", collapse = ""), file = fname_full)
        write.table(counts15sec, file = fname_full, row.names = F, 
                    col.names = F,sep = ",", fileEncoding = "UTF-8", append = TRUE)
      } else if (way_to_save == "ruben") {
        sink(fname_full)
        for (hi in 1:length(header)) {
          cat(paste0(header[hi],"\n"))
        }
        sink()
        write.table(counts15sec, fname_full,append = TRUE, col.names = FALSE, row.names = FALSE, sep = ',')
      }
    }
  }
}
