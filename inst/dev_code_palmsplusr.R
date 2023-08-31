
rm(list = ls())

dirR = "D:/Code/HabitusGUI/R"

ffnames = dir(dirR) # creating list of filenames of scriptfiles to load
for (i in 1:length(ffnames)) {
  source(paste(dirR,"/",ffnames[i], sep = "")) #loading scripts for reading geneactiv data
}

# 
# source("~/projects/HabitusGUI/R/palmsplusr_shiny.R")
# source("~/projects/HabitusGUI/R/hbt_build_days.R")
# source("~/projects/HabitusGUI/R/hbt_build_palmsplus.R")
# source("~/projects/HabitusGUI/R/hbt_build_multimodal.R")
# source("~/projects/HabitusGUI/R/hbt_build_trajectories.R")
# source("~/projects/HabitusGUI/R/load_params.R")

setwd("D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/BEtestdata")
library(readr)
library(dplyr)
library(tidyr)
library(palmsplusr)
library(rlang)
library(data.table)
library(purrr)
library(geosphere)
library(lwgeom)

palmsplusr_shiny(gisdir = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/BEtestdata/GIS",
                 palmsdir = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/BEtestdata/hbGPSoutput",
                 gislinkfile = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/BEtestdata/Tables/participant_basis.csv",
                 outputdir = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/BEtestdata",
                 dataset_name = "BEtestdata",
                 configfile = "D:/Dropbox/Work/sharedfolder/DATA/Habitus/GPSprocessing/BEtestdata/config_palmsplusr.csv")

# palmsplusr_shiny(gisdir = "/media/vincent/projects/Habitus/palmsplusr/testdata/GIS",
#                  palmsdir = "/media/vincent/projects/Habitus/palmsplusr/testdata/PALMS_output/",
#                  gislinkfile = "/media/vincent/projects/Habitus/palmsplusr/testdata/Tables/participant_basis.csv",
#                  outputdir = "~/projects/fontys",
#                  dataset_name = "test_palmsplusr",
#                  configfile = "/media/vincent/projects/Habitus/palmsplusr/config_palmsplusr.csv")
# palmsplusr_shiny(gisdir = "/home/vincent/projects/fontys/test_palmsplusr/GIS",
#                  palmsdir = "/home/vincent/projects/fontys/test_palmsplusr/PALMS_output/",
#                  gislinkfile = "~/projects/fontys/test_palmsplusr/Tables/participant_basis.csv",
#                  outputdir = "~/projects/fontys",
#                  dataset_name = "test_palmsplusr",
#                  configfile = "~/projects/fontys/config_palmsplusr.csv")
# palmsplusr_shiny(gisdir = "/home/vincent/projects/fontys/test_palmsplusr/GIS",
#                  palmsdir = "/home/vincent/projects/fontys/test_palmsplusr/PALMS_output/",
#                  gislinkfile = "~/projects/fontys/test_palmsplusr/Tables/participant_basis.csv",
#                  outputdir = "~/projects/fontys",
#                  dataset_name = "test_palmsplusr",
#                  configfile = "~/projects/fontys/config_palmsplusr_v2.csv")


DROP = read.csv(file = "/media/vincent/projects/Habitus/palmsplusr/config_palmsplusr.csv")
UBU = read.csv("~/projects/fontys/config_palmsplusr_v2.csv")

for (i in names(UBU)) {
  print("-----")
  print(i)
  T1 = table(UBU[,i] %in% DROP[, i])
  T2 = table(DROP[,i] %in% UBU[, i])
  # if (length(T1) > 1) {
    print("t1")
    print(T1)
    print(UBU[which(UBU[,i] %in% DROP[, i] == FALSE), i])
  # }
  
  # if (length(T2) > 1)  {
    print("t2")
    print(T2)
    print(DROP[which(DROP[,i] %in% UBU[, i] == FALSE), i])
  # }
}

# DROP$context[which(DROP$context %in% UBU$context == FALSE)]
# 

# data("palms")
# palms_remove_tables()
# 
# palms_add_field("mvpa", "activityintensity > 1", TRUE)
# palmsplus <- palms_build_palmsplus(palms)
# 
# print(head(palmsplus))
# kkk
# 
# # Just with default columns
# trajectories <- palms_build_trajectories(palmsplus)
# 
# # Adding new fields before building
# palms_add_trajectory_field("mot", "first(tripmot)")
# palms_add_trajectory_field("mvpa", "sum(mvpa)")
# 
# trajectories <- palms_build_trajectories(palmsplus)