
rm(list = ls())

dirR = "~/projects/HabitusGUI/R"

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

setwd("~/projects/HabitusGUI")
library(readr)
library(dplyr)
library(tidyr)
library(palmsplusr)
library(rlang)
library(data.table)
library(purrr)
library(geosphere)
palmsplusr_shiny(gisdir = "/home/vincent/projects/fontys/test_palmsplusr/GIS",
                 palmsdir = "/home/vincent/projects/fontys/test_palmsplusr/PALMS_output",
                 gislinkfile = "~/projects/fontys/test_palmsplusr/Tables/participant_basis.csv",
                 outputdir = "~/projects/fontys", 
                 dataset_name = "test_palmsplusr",
                 configfile = "~/projects/fontys/config_palmsplusr.csv")


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