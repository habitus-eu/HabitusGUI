
rm(list = ls())
source("~/projects/HabitusGUI/R/palmsplusr_shiny.R")
source("~/projects/HabitusGUI/R/hbt_build_days.R")
source("~/projects/HabitusGUI/R/hbt_build_palmsplus.R")
source("~/projects/HabitusGUI/R/hbt_build_multimodal.R")
source("~/projects/HabitusGUI/R/hbt_build_trajectories.R")
library(readr)
library(dplyr)
library(tidyr)
library(palmsplusr)
library(rlang)
library(data.table)
library(purrr)
palmsplusr_shiny(gisdir = "/home/vincent/projects/fontys/test_palmsplusr/GIS",
                 palmsdir = "/home/vincent/projects/fontys/test_palmsplusr/PALMS_output",
                 gislinkfile = "~/projects/fontys/test_palmsplusr/Tables/participant_basis.csv",
                 outputdir = "~/projects/fontys", 
                 dataset_name = "test_palmsplusr",
                 configfile = "~/projects/fontys/config_palmsplusr.csv")

