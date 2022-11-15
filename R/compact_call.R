

# Purpose: Calculate PSI
# Author: Angus Watters
# Date: 11/8/2022

# clear R working directory
rm(list = ls())

# Libraries
library(tidyverse)
library(janitor)
library(data.table)
library(lubridate)
library(gridExtra)
library(RColorBrewer)
library(gt)

# helper functions
source("R/make_lookup.R")
source("R/process_output.R")
source("R/process_demand.R")
source("R/parse_directory.R")
source("R/get_cc_outputs.R")
source("R/make_plots.R")
source("R/utils.R")


# **********************
# ---- Locate files ----
# **********************
# File paths
# list.files("D:/cob/psi", full.names = TRUE)
# list.files("D:/cob/latest/latest", pattern = "OutputSheet", full.names = TRUE)

# base model output folder
# cc_base_folder <- "D:/cob/compact_call/"
# list.files(cc_base_folder, full.names = T)

# folder with all Compact call runs
base_folder <- "G:/.shortcut-targets-by-id/1B8Jfl31Nww6VN-dRe1H6jov-ogkJhMBW/2022 Modeling/Model Runs/Compact Call Analysis/ID5/"

# path to save out processed outputs/plots
save_path   <- "D:/cob/compact_call/outputs"

# *****************************************
# ---- build model directory dataframe ----
# *****************************************
# COB Total Demand Flow

date_df <- readr::read_csv("D:/cob/compact_call/qm_to_date_conversion2.csv")

# info on model files
model_dirs  <- parse_directory_cc(base_folder = base_folder)

# baseline NOCC model info
base_mods <-
  model_dirs %>%
  dplyr::filter(
    compact_call == "0500",
    model_version == "055ac",
    model_num == "7525",
    output == "OutputSheet"
    )

# comparison CC model info
comp_mods <-
  model_dirs %>%
  dplyr::filter(
    compact_call != "0500",
    # compact_call == "0501",
    model_version == "055ac",
    model_num == "7525",
    output == "OutputSheet"
  )

# scenario print name
scenario_name <- paste0(
  base_mods$model_id[1], "-", base_mods$model_num[1], "_", base_mods$model_version[1], "_nocc",
  " vs. ",
  comp_mods$model_id[1], "-",comp_mods$model_num[1], "_", comp_mods$model_version[1], "_cc"
)

# output folder name
output_folder <- paste0(
  base_mods$model_id[1], "-", base_mods$model_num[1], "_", base_mods$model_version[1], "_nocc",
  " vs. ",
  comp_mods$model_id[1],  "-", comp_mods$model_num[1], "_", comp_mods$model_version[1], "_cc"
)


# Call get_cc_outputs function to create output folder + figures
get_cc_outputs(
  base_mod_dir         = base_mods,
  comp_mod_dir         = comp_mods,
  date_df              = date_df,
  analysis_years       = 4,
  title_size           = 10,
  xaxis_size           = 9,
  output_folder_name   = output_folder,
  scenario_name        = scenario_name,
  save_path            = save_path
)


# **********************************************************
# **********************************************************


