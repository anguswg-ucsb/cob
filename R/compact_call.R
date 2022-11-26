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

# parameters to get desired model to compare
mod_id   <- "ID1"
cc       <- "0121"
mod_clim <- "7525"


# folder with all Compact call runs
base_folder <- paste0("G:/.shortcut-targets-by-id/1B8Jfl31Nww6VN-dRe1H6jov-ogkJhMBW/2022 Modeling/Model Runs/Compact Call Analysis/", mod_id, "/")

# path to date converter csv
date_path   <- "D:/cob/compact_call/qm_to_date_conversion2.csv"

# path to save out processed outputs/plots
save_path   <- "D:/cob/compact_call/outputs"

# *****************************************
# ---- build model directory dataframe ----
# *****************************************

# COB Total Demand Flow
date_df <- readr::read_csv(date_path)

# use parse_directory_cc() to get parse through the model file names and create an organized dataframe w/ model details (runs, versions, ID, path, etc)
model_dirs  <- parse_directory_cc(
                    base_folder = base_folder
                    )

# Filter down mode_dirs to get the Baseline NOCC model info
base_mods <-
  model_dirs %>%
  dplyr::filter(
    compact_call  == cc,
    model_version == "055ac",
    model_num     == mod_clim,
    output        == "OutputSheet"
    )

# Filter down mode_dirs to get the Comparison CC model info
comp_mods <-
  model_dirs %>%
  dplyr::filter(
    compact_call  != cc,
    model_version == "055ac",
    model_num     == mod_clim,
    output        == "OutputSheet"
  )

# TODO needs easy, automated fix
# scenario print name
scenario_name <- paste0(
  base_mods$model_id[1], "-", base_mods$model_num[1], "_", base_mods$model_version[1], "_nocc",
  " vs. ",
  comp_mods$model_id[1], "-", comp_mods$model_num[1], "_", comp_mods$model_version[1], "_cc"
)

# output folder name, redundent
output_folder <- paste0(
  base_mods$model_id[1], "-", base_mods$model_num[1], "_", base_mods$model_version[1], "_nocc",
  " vs. ",
  comp_mods$model_id[1],  "-", comp_mods$model_num[1], "_", comp_mods$model_version[1], "_cc"
)

# ********************************************
# ---- generate output plots to save_path ----
# ********************************************

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

# ****************************************************************************
# ****************************************************************************
