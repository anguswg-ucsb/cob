# call get_model_comp() function to generate plots and save them to an output directory
# Author: Angus Watters

# clear R working directory
rm(list = ls())

library(tidyverse)
library(lubridate)
library(gridExtra)
library(RColorBrewer)
library(gtable)
library(grid)

# source function and all internal helper functions
source("R/get_model_comp.R")

# model_dir
base_path  <- "D:/cob/latest/latest"

# base and comp model names
base_model <- "0000.DRRP_WEP_2023_003_ID1_Base"
comp_model <- "0101.DRRP_WEP_2023_003_ID1_2050_18pctOutdoorReduction_7525"

# read in the quarter-monthly to date converter
date_df    <- readr::read_csv("data-raw/qm_to_date_conversion.csv")

# borrowing on or off input
borrow     <- "off"

# instream flow values
upper_dry_oct_apr_cfs = NULL
upper_dry_may_sep_cfs = NULL
lower_dry_oct_apr_cfs = NULL
lower_dry_may_sep_cfs = NULL
upper_avg_oct_apr_cfs = NULL
upper_avg_may_sep_cfs = NULL
lower_avg_oct_apr_cfs = NULL
lower_avg_may_sep_cfs = NULL

# start and end years
start_year = 1915
end_year   = 2014

# title and axis font sizes
title_size = 10
xaxis_size = 9

# path to create output directory and save plots into
save_path  <- "D:/cob/latest/latest"

get_model_comp(
  base_path  = "D:/cob/latest/latest",
  base_model = "0000.DRRP_WEP_2023_003_ID1_Base",
  comp_model = "0101.DRRP_WEP_2023_003_ID1_2050_18pctOutdoorReduction_7525",
  date_df    = date_df,
  borrow     = "off",
  start_year = 1915,
  end_year   = 2014,
  title_size = 10,
  xaxis_size = 9,
  save_path  = "D:/cob/latest/latest"
)
