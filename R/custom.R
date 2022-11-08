# Purpose: Read raw data from City of Boulder's CRAM model quarter monthly output ('OutputSheet')
# and Annual output ('OutputAnnualSummary') to produce a variety of summary data & graphics.
# Author: Bill Szafranski
# Date: 8/17/2021

# clear R working directory
rm(list = ls())
cat("\014")

library(tidyverse)
#source("f_my_ecdf.R")  # code to calculate empirical CDF
# library(plotly)
library(lubridate)
library(gridExtra)
# library(scales)
library(RColorBrewer)
library(gtable)
library(grid)

source("R/make_lookup.R")
source("R/process_output.R")
source("R/process_demand.R")
source("R/process_isf.R")
source("R/process_quota.R")
source("R/parse_directory.R")
source("R/utils.R")

base_folder <- "D:/cob/latest/latest"

# info on model files
model_dirs  <- parse_directory(base_folder = base_folder)

date_convert <- readr::read_csv("data-raw/qm_to_date_conversion.csv")
# select only the "Output" and "OtherDataSheet" files
# model_outs  <- model_dirs[grepl("Other|Output", model_dirs$output), ]

path_lst <-
  model_dirs %>%
  dplyr::group_by(output) %>%
  dplyr::group_split()

# date conversion dataframe
date_convert <- readr::read_csv("data-raw/qm_to_date_conversion.csv")

# process quota file
quota_df <- process_quota(
  quota_path = model_dirs[model_dirs$output == "Quota",]$path,
  model_ids  = model_dirs$plot_id[!grepl("NANA", model_dirs$plot_id)]
  )

# OutputSheet paths
path_df <-
  model_dirs %>%
  dplyr::filter(output == "OutputSheet") %>%
  dplyr::tibble()
path_df
# y = 3

# Process OutputSheets
out_df <- lapply(1:nrow(path_df), function(y) {

  message(paste0("Iteration --> ", y))

  out <- process_output(
    file_df     = path_df[y, ],
    date_df     = date_convert,
    verbose     = TRUE
  )

}) %>%
  dplyr::bind_rows()

# tmp <-
#   out_df %>%
#   dplyr::filter(model_version == "055a")

# retrieve loopup table from all Output sheets and keep the distinct rows (no duplicate definitions)
definitions <-  lapply(1:nrow(path_df), function(y) {

   make_lookup(output_path = path_df$path[y])

  }
) %>%
  dplyr::bind_rows() %>%
  dplyr::distinct()

# ISF Data
isf <- process_isf(
  isf_path  = dplyr::filter(model_dirs, grepl("ISF", file))$path,
  model_ids = unique(na.omit(model_dirs$model_id))
  )

# Quota data
quota <- process_quota(
  quota_path = dplyr::filter(model_dirs, grepl("Quota", file))$path,
  model_ids  = unique(na.omit(model_dirs$model_id))
)
model_dirs %>%
  tibble()

data_annual <- readr::read_csv(model_dirs$path[2],
         col_names = FALSE
         )
