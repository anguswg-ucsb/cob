# Purpose: Read raw data from City of Boulder's CRAM model quarter monthly output ('OutputSheet')
# and Annual output ('OutputAnnualSummary') to produce a variety of summary data & graphics.
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

model_sub <-
  model_dirs %>%
  dplyr::filter(
    model_id %in% c("ID1", "ID5"),
    model_version == "055a",
    model_num == "7525",
    output == "OutputSheet"
    )

output_folder <- paste0(
  model_sub$model_id[1], "-", model_dirs$model_num[1],
  " vs. ",
  model_sub$model_id[2], "-", model_dirs$model_num[2]
                        )

# ISF Data
isf <- process_isf(
  isf_path  = dplyr::filter(model_dirs, grepl("ISF", file))$path,
  model_ids = c(model_sub$model_id[1], model_sub$model_id[2])
)

### Plot the quotas
# use 'aes_string' instead of the normal aes to read the site name column headers as strings!
p_quota <- ggplot(Quota_annual, aes_string(x = "Year", y = "Quota", color = "ModelRun",
                                           linetype = "ModelRun")) +
  geom_line() +  #col = color_list[i]
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.20)) +
  #ylab("Flow (af)") +
  xlab("Water Year") +
  ggtitle("Annual C-BT Quota") +
  theme(plot.title = element_text(size = title_size),
        axis.title = element_text(size = xaxis_size))
model_dirs$model_id
model_dirs$model_version
model_dirs$model_num
model_dirs$plot_id

### User controlled data
model_version <- c("v075")

base_model_ID <- "ID1"                # Leave as ID1 (always compare with base)
base_model_ID_suffix <- ""        # if this is not needed, keep blank ""
base_climate <- "7525"               # Base or 9010, 7525, Center
base_model_ID_prefix <- "0101."   # if this is not needed, keep blank ""


compare_model_version <- c("v075")

compare_model_ID <- "ID5"            # ID2, ID3, ID4, ID5
compare_model_ID_suffix <- ""   # if this is not needed, keep blank ""
compare_climate <- "7525"             # Base or 9010, 7525, Center
compare_model_ID_prefix <- "0101."   # if this is not needed, keep blank ""

# set plot parameters
title_size = 10
xaxis_size = 9

model_folder <- "latest"

device_type <- ".png"     # .png, .pdf


# Calculated parameters ---------------------------------------------------


output_folder <- paste(base_model_ID, "-", base_climate, " vs ",
                       compare_model_ID, "-", compare_climate, sep = "")


model_version_text <- substr(model_version, start = 2, stop = nchar(model_version))
compare_model_version_text <- substr(compare_model_version, start = 2, stop = nchar(compare_model_version))


file_prefix <- c(paste0(base_model_ID_prefix, "DRRP_DroughtPlan_2020_", model_version_text, "_",
                        compare_model_ID, compare_model_ID_suffix, "_", compare_climate, sep = ""),
                 paste0(compare_model_ID_prefix, "DRRP_DroughtPlan_2020_", compare_model_version_text, "_",
                        base_model_ID, base_model_ID_suffix, "_", base_climate, sep = ""))
n_file_prefix <- length(file_prefix)
scenario_name <- c(paste(compare_climate, "-", compare_model_ID, compare_model_ID_suffix, "_",
                         compare_model_version, sep = ""),
                   paste(base_climate, "-", base_model_ID, "_", model_version,
                         base_model_ID_suffix, sep = ""))
scenario_name


# model_subset <- model_dirs %>%
  # dplyr::filter(
  # )
