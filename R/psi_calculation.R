# Purpose: Calculate PSI
# Author: Angus Watters
# Date: 11/8/2022

# clear R working directory
rm(list = ls())

# Libraries
library(tidyverse)
library(janitor)

# helper functions
source("R/make_lookup.R")
source("R/process_output.R")
source("R/process_demand.R")
source("R/parse_directory.R")
source("R/utils.R")

# **********************
# ---- Locate files ----
# **********************
# File paths
list.files("D:/cob/psi", full.names = TRUE)
list.files("D:/cob/latest/latest", pattern = "OutputSheet", full.names = TRUE)

# base model output folder
base_folder <- "D:/cob/latest/latest"

# *****************************************
# ---- build model directory dataframe ----
# *****************************************
# COB Total Demand Flow

# info on model files
model_dirs  <- parse_directory(base_folder = base_folder)

# **********************
# ---- Read in data ----
# **********************

# read in the quarter-monthly to date converter
date_convert <- readr::read_csv("data-raw/qm_to_date_conversion.csv")

# read and process OutputSheet: DRRP_DroughtPlan_2020_055a_ID1_7525.OutputSheet.csv"
output <- process_output(
  file_df = model_dirs[4, ],
  date_df = date_convert
  )

# Read in OutputDemandSheet
output_demand <- readr::read_csv(
  model_dirs[3, ]$path,
  col_names      = FALSE,
  show_col_types = FALSE
  )

# Read in OutputAnnualSummary
output_summary <- readr::read_csv(
  model_dirs[2, ]$path,
  col_names      = FALSE,
  show_col_types = FALSE
  )

# OutputSheet lookup table
out_defs <- make_lookup(output_path = model_dirs[4, ]$path)

# OutputDemandSheet lookup table
demand_defs <-  make_lookup(output_path = model_dirs[3, ]$path)

# OutputAnnualSummary lookup table
summary_defs <-  make_lookup(output_path = model_dirs[2, ]$path)

# exchange potential
exchange_potent <- readr::read_csv("D:/cob/psi/ExchangePotential_Lookup.csv", show_col_types = FALSE)
exchange_potent

# projected inflow
proj_inflow <- readr::read_csv("D:/cob/psi/ProjectedInflow_May-June_Inflow1_Inflow10_7525.csv", show_col_types = FALSE)
proj_inflow

# COB annual demands 2050
demands <- readr::read_csv("D:/cob/psi/COB_Annual_Demands_2050.csv", show_col_types = FALSE)
demands

# fixed BC Demands
fixed_demands <- readr::read_csv("D:/cob/psi/FixedBCDemands.csv", show_col_types = FALSE)
fixed_demands

# fixed BC demands for scenario 7525
fbc_dmd <-
  fixed_demands %>%
  dplyr::filter(grepl("7525", Scenario)) %>%
  .$FixedBCDemands

# COBAnnualDemand (lookup from csv file)
COBAnnualDemand <-
  output %>%
  dplyr::select(model_version:end_date, Demand_58_Flow, Demand_91_Flow) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    Demand_58_Flow    = sum(as.numeric(Demand_58_Flow)),
    Demand_91_Flow    = sum(as.numeric(Demand_91_Flow)),
    annual_demand     = as.numeric(Demand_58_Flow) + as.numeric(Demand_91_Flow)
    ) %>%
  dplyr::mutate(
    may_pct_dmd       = 0.0913,
    june_pct_dmd      = 0.1193,
    may_june_demand   = may_pct_dmd*annual_demand + june_pct_dmd*annual_demand,
    pipeline_capacity = 8686.4
    ) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(
    cob_mtn_direct    = min(may_june_demand, pipeline_capacity),
    fixed_bc_demands  = fbc_dmd
  ) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    dplyr::mutate(
      proj_inflow,
      year       = as.character(year),
      mtn_inflow = Inflow1 + Inflow10
    ),
    by = "year"
  ) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(
    total_bypass_plus_direct = min(cob_mtn_direct + max(fixed_bc_demands - 0.346*mtn_inflow, 0), mtn_inflow)
  ) %>%
  dplyr::ungroup()

# calculate new storage water using mtn_inflow, avg_mtn_inflow, pct of mtn_inflow compared to average mountain inflow
NewStorageWater <-
  COBAnnualDemand %>%
  dplyr::select(year, annual_demand, cob_mtn_direct, mtn_inflow, total_bypass_plus_direct) %>%
  dplyr::mutate(
    avg_mtn_inflow     = mean(mtn_inflow),
    pct_avg            = round(mtn_inflow/avg_mtn_inflow, 3)*100,
    exchange_potential = dplyr::case_when(
                                    pct_avg < 40                 ~ 0,
                                    pct_avg >= 40 & pct_avg < 70 ~ 3000,
                                    pct_avg >= 70 & pct_avg < 90 ~ 7000,
                                    pct_avg >= 90                ~ 14000
                                    )
    ) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(
    new_storage_water = min(exchange_potential, mtn_inflow - total_bypass_plus_direct)
  )

# PredictedStorage = min(18250, May1BarkerStorage + May1NBCStorage + NewStorageWater)
PredictedStorage <-
  output %>%
  dplyr::select(model_version:end_date, Reservoir_3_Content, Reservoir_1_Content, DataObject_1_Flow, DataObject_39_Flow) %>%
  dplyr::filter(qm == "28") %>%
  dplyr::left_join(
    NewStorageWater,
    by = "year"
  ) %>%
  dplyr::group_by(year) %>%
  dplyr::select(year, annual_demand, Reservoir_3_Content, Reservoir_1_Content, DataObject_1_Flow, DataObject_39_Flow, exchange_potential, new_storage_water) %>%
  dplyr::mutate(
    mtn_storage             = as.numeric(Reservoir_3_Content) + as.numeric(Reservoir_1_Content),
    pred_storage            = min(18250, as.numeric(Reservoir_3_Content) + as.numeric(Reservoir_1_Content) + new_storage_water),
    cbt_exchange_adjust     = min(18250 - mtn_storage, exchange_potential),
    cbt_capacity            = min(new_storage_water, cbt_exchange_adjust),
    cbt_numerator           = (as.numeric(DataObject_39_Flow) - cbt_capacity)*0.40,
    total_numerator         = pred_storage + cbt_numerator,
    psi_orig                = total_numerator/annual_demand,
    psi_new                 = (total_numerator + as.numeric(DataObject_1_Flow))/annual_demand
  )

# save out PSI calculations
write.csv(PredictedStorage, "D:/cob/psi/cob_psi_new.csv")=
