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

model_dirs[3, ]$path

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

fixed_demands <- readr::read_csv("D:/cob/psi/FixedBCDemands.csv", show_col_types = FALSE)
fixed_demands

# Inflow 1 (North Boulder Creek above Silver Lake) + Inflow 10 (MBC abv Barker Reservoir) from May 1 - June 30

# ExchangePotential = read this in via csv
#
# TotalBypassplusDirect = get static value

# NewStorageWater = min(ExchangePotential, mtnInflow - TotalBypassplusDirect)
#For City of Boulder total demand, use Demand_58_Flow + Demand_91_Flow

### COB Mtn Direct Calc
MayPctDmd = 0.0913
JunPctDmd = 0.1193
# COBAnnualDemand (lookup from csv file)

out_defs %>%
  dplyr::filter(Name %in% c("Demand_58_Flow", "Demand_91_Flow"))

COBAnnualDemand <-
  output %>%
  dplyr::select(model_version:end_date, Demand_58_Flow, Demand_91_Flow) %>%
  dplyr::mutate(
    annual_demand     = as.numeric(Demand_58_Flow) + as.numeric(Demand_91_Flow),
    may_pct_dmd       = 0.0913,
    june_pct_dmd      = 0.1193,
    may_june_demand   = may_pct_dmd*annual_demand + june_pct_dmd*annual_demand,
    pipeline_capacity = 8686.4
  ) %>%
  dplyr::group_by(wyqm) %>%
  dplyr::mutate(
    cob_mtn_direct    = min(may_june_demand, pipeline_capacity)
  )
  # dplyr::filter(qm == "28")
# MayJuneDemand = (MayPctDmdCOBAnnualDemand) + (JunPctDmdCOBAnnualDemand)
# PipelineCapacity = 8686.4 #af capacity May-June
# COBMtnDirect = min(MayJuneDemand, PipelineCapacity)
# TotalBypassplusDirect = min(COBMtnDirect + max(FixedBCDemands[10 ditches]-0.346*MtnInflow,0), mtnInflow)
# TotalBypassplusDirect = min(COBMtnDirect + max(FixedBCDemands-0.346*MtnInflow,0), mtnInflow)

# # Barker Res
# May1BarkerStorage = Reservoir_3_Content on QM 28
May1BarkerStorage <-
  output %>%
  dplyr::select(model_version:end_date, Reservoir_3_Content) %>%
  dplyr::filter(qm == "28")

# # Watershed Reservoirs
# May1NBCStorage = Reservoir_1_Content on QM 28
May1NBCStorage <-
  output %>%
  dplyr::select(model_version:end_date, Reservoir_1_Content) %>%
  dplyr::filter(qm == "28")

# # predicted May 1 storage
# PredictedStorage = min(18250, May1BarkerStorage + May1NBCStorage + NewStorageWater)
#
# # COB Boulder Res Storage
# BoulderResStorage = DataObject_1_Flow on QM 28
BoulderResStorage <-
  output %>%
  dplyr::select(model_version:end_date, DataObject_1_Flow) %>%
  dplyr::filter(qm == "28")

# # CBT half of equation
# Boulder_CBT_Storage = DataObject_39_Flow QM 28
Boulder_CBT_Storage <-
  output %>%
  dplyr::select(model_version:end_date, DataObject_39_Flow) %>%
  dplyr::filter(qm == "28")

# COBCBTExchangeAdjustment = min(18250 - mtnStorage, exchangePotential)
# COBCBTExchangeAdjustment <- min()
# COB_CBT_capacity = min(NewStorageWater, COBCBTExchangeAdjustment)
#

# CBT fraction value
CBT_Fraction = 0.40
#
# CBT_Numerator = (Boulder_CBT_Storage - COB_CBT_capacity)*CBT_Fraction
#
# Total_Numerator = PredictedStorage + CBT_Numerator
#
# COB_Annual_Demand = lookup via csv
#
# PSI_orig = Total_Numerator / COB_Annual_Demand
#
# PSI_new = (Total_Numerator + BoulderResStorage)/ COB_Annual_Demand
