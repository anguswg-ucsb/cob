# Purpose: Read raw data from City of Boulder's CRAM model quarter monthly output ('OutputSheet')
# and Annual output ('OutputAnnualSummary') to produce a variety of summary data & graphics.
# Author: Bill Szafranski
# Date: 2/22/2022

# clear R working directory
rm(list = ls())

library(tidyverse)
library(data.table)
library(lubridate)
library(gridExtra)
library(RColorBrewer)
library(gt)
# library(webshot)

#source("f_my_ecdf.R")  # code to calculate empirical CDF
# library(plotly)
# library(lubridate)
# library(scales)
# library(RColorBrewer)
# library(gtable)



# set the project working directory
currwd <- "G:/.shortcut-targets-by-id/1B8Jfl31Nww6VN-dRe1H6jov-ogkJhMBW/2022 Modeling/Model Runs/Compact Call Analysis/ID5/"
# "G:/.shortcut-targets-by-id/1B8Jfl31Nww6VN-dRe1H6jov-ogkJhMBW/2022 Modeling/Model Runs/Compact Call Analysis/ID5/"

# "G:/.shortcut-targets-by-id/1B8Jfl31Nww6VN-dRe1H6jov-ogkJhMBW/2022 Modeling/Model Runs/Compact Call Analysis/ID5/0500.DRRP_DroughtPlan_2020_055a_CC_ID5_7525.OtherDataSheet.csv"
# "G:/My Drive/American_Whitewater_LHDP.lnk"
# list.files(currwd)
# setwd(currwd)


### User controlled data
# model_version <- c("v062")

# base_model_ID <- "ID2"                # Leave as ID1 (always compare with base)
# base_climate <- "Base"               # Base or 7525
# base_model_ID_suffix <- "nocc"        # no compact call (nocc)
# compare_model_ID = base_model_ID            # ID2, ID3, ID4, ID5
# compare_climate = base_climate             # Base or 9010, 7525, Center
# compare_model_ID_suffix <- "cc"           # compact call ON (cc)

model_version <- c("v055ac")

base_model_ID <- "ID5"                # Leave as ID1 (always compare with base)
base_climate <- "7525"               # Base or 7525
base_model_ID_suffix <- "nocc"

compare_model_ID = "ID5"
compare_climate = "7525"
compare_model_ID_suffix <- "cc"

# number of years to analyze in ensemble model run (after the shut down year)
analysis_years = 4

# number of columns in output sheet
output_sheet_columns <- 311

# number of compact call ensemble runs
# ensemble_model_runs = 90
# use this to run a small subset of the ensemble
temp_run_number = 89

# start of ensemble compact call outages
prefix_year_start <- c(1919:2014)

# set plot parameters
title_size = 10
xaxis_size = 9

model_folder <- base_model_ID

# fix current working directory to remove model folder created above
currwd_fix <- gsub(paste0("/", model_folder , "/"), "", currwd)


# Calculated parameters ---------------------------------------------------

# get the total number of model runs (compact call + no compact call)
total_simulation_count <- temp_run_number * (analysis_years+1) * 2


# build the scenario name
print_scenario_name <- paste0(base_model_ID, "-", base_climate, "_", base_model_ID_suffix, " vs ",
                              compare_model_ID, "-", compare_climate, "_", compare_model_ID_suffix)
output_folder <- paste0("output/", base_model_ID, "-", base_climate, "_", base_model_ID_suffix, " vs ",
                        compare_model_ID, "-", compare_climate, "_", compare_model_ID_suffix)
output_folder


model_version_text <- substr(model_version, start = 2, stop = nchar(model_version))
model_version_text

base_prefix <- NA
# Create the prefix of the model run #
if(base_model_ID == "ID1"){

  base_prefix <- "0100"
  # make a set of numbers 1-100 with leading zeros
  # 0 is base, 1-90 is ensemble 'comparison'
  prefix_numbers <- c(101:190)
  prefix_numbers <- formatC(prefix_numbers, digits = 3, flag = "0")
  prefix_numbers

}else if(base_model_ID == "ID2"){

  base_prefix <- "0200"
  # make a set of numbers 1-100 with leading zeros
  # 0 is base, 1-90 is ensemble 'comparison'
  prefix_numbers <- c(201:290)
  prefix_numbers <- formatC(prefix_numbers, digits = 3, flag = "0")
  prefix_numbers

}else if(base_model_ID == "ID3"){

  base_prefix <- "0300"
  # make a set of numbers 1-100 with leading zeros
  # 0 is base, 1-90 is ensemble 'comparison'
  prefix_numbers <- c(301:390)
  prefix_numbers <- formatC(prefix_numbers, digits = 3, flag = "0")
  prefix_numbers

}else if(base_model_ID == "ID5"){

  base_prefix <- "0500"
  # make a set of numbers 1-100 with leading zeros
  # 0 is base, 1-90 is ensemble 'comparison'
  prefix_numbers <- c(501:590)
  prefix_numbers <- formatC(prefix_numbers, digits = 3, flag = "0")
  prefix_numbers

}


# ensemble file prefixes
file_prefix <- c(paste0(prefix_numbers, ".", "DRRP_DroughtPlan_2020_", model_version_text, "_CC_", compare_model_ID, "_", compare_climate))
# file_prefix
n_file_prefix <- length(file_prefix)

# base file prefix
base_file_prefix <- c(paste0(base_prefix, ".", "DRRP_DroughtPlan_2020_", model_version_text, "_CC_", compare_model_ID, "_", compare_climate))



scenario_name <- c(paste0(compare_climate, "-", compare_model_ID, "_", prefix_numbers))
# scenario_name <- c(paste0(compare_climate, "-", compare_model_ID, "_", compare_model_ID_suffix),
#                    paste0(base_climate, "-", base_model_ID, "_", base_model_ID_suffix))
scenario_name
n_scenario_name <- length(scenario_name)

### no compact call file prefix
# for ensemble tracking
scenario_name_nocc <- paste0(compare_climate, "-", compare_model_ID, "_", prefix_numbers,"b")
scenario_name_nocc



# Import the compact call ensemble data --------------------------------------------------------


# create the data list to store the data
data_list <- list()

start_time <- Sys.time()
# rm(i)
# paste0(currwd_fix, "/", model_folder, "//", file_prefix[i], ".OutputSheet.csv")
# model_dirs$path[1]
# i = 1
# rm(i, data)
# currwd_fix <- gsub(paste0("/",model_folder , "/"), "", currwd)
# import the two scenarios to compare
#for (i in 1:n_file_prefix){
# i <- 1
# model_dirs$path[1]
# data <- read_csv(
#   # paste0(currwd_fix, "/", model_folder, "/", file_prefix[i], ".OutputSheet.csv"),
#   model_dirs$path[1],
#   col_names      = FALSE,
#   col_types      = readr::cols(.default="c"),
#   show_col_types = T
# )

# "G:/.shortcut-targets-by-id/1B8Jfl31Nww6VN-dRe1H6jov-ogkJhMBW/2022 Modeling/Model Runs/Compact Call Analysis/ID5/0501.DRRP_DroughtPlan_2020_055ac_CC_ID5_7525.OutputSheet.csv"
# "G:/.shortcut-targets-by-id/1B8Jfl31Nww6VN-dRe1H6jov-ogkJhMBW/2022 Modeling/Model Runs/Compact Call Analysis/ID5//0501.DRRP_DroughtPlan_2020_055ac_ID5_CC_7525.OutputSheet.csv"
# paste0(currwd_fix, "/", model_folder, "//", file_prefix[i], ".OutputSheet.csv")
# i
for (i in 1:temp_run_number){

  message(paste0(i, "/", temp_run_number))

  if(i == 1){

    # old read data method, this is slower
    # data <- read_csv(paste0(model_folder, "/", file_prefix[i], ".OutputSheet.csv"),
    #                  col_names = FALSE)

    # read in the quarter-monthly CRAM model data
    data <- fread(paste0(currwd_fix, "/", model_folder, "//", file_prefix[i], ".OutputSheet.csv"),
                  header = FALSE)
    # data <- read_csv(
    #   paste0(currwd_fix, "/", model_folder, "/", file_prefix[i], ".OutputSheet.csv"),
    #   col_names      = FALSE,
    #   col_types      = readr::cols(.default="c"),
    #   show_col_types = T
    #   )
    # get the column name & column descriptions
    # use these later to build definitions
    column_names <- data[5, ]
    column_parameter <- data[4, ]
    column_descriptions <- data[3, ]

    # name the columns using row 5
    temp_colnames <- as.character(data[5,])

    # rename the first 3 columns
    temp_colnames[1:3] <- c("year", "qm", "step")
    # set the column names
    colnames(data) <- temp_colnames
    # remove rows 1-5, extract all columns needed
    data2 <- data[6:dim(data)[1], 1:output_sheet_columns]
    # convert all columns from character to numeric
    data2 <- data2 %>% mutate(across(c(-qm, -step), as.numeric))


    # read in the quarter-monthly to date converter
    # qm_convert <- read_csv("qm_to_date_conversion2.csv")
    qm_convert <- readr::read_csv("D:/cob/compact_call/qm_to_date_conversion2.csv")
    #qm_convert


    # rename basic data components
    data_list[[i]] <- data2 %>%
      # make a year-qm column to attach qm-lookup
      mutate(wyqm = paste(year, qm, sep = '-')) %>%
      left_join(., qm_convert, by = "wyqm") %>%
      mutate(Date = mdy(Start.Date)) %>%
      mutate(ModelRun = scenario_name[i]) %>%
      mutate(ModelGroup = "compact call") %>%
      filter(year >= prefix_year_start[i] & year <= prefix_year_start[i+analysis_years])
    data_list[[i]]

    rm(data, data2)

  }else{

    # read in the quarter-monthly CRAM model data
    # data <- read_csv(paste0(model_folder, "/", file_prefix[i], ".OutputSheet.csv"),
    #                  col_names = FALSE, skip = 4)

    # read in the quarter-monthly CRAM model data
    data <- fread(paste0(currwd_fix, "/", model_folder, "//", file_prefix[i], ".OutputSheet.csv"),
                  header = FALSE, skip = 4)

    # name the columns using row 5 (now row 1 after skip =4)
    temp_colnames <- as.character(data[1,])
    # rename the first 3 columns
    temp_colnames[1:3] <- c("year", "qm", "step")
    # set the column names
    colnames(data) <- temp_colnames
    # remove rows 1, extract all columns needed
    data2 <- data[2:dim(data)[1], 1:output_sheet_columns]
    # convert all columns from character to numeric
    data2 <- data2 %>% mutate(across(c(-qm, -step), as.numeric))


    # rename basic data components
    data_list[[i]] <- data2 %>%
      # make a year-qm column to attach qm-lookup
      mutate(wyqm = paste(year, qm, sep = '-')) %>%
      left_join(., qm_convert, by = "wyqm") %>%
      mutate(Date = mdy(Start.Date)) %>%
      mutate(ModelRun = scenario_name[i]) %>%
      mutate(ModelGroup = "compact call") %>%
      filter(year >= prefix_year_start[i] & year <= prefix_year_start[i+analysis_years])
    data_list[[i]]

    rm(data, data2)
  }



}

end_time <- Sys.time()
end_time - start_time


# Import the base (no compact) data ---------------------------------------


# create the data list to store the data
data_list_nocc <- list()

start_time <- Sys.time()

# import the two scenarios to compare
#for (i in 1:n_file_prefix){
for (i in 1:temp_run_number){

  # read in the quarter-monthly CRAM model data
  data <- fread(paste0(model_folder, "/", base_file_prefix, ".OutputSheet.csv"),
                header = FALSE, skip = 4)

  # name the columns using row 5 (now row 1 after skip =4)
  temp_colnames <- as.character(data[1,])
  # rename the first 3 columns
  temp_colnames[1:3] <- c("year", "qm", "step")
  # set the column names
  colnames(data) <- temp_colnames
  # remove rows 1, extract all columns needed
  data2 <- data[2:dim(data)[1], 1:output_sheet_columns]
  # convert all columns from character to numeric
  data2 <- data2 %>% mutate(across(year:Link_560_Flow, as.numeric))


  # rename basic data components
  data_list_nocc[[i]] <- data2 %>%
    # make a year-qm column to attach qm-lookup
    mutate(wyqm = paste(year, qm, sep = '-')) %>%
    left_join(., qm_convert, by = "wyqm") %>%
    mutate(Date = mdy(Start.Date)) %>%
    mutate(ModelRun = scenario_name_nocc[i]) %>%
    mutate(ModelGroup = "no cmpt call") %>%
    filter(year >= prefix_year_start[i] & year <= prefix_year_start[i+analysis_years])
  data_list_nocc[[i]]

}

end_time <- Sys.time()
end_time - start_time


# build the definitions ---------------------------------------------------


definitions <- data.frame(Name = t(column_names), Description = t(column_descriptions),
                          Parameter = t(column_parameter))
# make a look up for parameter units
units.df <- data.frame(Parameter = c("Flow", "High", "Content", "Shortage", "Priority", "Evaporation", "Low"),
                       Units = c("Flow (af)", "Flow (af)", "Contents (af)", "Flow (cfs)", "Value",
                                 "Flow (af)", "Flow af)"))
# bind the units to the definitions table
definitions <- left_join(definitions, units.df, by = "Parameter")

# add new parameters here
new_data <- data.frame(Name        = c(
                                      "Total_Upper_Storage", "COB_Panama_qm_max_contents",
                                      "COB_Panama_qm_avg_contents", "COB_Panama_qm_min_contents",
                                      "COB_Wittemyer_qm_max_contents", "COB_Wittemyer_qm_avg_contents",
                                      "COB_Wittemyer_qm_min_contents",
                                      "Panama+Wittemyer_qm_max_contents",
                                      "Panama+Wittemyer_qm_avg_contents", "Panama+Wittemyer_qm_min_contents"
                                        ),
                       Description = c(
                                       "Barker+NBC Reservoirs", "Max COB Space in Panama Res",
                                       "Avg COB Space in Panama Res", "Min COB Space in Panama Res",
                                       "Max COB Space in Wittemyer Res",
                                       "Avg COB Space in Wittemyer Res", "Min COB Space in Wittemyer Res",
                                       "Max COB Panama+Wittemyer Res",
                                       "Avg COB Panama+Wittemyer Res", "Min COB Panama+Wittemyer Res"
                                       ),
                       Parameter = rep("Content", 10,),
                       Units     = rep("Contents (af)", 10)
                       # Parameter = c("Content", "Content", "Content", "Content",
                       #               "Content", "Content", "Content",
                       #               "Content", "Content", "Content"),
                       # Units = c("Contents (af)", "Contents (af)", "Contents (af)", "Contents (af)",
                       #           "Contents (af)", "Contents (af)", "Contents (af)",
                       #           "Contents (af)", "Contents (af)", "Contents (af)")
)

definitions <- rbind(definitions, new_data)




# Merge data, make factors and levels for compct call ensemble data -------------------------------------

# number of data_lists needs to equal # of iterations
temp_run_number
n_file_prefix

# merge the 'base and 'compare' datasets
data_list_merge <- rbind(data_list[[1]], data_list[[2]], data_list[[3]], data_list[[4]], data_list[[5]],
                         data_list[[6]], data_list[[7]], data_list[[8]], data_list[[9]], data_list[[10]],
                         data_list[[11]], data_list[[12]], data_list[[13]], data_list[[14]], data_list[[15]],
                         data_list[[16]], data_list[[17]], data_list[[18]], data_list[[19]], data_list[[20]],
                         data_list[[21]], data_list[[22]], data_list[[23]], data_list[[24]], data_list[[25]],
                         data_list[[26]], data_list[[27]], data_list[[28]], data_list[[29]], data_list[[30]],
                         data_list[[31]], data_list[[32]], data_list[[33]], data_list[[34]], data_list[[35]],
                         data_list[[36]], data_list[[37]], data_list[[38]], data_list[[39]], data_list[[40]],
                         data_list[[41]], data_list[[42]], data_list[[43]], data_list[[44]], data_list[[45]],
                         data_list[[46]], data_list[[47]], data_list[[48]], data_list[[49]], data_list[[50]],
                         data_list[[51]], data_list[[52]], data_list[[53]], data_list[[54]], data_list[[55]],
                         data_list[[56]], data_list[[57]], data_list[[58]], data_list[[59]], data_list[[60]],
                         data_list[[61]], data_list[[62]], data_list[[63]], data_list[[64]], data_list[[65]],
                         data_list[[66]], data_list[[67]], data_list[[68]], data_list[[69]], data_list[[70]],
                         data_list[[71]], data_list[[72]], data_list[[73]], data_list[[74]], data_list[[75]],
                         data_list[[76]], data_list[[77]], data_list[[78]], data_list[[79]], data_list[[80]],
                         data_list[[81]], data_list[[82]], data_list[[83]], data_list[[84]], data_list[[85]],
                         data_list[[86]], data_list[[87]], data_list[[88]], data_list[[89]]
)




# data_list_merge <- rbind(data_list[[1]], data_list[[2]], data_list[[3]], data_list[[4]], data_list[[5]],
#                          data_list[[6]], data_list[[7]], data_list[[8]]
#                          )

# data_list_merge <- rbind(data_list[[1]], data_list[[2]], data_list[[3]], data_list[[4]], data_list[[5]],
#                          data_list[[6]], data_list[[7]], data_list[[8]], data_list[[9]], data_list[[10]],
#                          data_list[[11]], data_list[[12]], data_list[[13]], data_list[[14]], data_list[[15]],
#                          data_list[[16]], data_list[[17]], data_list[[18]], data_list[[19]], data_list[[20]],
#                          data_list[[21]], data_list[[22]], data_list[[23]], data_list[[24]], data_list[[25]],
#                          data_list[[26]], data_list[[27]], data_list[[28]], data_list[[29]], data_list[[30]]
#                          )

# data_list_merge <- rbind(data_list[[1]], data_list[[2]], data_list[[3]], data_list[[4]], data_list[[5]],
#                          data_list[[6]], data_list[[7]], data_list[[8]], data_list[[9]], data_list[[10]],
#                          data_list[[11]], data_list[[12]], data_list[[13]], data_list[[14]], data_list[[15]],
#                          data_list[[16]], data_list[[17]], data_list[[18]], data_list[[19]], data_list[[20]]
# )

saveRDS(data_list_merge, file = paste0("merge_cc_", compare_climate, "-", compare_model_ID, ".rds"))
saveRDS(data_list, file = paste0("list_cc_", compare_climate, "-", compare_model_ID, ".rds"))




# merge no cc data together -----------------------------------------------

# merge the 'base and 'compare' datasets
data_list_nocc_merge <- rbind(data_list_nocc[[1]], data_list_nocc[[2]], data_list_nocc[[3]], data_list_nocc[[4]], data_list_nocc[[5]],
                              data_list_nocc[[6]], data_list_nocc[[7]], data_list_nocc[[8]], data_list_nocc[[9]], data_list_nocc[[10]],
                              data_list_nocc[[11]], data_list_nocc[[12]], data_list_nocc[[13]], data_list_nocc[[14]], data_list_nocc[[15]],
                              data_list_nocc[[16]], data_list_nocc[[17]], data_list_nocc[[18]], data_list_nocc[[19]], data_list_nocc[[20]],
                              data_list_nocc[[21]], data_list_nocc[[22]], data_list_nocc[[23]], data_list_nocc[[24]], data_list_nocc[[25]],
                              data_list_nocc[[26]], data_list_nocc[[27]], data_list_nocc[[28]], data_list_nocc[[29]], data_list_nocc[[30]],
                              data_list_nocc[[31]], data_list_nocc[[32]], data_list_nocc[[33]], data_list_nocc[[34]], data_list_nocc[[35]],
                              data_list_nocc[[36]], data_list_nocc[[37]], data_list_nocc[[38]], data_list_nocc[[39]], data_list_nocc[[40]],
                              data_list_nocc[[41]], data_list_nocc[[42]], data_list_nocc[[43]], data_list_nocc[[44]], data_list_nocc[[45]],
                              data_list_nocc[[46]], data_list_nocc[[47]], data_list_nocc[[48]], data_list_nocc[[49]], data_list_nocc[[50]],
                              data_list_nocc[[51]], data_list_nocc[[52]], data_list_nocc[[53]], data_list_nocc[[54]], data_list_nocc[[55]],
                              data_list_nocc[[56]], data_list_nocc[[57]], data_list_nocc[[58]], data_list_nocc[[59]], data_list_nocc[[60]],
                              data_list_nocc[[61]], data_list_nocc[[62]], data_list_nocc[[63]], data_list_nocc[[64]], data_list_nocc[[65]],
                              data_list_nocc[[66]], data_list_nocc[[67]], data_list_nocc[[68]], data_list_nocc[[69]], data_list_nocc[[70]],
                              data_list_nocc[[71]], data_list_nocc[[72]], data_list_nocc[[73]], data_list_nocc[[74]], data_list_nocc[[75]],
                              data_list_nocc[[76]], data_list_nocc[[77]], data_list_nocc[[78]], data_list_nocc[[79]], data_list_nocc[[80]],
                              data_list_nocc[[81]], data_list_nocc[[82]], data_list_nocc[[83]], data_list_nocc[[84]], data_list_nocc[[85]],
                              data_list_nocc[[86]], data_list_nocc[[87]], data_list_nocc[[88]], data_list_nocc[[89]]
)

# data_list_nocc_merge <- rbind(data_list_nocc[[1]], data_list_nocc[[2]], data_list_nocc[[3]], data_list_nocc[[4]], data_list_nocc[[5]],
#                               data_list_nocc[[6]], data_list_nocc[[7]], data_list_nocc[[8]]
#                               )

# data_list_nocc_merge <- rbind(data_list_nocc[[1]], data_list_nocc[[2]], data_list_nocc[[3]], data_list_nocc[[4]], data_list_nocc[[5]],
#                          data_list_nocc[[6]], data_list_nocc[[7]], data_list_nocc[[8]], data_list_nocc[[9]], data_list_nocc[[10]],
#                          data_list_nocc[[11]], data_list_nocc[[12]], data_list_nocc[[13]], data_list_nocc[[14]], data_list_nocc[[15]],
#                          data_list_nocc[[16]], data_list_nocc[[17]], data_list_nocc[[18]], data_list_nocc[[19]], data_list_nocc[[20]],
#                          data_list_nocc[[21]], data_list_nocc[[22]], data_list_nocc[[23]], data_list_nocc[[24]], data_list_nocc[[25]],
#                          data_list_nocc[[26]], data_list_nocc[[27]], data_list_nocc[[28]], data_list_nocc[[29]], data_list_nocc[[30]]
#                          )

# data_list_nocc_merge <- rbind(data_list_nocc[[1]], data_list_nocc[[2]], data_list_nocc[[3]], data_list_nocc[[4]], data_list_nocc[[5]],
#                               data_list_nocc[[6]], data_list_nocc[[7]], data_list_nocc[[8]], data_list_nocc[[9]], data_list_nocc[[10]],
#                               data_list_nocc[[11]], data_list_nocc[[12]], data_list_nocc[[13]], data_list_nocc[[14]], data_list_nocc[[15]],
#                               data_list_nocc[[16]], data_list_nocc[[17]], data_list_nocc[[18]], data_list_nocc[[19]], data_list_nocc[[20]]
# )

saveRDS(data_list_nocc_merge, file = paste0("merge_nocc_", compare_climate, "-", compare_model_ID, ".rds"))
saveRDS(data_list_nocc, file = paste0("list_nocc_", compare_climate, "-", compare_model_ID, ".rds"))


# adjust the factors ------------------------------------------------------

### Merge the compact call & no compact call datasets
data_list_merge_final <- rbind(data_list_merge, data_list_nocc_merge)


### Check the factor & levels for the 'ModelRun' column (we will plot by this)
# This is likely unique to R and how things plot using ggplot & data frames
factor(data_list_merge_final$ModelRun)
levels(data_list_merge_final$ModelRun)
# set the factor 'levels' to the correct plotting order
data_list_merge_final$ModelRun <-factor(data_list_merge_final$ModelRun,
                                        levels = c(scenario_name_nocc[1:temp_run_number],
                                                   scenario_name[1:temp_run_number]))

# Levels should be updated now
levels(data_list_merge_final$ModelRun)


factor(data_list_merge_final$ModelGroup)
levels(data_list_merge_final$ModelGroup)


# Delete old objects ------------------------------------------------------


rm(data, qm_convert, temp_colnames,
   column_names, column_descriptions, column_parameter,
   units.df, new_data,
   data_list, data_list_nocc, data2, data_list_merge, data_list_nocc_merge)


# convert output from character to numeric
#extract_data$Output <- as.numeric(extract_data$Output)



# May 1 Res Contents (Plot 1a) ------------------------------------------------

# Reservoir_12_Content = Boulder Reservor (total)
# DataObject_12_Flow = Drought Response Level
# "DataObject_1_Flow" = COB's share of Boulder Res Storage

# select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "ModelGroup", "Reservoir_3_Content",
                    "Reservoir_1_Content")
n_site_selection <- length(site_selection)
site_start_no <- 6
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)
# add calculated column name
site_selection_short <- c(site_selection_short, "Total_Upper_Storage")
n_site_selection_short <- length(site_selection_short)



### extract the data selected above, put in ggplot format
extract_data <- data_list_merge_final %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(all_of(site_selection)) %>%
  mutate(Total_Upper_Storage = rowSums(across(c(Reservoir_3_Content, Reservoir_1_Content)))) %>%
  #mutate(DataObject_15_Flow = DataObject_15_Flow/100) %>%
  #select(!!!syms(site_selection)) %>%
  # filter for May 1 reservoir contents
  filter(qm == 29) %>%
  group_by(year, ModelRun) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(site_selection_short),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name")



### plot the output
p <- list()
for (i in 1:n_site_selection_short){

  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == site_selection_short[i])

  # extract_plot_nocc <- extract_data %>%
  #   filter(Name == site_selection_short[i] & ModelGroup == "no_cc") %>%
  #   ungroup() %>%
  #   summarise(Output = min(Output))

  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  p[[i]] <- ggplot(extract_plot, aes_string(x = "Output", color = "ModelGroup", shape = "ModelGroup")) +
    stat_ecdf(geom = "point") +
    scale_color_brewer(palette="Dark2") +
    #geom_vline(xintercept = extract_plot_nocc$Output, linetype="solid", color="black") +
    theme_bw() +
    xlab(extract_plot$Units[i]) +
    scale_y_continuous(name = "Percent", breaks=seq(0, 1, by=0.2), limits=c(0,1)) +
    ggtitle(extract_plot$Description[i]) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  p[[i]]

}


### plot the output
g <- list()
for (i in 1:n_site_selection_short){

  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == site_selection_short[i])

  # extract_plot_nocc <- extract_data %>%
  #   filter(Name == site_selection_short[i] & ModelGroup == "no_cc") %>%
  #   ungroup() %>%
  #   summarise(Output = min(Output))

  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  g[[i]] <- ggplot(extract_plot, aes_string(x = "Output", color = "ModelGroup", linetype = "ModelGroup")) +
    stat_ecdf(geom = "step") +
    scale_color_brewer(palette="Dark2") +
    #geom_vline(xintercept = extract_plot_nocc$Output, linetype="solid", color="black") +
    theme_bw() +
    xlab(extract_plot$Units[i]) +
    scale_y_continuous(name = "Percent", breaks=seq(0, 1, by=0.2), limits=c(0,1)) +
    ggtitle(extract_plot$Description[i]) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  #g[[5]]

}


# define the plot name
plot_title <- paste0("1a. May 1 Reservoir Contents (point)", " (", print_scenario_name, ")")
file_name <- paste(plot_title,  " 2x2 ", sep = "")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, " ", model_version, ".pdf"),
  width = 14, height = 8,
  grid.arrange(p[[1]], p[[2]], p[[3]], nrow = 2,
               top = plot_title,
               right = ""))


# define the plot name
plot_title <- paste0("1a. May 1 Reservoir Contents (step)", " (", print_scenario_name, ")")
file_name <- paste(plot_title, " 2x2 ", sep = "")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, " ", model_version, ".pdf"),
  width = 14, height = 8,
  grid.arrange(g[[1]], g[[2]], g[[3]], nrow = 2,
               top = plot_title,
               right = ""))


# remove data no longer needed
rm(g, p, extract_data, extract_plot)


# May 1 Drought Metrics Plot (1d) & Drought Triggers Table (1c) --------------------------------------------------


# select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "ModelGroup","DataObject_15_Flow",
                    "DataObject_12_Flow", "DataObject_14_Flow", "DataObject_39_Flow")
n_site_selection <- length(site_selection)
site_start_no <- 6
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)
# add calculated column name
# site_selection_short <- c(site_selection_short, "Total_Upper_Storage")
# n_site_selection_short <- length(site_selection_short)



### extract the data selected above, put in ggplot format
extract_data <- data_list_merge_final %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(all_of(site_selection)) %>%
  #mutate(Total_Upper_Storage = rowSums(across(c(Reservoir_3_Content, Reservoir_1_Content)))) %>%
  mutate(DataObject_15_Flow = DataObject_15_Flow/100) %>%
  #select(!!!syms(site_selection)) %>%
  # filter for May 1 reservoir contents
  filter(qm == 29) %>%
  group_by(year, ModelRun) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(site_selection_short),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name")


title_temp <- c("Projected Storage Index (PSI)", "Drought Response Level", "C-BT Quota", "blank")
xaxis_title_temp <- c("PSI Value", "Drought Response Level", "Quota (%)", "blank")
xaxis_min <- c(0, 0, 0, 0)
xaxis_max <- c(2.5, 4, 100, 100)
xaxis_break <- c(0.5, 1, 25, 25)


### plot the output
g <- list()
for (i in 1:n_site_selection_short){

  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == site_selection_short[i])

  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  g[[i]] <- ggplot(extract_plot, aes_string(x = "Output", color = "ModelGroup", linetype = "ModelGroup")) +
    stat_ecdf(geom = "step") +
    scale_color_brewer(palette="Dark2") +
    #geom_vline(xintercept = extract_plot_nocc$Output, linetype="solid", color="black") +
    theme_bw() +
    xlab(extract_plot$Units[i]) +
    scale_y_continuous(name = "Percent", breaks=seq(0, 1, by = 0.2), limits=c(0, 1)) +
    scale_x_continuous(name = xaxis_title_temp[i], breaks=seq(xaxis_min[i], xaxis_max[i],
                                                              by = xaxis_break[i]),
                       limits=c(xaxis_min[i], xaxis_max[i])) +
    ggtitle(title_temp[i]) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))

  # if(i == 2){
  #
  #
  # }else {
  #   # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  #   g[[i]] <- ggplot(extract_plot, aes_string(x = "Output", color = "ModelGroup", linetype = "ModelGroup")) +
  #     stat_ecdf(geom = "step") +
  #     scale_color_brewer(palette="Dark2") +
  #     #geom_vline(xintercept = extract_plot_nocc$Output, linetype="solid", color="black") +
  #     theme_bw() +
  #     xlab(extract_plot$Units[i]) +
  #     scale_y_continuous(name = "Percent", breaks=seq(0, 1, by=0.2), limits=c(0,1)) +
  #     ggtitle(extract_plot$Description[i]) +
  #     theme(plot.title = element_text(size = title_size),
  #           axis.title = element_text(size = xaxis_size))
  # }
  #
  #g[[5]]

}


# define the plot name
plot_title <- paste0("1d. May 1 Drought Metrics", " (", print_scenario_name, ")")
file_name <- paste(plot_title, " 2x2 ", sep = "")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, " ", model_version, ".pdf"),
  width = 14, height = 8,
  grid.arrange(g[[1]], g[[2]], g[[3]], nrow = 2,
               top = plot_title,
               right = ""))



### Extract a subset of data - Drought Triggers Only (Level 0, 1, 2, 3, 4)
### Make a table using
extract_table <- extract_data %>%
  filter(Name == "DataObject_12_Flow") %>%
  group_by(ModelGroup) %>%
  count(Output) %>%
  pivot_wider(., names_from = ModelGroup, values_from = n) %>%
  rename("Drought Response Level" = Output) %>%
  gt() %>%
  tab_header(
    title = md("Comparison of Drought Response Triggers"),
    subtitle = md(output_folder)
  )
extract_table

# extract a second table that summarizes data with no drought triggers (output = 0)
extract_table2 <- extract_data %>%
  filter(Name == "DataObject_12_Flow") %>%
  filter(Output == 0) %>%
  ungroup() %>%
  group_by(ModelGroup) %>%
  count(Output)
extract_table2
# save compact call & no compact call data
cc_temp_1 <- filter(extract_table2, ModelGroup == "compact call")$n
nocc_temp_1 <- filter(extract_table2, ModelGroup == "no cmpt call")$n

temp1 <- tibble(Count = c("Total Simulation", "No Drought Trigger", "No Drought Trigger (%)"),
                'compact call' = c(total_simulation_count, cc_temp_1,
                                   round(cc_temp_1/total_simulation_count*100,1)),
                'no compact call' = c(total_simulation_count, nocc_temp_1,
                                      round(nocc_temp_1/total_simulation_count*100,1)))
temp1_table <- temp1 %>%
  gt() %>%
  tab_header(
    title = md("No Drought Trigger Summary"),
    subtitle = md(output_folder))
temp1_table



# define the table export name & save as pdf
plot_title <- "1c. May 1 Drought Triggers"
file_name <- paste(plot_title, " (", print_scenario_name, ")", sep = "")

gtsave(extract_table,
       file = paste0(output_folder, "/", file_name, " ", model_version, ".pdf"),
       zoom = 1
)


# define the table export name & save as pdf
plot_title <- "1c2. May 1 Drought Triggers Summary"
file_name <- paste(plot_title, " (", print_scenario_name, ")", sep = "")

gtsave(temp1_table,
       file = paste0(output_folder, "/", file_name, " ", model_version, ".pdf"),
       zoom = 1
)


# remove extracted data
rm(g, extract_data, extract_plot, extract_table)
rm(extract_table2, cc_temp_1, nocc_temp_1, temp1, temp1_table)


# PSI Diagnostics (plot 6) ---------------------------------------------------------

# # select the sites you want to plot
# site_selection <- c("year", "qm", "Date", "ModelRun", "ModelGroup","DataObject_15_Flow",
#                     "DataObject_12_Flow", "DataObject_14_Flow", "DataObject_39_Flow")
# n_site_selection <- length(site_selection)
# site_start_no <- 6
# site_selection_short <- site_selection[site_start_no:n_site_selection]
# n_site_selection_short <- length(site_selection_short)
# # add calculated column name
# # site_selection_short <- c(site_selection_short, "Total_Upper_Storage")
# # n_site_selection_short <- length(site_selection_short)
#
#
#
# ### extract the data selected above, put in ggplot format
# extract_data <- data_list_merge_final %>%
#   # select the columns of interest from the vector above using !!!syms to read it properly
#   select(all_of(site_selection)) %>%
#   #mutate(Total_Upper_Storage = rowSums(across(c(Reservoir_3_Content, Reservoir_1_Content)))) %>%
#   mutate(DataObject_15_Flow = DataObject_15_Flow/100) %>%
#   #select(!!!syms(site_selection)) %>%
#   # filter for May 1 reservoir contents
#   filter(qm == 29) %>%
#   group_by(year, ModelRun) %>%
#   # convert from 'wide' to 'long' format for plotting w/ ggplot
#   pivot_longer(., cols = all_of(site_selection_short),
#                names_to = "Name", values_to = "Output") %>%
#   #join the CRAM model descriptions to this dataset
#   left_join(., definitions, by = "Name")
#
#
# ### Make a diagnostic plot of data
# temp1 = tibble(count_fill= rep(c(1:(temp_run_number*(analysis_years+1))),2))
# extract_plot3 <- extract_data %>%
#   filter(Name == "DataObject_15_Flow")
# extract_plot3 <- cbind(extract_plot3, temp1)
#
#
#
# # plot the data
# h <- ggplot(extract_plot3, aes_string(y = "Output", x = "count_fill",
#                                       color = "ModelGroup", linetype = "ModelGroup")) +
#   geom_line() +
#   scale_color_brewer(palette="Dark2") +
#   theme_bw() +
#   ylab("PSI/DRI") +
#   xlab(paste0("Year Sequence (1-5), ", temp_run_number, " Simulations")) +
#   ggtitle("PSI/DRI Annual Values")
#
#
# # define the plot name
# plot_title <- paste0("6. PSI Diagnostics", " (", print_scenario_name, ")")
# file_name <- paste0(plot_title,  " 1x1")
#
# # save the plot
# ggsave(
#   paste0(output_folder, "/", file_name, " ", model_version, ".pdf"),
#   width = 14, height = 8, device = "pdf")
#
#
# rm(temp1, extract_plot3, h)


# June 1 Res Contents (1b) -----------------------------------------------------


# select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "ModelGroup", "Reservoir_3_Content",
                    "Reservoir_1_Content")
n_site_selection <- length(site_selection)
site_start_no <- 6
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)
# add calculated column name
site_selection_short <- c(site_selection_short, "Total_Upper_Storage")
n_site_selection_short <- length(site_selection_short)



### extract the data selected above, put in ggplot format
extract_data <- data_list_merge_final %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(all_of(site_selection)) %>%
  mutate(Total_Upper_Storage = rowSums(across(c(Reservoir_3_Content, Reservoir_1_Content)))) %>%
  # mutate(DataObject_15_Flow = DataObject_15_Flow/100) %>%
  #select(!!!syms(site_selection)) %>%
  # filter for May 1 reservoir contents
  filter(qm == 33) %>%
  group_by(year, ModelRun) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(site_selection_short),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name")




### plot the output
g <- list()
for (i in 1:n_site_selection_short){

  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == site_selection_short[i])

  # extract_plot_nocc <- extract_data %>%
  #   filter(Name == site_selection_short[i] & ModelGroup == "no_cc") %>%
  #   ungroup() %>%
  #   summarise(Output = min(Output))

  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  g[[i]] <- ggplot(extract_plot, aes_string(x = "Output", color = "ModelGroup", linetype = "ModelGroup")) +
    stat_ecdf(geom = "step") +
    scale_color_brewer(palette="Dark2") +
    #geom_vline(xintercept = extract_plot_nocc$Output, linetype="solid", color="black") +
    theme_bw() +
    xlab(extract_plot$Units[i]) +
    scale_y_continuous(name = "Percent", breaks=seq(0, 1, by=0.2), limits=c(0,1)) +
    ggtitle(extract_plot$Description[i]) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  #p[[i]]

}


# define the plot name
plot_title <- paste0("1b. June 1 Reservoir Contents", " (", print_scenario_name, ")")
file_name <- paste(plot_title, " 2x2 ", sep = "")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, " ", model_version, ".pdf"),
  width = 14, height = 8,
  grid.arrange(g[[1]], g[[2]], g[[3]], nrow = 2,
               top = plot_title,
               right = ""))






# Annual City Demand and Shortage (1e) -----------------------------------------


# select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "ModelGroup","DataObject_23_Flow",
                    "DataObject_19_Flow", "DataObject_20_Flow")
n_site_selection <- length(site_selection)
site_start_no <- 6
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)
# add calculated column name
# site_selection_short <- c(site_selection_short, "Total_Upper_Storage")
# n_site_selection_short <- length(site_selection_short)



### extract the data selected above, put in ggplot format
extract_data <- data_list_merge_final %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(all_of(site_selection)) %>%
  #mutate(Total_Upper_Storage = rowSums(across(c(Reservoir_3_Content, Reservoir_1_Content)))) %>%
  #mutate(DataObject_15_Flow = DataObject_15_Flow/100) %>%
  #select(!!!syms(site_selection)) %>%
  # filter for May 1 reservoir contents
  #filter(qm == 29) %>%
  group_by(year, ModelRun, ModelGroup) %>%
  summarise(across(DataObject_23_Flow:DataObject_20_Flow, sum) )%>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(site_selection_short),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name")



### plot the output
g <- list()
for (i in 1:n_site_selection_short){

  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == site_selection_short[i])

  # extract_plot_nocc <- extract_data %>%
  #   filter(Name == site_selection_short[i] & ModelGroup == "no_cc") %>%
  #   ungroup() %>%
  #   summarise(Output = min(Output))

  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  g[[i]] <- ggplot(extract_plot, aes_string(x = "Output", color = "ModelGroup", linetype = "ModelGroup")) +
    stat_ecdf(geom = "step") +
    scale_color_brewer(palette="Dark2") +
    #geom_vline(xintercept = extract_plot_nocc$Output, linetype="solid", color="black") +
    theme_bw() +
    xlab(extract_plot$Units[i]) +
    scale_y_continuous(name = "Percent", breaks=seq(0, 1, by=0.2), limits=c(0,1)) +
    ggtitle(extract_plot$Description[i]) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  #g[[5]]

}



# define the plot name
plot_title <- paste0("1e. Annual City Demand and Shortage", " (", print_scenario_name, ")")
file_name <- paste(plot_title, " 2x2 ", sep = "")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, " ", model_version, ".pdf"),
  width = 14, height = 8,
  grid.arrange(g[[1]], g[[2]], g[[3]], nrow = 2,
               top = plot_title,
               right = ""))



# Panama Reservoir (Plot 2a) --------------------------------------------------------


### select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "ModelGroup", "Reservoir_25_Content")
n_site_selection <- length(site_selection)
site_start_no <- 6
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)

# names to call reservoir in plots
names_temp <- c("COB_Panama_qm_max_contents", "COB_Panama_qm_avg_contents", "COB_Panama_qm_min_contents")
n_names_temp <- length(names_temp)


### extract the data selected above, put in ggplot format
extract_data <- data_list_merge_final %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(all_of(site_selection)) %>%
  #select(!!!syms(site_selection)) %>%
  # filter for May 1 reservoir contents
  group_by(year, ModelRun, ModelGroup) %>%
  #summarise(across(Reservoir_25_Content, min)) %>%
  summarize(COB_Panama_qm_max_contents = max(Reservoir_25_Content),
            COB_Panama_qm_avg_contents = mean(Reservoir_25_Content),
            COB_Panama_qm_min_contents = min(Reservoir_25_Content)) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(names_temp),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name")



### plot the output
g <- list()
for (i in 1:n_names_temp){

  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == names_temp[i])

  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  g[[i]] <- ggplot(extract_plot, aes_string(x = "Output", color = "ModelGroup", linetype = "ModelGroup")) +
    stat_ecdf(geom = "step") +
    scale_color_brewer(palette="Dark2") +
    theme_bw() +
    xlab(extract_plot$Units[i]) +
    #xlab("Water Year") +
    scale_y_continuous(name = "Percent", breaks=seq(0, 1, by=0.2), limits=c(0,1)) +
    xlim(0, 5000) +
    ggtitle(extract_plot$Description[i]) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  #g[[i]]

}



# define the plot name
plot_title <- paste0("2a. Panama Reservoir Contents", " (", print_scenario_name, ")")
file_name <- paste(plot_title, " 2x2 ", sep = "")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, " ", model_version, ".pdf", sep = ""),
  width = 14, height = 8,
  grid.arrange(g[[1]], g[[2]], g[[3]], nrow = 2,
               top = plot_title,
               right = ""))


# Wittemyer Res Contents (Plot 2b) ---------------------------------------------------------------


### select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "ModelGroup", "Reservoir_13_Content")
n_site_selection <- length(site_selection)
site_start_no <- 6
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)

# names to call reservoir in plots
names_temp <- c("COB_Wittemyer_qm_max_contents", "COB_Wittemyer_qm_avg_contents",
                "COB_Wittemyer_qm_min_contents")
n_names_temp <- length(names_temp)


### extract the data selected above, put in ggplot format
extract_data <- data_list_merge_final %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(all_of(site_selection)) %>%
  #select(!!!syms(site_selection)) %>%
  # filter for May 1 reservoir contents
  group_by(year, ModelRun, ModelGroup) %>%
  #summarise(across(Reservoir_25_Content, min)) %>%
  summarize(COB_Wittemyer_qm_max_contents = max(Reservoir_13_Content),
            COB_Wittemyer_qm_avg_contents = mean(Reservoir_13_Content),
            COB_Wittemyer_qm_min_contents = min(Reservoir_13_Content)) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(names_temp),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name")


### plot the output
g <- list()
for (i in 1:n_names_temp){

  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == names_temp[i])

  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  g[[i]] <- ggplot(extract_plot, aes_string(x = "Output", color = "ModelGroup", linetype = "ModelGroup")) +
    stat_ecdf(geom = "step") +
    scale_color_brewer(palette="Dark2") +
    theme_bw() +
    xlab(extract_plot$Units[i]) +
    xlim(0, 2000) +
    scale_y_continuous(name = "Percent", breaks=seq(0, 1, by=0.2), limits=c(0,1)) +
    #ylim(0, x_axis_max_list[i]) +
    ggtitle(extract_plot$Description[i]) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  #g[[i]]

}


# define the plot name
plot_title <- paste0("2b. Wittemyer Reservoir Contents", " (", print_scenario_name, ")")
file_name <- paste(plot_title, " 2x2 ", sep = "")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, " ", model_version, ".pdf", sep = ""),
  width = 14, height = 8,
  grid.arrange(g[[1]], g[[2]], g[[3]], nrow = 2,
               top = plot_title,
               right = ""))



# Wittemyer + Panama Contents (Plot 2c) ---------------------------------------------

# "Reservoir_25_Content" = Panama
# "Reservoir_13_Content" = Wittemyer

### select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "ModelGroup", "Reservoir_13_Content",
                    "Reservoir_25_Content")
n_site_selection <- length(site_selection)
site_start_no <- 6
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)

# names to call reservoir in plots
names_temp <- c("Panama+Wittemyer_qm_max_contents",
                "Panama+Wittemyer_qm_avg_contents", "Panama+Wittemyer_qm_min_contents")
n_names_temp <- length(names_temp)


### extract the data selected above, put in ggplot format
extract_data <- data_list_merge_final %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(all_of(site_selection)) %>%
  mutate(Wittemyer_Panama_Storage = rowSums(across(c(Reservoir_13_Content, Reservoir_25_Content)))) %>%
  #select(!!!syms(site_selection)) %>%
  # filter for May 1 reservoir contents
  group_by(year, ModelRun, ModelGroup) %>%
  #summarise(across(Reservoir_25_Content, min)) %>%
  summarize("Panama+Wittemyer_qm_max_contents" = max(Wittemyer_Panama_Storage),
            "Panama+Wittemyer_qm_avg_contents" = mean(Wittemyer_Panama_Storage),
            "Panama+Wittemyer_qm_min_contents" = min(Wittemyer_Panama_Storage)) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(names_temp),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name")


### plot the output
g <- list()
for (i in 1:n_names_temp){

  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == names_temp[i])

  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  g[[i]] <- ggplot(extract_plot, aes_string(x = "Output", color = "ModelGroup", linetype = "ModelGroup")) +
    stat_ecdf(geom = "step") +
    scale_color_brewer(palette="Dark2") +
    theme_bw() +
    xlab(extract_plot$Units[i]) +
    xlim(0, 7000) +
    scale_y_continuous(name = "Percent", breaks=seq(0, 1, by = 0.2), limits=c(0, 1)) +
    #ylim(0, x_axis_max_list[i]) +
    ggtitle(extract_plot$Description[i]) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  #g[[i]]

}


# define the plot name
plot_title <- paste0("2c. Panama + Wittemyer Reservoir Contents", " (", print_scenario_name, ")")
file_name <- paste(plot_title, " 2x2 ", sep = "")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, " ", model_version, ".pdf", sep = ""),
  width = 14, height = 8,
  grid.arrange(g[[1]], g[[2]], g[[3]], nrow = 2,
               top = plot_title,
               right = ""))





