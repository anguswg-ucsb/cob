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
source("R/process_annual_summary.R")
source("R/parse_directory.R")
source("R/utils.R")

base_folder <- "D:/cob/latest/latest"

# info on model files
model_dirs  <- parse_directory(base_folder = base_folder)

# set plot parameters
title_size = 10
xaxis_size = 9

# model_folder <- "latest"
model_folder <- "D:/cob/latest/latest"

device_type <- ".png"     # .png, .pdf

# read in the quarter-monthly to date converter
qm_convert <- readr::read_csv("data-raw/qm_to_date_conversion.csv")

# path to ISF data
isf_path <-
  model_dirs %>%
  dplyr::filter(grepl("ISF", file)) %>%
  .$path
isf_path

# path to Quota data
quota_path <-
  model_dirs %>%
  dplyr::filter(grepl("Quota", file)) %>%
  .$path
quota_path

# tmp2 <-
base_mods <-
  model_dirs %>%
  dplyr::filter(
    model_id %in% c("ID1"),
    model_version %in% c("055c"),
    extra_info %in% c("3143"),
    # grepl("055c_3143_ID1_7525", file),
    output == "OutputSheet"
  ) %>%
  dplyr::group_by(model_version, model_id, model_num) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::filter(
    name != "Quota",
    model_num == "7525"
    ) %>%
  dplyr::mutate(
    prefix = substr(file, 1, 4)
    # prefix = substr(file, 28, 31)
  ) %>%
  dplyr::select(prefix, model_version, model_id, model_num, extra_info, path)

comp_mods <-
  model_dirs %>%
  dplyr::filter(
    model_id %in% c("ID1"),
    model_version %in% c("055c"),
    extra_info %in% c("8500"),
    output == "OutputSheet"
  ) %>%
  dplyr::group_by(model_version, model_id, model_num) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    prefix = substr(file, 1, 4)
  ) %>%
  dplyr::select(prefix, model_version, model_id, model_num, extra_info, path)

  # base model annual summary
  base_summary <-
    model_dirs %>%
    dplyr::filter(
      model_id %in% c("ID1"),
      model_version %in% c("055c"),
      extra_info %in% c("3143"),
      output == "OutputAnnualSummary"
    )

  # comparison model annual summary
  comp_summary <-
    model_dirs %>%
    dplyr::filter(
      model_id %in% c("ID1"),
      model_version %in% c("055c"),
      extra_info %in% c("8500"),
      output == "OutputAnnualSummary"
    )

  # Model versions
  base_model_version        <-  paste0("v", base_mods$model_version)
  compare_model_version     <-  paste0("v", comp_mods$model_version)

  # version text strings
  base_model_version_text    <- base_mods$model_version
  compare_model_version_text <- comp_mods$model_version

  # model IDs
  base_model_ID        <- base_mods$model_id
  compare_model_ID     <- comp_mods$model_id

  # model ID suffix
  base_model_ID_suffix    <- ""        # if this is not needed, keep blank ""
  compare_model_ID_suffix <- ""        # if this is not needed, keep blank ""

  # climate scenerios
  base_climate         <- base_mods$model_num
  compare_climate      <- comp_mods$model_num

  # model prefixes
  base_model_ID_prefix    <- base_mods$prefix
  compare_model_ID_prefix <- comp_mods$prefix

  # Extra info text that would be found between the model version and the model ID in the file name
  base_model_extra_info    <- base_mods$extra_info
  compare_model_extra_info <- comp_mods$extra_info

  message(paste0("Base model ver: ", base_model_version,
                 "\nBase model ID: ", base_model_ID,
                 "\nBase model ID suffix: ", base_model_ID_suffix,
                 "\nBase model climate: ", base_climate,
                 "\nBase model ID prefix: ", base_model_ID_prefix
  )
  )
  message(paste0("Comp model ver: ", compare_model_version,
                 "\nComp model ID: ", compare_model_ID,
                 "\nComp model ID suffix: ", compare_model_ID_suffix,
                 "\nComp model climate: ", compare_climate,
                 "\nComp model ID prefix: ", compare_model_ID_prefix
  )
  )


  output_folder <- paste0(
    base_model_ID, "-", base_climate, "-",  gsub("v", "",base_model_version), "-", base_model_ID_prefix,
    ifelse(base_model_extra_info != "", paste0("-", base_model_extra_info), ""),
    " vs. ",
    compare_model_ID,  "-", compare_climate, "-",  gsub("v", "", compare_model_version), "-", compare_model_ID_prefix,
    ifelse(compare_model_extra_info != "", paste0("-", compare_model_extra_info), "")
  )

  output_folder

  # file_prefix
  path_lst <- c(base_mods$path, comp_mods$path)

  # Scenario names
  scenario_name <- c(
    paste0(
      base_climate,  "-", base_model_ID,
      ifelse(base_model_extra_info == "", "", paste0("_", base_model_extra_info)),
      base_model_ID_suffix, "-",
      base_model_version, "_", base_model_ID_prefix
      ),
    paste0(
      compare_climate,  "-", compare_model_ID,
      ifelse(compare_model_extra_info == "", "", paste0("_", compare_model_extra_info)),
      compare_model_ID_suffix, "-",
      compare_model_version, "_", compare_model_ID_prefix
      )
    )

  # names of columns to select from ISF dataset
  model_id_subs <- c(
                    paste0(base_model_ID, base_climate),
                    paste0(compare_model_ID, compare_climate)
                    )

  # Load ISF data and select columns of interest
  isf_year_type <-
    process_isf(
      isf_path = isf_path
      ) %>%
    dplyr::select(wyqm, days_in_qm, dplyr::contains(model_id_subs))

  # Read in the annual CBT Quota data ---------------------------------------

  quota <-
    process_quota(
      quota_path = quota_path,
      model_ids  = model_id_subs,
      start_year = 1915,
      end_year   = 2014
      )

  quota <-
    dplyr::bind_rows(
      dplyr::mutate(quota, scenario = scenario_name[1]),
      dplyr::mutate(quota, scenario = scenario_name[2])
      ) %>%
    dplyr::mutate(
      model_run = factor(scenario, levels = c(scenario_name))
      ) %>%
    dplyr::select(-scenario)


  levels(quota$model_run)

  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  p_quota <-
    quota %>%
    ggplot2::ggplot(aes(x = year, y = quota, color = model_run,  linetype = model_run)) +
    ggplot2::geom_line() +  #col = color_list[i]
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.20)) +
    ggplot2::labs(
      title = "Annual C-BT Quota",
      x     = "Water Year"
    ) +
    ggplot2::theme(
      plot.title = element_text(size = title_size),
      axis.title = element_text(size = xaxis_size)
      )

  # list of model dataframes
  mod_lst <- list(base_mods, comp_mods)

  # loop through model run output sheets and read in and process data
  outputs <- lapply(1:length(mod_lst), function(y) {

    outs <- process_output(
      file_df = mod_lst[[y]],
      date_df = qm_convert
    ) %>%
      dplyr::mutate(
        model_run = scenario_name[y]
      ) %>%
      dplyr::relocate(model_run)

  }) %>%
    dplyr::bind_rows()

  # retrieve lookup table from all Output sheets and keep the distinct rows (no duplicate definitions)
  definitions <-  lapply(1:length(mod_lst), function(y) {

    make_lookup(output_path = mod_lst[[y]]$path)

  }
  ) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct()

  # May 1 Drought Trigger Calcs ---------------------------------------------
  data_annual <- readr::read_csv(
    base_summary$path,
    col_names = FALSE
  )
  mod_summary_lst <- list(base_summary, comp_summary)

  # loop through model run output sheets and read in and process data
  data_annual_lst <- lapply(1:length(mod_summary_lst), function(y) {

    message(paste0("Processing annual summary:\n", mod_summary_lst[[y]]$file))

    process_annual_summary(
      summary_path = mod_summary_lst[[y]]$path,
      model_run    = scenario_name[y]
      )

  }) %>%
    dplyr::bind_rows()

  # set up
  drought.df <- data.frame(DroughtLevel = c(0, 1, 2, 3, 4))


  # remove_rownames(extract)
  # colnames(extract) <- c("ModelRun", "Drought Response", "Count", "Revised Count",
  #                        "Adj Drought Response", "Precent", "Criteria", "Pass Fail")

  # table specs/setup
  size <- 0.65
  size1 <- 0.65
  tt <-
    gridExtra::ttheme_default(
      core    = list(
        fg_params = list(cex = size)
        ),
      colhead = list(fg_params=list(cex = size1)),
      rowhead = list(fg_params=list(cex = size))
    )

  # Table 2
  tbl_temp2 <- gridExtra::tableGrob(
      data_annual_lst,
      theme = tt,
      rows  = NULL
    )

  # add box around the column headers
  tbl_temp2 <- gtable::gtable_add_grob(
      tbl_temp2,
      grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
      t = 1,
      l = 1,
      r = ncol(tbl_temp2)
    )
  # add box around the first model run of data
  tbl_temp2 <- gtable::gtable_add_grob(
      tbl_temp2,
      grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
      t = 2,
      b = nrow(tbl_temp2),
      l = 1,
      r = ncol(tbl_temp2)
    )

  # add box around the second model run of data
  tbl_temp2 <- gtable::gtable_add_grob(
      tbl_temp2,
      grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
      t = 7,
      b = nrow(tbl_temp2),
      l = 1,
      r = ncol(tbl_temp2)
    )

  grid.draw(tbl_temp2)
  # For Plot 1a
  data_annual[132:136, 11:12]
  # DataObject 15 = PSI/DRI drought index,
  # Dataobject 12 = Drought Trigger Level,

  # select the sites you want to plot
  site_selection <- c("year", "qm", "Date", "ModelRun", "DataObject_12_Flow")
  n_site_selection <- length(site_selection)

  data_annual_list <- list()
  # import the two scenarios to compare
  for (i in 1:n_file_prefix){

    # read in the quarter-monthly CRAM model data
    data_annual_list[[i]] <- read_csv(paste(model_folder, "/", file_prefix[i], ".OutputAnnualSummary.csv", sep = ""),
                                      col_names = FALSE)

  }
  data_annual <- bind_rows(data_annual_list[[1]][132:136, 11:12],
                           data_annual_list[[2]][132:136, 11:12]
                           )
  colnames(data_annual) <- c("DroughtResponse", "Count")
  data_annual$Count <- as.numeric(data_annual$Count)
  data_annual2 <- bind_cols(ModelRun = c(rep(scenario_name[1], 5),
                                         rep(scenario_name[2], 5)),
                            data_annual)

  data_annual2$CityReliability <- rep(c(NA, NA, 1, 2, 3), 2)
  data_annual2$ExceedancePercent = c(NA, NA,
                                     sum(data_annual2$Count[2:3]),
                                     data_annual2$Count[4],
                                     data_annual2$Count[5],
                                     NA, NA,
                                     sum(data_annual2$Count[7:8]),
                                     data_annual2$Count[9],
                                     data_annual2$Count[10])
  # calculate the percentiles
  #data_annual2$Percent = data_annual2$RevisedCount/100
  # data_annual2$Criteria = rep(c(NA, NA, 0.05, 0.01, 0.001),2)
  data_annual2$Criteria = rep(c(NA, NA, 5, 1, 0.1),2)
  data_annual2$PassFail = ifelse(data_annual2$ExceedancePercent > data_annual2$Criteria, "fail", "pass")
  colnames(data_annual2) <- c("Model Run", "Drought Response", "Count of Triggers",
                              "City Reliability", "Reliability Percent",
                              "Reliability Criteria", "Pass/Fail")

  # set up
  drought.df <- data.frame(DroughtLevel = c(0, 1, 2, 3, 4))


  # remove_rownames(extract)
  # colnames(extract) <- c("ModelRun", "Drought Response", "Count", "Revised Count",
  #                        "Adj Drought Response", "Precent", "Criteria", "Pass Fail")
  size <- 0.65
  size1 <- 0.65
  tt <- ttheme_default(core = list(fg_params=list(cex = size)),
                       colhead = list(fg_params=list(cex = size1)),
                       rowhead = list(fg_params=list(cex = size)))

  ### Table 2
  tbl_temp2 <- tableGrob(data_annual2, theme = tt, rows = NULL)
  # add box around the column headers
  tbl_temp2 <- gtable_add_grob(tbl_temp2,
                               grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                               t = 1, l = 1, r = ncol(tbl_temp2))
  # add box around the first model run of data
  tbl_temp2 <- gtable_add_grob(tbl_temp2,
                               grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                               t = 2, b = nrow(tbl_temp2), l = 1, r = ncol(tbl_temp2))
  # add box around the second model run of data
  tbl_temp2 <- gtable_add_grob(tbl_temp2,
                               grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                               t = 7, b = nrow(tbl_temp2), l = 1, r = ncol(tbl_temp2))

  grid.draw(tbl_temp2)
  # rename basic data components
  # data_list[[i]] <-
    # data %>%
    # rename(year = Step...1) %>%
    # rename(qm = Step...2) %>%
    # rename(OpStep = Step...3) %>%
    # mutate(wyqm = paste(year, qm, sep = '-')) %>%
    # left_join(., qm_convert, by = "wyqm") %>%
    # mutate(Date = mdy(Start.Date)) %>%
    # mutate(ModelRun = scenario_name[i])
  #p_quota
  factor(quota$model_run, levels = c(scenario_name))
  Quota_annual$ModelRun <-factor(Quota_annual$ModelRun, levels = c(scenario_name))
  # Levels should be updated
  levels(Quota_annual$ModelRun)

  # for(i in 1:length(unique(model_id_subs))) {
  #   model_id_subs
  #
  # }
  # model_id_subs[1] == model_id_subs[2]
  # if(  model_id_subs[1] == model_id_subs[2]) {
  #   quota_a <-
  #     quota %>%
  #     dplyr::mutate(
  #       model_run
  #     )
  # }
  # convert to lower to match clean names
  # model_ids <- tolower(model_ids)

  # quota     <- readr::read_csv(
  #   file           = quota_path,
  #   col_names      = T,
  #   show_col_types = FALSE
  # ) %>%
  #   janitor::clean_names() %>%
  #   dplyr::select(year, dplyr::contains(model_ids)) %>%
  #   dplyr::filter(
  #     year >= 1915,
  #     year <= 2014
  #   ) %>%
  #   tidyr::pivot_longer(
  #     cols      = c(-year),
  #     names_to  = "model_run",
  #     values_to = "quota"
  #   ) %>%
  #   dplyr::arrange(model_run, year)
  # get the model configuration we are analyzing
  Quota_compare_year <- paste(compare_model_ID, compare_climate, sep = "")
  Quota_base_year <- paste(base_model_ID, base_climate, sep = "")

  quota_list <- c("Year", Quota_compare_year, Quota_base_year)
  quota_list_scenarios <- quota_list[-1]

  if(quota_list_scenarios[1] == quota_list_scenarios[2]){
    # Get the CBT quota year type data (annual data from 1915-2014)
    Quota_annual <- read_csv(paste(model_folder, "/", "CBT_Quota_Annual4.csv", sep = "")) %>%
      # extract only the YearType for the model runs we're interested in
      # use syms to convert a character vector to a symbol
      # The big-bang operator !!! forces-splice a list of objects. The elements of the list
      # are spliced in place, meaning that they each become one single argument (see R help ?'!!!").
      select(!!!syms(quota_list)) %>%
      filter(Year >= 1915 & Year <= 2014) %>%
      # convert from 'wide' format to 'long' format so it can be plotted with ggplot2
      pivot_longer(., cols = c(quota_list_scenarios[1]),
                   names_to = "ModelRun", values_to = "Quota") %>%
      arrange(ModelRun, Year)

    Quota_annual_a <-
      Quota_annual %>%
      mutate(ModelRun = scenario_name[1])

    Quota_annual_b <-
      Quota_annual %>%
      mutate(ModelRun = scenario_name[2])

    Quota_annual_temp = rbind(Quota_annual_a, Quota_annual_b)
    Quota_annual <- Quota_annual_temp


    # fix the factors/levels
    #Quota_annual$ModelRun <-factor(Quota_annual$ModelRun, levels = c(quota_list_scenarios[1]))
    Quota_annual$ModelRun <-factor(Quota_annual$ModelRun, levels = c(scenario_name))
    # Levels should be updated
    levels(Quota_annual$ModelRun)

    rm(Quota_annual_a, Quota_annual_b, Quota_annual_temp)

  }else {

    # Get the CBT quota year type data (annual data from 1915-2014)
    Quota_annual <- read_csv(paste(model_folder, "/", "CBT_Quota_Annual4.csv", sep = "")) %>%
      # extract only the YearType for the model runs we're interested in
      # use syms to convert a character vector to a symbol
      # The big-bang operator !!! forces-splice a list of objects. The elements of the list
      # are spliced in place, meaning that they each become one single argument (see R help ?'!!!").
      select(!!!syms(quota_list)) %>%
      filter(Year >= 1915 & Year <= 2014) %>%
      # convert from 'wide' format to 'long' format so it can be plotted with ggplot2
      pivot_longer(., cols = c(quota_list_scenarios[1], quota_list_scenarios[2]),
                   names_to = "ModelRun", values_to = "Quota") %>%
      arrange(ModelRun, Year)


    # Check the factor & levels for the 'ModelRun' column (we will plot by this)
    # factor(Quota_annual$ModelRun)
    # levels(Quota_annual$ModelRun)
    # # set the factor 'levels' to the correct plotting order
    Quota_annual$ModelRun <-factor(Quota_annual$ModelRun, levels = c(quota_list_scenarios))
    # Levels should be updated

    levels(Quota_annual$ModelRun)
    # get size of dataset


  }
  # ISF year type data (qm data from 1915-2014)
  isf_year_type <- read_csv(paste(model_folder, "/", "ISF_year_type3.csv", sep = "")) %>%
    # remove unnecessary columns of data
    select(-'WY-QM', -Cal.Year, -Month, -'QM start day', -'QM end day',
           -Start.Date, -End.Date) %>%
    # extract only the YearType for the model run we're interested in
    # use syms to convert a character vector to a symbol
    # The big-bang operator !!! forces-splice a list of objects. The elements of the list
    # are spliced in place, meaning that they each become one single argument (see R help ?'!!!").
    select(wyqm, DaysInQM, !!!syms(ISF_list))
  scenario_name

  scenario_name <- c(
    paste(compare_climate, "-", compare_model_ID, "_8500_", compare_model_ID_suffix,
          compare_model_version, "_", compare_model_ID_prefix, sep = ""),
    paste(base_climate, "-", base_model_ID, "_3143_", base_model_version,
          base_model_ID_suffix, "_", base_model_ID_prefix, sep = ""))
  file_prefix <- c(paste0(
    # base_model_ID_prefix,
    # "DRRP_DroughtPlan_2020_",
    ifelse(base_model_ID_prefix == "DRRP", "DRRP_DroughtPlan_2020_",
           paste0(base_model_ID_prefix, ".DRRP_DroughtPlan_2020_")),
    base_model_version_text, "_8500_",
    compare_model_ID, compare_model_ID_suffix, "_", compare_climate, sep = ""),
    paste0(
      # compare_model_ID_prefix,
      # "DRRP_DroughtPlan_2020_",
      ifelse(compare_model_ID_prefix == "DRRP", "DRRP_DroughtPlan_2020_",
             paste0(compare_model_ID_prefix, ".DRRP_DroughtPlan_2020_")),
      compare_model_version_text, "_3143_",
      base_model_ID, base_model_ID_suffix, "_", base_climate, sep = ""))
  n_file_prefix <- length(file_prefix)
  # scenario_name <- c(paste(compare_climate, "-", compare_model_ID, compare_model_ID_suffix, "_",
  #                          compare_model_version, "_", compare_model_ID_prefix, "_8500", sep = ""),
  #                    paste(base_climate, "-", base_model_ID, "_",base_model_version,
  #                          base_model_ID_suffix, "_", base_model_ID_prefix, "_3143", sep = ""))

  scenario_name <- c(paste(compare_climate, "-", compare_model_ID, "_8500_", compare_model_ID_suffix,
                           compare_model_version, "_", compare_model_ID_prefix, sep = ""),
                     paste(base_climate, "-", base_model_ID, "_3143_", base_model_version,
                           base_model_ID_suffix, "_", base_model_ID_prefix, sep = ""))
  scenario_name
  n_scenario_name <- length(scenario_name)


  # Read in the ISF Data ----------------------------------------------------

  # get the model configuration we are analyzing
  ISF_compare_year <- paste(compare_model_ID, compare_climate, sep = "")
  ISF_base_year <- paste(base_model_ID, base_climate, sep = "")
  # ISF_compare_year <- paste(
  #   gsub(paste0(comp_mod_size, "_"), "", compare_model_ID),
  #   compare_climate, sep = "")
  # ISF_base_year <- paste(
  #   gsub(paste0(base_mod_size, "_"), "", base_model_ID),
  #   base_climate, sep = "")
  ISF_list <- c(ISF_compare_year, ISF_base_year)

  # ISF year type data (qm data from 1915-2014)
  isf_year_type <- read_csv(paste(model_folder, "/", "ISF_year_type3.csv", sep = "")) %>%
    # remove unnecessary columns of data
    select(-'WY-QM', -Cal.Year, -Month, -'QM start day', -'QM end day',
           -Start.Date, -End.Date) %>%
    # extract only the YearType for the model run we're interested in
    # use syms to convert a character vector to a symbol
    # The big-bang operator !!! forces-splice a list of objects. The elements of the list
    # are spliced in place, meaning that they each become one single argument (see R help ?'!!!").
    select(wyqm, DaysInQM, !!!syms(ISF_list))
# output_folder <-

# model_dirs %>%
#   dplyr::filter(
#     !model_id %in% c("ID1"),
#     output == "OutputSheet"
#   ) %>%
#   dplyr::group_by(base_model_version, model_id, model_num) %>%
#   dplyr::slice(1) %>%
#   dplyr::ungroup() %>%
#   dplyr::filter(name != "Quota", model_num == "7525") %>%
#   dplyr::mutate(
#     prefix = substr(file, 1, 4)
#   )
# dplyr::select(base_model_version, model_id, model_num)

# # retrieve loopup table from all Output sheets and keep the distinct rows (no duplicate definitions)
# definitions <-  lapply(1:nrow(path_df), function(y) {
#
#    make_lookup(output_path = path_df$path[y])
#
#   }
# ) %>%
#   dplyr::bind_rows() %>%
#   dplyr::distinct()
# z <- 1
# rm(z)# rm(i)
# z <- 3
# z <- 1
# k=1
comp_mod_size = "8500"
base_mod_size = "3143"
# z = 1
# iterate through base models
for(z in 1:nrow(base_mods)) {


  # base folder path
  base_folder          <- "D:/cob/latest/latest"

  currwd               <- "D:/cob/latest/latest"

  # message(paste0("Base model: ", z, "/", nrow(base_mods)))

 base_model_version        <-  paste0("v", base_mods$model_version[z])

  base_model_ID        <- base_mods$model_id[z]

  base_model_ID_suffix <- ""        # if this is not needed, keep blank ""

  base_climate         <- base_mods$model_num[z]

  # base_model_ID_prefix <- ""   # if this is not needed, keep blank ""
  base_model_ID_prefix <- base_mods$prefix[z]
  # message(paste0("Base model ver: ",base_model_version,
  #                "\nBase model ID: ", base_model_ID,
  #                "\nBase model ID suffix: ", base_model_ID_suffix,
  #                "\nBase model climate: ", base_climate,
  #                "\nBase model ID prefix: ", base_model_ID_prefix,
  #                )
  #         )

  # base_model_ID <- "ID1"                # Leave as ID1 (always compare with base)
  # base_model_ID_suffix <- ""        # if this is not needed, keep blank ""
  # base_climate <- "7525"               # Base or 9010, 7525, Center
  # base_model_ID_prefix <- ""
  #base_model_version
  # k  <- 6
  # k = 1
  # iterate through comparison models
  for(k in 1:nrow(comp_mods)) {

    message(paste0("Base model: ", z, "/", nrow(base_mods)))
    message(paste0("Comp model: ", k, "/", nrow(comp_mods)))

    compare_model_version   <-  paste0("v", comp_mods$model_version[k])

    compare_model_ID        <- comp_mods$model_id[k]

    compare_model_ID_suffix <- ""        # if this is not needed, keep blank ""

    compare_climate         <- comp_mods$model_num[k]

    # compare_model_ID_prefix <- ""
    compare_model_ID_prefix <- comp_mods$prefix[k]

    message(paste0("Base model ver: ",base_model_version,
                   "\nBase model ID: ", base_model_ID,
                   "\nBase model ID suffix: ", base_model_ID_suffix,
                   "\nBase model climate: ", base_climate,
                   "\nBase model ID prefix: ", base_model_ID_prefix
    )
    )
    message(paste0("Comp model ver: ", compare_model_version,
                   "\nComp model ID: ", compare_model_ID,
                   "\nComp model ID suffix: ", compare_model_ID_suffix,
                   "\nComp model climate: ", compare_climate,
                   "\nComp model ID prefix: ", compare_model_ID_prefix
    )
    )
    ### User controlled data
    #base_model_version <- c("v055a")

    # base_model_ID <- "ID1"                # Leave as ID1 (always compare with base)
    # base_model_ID_suffix <- ""        # if this is not needed, keep blank ""
    # base_climate <- "7525"               # Base or 9010, 7525, Center
    # base_model_ID_prefix <- ""   # if this is not needed, keep blank ""
    #
    # # rm(base_model_ID_prefix, compare_model_ID_prefix)
    # compare_model_version <- c("v055a")
    #
    # compare_model_ID <- "ID2"            # ID2, ID3, ID4, ID5
    # compare_model_ID_suffix <- ""   # if this is not needed, keep blank ""
    # compare_climate <- "7525"             # Base or 9010, 7525, Center
    # compare_model_ID_prefix <- ""   # if this is not needed, keep blank ""

    # set plot parameters
    title_size = 10
    xaxis_size = 9

    # model_folder <- "latest"
    model_folder <- "D:/cob/latest/latest"

    device_type <- ".png"     # .png, .pdf


    # Calculated parameters ---------------------------------------------------


    output_folder <-
      paste0(
        base_model_ID, "-", base_climate,  "-", gsub("v", "",base_model_version), "-", base_model_ID_prefix,
        " vs ",
        compare_model_ID, "-", compare_climate, "-", gsub("v", "", compare_model_version), "-", compare_model_ID_prefix
      )

    base_model_version_text <- substr(base_model_version, start = 2, stop = nchar(base_model_version))
    compare_model_version_text <- substr(compare_model_version, start = 2, stop = nchar(compare_model_version))

    # model_dirs %>%
    #   dplyr::filter(
    #   !model_id %in% c("ID1"),
    #   output == "OutputSheet"
    # ) %>%
    #   dplyr::group_by(base_model_version, model_id, model_num) %>%
    #   dplyr::slice(1) %>%
    #   dplyr::ungroup() %>%
    #   dplyr::filter(name != "Quota", model_num == "7525") %>%
    #   .$file
    # file_prefix <- c(paste0(base_model_ID_prefix, "DRRP_DroughtPlan_2020_",base_model_version_text, "_",
    #                         compare_model_ID, compare_model_ID_suffix, "_", compare_climate, sep = ""),
    #                  paste0(compare_model_ID_prefix, "DRRP_DroughtPlan_2020_", compare_model_version_text, "_",
    #                         base_model_ID, base_model_ID_suffix, "_", base_climate, sep = ""))

    # file_prefix <- c(paste0(
    #   # base_model_ID_prefix,
    #                         # "DRRP_DroughtPlan_2020_",
    #                         ifelse(base_model_ID_prefix == "DRRP", "DRRP_DroughtPlan_2020_",
    #                                paste0(base_model_ID_prefix, ".DRRP_DroughtPlan_2020_")),
    #                        base_model_version_text, "_",
    #                         compare_model_ID, compare_model_ID_suffix, "_", compare_climate, sep = ""),
    #                  paste0(
    #                    # compare_model_ID_prefix,
    #                         # "DRRP_DroughtPlan_2020_",
    #                         ifelse(compare_model_ID_prefix == "DRRP", "DRRP_DroughtPlan_2020_",
    #                                paste0(compare_model_ID_prefix, ".DRRP_DroughtPlan_2020_")),
    #                         compare_model_version_text, "_",
    #                         base_model_ID, base_model_ID_suffix, "_", base_climate, sep = ""))

    file_prefix <- c(paste0(
      # base_model_ID_prefix,
      # "DRRP_DroughtPlan_2020_",
      ifelse(base_model_ID_prefix == "DRRP", "DRRP_DroughtPlan_2020_",
             paste0(base_model_ID_prefix, ".DRRP_DroughtPlan_2020_")),
     base_model_version_text, "_8500_",
      compare_model_ID, compare_model_ID_suffix, "_", compare_climate, sep = ""),
      paste0(
        # compare_model_ID_prefix,
        # "DRRP_DroughtPlan_2020_",
        ifelse(compare_model_ID_prefix == "DRRP", "DRRP_DroughtPlan_2020_",
               paste0(compare_model_ID_prefix, ".DRRP_DroughtPlan_2020_")),
        compare_model_version_text, "_3143_",
        base_model_ID, base_model_ID_suffix, "_", base_climate, sep = ""))
    n_file_prefix <- length(file_prefix)
    # scenario_name <- c(paste(compare_climate, "-", compare_model_ID, compare_model_ID_suffix, "_",
    #                          compare_model_version, "_", compare_model_ID_prefix, "_8500", sep = ""),
    #                    paste(base_climate, "-", base_model_ID, "_",base_model_version,
    #                          base_model_ID_suffix, "_", base_model_ID_prefix, "_3143", sep = ""))

    scenario_name <- c(paste(compare_climate, "-", compare_model_ID, "_8500_", compare_model_ID_suffix,
                             compare_model_version, "_", compare_model_ID_prefix, sep = ""),
                       paste(base_climate, "-", base_model_ID, "_3143_", base_model_version,
                             base_model_ID_suffix, "_", base_model_ID_prefix, sep = ""))
    scenario_name
    n_scenario_name <- length(scenario_name)


    # Read in the ISF Data ----------------------------------------------------

    # get the model configuration we are analyzing
    ISF_compare_year <- paste(compare_model_ID, compare_climate, sep = "")
    ISF_base_year <- paste(base_model_ID, base_climate, sep = "")
    # ISF_compare_year <- paste(
    #   gsub(paste0(comp_mod_size, "_"), "", compare_model_ID),
    #   compare_climate, sep = "")
    # ISF_base_year <- paste(
    #   gsub(paste0(base_mod_size, "_"), "", base_model_ID),
    #   base_climate, sep = "")
    ISF_list <- c(ISF_compare_year, ISF_base_year)

    # ISF year type data (qm data from 1915-2014)
    isf_year_type <- read_csv(paste(model_folder, "/", "ISF_year_type3.csv", sep = "")) %>%
      # remove unnecessary columns of data
      select(-'WY-QM', -Cal.Year, -Month, -'QM start day', -'QM end day',
             -Start.Date, -End.Date) %>%
      # extract only the YearType for the model run we're interested in
      # use syms to convert a character vector to a symbol
      # The big-bang operator !!! forces-splice a list of objects. The elements of the list
      # are spliced in place, meaning that they each become one single argument (see R help ?'!!!").
      select(wyqm, DaysInQM, !!!syms(ISF_list))

    #rm(ISF_compare_year, ISF_list)



    # Read in the annual CBT Quota data ---------------------------------------

    # get the model configuration we are analyzing
    Quota_compare_year <- paste(compare_model_ID, compare_climate, sep = "")
    Quota_base_year <- paste(base_model_ID, base_climate, sep = "")

    quota_list <- c("Year", Quota_compare_year, Quota_base_year)
    quota_list_scenarios <- quota_list[-1]

    if(quota_list_scenarios[1] == quota_list_scenarios[2]){
      # Get the CBT quota year type data (annual data from 1915-2014)
      Quota_annual <- read_csv(paste(model_folder, "/", "CBT_Quota_Annual4.csv", sep = "")) %>%
        # extract only the YearType for the model runs we're interested in
        # use syms to convert a character vector to a symbol
        # The big-bang operator !!! forces-splice a list of objects. The elements of the list
        # are spliced in place, meaning that they each become one single argument (see R help ?'!!!").
        select(!!!syms(quota_list)) %>%
        filter(Year >= 1915 & Year <= 2014) %>%
        # convert from 'wide' format to 'long' format so it can be plotted with ggplot2
        pivot_longer(., cols = c(quota_list_scenarios[1]),
                     names_to = "ModelRun", values_to = "Quota") %>%
        arrange(ModelRun, Year)

      Quota_annual_a <-
        Quota_annual %>%
        mutate(ModelRun = scenario_name[1])

      Quota_annual_b <-
        Quota_annual %>%
        mutate(ModelRun = scenario_name[2])

      Quota_annual_temp = rbind(Quota_annual_a, Quota_annual_b)
      Quota_annual <- Quota_annual_temp


      # fix the factors/levels
      #Quota_annual$ModelRun <-factor(Quota_annual$ModelRun, levels = c(quota_list_scenarios[1]))
      Quota_annual$ModelRun <-factor(Quota_annual$ModelRun, levels = c(scenario_name))
      # Levels should be updated
      levels(Quota_annual$ModelRun)

      rm(Quota_annual_a, Quota_annual_b, Quota_annual_temp)

    }else {

      # Get the CBT quota year type data (annual data from 1915-2014)
      Quota_annual <- read_csv(paste(model_folder, "/", "CBT_Quota_Annual4.csv", sep = "")) %>%
        # extract only the YearType for the model runs we're interested in
        # use syms to convert a character vector to a symbol
        # The big-bang operator !!! forces-splice a list of objects. The elements of the list
        # are spliced in place, meaning that they each become one single argument (see R help ?'!!!").
        select(!!!syms(quota_list)) %>%
        filter(Year >= 1915 & Year <= 2014) %>%
        # convert from 'wide' format to 'long' format so it can be plotted with ggplot2
        pivot_longer(., cols = c(quota_list_scenarios[1], quota_list_scenarios[2]),
                     names_to = "ModelRun", values_to = "Quota") %>%
        arrange(ModelRun, Year)


      # Check the factor & levels for the 'ModelRun' column (we will plot by this)
      # factor(Quota_annual$ModelRun)
      # levels(Quota_annual$ModelRun)
      # # set the factor 'levels' to the correct plotting order
      Quota_annual$ModelRun <-factor(Quota_annual$ModelRun, levels = c(quota_list_scenarios))
      # Levels should be updated

      levels(Quota_annual$ModelRun)
      # get size of dataset


    }

    # quota <- process_quota(
    #   quota_path = quota_path,
    #   model_ids  = paste0(path_lst[[4]]$id, path_lst[[4]]$climate),
    #   verbose    = TRUE
    # )


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
    #p_quota


    rm(Quota_compare_year, quota_list, quota_list_scenarios)


    # Import the data --------------------------------------------------------


    # create the data list to store the data
    data_list <- list()

    # import the two scenarios to compare
    for (i in 1:n_file_prefix){
      # i = 1
      # read in the quarter-monthly CRAM model data
      # data <- read_csv(paste(model_folder, "/", file_prefix[i], ".OutputSheet.csv", sep = ""),
      #                  col_names = FALSE)
      data <- read_csv(paste(model_folder, "/", file_prefix[i], ".OutputSheet.csv", sep = ""),
                       col_names = FALSE)

      # get the column name & column descriptions
      column_names <- data[5, ]
      column_parameter <- data[4, ]
      column_descriptions <- data[3, ]
      # i =1
      # re-read in the quarter-monthly CRAM model data (skipping name rows at the top)
      data <- read_csv(
        paste(model_folder, "/", file_prefix[i], ".OutputSheet.csv", sep = ""),
        # col_names = F,
        skip = 4)
      data <- data[1:4800, ]

      # read in the quarter-monthly to date converter
      qm_convert <- read_csv("data-raw/qm_to_date_conversion.csv")



      # rename basic data components
      data_list[[i]] <-
        data %>%
        rename(year = Step...1) %>%
        rename(qm = Step...2) %>%
        rename(OpStep = Step...3) %>%
        mutate(wyqm = paste(year, qm, sep = '-')) %>%
        left_join(., qm_convert, by = "wyqm") %>%
        mutate(Date = mdy(Start.Date)) %>%
        mutate(ModelRun = scenario_name[i])
      data_list[[i]]

    }

    # create a lookup for names, definitions, descriptions
    column_names2 <- t(column_names)
    #column_names2
    column_descriptions2 <- t(column_descriptions)
    #column_descriptions2
    column_parameter2 <- t(column_parameter)
    #column_parameter2

    definitions <- data.frame(Name = column_names2, Description = column_descriptions2,
                              Parameter = column_parameter2)
    n_definitions <- dim(definitions)
    definitions <- definitions[4:n_definitions[1], ]

    # make a look up for parameter units
    units.df <- data.frame(Parameter = c("Flow", "High", "Content", "Shortage", "Priority", "Evaporation", "Low"),
                           Units = c("Flow (af)", "Flow (af)", "Contents (af)", "Flow (cfs)", "Value",
                                     "Flow (af)", "Flow af)"))
    # bind the units to the definitions table
    definitions <- left_join(definitions, units.df, by = "Parameter")


    #definitions
    rm(column_names2, column_descriptions2, column_parameter2, units.df)



    # May 1 Drought Trigger Calcs ---------------------------------------------

    # For Plot 1a

    # DataObject 15 = PSI/DRI drought index,
    # Dataobject 12 = Drought Trigger Level,

    # select the sites you want to plot
    site_selection <- c("year", "qm", "Date", "ModelRun", "DataObject_12_Flow")
    n_site_selection <- length(site_selection)

    data_annual_list <- list()
    # import the two scenarios to compare
    for (i in 1:n_file_prefix){

      # read in the quarter-monthly CRAM model data
      data_annual_list[[i]] <- read_csv(paste(model_folder, "/", file_prefix[i], ".OutputAnnualSummary.csv", sep = ""),
                                        col_names = FALSE)

    }
    data_annual <- bind_rows(data_annual_list[[1]][132:136, 11:12],
                             data_annual_list[[2]][132:136, 11:12])
    colnames(data_annual) <- c("DroughtResponse", "Count")
    data_annual$Count <- as.numeric(data_annual$Count)
    data_annual2 <- bind_cols(ModelRun = c(rep(scenario_name[1], 5),
                                           rep(scenario_name[2], 5)),
                              data_annual)

    data_annual2$CityReliability <- rep(c(NA, NA, 1, 2, 3), 2)
    data_annual2$ExceedancePercent = c(NA, NA, sum(data_annual2$Count[2:3]),
                                       data_annual2$Count[4], data_annual2$Count[5],
                                       NA, NA, sum(data_annual2$Count[7:8]),
                                       data_annual2$Count[9], data_annual2$Count[10])
    # calculate the percentiles
    #data_annual2$Percent = data_annual2$RevisedCount/100
    # data_annual2$Criteria = rep(c(NA, NA, 0.05, 0.01, 0.001),2)
    data_annual2$Criteria = rep(c(NA, NA, 5, 1, 0.1),2)
    data_annual2$PassFail = ifelse(data_annual2$ExceedancePercent > data_annual2$Criteria, "fail", "pass")
    colnames(data_annual2) <- c("Model Run", "Drought Response", "Count of Triggers",
                                "City Reliability", "Reliability Percent",
                                "Reliability Criteria", "Pass/Fail")

    # set up
    drought.df <- data.frame(DroughtLevel = c(0, 1, 2, 3, 4))

    # extract_list <- list()
    # temp_aa <- list()
    # for (i in 1:n_file_prefix){
    #
    #   extract_list[[i]] <- data_list[[i]] %>%
    #     filter(qm == 29) %>%
    #     group_by(ModelRun, DataObject_12_Flow) %>%
    #     count(DataObject_12_Flow) %>%
    #     rename(Count = n, DroughtResponse = DataObject_12_Flow)
    #
    #   temp_aa[[i]] <- left_join(drought.df, extract_list[[i]], by = c("DroughtLevel" = "DroughtResponse"))
    #   # extract_list[[i]]$AdjDroughtResponse = c(NA, NA, 1, 2, 3)
    #   # # calculate the combine drought trigger counts
    #   # extract_list[[i]]$RevisedCount = c(NA, NA, sum(extract_list[[i]]$Count[2:3]),
    #   #                                    extract_list[[i]]$Count[4], extract_list[[i]]$Count[5])
    #   # # calculate the percentiles
    #   # extract_list[[i]]$Percent = extract_list[[i]]$RevisedCount/100
    #   # extract_list[[i]]$Criteria = c(NA, NA, 0.05, 0.01, 0.001)
    #   # extract_list[[i]]$PassFail = ifelse(extract_list[[i]]$Percent > extract_list[[i]]$Criteria, "fail", "pass")
    #
    # }
    # extract <- bind_rows(temp_aa[[1]], temp_aa[[2]])



    # merge the data from the loops together
    #extract <- bind_rows(extract_list[[1]], extract_list[[2]])
    # remove_rownames(extract)
    # colnames(extract) <- c("ModelRun", "Drought Response", "Count", "Revised Count",
    #                        "Adj Drought Response", "Precent", "Criteria", "Pass Fail")
    size <- 0.65
    size1 <- 0.65
    tt <- ttheme_default(core = list(fg_params=list(cex = size)),
                         colhead = list(fg_params=list(cex = size1)),
                         rowhead = list(fg_params=list(cex = size)))
    # tbl_temp <- grid.table(extract, theme = tt)
    # tbl_temp <- tableGrob(extract, theme = tt)



    # extract2 <- extract %>%
    #   select(ModelRun, DroughtResponse, Count, Percent, Criteria, PassFail)
    # extract2
    #
    # extract3 <- extract %>%
    #   select(ModelRun, DroughtResponse, Count, AdjDroughtResponse, Percent) %>%
    #   pivot_wider(names_from = c(DroughtResponse, AdjDroughtResponse),
    #               values_from = c(Count, Percent)) %>%
    #   select(-Percent_0_NA, -Percent_1_NA) %>%
    #   rename('No Trigger Count' = Count_0_NA, 'Level 1 Count' = Count_1_NA,
    #          'Level 2 Count' = Count_2_1, 'Level 3 Count' = Count_3_2,
    #          'Level 4 Count' = Count_4_3,
    #          'Criteria 1 %' = Percent_2_1, 'Criteria 2 %' = Percent_3_2, 'Criteria 3 %' = Percent_4_3,)
    # extract3
    #
    # extract3$'Lvl 1 Criteria' = c(0.05, 0.05)
    # extract3$'Lvl 2 Criteria' = c(0.05, 0.01)
    # extract3$'Lvl 3 Criteria' = c(0.05, 0.001)

    ### Table 2
    tbl_temp2 <- tableGrob(data_annual2, theme = tt, rows = NULL)
    # add box around the column headers
    tbl_temp2 <- gtable_add_grob(tbl_temp2,
                                 grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                                 t = 1, l = 1, r = ncol(tbl_temp2))
    # add box around the first model run of data
    tbl_temp2 <- gtable_add_grob(tbl_temp2,
                                 grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                                 t = 2, b = nrow(tbl_temp2), l = 1, r = ncol(tbl_temp2))
    # add box around the second model run of data
    tbl_temp2 <- gtable_add_grob(tbl_temp2,
                                 grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                                 t = 7, b = nrow(tbl_temp2), l = 1, r = ncol(tbl_temp2))

    grid.draw(tbl_temp2)


    # ### Table 3
    # tbl_temp3 <- tableGrob(extract3, theme = tt, rows = NULL)
    # # add box around the column headers
    # tbl_temp3 <- gtable_add_grob(tbl_temp3,
    #                              grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
    #                              t = 1, l = 1, r = ncol(tbl_temp3))
    # # add box around the row headers
    # tbl_temp3 <- gtable_add_grob(tbl_temp3,
    #                              grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
    #                              t = 2, b = nrow(tbl_temp3), l = 1, r = 1)
    #
    # # add box around the first model run of data
    # tbl_temp3 <- gtable_add_grob(tbl_temp3,
    #                              grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
    #                              t = 2, b = nrow(tbl_temp3), l = 2, r = 6)
    # # add box around the first model run of data
    # tbl_temp3 <- gtable_add_grob(tbl_temp3,
    #                              grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
    #                              t = 2, b = nrow(tbl_temp3), l = 7, r = 9)
    # # add box around the first model run of data
    # tbl_temp3 <- gtable_add_grob(tbl_temp3,
    #                              grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
    #                              t = 2, b = nrow(tbl_temp3), l = 10, r = 12)

    #grid.draw(tbl_temp3)


    # PSI Components ----------------------------------------------------------

    # For Plot 1a

    # predictedStorage = min(18250, mtnStorage + newStorageWater)
    #DroughtStorageIndex
    # Function GetCOBMtnStorage() = Res 1 + Res 3 = mtnStorage

    # # select the sites you want to plot
    # site_selection <- c("year", "qm", "Date", "ModelRun", "DataObject_12_Flow")
    # n_site_selection <- length(site_selection)

    data_annual_list <- list()
    # import the two scenarios to compare
    for (i in 1:n_file_prefix){

      # read in the quarter-monthly CRAM model data
      data_annual_list[[i]] <- read_csv(paste(model_folder, "/", file_prefix[i], ".OutputAnnualSummary.csv", sep = ""),
                                        col_names = FALSE)

    }

    data_annual <- bind_rows(data_annual_list[[1]][21:120, 1:23],
                             data_annual_list[[2]][21:120, 1:23])
    data_annual$ModelRun = c(rep(scenario_name[1], 100), rep(scenario_name[2], 100))


    colnames(data_annual) <- c("year", "blank1", "blank2", "COBTOtalDemandFlow", "COBTotalDemandShortage",
                               "COBDemandMaytoApr", "LakewoodPipeBetass", "BarkerPipeBetass",
                               "May1MtnStorage", "May1COBStorage", "PSI", "DroughtResponseLevel", "IndoorDemand",
                               "PRVrelease", "COBTotalDemand", "EOYResStorage", "DemandinExcessTreatment",
                               "MaxDemandforDRI", "DirectFlowDemand", "SouthPlatteCall", "SPCallRank",
                               "COBCBTVolume", "COBCBTWater", "ModelRun")

    #data_annual2 <- mutate_all(data_annual, function(x) as.numeric(as.character(x)))

    data_annual2 <- data_annual %>%
      select(year, ModelRun, May1MtnStorage, May1COBStorage, PSI, DroughtResponseLevel, COBCBTVolume, COBCBTWater) %>%
      mutate(year = as.numeric(year)) %>%
      mutate(May1MtnStorage = as.numeric(May1MtnStorage)) %>%
      mutate(May1COBStorage = as.numeric(May1COBStorage)) %>%
      mutate(PSI = as.numeric(PSI)) %>%
      mutate(DroughtResponseLevel = as.numeric(DroughtResponseLevel)) %>%
      mutate(COBCBTVolume = round(as.numeric(COBCBTVolume),1)) %>%
      mutate(COBCBTWater = as.numeric(COBCBTWater)) %>%
      filter(year >= 2000 & year <= 2008)


    extract_temp <- list()
    for (i in 1:n_file_prefix){

      extract_temp[[i]] <- data_list[[i]] %>%
        # drought index is set May 1 each year (model sets in QM 29)
        filter(qm == 28 | qm == 28) %>%
        # DataObject 15 = PSI/DRI drought index,
        # Dataobject 12 = Drought Trigger Level,
        # Res 3 = barker reservoir storage,
        # Res 1 = watershed res storage,
        # DataObject 1 = COB's share of Boulder res contents,
        # Res 12 = Total Boulder Res Contents
        select(year, qm, Date, ModelRun,
               Reservoir_1_Content, Reservoir_3_Content) %>%
        mutate(mtnStorage = Reservoir_1_Content + Reservoir_3_Content)

    }

    # merge the data from the loops together
    extract_temp2 <- bind_rows(extract_temp[[1]], extract_temp[[2]]) %>%
      filter(year >= 2000 & year <= 2008)

    # Check the factor & levels for the 'ModelRun' column (we will plot by this)
    # factor(drought_index$ModelRun)
    # levels(drought_index$ModelRun)
    # set the factor 'levels' to the correct plotting order
    extract_temp$ModelRun <-factor(extract_temp$ModelRun, levels = scenario_name)
    # Levels should be updated
    levels(extract_temp$ModelRun)


    rm(extract_temp, extract_temp2, data_annual, data_annual2, data_annual_list)


    # May 1 Annual PSI and Reservoir Storage Plots (1a) --------------------------------------------------------



    drought_index_list <- list()
    for (i in 1:n_file_prefix){

      drought_index_list[[i]] <- data_list[[i]] %>%
        # drought index is set May 1 each year (model sets in QM 29)
        filter(qm == 29) %>%
        # DataObject 15 = PSI/DRI drought index,
        # Dataobject 12 = Drought Trigger Level,
        # Res 3 = barker reservoir storage,
        # Res 1 = watershed res storage,
        # DataObject 1 = COB's share of Boulder res contents,
        # Res 12 = Total Boulder Res Contents
        select(year, qm, Date, ModelRun, DataObject_15_Flow, DataObject_12_Flow, Reservoir_3_Content,
               Reservoir_1_Content, DataObject_1_Flow, Reservoir_12_Content,
               DataObject_13_Flow) %>%
        mutate(PSI = DataObject_15_Flow/100) %>%
        rename(DroughtResponseLevel = DataObject_12_Flow) %>%
        rename(Barker_Res_Contents_af = Reservoir_3_Content) %>%
        rename(NBC_Res_Contents_af = Reservoir_1_Content) %>%
        rename(Boulder_Res_Contents_af = DataObject_1_Flow) %>%
        rename(TotalBoulder_Res_Contents_af = Reservoir_12_Content) %>%
        mutate(Upper_Storage_af = rowSums(across(c(Barker_Res_Contents_af, NBC_Res_Contents_af)))) %>%
        rename(Predicted_Storage_af = DataObject_13_Flow)
      drought_index_list[[i]]

    }

    # merge the data from the loops together
    drought_index <- bind_rows(drought_index_list[[1]], drought_index_list[[2]])

    # Check the factor & levels for the 'ModelRun' column (we will plot by this)
    # factor(drought_index$ModelRun)
    # levels(drought_index$ModelRun)
    # set the factor 'levels' to the correct plotting order
    drought_index$ModelRun <-factor(drought_index$ModelRun, levels = scenario_name)
    # Levels should be updated
    levels(drought_index$ModelRun)


    temp <- drought_index %>%
      #select(year, qm, PSI) %>%
      filter(year >= 2000 & year <= 2008)

    ### Make the DRI & Reservoir Contents Time Series Plots ###

    site_list <- c("PSI", "DroughtResponseLevel", "Barker_Res_Contents_af",
                   "NBC_Res_Contents_af", "Upper_Storage_af", "Boulder_Res_Contents_af")
    title_list <- c("Projected Storage Index","Drought Response Level",
                    "Barker Reservoir Contents", "NBC Reservoir Contents",
                    "Total Upper Reservoir Contents", "COB Boulder Reservoir Contents")
    n_site_list <- length(site_list)
    y_axis_max_list <- c(2.5, 5, 13000, 8000, 20000, 7000)
    #color_list <- c("black", "#4169E1", "#4169E1", "#4169E1")
    ylab_list <- c("PSI", "Drought Response Level", "Reservoir Contents (af)",
                   "Reservoir Contents (af)", "Reservoir Contents (af)", "Reservoir Contents (af)")
    storage_max_list <- c(NA, NA, 11277, 6927, 18204) #NBC Res, Barker Res, Boulder Res (summer storage)


    p <- list()
    p_drought_triggers <- list()
    for (i in 1:n_site_list){

      if (i == 1){

        p[[i]] <- ggplot(drought_index, aes_string(x = "year", y = site_list[i],
                                                   color = "ModelRun", linetype = "ModelRun")) +
          geom_line() + #col = color_list[i]
          geom_hline(yintercept = 0.40, linetype="solid", color="red") +
          geom_hline(yintercept = 0.55, linetype="solid", color="darkorange") +
          geom_hline(yintercept = 0.70, linetype="solid", color="darkgreen") +
          geom_hline(yintercept = 0.85, linetype="solid", color="blue") +
          #geom_hline(yintercept = 1.0, linetype="solid", color="black") +
          theme_bw() +
          ylim(0, y_axis_max_list[i]) +
          ylab(ylab_list[i]) +
          xlab("Water Year") +
          ggtitle(title_list[i]) +
          theme(plot.title = element_text(size = title_size),
                axis.title = element_text(size = xaxis_size))
        #p[[i]]

      } else if(i == 2){

        p_drought_triggers <- ggplot(drought_index, aes_string(x = "year", y = site_list[i],
                                                               color = "ModelRun", linetype = "ModelRun")) +
          geom_line() + #col = color_list[i]
          theme_bw() +
          ylim(0, y_axis_max_list[i]) +
          ylab(ylab_list[i]) +
          xlab("Water Year") +
          ggtitle(title_list[i]) +
          theme(plot.title = element_text(size = title_size),
                axis.title = element_text(size = xaxis_size),
                panel.grid.minor.y = element_blank())

      } else {

        # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
        p[[i]] <- ggplot(drought_index, aes_string(x = "year", y = site_list[i],
                                                   color = "ModelRun", linetype = "ModelRun")) +
          geom_line() + #col = color_list[i]
          geom_hline(yintercept = storage_max_list[i], color = "black", size = 0.25) +
          theme_bw() +
          ylim(0, y_axis_max_list[i]) +
          ylab(ylab_list[i]) +
          xlab("Water Year") +
          ggtitle(title_list[i]) +
          theme(plot.title = element_text(size = title_size),
                axis.title = element_text(size = xaxis_size))
        #p[[i]]

      }


    }

    # define the plot name
    plot_title <- "1a. May 1 Annual Reservoir Contents - Time Series Plot"
    file_name <- paste(plot_title, " 3x2 ", sep = "")

    # save the plot
    ggsave(
      filename = paste(model_folder, "/", output_folder, "/", file_name, model_version, " ",
                       output_folder, device_type, sep = ""),
      width = 14, height = 8,
      grid.arrange(p[[1]], p_drought_triggers, p[[3]], p[[4]], p[[5]], tbl_temp2, nrow = 3,
                   top = plot_title,
                   right = "", bottom = ""))

    rm(storage_max_list)


    # # define the plot name
    # plot_title <- "May 1 Annual Reliability Criteria Tables"
    # file_name <- paste(plot_title, " 2x1 ", sep = "")
    #
    # # save the plot
    # ggsave(
    #   paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
    #         output_folder, ".pdf", sep = ""),
    #   width = 14, height = 8,
    #   grid.arrange(tbl_temp2, tbl_temp3, nrow = 2,
    #                top = plot_title,
    #                right = "", bottom = ""))

    # ggsave(
    #   "May 1 Annual Reservoir Time Series 4x1.png", width = 8, height = 11,
    #   grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], nrow = 4,
    #                top = "May 1 Annual Reservoir Time Series Plot",
    #                right = ""))


    # rm(p, drought_index_list, drought_index, y_axis_max_list, ylab_list,
    #    tbl_temp2, site_list, title_list, n_site_list, plot_title, file_name)


    # #### Make the DRI & Reservoir Contents Time Series Plots (w/Boulder Res) ##########
    #
    # site_list <- c("PSI", "Barker_Res_Contents_af", "NBC_Res_Contents_af", "Upper_Storage_af",
    #                "Boulder_Res_Contents_af", "TotalBoulder_Res_Contents_af")
    # title_list <- c("Projected Storage Index", "Barker Reservoir Contents",
    #                 "NBC Reservoir Contents", "Total Upper Reservoir Contents",
    #                 "COB Boulder Reservoir Contents", "Total Boulder Reservoir Contents")
    # n_site_list <- length(site_list)
    # y_axis_max_list <- c(2.5, 12000, 8000, 20000, 7000, 15000)
    # color_list <- c("black", "#4169E1", "#4169E1", "#4169E1")
    # ylab_list <- c("PSI", "Reservoir Contents (af)", "Reservoir Contents (af)", "Reservoir Contents (af)",
    #                "Reservoir Contents (af)", "Reservoir Contents (af)")
    # p <- list()
    #
    #
    # for (i in 1:n_site_list){
    #
    #   if (i == 1){
    #
    #     p[[i]] <- ggplot(drought_index, aes_string(x = "year", y = site_list[i],
    #                                                color = "ModelRun", linetype = "ModelRun")) +
    #       geom_line() + #col = color_list[i]
    #       #geom_hline(yintercept = 0.40, linetype="solid", color="red") +
    #       #geom_hline(yintercept = 0.55, linetype="solid", color="orange") +
    #       #geom_hline(yintercept = 0.70, linetype="solid", color="yellow") +
    #       #geom_hline(yintercept = 0.85, linetype="solid", color="green") +
    #       #geom_hline(yintercept = 1.0, linetype="solid", color="black") +
    #       theme_bw() +
    #       ylim(0, y_axis_max_list[i]) +
    #       ylab(ylab_list[i]) +
    #       xlab("Water Year") +
    #       ggtitle(title_list[i]) +
    #       theme(plot.title = element_text(size = title_size),
    #             axis.title = element_text(size = xaxis_size))
    #     p[[i]]
    #
    #   } else {
    #
    #     # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
    #     p[[i]] <- ggplot(drought_index, aes_string(x = "year", y = site_list[i],
    #                                                color = "ModelRun", linetype = "ModelRun")) +
    #       geom_line() + #col = color_list[i]
    #       theme_bw() +
    #       ylim(0, y_axis_max_list[i]) +
    #       ylab(ylab_list[i]) +
    #       xlab("Water Year") +
    #       ggtitle(title_list[i]) +
    #       theme(plot.title = element_text(size = title_size),
    #             axis.title = element_text(size = xaxis_size))
    #     p[[i]]
    #
    #   }
    #
    #
    # }
    #
    # ggsave(
    #   paste(model_folder, "/May 1 Annual Reservoir Time Series with Boulder Res 3x2 ",base_model_version, ".png", sep = ""),
    #   width = 14, height = 8,
    #   grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], nrow = 3,
    #                top = "May 1 Annual Reservoir Time Series Plots",
    #                right = ""))



    # Mass Balance by Source calcs & plots ------------------------------------


    # get the data
    water_by_source_list <- list()
    for (i in 1:n_file_prefix){

      water_by_source_list[[i]] <- data_list[[i]] %>%
        # group by year to calculate annual sums
        group_by(year, ModelRun) %>%
        # link 447 = COB Demand Met by Direct Exchange,
        # Link 448 = COB Demand Met by Reservoir Releases
        # link 406 = COB demand met by direct flow rights
        # DataObject_23_Flow = COB demand before demand reduction
        # Demand_58_Flow = COB indoor demand
        # Demand_91_Flow = COB outdoor demand
        # Link 499 = windy gap inflow
        # Decree 75 = CBT inflow
        select(year, qm, Date, ModelRun, Link_447_Flow, Link_448_Flow, Link_406_Flow,
               DataObject_23_Flow, Demand_58_Flow, Demand_91_Flow,
               Decree_75_Flow, Link_499_Flow) %>%
        # sum the sources by year
        summarize(Direct_Exchange = sum(Link_447_Flow), Reservoir_Release = sum(Link_448_Flow),
                  Direct_Flow_Rights = sum(Link_406_Flow), COB_Water_Demand = sum(DataObject_23_Flow),
                  COB_Indoor_Demand = sum(Demand_58_Flow), COB_Outdoor_Demand = sum(Demand_91_Flow),
                  CBT_Inflow = sum(Decree_75_Flow), WindyGap_Inflow = sum(Link_499_Flow))
      ## faster way to do this (but it doesn't adjust column names)
      # summarise(across(Link_447_Flow:Link_406_Flow, sum))
      water_by_source_list[[i]]

    }


    # merge the data from the loops together
    water_by_source <- bind_rows(water_by_source_list[[1]], water_by_source_list[[2]])

    # Check the factor & levels for the 'ModelRun' column (we will plot by this)
    # factor(water_by_source$ModelRun)
    # levels(water_by_source$ModelRun)
    # set the factor 'levels' to the correct plotting order
    water_by_source$ModelRun <-factor(water_by_source$ModelRun, levels = c(scenario_name))
    # Levels should be updated
    levels(water_by_source$ModelRun)



    ### Time Series Plots for demand met by source w/COB Demands

    site_list <- c("Direct_Flow_Rights", "Reservoir_Release",
                   "Direct_Exchange", "COB_Water_Demand", "COB_Indoor_Demand", "COB_Outdoor_Demand")
    title_list <- c("COB Demand met by Direct Flow Rights", "COB Demand met by Reservoir Releases",
                    "COB Demand met by Direct Exchange", "COB Total Demand",
                    "COB Indoor Demand", "COB Outdoor Demand")
    n_site_list <- length(site_list)
    #x_axis_max_list <- c(18000, 18000, 4000, 30000, 15000, 16000)
    y_axis_max_list <- c(25000, 25000, 25000, 30000, 30000, 30000)
    p <- list()

    for (i in 1:n_site_list){

      # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
      p[[i]] <- ggplot(water_by_source, aes_string(x = "year", y = site_list[i], color = "ModelRun",
                                                   linetype = "ModelRun")) +
        geom_line() + #col = color_list
        theme_bw() +
        ylim(0, y_axis_max_list[i]) +
        ylab("Flow (af)") +
        xlab("Water Year") +
        ggtitle(title_list[i]) +
        theme(plot.title = element_text(size = title_size),
              axis.title = element_text(size=xaxis_size))
      p[[i]]

    }


    # define the plot name
    plot_title <- "1b. Annual Supply by Water Type with Demands - Time Series Plot"
    file_name <- paste(plot_title, " 3x2 ", sep = "")

    # # save the plot w/display
    # ggsave(
    #   paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
    #         output_folder, ".pdf", sep = ""),
    #   width = 14, height = 8,
    #   grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]],
    #                p[[5]], p[[6]], nrow = 3,
    #                top = plot_title, right = ""))

    p_final <- arrangeGrob(p[[1]], p[[2]], p[[3]], p[[4]],
                           p[[5]], p[[6]], nrow = 3, top = plot_title, right = "", left = "", bottom = "")

    # save the plot without the display
    ggsave(paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
                 output_folder, device_type, sep = ""),
           width = 14, height = 8, plot = p_final)


    rm(water_by_source_list, water_by_source, y_axis_max_list,
       site_list, title_list, n_site_list, p, plot_title, file_name)

    ############# Time Series Plots for demand met by source #################
    #
    # site_list <- c("Direct_Flow_Rights", "Reservoir_Release",
    #                 "Direct_Exchange", "COB_Water_Demand")
    # title_list <- c("COB Demand met by Direct Flow Rights", "COB Demand met by Reservoir Releases",
    #                "COB Demand met by Direct Exchange", "COB Total Demand")
    # n_site_list <- length(site_list)
    # y_axis_max_list <- c(18000, 20000, 6000, 30000)
    # p <- list()
    #
    # for (i in 1:n_site_list){
    #
    #   # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
    #   p[[i]] <- ggplot(water_by_source, aes_string(x = "year", y = site_list[i], color = "ModelRun",
    #                                                linetype = "ModelRun")) +
    #     geom_line() +
    #     theme_bw() +
    #     ylim(0, y_axis_max_list[i]) +
    #     ylab("Flow (af)") +
    #     xlab("Water Year") +
    #     ggtitle(title_list[i]) +
    #     theme(plot.title = element_text(size = title_size),
    #           axis.title = element_text(size=xaxis_size))
    #   p[[i]]
    #
    # }


    # # define the plot name
    # plot_title <- "Annual Supply by Water Type - Time Series Plot"
    # file_name <- paste(plot_title, " 2x2 ", sep = "")
    #
    # # plot the data
    # ggsave(
    #   paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
    #         output_folder, ".pdf", sep = ""),
    #   width = 14, height = 8,
    #   grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], nrow = 2,
    #                top = plot_title,
    #                right = ""))
    #
    #
    # rm(p)

    #### Time Series Plots for demand met by source w/CBT & WIndy Gap ###
    #
    # site_list <- c("Direct_Flow_Rights", "Reservoir_Release", "Direct_Exchange",
    #                "COB_Water_Demand", "CBT_Inflow", "WindyGap_Inflow")
    # title_list <- c("COB Demand met by Direct Flow Rights", "COB Demand met by Reservoir Releases",
    #                 "COB Demand met by Direct Exchange", "COB Total Demand",
    #                 "C-BT Inflow", "Windy Gap Inflow")
    # n_site_list <- length(site_list)
    # #color_list <- c("#4169E1", "#4169E1", "#4169E1", "black", "#00A36C", "#00A36C")
    # y_axis_max_list <- c(16000, 16000, 6000, 25000, 15000, 15000)
    # p <- list()
    #
    # for (i in 1:n_site_list){
    #
    #   # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
    #   p[[i]] <- ggplot(water_by_source, aes_string(x = "year", y = site_list[i], color = "ModelRun",
    #                                                linetype = "ModelRun")) +
    #     geom_line() + #col = color_list[i]
    #     theme_bw() +
    #     ylim(0, y_axis_max_list[i]) +
    #     ylab("Flow (af)") +
    #     xlab("Water Year") +
    #     ggtitle(title_list[i]) +
    #     theme(plot.title = element_text(size = title_size),
    #           axis.title = element_text(size=xaxis_size))
    #   p[[i]]
    #
    # }

    # ggsave(
    #   "Annual Demand met by Source with CBT Windy Gap Time Series 3x2.png",
    #   grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]],
    #                p[[5]], p[[6]], nrow =3,
    #                top = "Annual Demand met by Source Time Series Plot",
    #                right = ""))

    # ggsave(
    #   paste(model_folder, "/Annual Demand met by Source with CBT Windy Gap Time Series 5x1 ",base_model_version, ".png", sep = ""),
    #   width = 10, height = 11,
    #   grid.arrange(p[[1]], p[[2]], p[[3]], p[[5]], p[[6]], nrow =5,
    #                top = "Annual Demand met by Source Time Series Plot",
    #                right = ""))
    #




    # Mass Balance by Pipeline (1c) ------------------------------------------------

    water_by_pipe_list <- list()
    for (i in 1:n_file_prefix){

      water_by_pipe_list[[i]] <- data_list[[i]] %>%
        # group by year to calculate annual sums
        group_by(year, ModelRun) %>%
        # link 255 = Lakewood pipeline to Betasso
        # Link 263 = Barker Pipeline to Betasso
        # link 380 + 520 = Boulder Res to 63rd St WTP
        # Link 435 = Farmers Municipal Right to Boulder WTP
        select(year, qm, Date, ModelRun, Link_255_Flow, Link_263_Flow, Link_380_Flow,
               Link_520_Flow, Link_435_Flow) %>%
        # sum links 380 & link 520 for total Boulder Res to WTP
        mutate(BoulderWTPDeliveryTemp = rowMeans(across(c(Link_380_Flow, Link_520_Flow)), na.rm=T)) %>%
        # sum the sources by year
        summarize(LakewoodtoBetasso = sum(Link_255_Flow), BarkerGravitytoBetasso = sum(Link_263_Flow),
                  BoulderRestoWTP = sum(BoulderWTPDeliveryTemp), FarmersRighttoWTP = sum(Link_435_Flow))
      ## faster way to do this (but it doesn't adjust column names)
      # summarise(across(Link_447_Flow:Link_406_Flow, sum))
      water_by_pipe_list[[i]]

    }


    # merge the data from the loops together
    water_by_pipe <- bind_rows(water_by_pipe_list[[1]], water_by_pipe_list[[2]])

    ### Check the factor & levels for the 'ModelRun' column (we will plot by this)
    #factor(water_by_pipe$ModelRun)
    #levels(water_by_pipe$ModelRun)
    # set the factor 'levels' to the correct plotting order
    water_by_pipe$ModelRun <-factor(water_by_pipe$ModelRun, levels = c(scenario_name))
    # Levels should be updated
    levels(water_by_pipe$ModelRun)



    ### Time Series Plots for demand met by source ###
    site_list <- c("LakewoodtoBetasso", "BarkerGravitytoBetasso",
                   "BoulderRestoWTP", "FarmersRighttoWTP")
    title_list <- c("Lakewood Pipeline to Betasso WTP", "Barker Gravity Line to Betasso WTP",
                    "Boulder Reservoir to 63rd St WTP", "Farmers Right to WTP")
    n_site_list <- length(site_list)
    #color_list <- brewer.pal(4, "Set1")
    # y_axis_max_list <- c(12000, 12000, 6000, 3000)
    y_axis_max_list <- c(15000, 15000, 15000, 15000)
    p <- list()

    for (i in 1:n_site_list){

      # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
      p[[i]] <- ggplot(water_by_pipe, aes_string(x = "year", y = site_list[i], color = "ModelRun",
                                                 linetype = "ModelRun")) +
        geom_line() +  #col = color_list[i]
        theme_bw() +
        ylim(0, y_axis_max_list[i]) +
        ylab("Flow (af)") +
        xlab("Water Year") +
        ggtitle(title_list[i]) +
        theme(plot.title = element_text(size = title_size),
              axis.title = element_text(size = xaxis_size))
      #p[[i]]

    }


    # define the plot name
    plot_title <- "1c. Annual Water Delivery by Pipeline - Time Series Plot"
    file_name <- paste(plot_title, " 2x2 ", sep = "")


    ggsave(
      paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
            output_folder, device_type, sep = ""),
      width = 14, height = 8,
      grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], nrow = 2,
                   top = plot_title,
                   right = ""))

    rm(water_by_pipe_list, water_by_pipe, y_axis_max_list,
       site_list, title_list, n_site_list, p, plot_title, file_name)


    # CBT, Windy Gap, Reusable Water Exchange Analysis (2c, 2d) ---------------------------------------


    cbt_windygap_list <- list()
    for (i in 1:n_file_prefix){

      cbt_windygap_list[[i]] <- data_list[[i]] %>%
        # group by ModelRun to calculate values by group
        group_by(ModelRun) %>%
        # Link 499 = windy gap inflow
        # Decree 75 = CBT inflow
        # DataObject 29 = Windy Gap volume in Boulder Res
        # Link 388 = Boulder Reservoir COB Release Link
        # Link 569 = Reusable water exchange to Barker Res
        # Link 596 = Boulder Reusable exchange via Boulder Ck Supply Canal (BCSC)
        # Link 568 = Boulder Resuable exchange to Watershed (NBC) Reservoir
        # Link 350 = South Boulder Creek ISF Link
        # DataObject 28 = Reusable water in Barker Res
        # Dataobject 30 = Reusable water in watershed (NBC) Reservoir
        # Dataobject 41 = Windy Gap exchange from Boulder Reservoir
      # Dataobject 42 = WG exchange from Boulder Res to Watershed (NBC) Res
      # Dataobject 43 = WG exchange from Boulder Res to Barker Res
      select(year, qm, Date, ModelRun, Decree_75_Flow, Link_499_Flow,
             DataObject_29_Flow, Link_388_Flow, Link_569_Flow, Link_596_Flow,
             Link_568_Flow, Link_350_Flow, DataObject_28_Flow, DataObject_30_Flow,
             DataObject_41_Flow, DataObject_42_Flow, DataObject_43_Flow,
             Reservoir_1_Content) %>%
        ### calculate Boulder Res Windy Gap to the City
        # subtract storage between two time steps
        mutate(DO29diff = DataObject_29_Flow - lead(DataObject_29_Flow, n = 1)) %>%
        # shift these differences 1 timestep so the NA is the first value not the last value
        mutate(DO29diff2 = c(NA, DO29diff[1:length(DO29diff)-1])) %>%
        ### Rowwise Operations: perform these analysis for the specified columns for every row
        # take the min of the diff & link 388, then take the max of that or 0 to remove negative values
        rowwise() %>% mutate(BoulderResWGtoCity = max(min(DO29diff2, Link_388_Flow, na.rm = TRUE), 0, na.rm = TRUE)) %>%
        # sum the reusable storage Barker + NBC + Boulder
        rowwise() %>% mutate(ReuseStorage = sum(DataObject_28_Flow, DataObject_30_Flow, DataObject_29_Flow)) %>%
        # now, group data by Year & ModelRun to sum data annually
        group_by(year, ModelRun) %>%
        # sum the sources by year
        summarize(CBT_Inflow = sum(Decree_75_Flow), WindyGap_Inflow = sum(Link_499_Flow),
                  BoulderRes_WGtoCity = sum(BoulderResWGtoCity),
                  SBC_ISF = sum(Link_350_Flow),
                  BarkerRes_ReusableWater = max(DataObject_28_Flow),  #max storage by year
                  NBCRes_ReusableWater = max(DataObject_30_Flow),     #max storage by year
                  BoulderRes_ReusableWater = max(DataObject_29_Flow), #max storage by year
                  BoulderRes_WGExchtoBarker = sum(DataObject_43_Flow),
                  BoulderRes_WGExchtoNBCRes = sum(DataObject_42_Flow),
                  BoulderRes_WGExctoUpperStor = sum(DataObject_41_Flow),
                  NBCRes_Contents = mean(Reservoir_1_Content),
                  COB_Reusable_Contents = max(ReuseStorage)
        )
      cbt_windygap_list[[i]]

    }


    # merge the data from the loops together
    cbt_windygap <- bind_rows(cbt_windygap_list[[1]], cbt_windygap_list[[2]])

    ### Check the factor & levels for the 'ModelRun' column (we will plot by this)
    # factor(cbt_windygap$ModelRun)
    # levels(cbt_windygap$ModelRun)
    # # set the factor 'levels' to the correct plotting order
    cbt_windygap$ModelRun <-factor(cbt_windygap$ModelRun, levels = c(scenario_name))
    # Levels should be updated
    levels(cbt_windygap$ModelRun)

    # ### No Reuse scenario is list item #2. We want to remove some of these variables
    # cbt_windygap <- cbt_windygap %>%
    #   filter(ModelRun == scenario_name[1])




    ### Plot the annual time series ###
    site_list <- c("CBT_Inflow", "WindyGap_Inflow",
                   "BoulderRes_WGExchtoBarker", "BoulderRes_WGExchtoNBCRes",
                   "BoulderRes_WGExctoUpperStor", "BoulderRes_WGtoCity",
                   "SBC_ISF", "BarkerRes_ReusableWater", "NBCRes_ReusableWater",
                   "BoulderRes_ReusableWater", "COB_Reusable_Contents")
    title_list <- c("C-BT Inflow", "Windy Gap Inflow",
                    "Boulder Reservoir: Windy Gap Exch. to Barker Res",
                    "Boulder Reservoir: Windy Gap Exch. to NBC Res",
                    "Boulder Reservoir: Windy Gap Total Exch to Upper Storage",
                    "Boulder Reservoir: Windy Gap to City",
                    "South Boulder Creek Instream Flow",
                    "Barker Reservoir: Maximum Annual Reusable Water",
                    "NBC Reservoir: Maximum Annual Reusable Water",
                    "Boulder Reservoir: Maximum Annual Reusable Water",
                    "Barker + NBC + Boulder: Maximum Annual Reusable Contents")
    n_site_list <- length(site_list)
    y_axis_max_list <- c(18000, 18000, 4000, 4000, 4000, 3500, 50000, 12000, 12000, 12000, 20000)
    y_lab_list <- c(rep("Flow (af)", 7), rep("Contents (af)", 4))

    p <- list()

    for (i in 1:n_site_list){

      if (i <= 2){
        # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
        p[[i]] <- ggplot(cbt_windygap, aes_string(x = "year", y = site_list[i], color = "ModelRun",
                                                  linetype = "ModelRun")) +
          geom_line() +
          theme_bw() +
          ylim(0, y_axis_max_list[i]) +
          ylab(y_lab_list[i]) +
          xlab("Water Year") +
          ggtitle(title_list[i]) +
          theme(plot.title = element_text(size = title_size),
                axis.title = element_text(size = xaxis_size))
      }else {
        # plot only the "reuse" filtered timeseries
        # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
        p[[i]] <- ggplot(cbt_windygap, aes_string(x = "year", y = site_list[i], color = "ModelRun",
                                                  linetype = "ModelRun")) +
          geom_line() +
          theme_bw() +
          ylim(0, y_axis_max_list[i]) +
          ylab(y_lab_list[i]) +
          xlab("Water Year") +
          ggtitle(title_list[i]) +
          theme(plot.title = element_text(size = title_size),
                axis.title = element_text(size = xaxis_size))

      }

      # p[[i]]

    }

    # define the plot name
    plot_title <- "2c. CBT-Windy Gap Exchange Total Annual - Time Series Plot"
    file_name <- paste(plot_title, " 3x2 ", sep = "")

    # save the plot
    ggsave(
      paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
            output_folder, device_type, sep = ""),
      width = 14, height = 8,
      grid.arrange(p[[1]], p_quota, p[[2]], p[[3]], p[[4]], p[[5]], nrow = 3,
                   top = plot_title,
                   right = ""))



    # define the plot name
    plot_title <- "2d. Reusable Water Annual Storage - Time Series Plot"
    file_name <- paste(plot_title, " 3x2 ", sep = "")

    ggsave(
      paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
            output_folder, device_type, sep = ""),
      width = 14, height = 8,
      grid.arrange(p[[2]], p[[5]], p[[8]], p[[9]], p[[10]], p[[11]], nrow = 3,
                   top = plot_title,
                   right = ""))


    rm(cbt_windygap_list, cbt_windygap, y_axis_max_list,
       y_lab_list, site_list, title_list, n_site_list, p, plot_title, file_name)

    # ggsave(
    #   paste("Annual CBT-Windy Gap Exchange Time Series 4x2 ",base_model_version, ".png", sep = ""),
    #   grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]],
    #                p[[7]], p[[8]],  nrow =4,
    #                top = "Annual Demand met by Source Time Series Plot",
    #                right = ""))

    # ggsave(
    #   paste("CBT-Windy Gap Exchange Annual Total Time Series 5x2 ",base_model_version, ".png", sep = ""),
    #   grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]],
    #                p[[7]], p[[8]], p[[9]], p[[10]], nrow = 5,
    #                top = "CBT-Windy Gap Exchange Annual Total Time Series Plot",
    #                right = ""))



    # CBT Quota Tabulation ----------------------------------------------------

    # select the sites you want to plot
    site_selection <- c("year", "qm", "Month", "Date", "ModelRun", "Decree_75_Content", "Decree_75_Flow",
                        "DataObject_39_Flow", "DataObject_3_Flow", "DataObject_44_Flow")
    n_site_selection <- length(site_selection)
    site_start_no <- 6
    site_selection_short <- site_selection[site_start_no:n_site_selection]
    n_site_selection_short <- length(site_selection_short)


    # Get the half of the annual CBT water (Decree 75)
    ### cbt use extract decree 75 for QM's 1-24, which are off by 1 year from CBT accounting (decree 75 resets to 0 at QM25)
    cbt_dec_75_QM1_24_list <- list()
    for (i in 1:n_file_prefix){

      cbt_dec_75_QM1_24_list[[i]] <- data_list[[i]] %>%
        # select the columns of interest
        select(year, qm, ModelRun, Decree_75_Flow) %>%
        # get the Boulder CBT capacity QMs 1-24, Oct-Mar, before the decree is reset on April 1 (qm 25)
        # (the CBT decree capacity resets to 0 in model on QM 25)
        filter(qm <= 24) %>%
        # group by year and then sum the flow by year, so sum QMs 1-24
        group_by(year, ModelRun) %>%
        summarise(Decree75_QM1_24 = sum(Decree_75_Flow)) %>%
        # make a new column that offsets the year by 1 to reflect when that water was actually used
        # to align with the CBT quota which resets this Decree 75 on QM 25
        mutate(cbt_year_taken = year - 1)

    }
    cbt_dec_75_QM1_24 <- bind_rows(cbt_dec_75_QM1_24_list[[1]], cbt_dec_75_QM1_24_list[[2]]) %>%
      # order output by modelID then year (to help with column binds later)
      ungroup() %>%
      arrange(ModelRun, year) %>%
      filter(cbt_year_taken >= 1915 & cbt_year_taken <= 2013) %>%
      group_by(cbt_year_taken, ModelRun) %>%
      select(-year) %>%
      rename(year = cbt_year_taken)

    # Get the half of the annual CBT water (Decree 75)
    ### cbt use extract decree 75 for QM's 15-48 (these are NOT off by 1 year)
    cbt_dec_75_QM25_48_list <- list()
    for (i in 1:n_file_prefix){

      cbt_dec_75_QM25_48_list[[i]] <- data_list[[i]] %>%
        # select the columns of interest
        select(year, qm, ModelRun, Decree_75_Flow) %>%
        # get the Boulder CBT capacity QMs 1-24, Oct-Mar, before the decree is reset on April 1 (qm 25)
        # (the CBT decree capacity resets to 0 in model on QM 25)
        filter(qm >= 25) %>%
        # group by year and then sum the flow by year, so sum QMs 1-24
        group_by(year, ModelRun) %>%
        summarise(Decree75_QM25_48 = sum(Decree_75_Flow))


    }
    cbt_dec_75_QM25_48 <- bind_rows(cbt_dec_75_QM25_48_list[[1]], cbt_dec_75_QM25_48_list[[2]]) %>%
      # order output by modelID then year (to help with column binds later)
      arrange(ModelRun, year) %>%
      filter(year >= 1915 & year <= 2013)



    ### run an annual analysis
    extract_list1 <- list()
    for (i in 1:n_file_prefix){

      extract_list1[[i]] <- data_list[[i]] %>%
        # select the columns of interest from the vector above using !!!syms to read it properly
        select(year, qm, ModelRun, DataObject_3_Flow) %>%
        # grab data from QM 24, which is when the max value is set, before it's cleared on QM 25.
        filter(qm == 24) %>%
        # remove last year to align with Decree 75 qm 24
        filter(year >= 1915 & year <= 2013)
      #group_by(year, ModelRun) %>%
      # faster way to do this (but it doesn't adjust column names)
      # get the annual cbt decree sum
      #summarise(across(site_selection[site_start_no]:site_selection[n_site_selection], sum))

    }

    # merge the data from the loops together
    annual_extract1 <- bind_rows(extract_list1[[1]], extract_list1[[2]]) %>%
      group_by(ModelRun) %>%
      rename(qm24 = qm) %>%
      # order the data so it lines up with the other dataset
      arrange(., ModelRun, year)


    ### run an annual analysis part 2
    extract_list2 <- list()
    for (i in 1:n_file_prefix){

      extract_list2[[i]] <- data_list[[i]] %>%
        # select the columns of interest from the vector above using !!!syms to read it properly
        select(year, qm, ModelRun, DataObject_39_Flow, DataObject_44_Flow) %>%
        # grab data from QM 25, when these values are set.
        filter(qm == 25) %>%
        # remove last year to align with Decree 75 qm 24
        filter(year >= 1915 & year <= 2013)
      #group_by(year, ModelRun) %>%
      # faster way to do this (but it doesn't adjust column names)
      # get the annual cbt decree sum
      #summarise(across(site_selection[site_start_no]:site_selection[n_site_selection], sum))

    }

    # merge the data from the loops together
    annual_extract2 <- bind_rows(extract_list2[[1]], extract_list2[[2]]) %>%
      group_by(ModelRun) %>%
      rename(qm25 = qm) %>%
      # order the data so it lines up with the other dataset
      arrange(., ModelRun, year)

    # get the actual CBT quota
    Quota_annual2 <- Quota_annual %>%
      arrange(., ModelRun, Year) %>%
      # remove last year to align with Decree 75 qm 24
      filter(Year >= 1915 & Year <= 2013) %>%
      mutate(ModelRun2 = c(rep(scenario_name[1], 99), rep(scenario_name[2], 99))) %>%
      arrange(., ModelRun2, Year) %>%
      mutate(COB_CBT_allotment = Quota * 21174)

    # merge the two datasets together
    annual_extract <- bind_cols(annual_extract1, annual_extract2) %>%
      bind_cols(., Quota_annual2) %>%
      bind_cols(., cbt_dec_75_QM1_24) %>%
      bind_cols(., cbt_dec_75_QM25_48) %>%
      select(year...1, ModelRun...3, DataObject_3_Flow, DataObject_44_Flow,
             Decree75_QM1_24, Decree75_QM25_48, COB_CBT_allotment) %>%
      rename(year = year...1, ModelRun = ModelRun...3) %>%
      rowwise() %>% mutate(COB_CBT_NormalUse = sum(Decree75_QM1_24, Decree75_QM25_48)) %>%
      rowwise() %>% mutate(COB_CBT_BorrowedWinter = (DataObject_3_Flow - DataObject_44_Flow)) %>%
      rowwise() %>% mutate(COB_CBT_YeartoYearDebt = (DataObject_44_Flow)) %>%
      rowwise() %>% mutate(COB_CBT_TotalUse = sum(COB_CBT_NormalUse, COB_CBT_BorrowedWinter, COB_CBT_YeartoYearDebt)) %>%
      rowwise() %>% mutate(COB_CBT_Unused = round(COB_CBT_allotment - COB_CBT_TotalUse, 0))


    cbt_group_description <- data.frame(Name = c("DataObject_3_Flow", "Decree75_QM1_24", "Decree75_QM25_48", "DataObject_44_Flow",
                                                 "COB_CBT_allotment", "COB_CBT_TotalUse", "COB_CBT_Unused",
                                                 "COB_CBT_NormalUse", "COB_CBT_BorrowedWinter", "COB_CBT_YeartoYearDebt"),
                                        Group = c("CBT Model Component", "CBT Model Component", "CBT Model Component", "CBT Model Component",
                                                  "Total CBT", "CBT Summary", "CBT Summary",
                                                  "CBT Component", "CBT Component", "CBT Component"))

    # add the new variables to 'definitions'
    temp_new_data <- matrix(NA, ncol = 4, nrow = 8)
    temp_new_data[1, 1:4] <- c("Decree75_QM1_24", "COB CBT use QM1-24", "Flow", "Flow (af)")
    temp_new_data[2, 1:4] <- c("Decree75_QM25_48", "COB CBT use QM25-48", "Flow", "Flow (af)")
    temp_new_data[3, 1:4] <- c("COB_CBT_allotment", "Annual CBT Allotment (af)", "Flow", "Flow (af)")
    temp_new_data[4, 1:4] <- c("COB_CBT_TotalUse", "COB Total CBT Water Used", "Flow", "Flow (af)")
    temp_new_data[5, 1:4] <- c("COB_CBT_Unused", "COB Unused CBT Water", "Flow", "Flow (af)")
    temp_new_data[6, 1:4] <- c("COB_CBT_NormalUse", "COB normal use of CBT water", "Flow", "Flow (af)")
    temp_new_data[7, 1:4] <- c("COB_CBT_BorrowedWinter", "COB borrowed CBT winter water", "Flow", "Flow (af)")
    temp_new_data[8, 1:4] <- c("COB_CBT_YeartoYearDebt", "COB CBT debt water", "Flow", "Flow (af)")
    temp_new_data <- as.data.frame(temp_new_data)
    colnames(temp_new_data) <- c("Name", "Description", "Parameter", "Units")

    definitions2 <- bind_rows(definitions, temp_new_data)


    # convert from wide to long format
    annual_extract_long <- annual_extract %>%
      pivot_longer(., cols = c(DataObject_3_Flow, Decree75_QM1_24, Decree75_QM25_48, DataObject_44_Flow,
                               COB_CBT_allotment, COB_CBT_TotalUse, COB_CBT_Unused,
                               COB_CBT_NormalUse, COB_CBT_BorrowedWinter, COB_CBT_YeartoYearDebt),
                   names_to = "Name", values_to = "Value") %>%
      left_join(., cbt_group_description, by = "Name") %>%
      left_join(., definitions2, by = "Name")



    g <- list()
    b <- list()
    for (i in 1:n_scenario_name){

      # # subset the data for only the data needed & filter for the necessary scenario
      # cbt_unused_extract_plot <- cbt_water_used_extract2 %>%
      #   select(cbt_year_taken, ModelRun, MaxQuotaWater, UsedQuota, UnusedQuota) %>%
      #   filter(ModelRun == scenario_name[i]) %>%
      #   pivot_longer(., cols = c(UsedQuota, UnusedQuota), names_to = "CBT_Water", values_to = "Flow_af")
      # annual_extract_long_plot <- annual_extract_long %>%
      #   filter(Group == "CBT Component" & ModelRun == scenario_name[i])


      # make a stacked area plot of the reusable supply scenario
      g[[i]] <- ggplot() +
        geom_bar(data = filter(annual_extract_long, Group == "CBT Component" & ModelRun == scenario_name[i]),
                 aes_string(x = "year", y = "Value", fill = "Description"),
                 position = "stack", stat = "identity", color = "black", size = 0.05) + #
        geom_line(data = filter(annual_extract_long, Group == "Total CBT" & ModelRun == scenario_name[i]),
                  aes_string(x = "year", y = "Value", color = "Name"),
                  size = 0.75) +
        scale_color_manual(values = "black") +
        theme_bw() +
        ylab("Flow (af)") +
        xlab("Water Year") +
        ylim(0, 22000) +
        scale_x_continuous(limits = c(1914, 2016), breaks = seq(1915, 2015, by = 5)) +
        ggtitle(paste(scenario_name[i], ": CBT Water by Year", sep = "")) +
        theme(plot.title = element_text(size = title_size),
              axis.title = element_text(size = xaxis_size))
      #scale_fill_manual(values = c("#0073C2FF", "#EFC000FF", ))
      g[[i]]

      annual_extract_long2 <- annual_extract_long %>%
        filter(Group == "CBT Summary", Description == "COB Unused CBT Water")

      b[[i]] <- ggplot() +
        geom_bar(data = filter(annual_extract_long2, Group == "CBT Summary" & ModelRun == scenario_name[i]),
                 aes_string(x = "year", y = "Value", fill = "Description"),
                 position = "stack", stat = "identity", color = "black", size = 0.05) + #
        geom_line(data = filter(annual_extract_long, Group == "Total CBT" & ModelRun == scenario_name[i]),
                  aes_string(x = "year", y = "Value", color = "Name"),
                  size = 0.75) +
        scale_color_manual(values = "black") +
        theme_bw() +
        ylab("Flow (af)") +
        xlab("Water Year") +
        ylim(0, 22000) +
        scale_x_continuous(limits = c(1914, 2016), breaks = seq(1915, 2015, by = 5)) +
        ggtitle(paste(scenario_name[i], ": CBT Water by Year", sep = "")) +
        theme(plot.title = element_text(size = title_size),
              axis.title = element_text(size = xaxis_size))
      #scale_fill_manual(values = c("#0073C2FF", "#EFC000FF", ))
      b[[i]]

    }

    # define the plot name
    plot_title <- "2a. C-BT Annual Water Use"
    file_name <- paste(plot_title, " 2x1 ", sep = "")

    ggsave(
      paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
            output_folder, device_type, sep = ""),
      width = 14, height = 8,
      grid.arrange(g[[1]], g[[2]], nrow = 2,
                   top = plot_title,
                   right = ""))


    plot_title <- "2ab. COB C-BT Annual Unused Water"
    file_name <- paste(plot_title, " 2x1 ", sep = "")

    ggsave(
      paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
            output_folder, device_type, sep = ""),
      width = 14, height = 8,
      grid.arrange(b[[1]], b[[2]], nrow = 2,
                   top = plot_title,
                   right = ""))


    ### New annual table analysis
    annual_extract_table <-
      annual_extract %>%
      select(year, ModelRun, COB_CBT_TotalUse, COB_CBT_Unused) %>%
      group_by(ModelRun) %>%
      summarise('CBT Use (mean)' = round(mean(COB_CBT_TotalUse),0), 'CBT Use (min)' = min(COB_CBT_TotalUse),
                'CBT Use (max)' = max(COB_CBT_TotalUse),
                'Unused CBT (mean)' = round(mean(COB_CBT_Unused),0), 'Unused CBT (min)' = min(COB_CBT_Unused),
                'Unused CBT (max)' = max(COB_CBT_Unused))
    annual_extract_table

    # when COB borrows water from CBT (year-to-year debt) it creates a negative CBT use (debit).
    # adjust the minimum value (if zero) so that it's not negative
    annual_extract_table$`Unused CBT (min)` <- if_else(annual_extract_table$`Unused CBT (min)` < 0, 0, annual_extract_table$`Unused CBT (min)`)


    # ### run a monthly analysis
    # extract_list <- list()
    # for (i in 1:n_file_prefix){
    #
    #   extract_list[[i]] <- data_list[[i]] %>%
    #     #group_by(ModelRun) %>%
    #     select(!!!syms(site_selection)) %>%
    #     group_by(year, Month, ModelRun) %>%
    #     summarise(across(site_selection[6]:site_selection[n_site_selection], sum))
    #
    #   # %>%
    #   #   left_join(., qm_convert, by = c("qm" = "QM")) %>%
    #   #   select(-'WY-QM', -Water.Year, -wyqm, -Cal.Year, -'QM start day', -'QM end day',
    #   #          -Start.Date, -End.Date, -'Days in QM')
    #   #   #group_by(qm, ModelRun) %>%
    #     # faster way to do this (but it doesn't adjust column names)
    #     # summarise(across(site_selection[5]:site_selection[n_site_selection], mean))
    #
    # }
    # extract_list[[1]]
    #
    # # merge the data from the loops together
    # monthly_extract <- bind_rows(extract_list[[1]], extract_list[[2]]) %>%
    #   group_by(Month, ModelRun) %>%
    #   summarise(., Min=min(Decree_75_Flow),
    #             Average=round(mean(Decree_75_Flow), 0), Max=max(Decree_75_Flow)) %>%
    #   pivot_wider(names_from = c(ModelRun), values_from = c(Min, Average, Max)) %>%
    #   # it's more complicated to rename the columns a variable names
    #   # use the !! (bang, bang) operator before paste to name the function
    #   # then use := operator to force names on the LHS of the equation.
    #   rename(!!(paste(scenario_name[1], " (Min)", sep = "")) :=  paste("Min_", scenario_name[1], sep = ""),
    #          !!(paste(scenario_name[1], " (Avg)", sep = "")) :=  paste("Average_", scenario_name[1], sep = ""),
    #          !!(paste(scenario_name[1], " (Max)", sep = "")) :=  paste("Max_", scenario_name[1], sep = "")
    #          ) %>%
    #   rename(!!(paste(scenario_name[2], " (Min)", sep = "")) :=  paste("Min_", scenario_name[2], sep = ""),
    #          !!(paste(scenario_name[2], " (Avg)", sep = "")) :=  paste("Average_", scenario_name[2], sep = ""),
    #          !!(paste(scenario_name[2], " (Max)", sep = "")) :=  paste("Max_", scenario_name[2], sep = "")
    #   ) %>%
    #   relocate(paste(scenario_name[2], " (Avg)", sep = ""), .after = paste(scenario_name[2], " (Min)", sep = "")) %>%
    #   relocate(paste(scenario_name[2], " (Max)", sep = ""), .after = paste(scenario_name[2], " (Min)", sep = ""))
    #
    #
    #
    #
    # ### Calculate the remaining CBT Quota
    # site_selection <- c("year", "qm", "Month", "Date", "ModelRun", "Decree_75_Content",
    #                     "DataObject_3_Flow")
    # n_site_selection <- length(site_selection)
    #
    # ### run an annual analysis
    # extract_list <- list()
    # for (i in 1:n_file_prefix){
    #
    #   extract_list[[i]] <- data_list[[i]] %>%
    #     # select the columns of interest from the vector above using !!!syms to read it properly
    #     select(!!!syms(site_selection)) %>%
    #     # get the Boulder CBT capacity on March 31, before the new quota is called on April 1
    #     # (the CBT decree capacity resets to 0 in model on QM 25)
    #     filter(qm == 24) %>%
    #     # make a new column that offsets the year by 1 to reflect when that water was actually used
    #     mutate(cbt_year_taken = year - 1) %>%
    #     select(-Month, -DataObject_3_Flow)
    #
    # }
    # cbt_water_used_extract <- bind_rows(extract_list[[1]], extract_list[[2]]) %>%
    #   # order output by modelID then year (to help with column binds later)
    #   arrange(ModelRun, year)
    #
    #
    #
    # ### get the annual CBT borrow/carryover water (DataObject 3)
    # cob_cbt_borrow_list <- list()
    # for (i in 1:n_file_prefix){
    #
    #   cob_cbt_borrow_list[[i]] <- data_list[[i]] %>%
    #     # select the columns of interest from the vector above using !!!syms to read it properly
    #     select(!!!syms(site_selection)) %>%
    #     # get the Boulder CBT borrow volume, which is decided on QM 5 in the model
    #     filter(qm == 5) %>%
    #     # make a new column that offsets the year by 1 to reflect when that water was actually used
    #     #mutate(cbt_year_taken = year - 1) %>%
    #     select(-Month, -Decree_75_Content) %>%
    #     # order output by modelID then year (to help with column binds later)
    #     arrange(ModelRun, year) %>%
    #     # rename date column so it is distinct from others
    #     rename(Date_QM5 = Date)
    #
    # }
    # cob_cbt_borrow <- bind_rows(cob_cbt_borrow_list[[1]], cob_cbt_borrow_list[[2]]) %>%
    #   # filter the years to match the cbt water taken vs water year
    #   filter(year >= 1915 & year <= 2013)
    #
    #
    # # get the cbt quota data & remove the last year of data
    # Quota_annual2 <- Quota_annual %>%
    #   filter(Year >= 1915 & Year <= 2013)
    #
    #
    # # # merge the data from the loops together
    # # cbt_unused_extract <- bind_rows(extract_list[[1]], extract_list[[2]]) %>%
    # #   arrange(ModelRun, year)
    #
    # # merge the data from the loops together
    # cbt_water_used_extract2 <- cbt_water_used_extract %>%
    #   # filter the years to match the cbt water taken vs water year
    #   filter(cbt_year_taken >= 1915 & cbt_year_taken <= 2013) %>%
    #   # add the annual COB CBT borrow water
    #   bind_cols(., cob_cbt_borrow) %>%
    #   # add the actual annual c-bt quota
    #   bind_cols(., Quota_annual2) %>%
    #   # calculate the maximum COB CBT quota water they can use
    #   mutate(MaxQuotaWater = 21174 * Quota) %>%
    #   # calculate CBT borrow + normal cbt COB quota use
    #   mutate(UsedQuota = Decree_75_Content + DataObject_3_Flow) %>%
    #   # calculate the unused quota water
    #   mutate(UnusedQuota = MaxQuotaWater - UsedQuota) %>%
    #   # rename duplicate columns from merging datasets
    #   rename(ModelRun = ModelRun...4, ModelRunBorrow = ModelRun...10, ModelRunQuota = ModelRun...13)
    #
    # cbt_unused_extract_table <- cbt_water_used_extract2 %>%
    #   # calculate annual min, max, avg of unused quota water
    #   group_by(ModelRun) %>%
    #   summarise(., Average=round(mean(UnusedQuota),0), Max=max(UnusedQuota))
    #
    # g <- list()
    # for (i in 1:n_scenario_name){
    #
    #   # subset the data for only the data needed & filter for the necessary scenario
    #   cbt_unused_extract_plot <- cbt_water_used_extract2 %>%
    #     select(cbt_year_taken, ModelRun, MaxQuotaWater, UsedQuota, UnusedQuota) %>%
    #     filter(ModelRun == scenario_name[i]) %>%
    #     pivot_longer(., cols = c(UsedQuota, UnusedQuota), names_to = "CBT_Water", values_to = "Flow_af")
    #
    #
    #   # make a stacked area plot of the reusable supply scenario
    #   g[[i]] <- ggplot(cbt_unused_extract_plot, aes_string(x = "cbt_year_taken", y = "Flow_af", fill = "CBT_Water")) +
    #     geom_bar(position="stack", stat="identity", color = "black", size = 0.05) + #
    #     #geom_line(aes_string(x = "cbt_year_taken", y = "MaxQuotaWater"), size = 0.75, color = "black") +
    #     #geom_area() +  #col = color_list[i]
    #     theme_bw() +
    #     ylab("Flow (af)") +
    #     xlab("Water Year") +
    #     ylim(0, 22000) +
    #     ggtitle(paste(scenario_name[i], ": CBT Water by Year", sep = "")) +
    #     theme(plot.title = element_text(size = title_size),
    #           axis.title = element_text(size = xaxis_size))
    #   #scale_fill_manual(values = c("#0073C2FF", "#EFC000FF", ))
    #   #g[[i]]
    #
    #
    # }

    # # define the plot name
    # plot_title <- "2a. C-BT Annual Water Use"
    # file_name <- paste(plot_title, " 2x1 ", sep = "")
    #
    # ggsave(
    #   paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
    #         output_folder, ".pdf", sep = ""),
    #   width = 14, height = 8,
    #   grid.arrange(g[[1]], g[[2]], nrow = 2,
    #                top = plot_title,
    #                right = ""))



    ### export the two datasets as tables to ggarrange
    # set the theme, sizes
    size <- 1
    size1 <- 1
    tt <- ttheme_default(core = list(fg_params=list(cex = size)),
                         colhead = list(fg_params=list(cex = size1)),
                         rowhead = list(fg_params=list(cex = size)))


    ### Table 1
    tbl_temp_a <- tableGrob(annual_extract_table, theme = tt, rows = NULL)
    # add grid around the headers
    tbl_temp_a <- gtable_add_grob(tbl_temp_a,
                                  grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                                  t = 1, l = 1, r = ncol(tbl_temp_a))
    # add box around the first column of data
    tbl_temp_a <- gtable_add_grob(tbl_temp_a,
                                  grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                                  t = 1, b = nrow(tbl_temp_a), l = 1, r = 1)
    # add box around the second model run of data
    tbl_temp_a <- gtable_add_grob(tbl_temp_a,
                                  grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                                  t = 1, b = nrow(tbl_temp_a), l = 2, r = 4)
    # add box around the second model run of data
    tbl_temp_a <- gtable_add_grob(tbl_temp_a,
                                  grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                                  t = 1, b = nrow(tbl_temp_a), l = 5, r = ncol(tbl_temp_a))

    grid.draw(tbl_temp_a)


    # ### Table 2
    # # add box around the column headers
    # tbl_temp_b <- tableGrob(monthly_extract, theme = tt, rows = NULL)
    # # add grid around the headers
    # tbl_temp_b <- gtable_add_grob(tbl_temp_b,
    #                              grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
    #                              t = 1, l = 1, r = ncol(tbl_temp_b))
    # # add box around the first column of data
    # tbl_temp_b <- gtable_add_grob(tbl_temp_b,
    #                              grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
    #                              t = 1, b = nrow(tbl_temp_b), l = 1, r = 1)
    # # add box around the first model run of data
    # tbl_temp_b <- gtable_add_grob(tbl_temp_b,
    #                              grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
    #                              t = 1, b = nrow(tbl_temp_b), l = 2, r = 4)
    # # add box around the second model run of data
    # tbl_temp_b <- gtable_add_grob(tbl_temp_b,
    #                               grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
    #                               t = 1, b = nrow(tbl_temp_b), l = 5, r = ncol(tbl_temp_b))
    # grid.draw(tbl_temp_b)
    #
    #
    # ### Table 3
    # tbl_temp_c <- tableGrob(cbt_unused_extract_table, theme = tt, rows = NULL)
    # # add grid around the headers
    # tbl_temp_c <- gtable_add_grob(tbl_temp_c,
    #                               grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
    #                               t = 1, l = 1, r = ncol(tbl_temp_c))
    # # add box around the first model run of data
    # tbl_temp_c <- gtable_add_grob(tbl_temp_c,
    #                               grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
    #                               t = 2, b = nrow(tbl_temp_c), l = 1, r = ncol(tbl_temp_c))
    # # add box around the first column of data
    # tbl_temp_c <- gtable_add_grob(tbl_temp_c,
    #                               grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
    #                               t = 1, b = nrow(tbl_temp_c), l = 1, r = 1)
    #
    # grid.draw(tbl_temp_c)
    #
    #
    #
    # # define the plot name
    # plot_title <- "2b. C-BT Quota Annual and Monthly Summary Tables"
    # file_name <- paste(plot_title, " 2x2 ", sep = "")
    #
    # # # save the plot
    # # ggsave(
    # #   paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
    # #         output_folder, ".pdf", sep = ""),
    # #   width = 14, height = 8,
    # #   grid.arrange(tbl_temp_a, tbl_temp_c, tbl_temp_b, nrow = 2,
    # #                top = plot_title,
    # #                right = "", bottom = "",
    # #                layout_matrix = rbind(c(1, 2),
    # #                                      c(3, 3))),
    # #   grid.text("Plot header", x = unit(0.5, "npc"), y = unit(.51, "npc"),
    # #             gp = gpar(fontsize=20, fontfamily="Times New Roman")))

    plot_title <- "2ac. COB C-BT Annual Water Use"
    file_name <- paste(plot_title, " 1x1 ", sep = "")

    # save the plot
    # pdf(
    #   paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
    #         output_folder, ".pdf", sep = ""),
    #   width = 14, height = 8)

    # grid.arrange(tbl_temp_a, tbl_temp_c, tbl_temp_b, nrow = 2,
    #              top = plot_title,
    #              right = "", bottom = "",
    #              layout_matrix = rbind(c(1, 1),
    #                                    c(3, 3)))
    tbl_temp_a2 <- grid.arrange(tbl_temp_a, nrow = 1,
                                top = plot_title,
                                right = "", bottom = "",
                                layout_matrix = rbind(c(1, 1),
                                                      c(3, 3)))

    ggsave(
      filename =  paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",                    output_folder, ".png", sep = ""),
      width = 12,
      height = 8,
      tbl_temp_a2
    )
    grid.text("Annual C-BT Water Use", x = unit(0.5, "npc"), y = unit(0.81, "npc"),
              gp = gpar(fontsize = 14))
    # grid.text("Annual Unused C-BT Water", x = unit(0.75, "npc"), y = unit(0.81, "npc"),
    #           gp = gpar(fontsize = 14))
    # grid.text("Monthly C-BT Water", x = unit(0.5, "npc"), y = unit(0.52, "npc"),
    #           gp = gpar(fontsize = 14))

    dev.off()

    # rm(site_selection, n_site_selection, extract_list, extract, annual_extract, monthly_extract,
    #    cob_cbt_borrow_list, cob_cbt_borrow, Quota_annual2,
    #    cbt_unused_extract_table, cbt_unused_extract_plot, g, tbl_temp_a,
    #    tbl_temp_b, tbl_temp_c, cbt_water_used_extract,
    #    cbt_water_used_extract2)


    # Reservoir Reusable Storage Annual ----------------------------------------------


    # make a look up between CRAM element and object 'type'
    cram_type = data.frame(Name = c("DataObject_30_Flow", "Reservoir_1_Content",
                                    "DataObject_28_Flow", "Reservoir_3_Content",
                                    "DataObject_29_Flow", "DataObject_1_Flow"),
                           Type = c("Reusable Water", "Total Contents",
                                    "Reusable Water", "Total Contents",
                                    "Reusable Water", "Total COB Contents"),
                           Group = c("NBC Reservoir", "NBC Reservoir",
                                     "Barker Reservoir", "Barker Reservoir",
                                     "Boulder Reservoir", "Boulder Reservoir"))

    # select the sites you want to plot
    site_selection <- c("year", "qm", "Date", "ModelRun", "DataObject_30_Flow",
                        "Reservoir_1_Content", "DataObject_28_Flow", "Reservoir_3_Content",
                        "DataObject_29_Flow", "DataObject_1_Flow")
    n_site_selection <- length(site_selection)

    extract_list <- list()
    for (i in 1:n_file_prefix){

      extract_list[[i]] <- data_list[[i]] %>%
        group_by(ModelRun) %>%
        select(!!!syms(site_selection)) %>%
        group_by(year, ModelRun) %>%
        # faster way to do this (but it doesn't adjust column names)
        summarise(across(site_selection[5]:site_selection[n_site_selection], mean))

    }

    # merge the data from the loops together
    extract <- bind_rows(extract_list[[1]], extract_list[[2]])

    ### Check the factor & levels for the 'ModelRun' column (we will plot by this)
    #factor(extract$ModelRun)
    #levels(extract$ModelRun)
    # # set the factor 'levels' to the correct plotting order
    extract$ModelRun <-factor(extract$ModelRun, levels = c(scenario_name))
    # Levels should be updated
    levels(extract$ModelRun)
    # get size of dataset
    n_size_data <- dim(extract)

    # ### No Reuse scenario is list item #2. We want to remove some of these variables
    extract2 <- extract %>%
      filter(ModelRun == scenario_name[1]) %>%
      pivot_longer(., cols = c(3:n_size_data[2]),
                   names_to = "Name", values_to = "Contents_af") %>%
      left_join(., definitions, by = "Name") %>%
      left_join(., cram_type, by = "Name")


    # %>%
    #   mutate(Type = c(rep("NBC", 2), rep("Barker", 2), rep("Boulder", 2)))

    # Get the names & titles
    # skip the first two columns which are the groups, year & ModelRun, but take the rest
    site_list <- data.frame(Name = colnames(extract)[3:length(extract)])
    site_list_join <- left_join(site_list, definitions, by = "Name")
    title_list_export <- site_list_join$Description
    site_list_export <- site_list_join$Name
    y_axis_label_list <- site_list_join$Parameter

    # ### Plot the annual time series ###
    # site_list <- c("NBCMaxReusableContents", "NBCMaxContents",
    #                "BarkerMaxReusableContents", "BarkerMaxContents",
    #                "BoulderMaxReusableContents", "BoulderMaxContents")
    # title_list <- c("NBCMaxReusableContents", "NBCMaxContents",
    #                 "BarkerMaxReusableContents", "BarkerMaxContents",
    #                 "BoulderMaxReusableContents", "BoulderMaxContents")


    n_site_list <- length(site_list)
    #y_axis_max_list <- c(14500, 14500, 2000, 2000, 2000, 3500, 50000, 12000, 22000, 12000)
    y_lab_list <- c(rep("Flow (af)", 7), rep("Contents (af)",3))
    storage_max_list <- c(6927, 11277, NA) #NBC Res, Barker Res, Boulder Res (summer storage)
    #storage_max_list2 <- c(NA, NA, 8500)   #NBC Res, Barker Res, Boulder Res (winter + summer storage)

    # make a list of the things we want to plot
    filter_items_temp <- extract2 %>%
      ungroup() %>%
      distinct(., Group)


    # filter_items_temp <- c("NBC", "Barker", "Boulder")
    # title_list_temp <- c("North Boulder Creek Reservoir: ", "Barker Reservoir: ",
    #                      "Boulder Reservoir: ")

    p <- list()
    for (i in 1:dim(filter_items_temp)[1]){

      # extract data for 1 reservoir at a time
      extract2a <- extract2 %>%
        filter(Group == as.character(filter_items_temp[i, 1]))

      # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
      p[[i]] <- ggplot(extract2a, aes_string(x = "year", y = "Contents_af", color = "Type",
                                             linetype = "Type")) +
        geom_line() +
        geom_hline(yintercept = storage_max_list[i], color = "red") +
        #geom_hline(yintercept = storage_max_list2[i], color = "red") +
        theme_bw() +
        ylim(0, 12500) +
        ylab("Contents (af)") +
        xlab("Water Year") +
        ggtitle(paste(filter_items_temp[i, 1], ": Reusable Water (", compare_model_ID, ")", sep = "")) +
        theme(plot.title = element_text(size = title_size),
              axis.title = element_text(size = xaxis_size)) +
        scale_color_manual(values = c("blue", "black")) +
        scale_linetype_manual(values = c("dashed", "solid"))

      #p[[i]]

    }



    # define the plot name
    plot_title <- "2e. Reusable Water in Reservoir - Average Annual Time Series Plot"
    file_name <- paste(plot_title, " 2x2 ", sep = "")

    ggsave(
      paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
            output_folder, device_type, sep = ""),
      width = 14, height = 8,
      grid.arrange(p[[1]], p[[2]], p[[3]], nrow = 2,
                   top = plot_title,
                   right = ""))



    rm(extract, extract2, extract2a, p, plot_title, file_name, n_site_list, y_lab_list,
       filter_items_temp, site_list, site_list_join, title_list_export, site_list_export,
       y_axis_label_list, storage_max_list, storage_max_list2)


    # Reusable Water in Upper Reservoir Avg Quarter-Monthly Plot --------------------------------


    # make a look up between CRAM element and object 'type'
    cram_type = data.frame(Name = c("DataObject_30_Flow", "Reservoir_1_Content",
                                    "DataObject_28_Flow", "Reservoir_3_Content",
                                    "DataObject_29_Flow", "DataObject_1_Flow"),
                           Type = c("Reusable Water", "Total Contents",
                                    "Reusable Water", "Total Contents",
                                    "Reusable Water", "Total COB Contents"),
                           Group = c("NBC Reservoir", "NBC Reservoir",
                                     "Barker Reservoir", "Barker Reservoir",
                                     "Boulder Reservoir", "Boulder Reservoir"))

    # select the sites you want to plot
    site_selection <- c("year", "qm", "Date", "ModelRun", "DataObject_30_Flow",
                        "Reservoir_1_Content", "DataObject_28_Flow", "Reservoir_3_Content",
                        "DataObject_29_Flow", "DataObject_1_Flow")
    n_site_selection <- length(site_selection)

    extract_list <- list()
    for (i in 1:n_file_prefix){

      extract_list[[i]] <- data_list[[i]] %>%
        group_by(ModelRun) %>%
        select(!!!syms(site_selection)) %>%
        group_by(qm, ModelRun) %>%
        # faster way to do this (but it doesn't adjust column names)
        summarise(across(site_selection[5]:site_selection[n_site_selection], mean))

    }

    # merge the data from the loops together
    extract <- bind_rows(extract_list[[1]], extract_list[[2]])

    ### Check the factor & levels for the 'ModelRun' column (we will plot by this)
    #factor(extract$ModelRun)
    #levels(extract$ModelRun)
    # # set the factor 'levels' to the correct plotting order
    extract$ModelRun <-factor(extract$ModelRun, levels = c(scenario_name))
    # Levels should be updated
    levels(extract$ModelRun)
    # get size of dataset
    n_size_data <- dim(extract)

    # ### No Reuse scenario is list item #2. We want to remove some of these variables
    extract2 <- extract %>%
      filter(ModelRun == scenario_name[1]) %>%
      pivot_longer(., cols = c(3:n_size_data[2]),
                   names_to = "Name", values_to = "Contents_af") %>%
      left_join(., definitions, by = "Name") %>%
      left_join(., cram_type, by = "Name")


    # Get the names & titles
    # skip the first two columns which are the groups, year & ModelRun, but take the rest
    site_list <- data.frame(Name = colnames(extract)[3:length(extract)])
    site_list_join <- left_join(site_list, definitions, by = "Name")
    title_list_export <- site_list_join$Description
    site_list_export <- site_list_join$Name
    y_axis_label_list <- site_list_join$Parameter


    n_site_list <- length(site_list)
    #y_axis_max_list <- c(14500, 14500, 2000, 2000, 2000, 3500, 50000, 12000, 22000, 12000)


    # make a list of the things we want to plot
    filter_items_temp <- extract2 %>%
      ungroup() %>%
      distinct(., Group)


    # Plot it!
    p <- list()
    for (i in 1:dim(filter_items_temp)[1]){

      # extract data for 1 reservoir at a time
      extract2a <- extract2 %>%
        filter(Group == as.character(filter_items_temp[i, 1]))

      # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
      p[[i]] <- ggplot(extract2a, aes_string(x = "qm", y = "Contents_af", color = "Type",
                                             linetype = "Type")) +
        geom_line() +
        theme_bw() +
        ylim(0, 12500) +
        ylab("Contents (af)") +
        xlab("Quarter-Month") +
        ggtitle(paste(filter_items_temp[i, 1], ": Reusable Water (", compare_model_ID, ")", sep = "")) +
        theme(plot.title = element_text(size = title_size),
              axis.title = element_text(size = xaxis_size)) +
        scale_color_manual(values = c("blue", "black")) +
        scale_linetype_manual(values = c("dashed", "solid"))

      #p[[i]]

    }



    # define the plot name
    plot_title <- "2f. Reusable Water in Reservoir - Average Quarter-Monthly Plot"
    file_name <- paste(plot_title, " 2x2 ", sep = "")

    # export the plot
    ggsave(
      paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
            output_folder, device_type, sep = ""),
      width = 14, height = 8,
      grid.arrange(p[[1]], p[[2]], p[[3]], nrow = 2,
                   top = plot_title,
                   right = ""))




    # Boulder Reuse Storage QM Time Series Plots ------------------------------------------


    # make a look up between CRAM element and object 'type'
    cram_type = data.frame(Name = c("DataObject_30_Flow", "Reservoir_1_Content",
                                    "DataObject_28_Flow", "Reservoir_3_Content",
                                    "DataObject_29_Flow", "DataObject_1_Flow",
                                    "TotalReuse", "TotalContents"),
                           Type = c("Reusable Water", "Total Contents",
                                    "Reusable Water", "Total Contents",
                                    "Reusable Water", "Total Contents",
                                    "Reusable Water", "Total Contents"),
                           Group = c("NBC Reservoir", "NBC Reservoir",
                                     "Barker Reservoir", "Barker Reservoir",
                                     "Boulder Reservoir", "Boulder Reservoir",
                                     "All 3 Reservoirs", "All 3 Reservoirs"))

    # select the sites you want to plot
    site_selection <- c("year", "qm", "Date", "ModelRun", "DataObject_30_Flow",
                        "Reservoir_1_Content", "DataObject_28_Flow", "Reservoir_3_Content",
                        "DataObject_29_Flow", "DataObject_1_Flow")
    n_site_selection <- length(site_selection)

    extract_list <- list()
    for (i in 1:n_file_prefix){

      extract_list[[i]] <- data_list[[i]] %>%
        group_by(ModelRun) %>%
        select(!!!syms(site_selection)) %>%
        rowwise() %>% mutate(TotalReuse = sum(DataObject_30_Flow, DataObject_28_Flow, DataObject_29_Flow)) %>%
        rowwise() %>% mutate(TotalContents = sum(Reservoir_1_Content, Reservoir_3_Content, DataObject_1_Flow))
      # rowwise() %>% mutate(WittTotalInflow = sum(WittemyerRecaptureWWTPReuse, WittemyerRecaptureGREP,
      #                                            WittemyerFirstFillRight))
      # group_by(year, ModelRun) %>%
      # # faster way to do this (but it doesn't adjust column names)
      # summarise(across(site_selection[5]:site_selection[n_site_selection], mean))

    }

    # merge the data from the loops together
    extract <- bind_rows(extract_list[[1]], extract_list[[2]])

    ### Check the factor & levels for the 'ModelRun' column (we will plot by this)
    #factor(extract$ModelRun)
    #levels(extract$ModelRun)
    # # set the factor 'levels' to the correct plotting order
    extract$ModelRun <-factor(extract$ModelRun, levels = c(scenario_name))
    # Levels should be updated
    levels(extract$ModelRun)
    # get size of dataset
    n_size_data <- dim(extract)

    temp_new_data <- matrix(NA, ncol = 4, nrow = 2)
    temp_new_data[1, 1:4] <- c("TotalReuse", "Barker + NBC + Boulder", "Content", "Contents (af)")
    temp_new_data[2, 1:4] <- c("TotalContents", "Barker + NBC + Boulder", "Content", "Contents (af)")
    temp_new_data <- as.data.frame(temp_new_data)
    colnames(temp_new_data) <- c("Name", "Description", "Parameter", "Units")

    definitions2 <- bind_rows(definitions, temp_new_data)

    # ### No Reuse scenario is list item #2. We want to remove some of these variables
    extract2 <- extract %>%
      filter(ModelRun == scenario_name[1]) %>%
      pivot_longer(., cols = c(5:n_size_data[2]),
                   names_to = "Name", values_to = "Contents_af") %>%
      left_join(., definitions2, by = "Name") %>%
      left_join(., cram_type, by = "Name")


    # %>%
    #   mutate(Type = c(rep("NBC", 2), rep("Barker", 2), rep("Boulder", 2)))

    # Get the names & titles
    # skip the first two columns which are the groups, year & ModelRun, but take the rest
    site_list <- data.frame(Name = colnames(extract)[3:length(extract)])
    site_list_join <- left_join(site_list, definitions, by = "Name")
    title_list_export <- site_list_join$Description
    site_list_export <- site_list_join$Name
    y_axis_label_list <- site_list_join$Parameter

    # ### Plot the annual time series ###
    # site_list <- c("NBCMaxReusableContents", "NBCMaxContents",
    #                "BarkerMaxReusableContents", "BarkerMaxContents",
    #                "BoulderMaxReusableContents", "BoulderMaxContents")
    # title_list <- c("NBCMaxReusableContents", "NBCMaxContents",
    #                 "BarkerMaxReusableContents", "BarkerMaxContents",
    #                 "BoulderMaxReusableContents", "BoulderMaxContents")


    n_site_list <- length(site_list)
    y_axis_max_list <- c(12500, 12500, 12500, 25000)
    y_lab_list <- c(rep("Flow (af)", 7), rep("Contents (af)",3))


    # make a list of the things we want to plot
    filter_items_temp <- extract2 %>%
      ungroup() %>%
      distinct(., Group)



    p <- list()
    for (i in 1:dim(filter_items_temp)[1]){

      # extract data for 1 reservoir at a time
      extract2a <- extract2 %>%
        filter(Group == as.character(filter_items_temp[i, 1]))

      # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
      p[[i]] <- ggplot(extract2a, aes_string(x = "Date", y = "Contents_af", color = "Type",
                                             linetype = "Type")) +
        geom_line() +
        theme_bw() +
        ylim(0, y_axis_max_list[i]) +
        ylab("Contents (af)") +
        xlab("Water Year") +
        ggtitle(paste(filter_items_temp[i, 1], ": Reusable Water (", compare_model_ID, ")", sep = "")) +
        theme(plot.title = element_text(size = title_size),
              axis.title = element_text(size = xaxis_size)) +
        scale_color_manual(values = c("blue", "black")) +
        scale_linetype_manual(values = c("twodash", "solid"))

      #p[[i]]

    }


    ### superceded by plot 2H below
    # # define the plot name
    # plot_title <- "2g. Reusable Water in Reservoir - Quarter-Monthly Time Series Plot"
    # file_name <- paste(plot_title, " 4x1 ", sep = "")
    #
    # ggsave(
    #   paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
    #         output_folder, ".pdf", sep = ""),
    #   width = 14, height = 8,
    #   grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], nrow = 4,
    #                top = plot_title,
    #                right = ""))



    # All Reusable Reservoir Contents Time Series Plots -----------------------


    # make a look up between CRAM element and object 'type'
    cram_type = data.frame(Name = c("DataObject_30_Flow", "Reservoir_1_Content",
                                    "DataObject_28_Flow", "Reservoir_3_Content",
                                    "DataObject_29_Flow", "DataObject_1_Flow",
                                    "Reservoir_13_Content", "Reservoir_25_Content",
                                    "TotalReuse", "TotalContents"),
                           Type = c("Reusable Water", "Total Contents",
                                    "Reusable Water", "Total Contents",
                                    "Reusable Water", "Total Contents",
                                    "Reusable Water", "Reusable Water",
                                    "Reusable Water", "Total Contents"),
                           Group = c("NBC Reservoir", "NBC Reservoir",
                                     "Barker Reservoir", "Barker Reservoir",
                                     "Boulder Reservoir", "Boulder Reservoir",
                                     "Wittemyer", "Panama",
                                     "All 5 Reservoirs", "All 5 Reservoirs"))

    # select the sites you want to plot
    site_selection <- c("year", "qm", "Date", "ModelRun", "DataObject_30_Flow",
                        "Reservoir_1_Content", "DataObject_28_Flow", "Reservoir_3_Content",
                        "DataObject_29_Flow", "DataObject_1_Flow", "Reservoir_13_Content",
                        "Reservoir_25_Content")
    n_site_selection <- length(site_selection)

    extract_list <- list()
    for (i in 1:n_file_prefix){

      extract_list[[i]] <- data_list[[i]] %>%
        group_by(ModelRun) %>%
        select(!!!syms(site_selection)) %>%
        rowwise() %>% mutate(TotalReuse = sum(DataObject_30_Flow, DataObject_28_Flow, DataObject_29_Flow,
                                              Reservoir_13_Content, Reservoir_25_Content)) %>%
        rowwise() %>% mutate(TotalContents = sum(Reservoir_1_Content, Reservoir_3_Content, DataObject_1_Flow,
                                                 Reservoir_13_Content, Reservoir_25_Content))
      # rowwise() %>% mutate(WittTotalInflow = sum(WittemyerRecaptureWWTPReuse, WittemyerRecaptureGREP,
      #                                            WittemyerFirstFillRight))
      # group_by(year, ModelRun) %>%
      # # faster way to do this (but it doesn't adjust column names)
      # summarise(across(site_selection[5]:site_selection[n_site_selection], mean))

    }

    # merge the data from the loops together
    extract <- bind_rows(extract_list[[1]], extract_list[[2]])

    ### Check the factor & levels for the 'ModelRun' column (we will plot by this)
    #factor(extract$ModelRun)
    #levels(extract$ModelRun)
    # # set the factor 'levels' to the correct plotting order
    extract$ModelRun <-factor(extract$ModelRun, levels = c(scenario_name))
    # Levels should be updated
    levels(extract$ModelRun)
    # get size of dataset
    n_size_data <- dim(extract)

    temp_new_data <- matrix(NA, ncol = 4, nrow = 2)
    temp_new_data[1, 1:4] <- c("TotalReuse", "Barker + NBC + Boulder", "Content", "Contents (af)")
    temp_new_data[2, 1:4] <- c("TotalContents", "Barker + NBC + Boulder", "Content", "Contents (af)")
    temp_new_data <- as.data.frame(temp_new_data)
    colnames(temp_new_data) <- c("Name", "Description", "Parameter", "Units")

    definitions2 <- bind_rows(definitions, temp_new_data)

    # ### No Reuse scenario is list item #2. We want to remove some of these variables
    extract2 <- extract %>%
      filter(ModelRun == scenario_name[1]) %>%
      pivot_longer(., cols = c(5:n_size_data[2]),
                   names_to = "Name", values_to = "Contents_af") %>%
      left_join(., definitions2, by = "Name") %>%
      left_join(., cram_type, by = "Name")



    # Get the names & titles
    # skip the first two columns which are the groups, year & ModelRun, but take the rest
    site_list <- data.frame(Name = colnames(extract)[3:length(extract)])
    site_list_join <- left_join(site_list, definitions, by = "Name")
    title_list_export <- site_list_join$Description
    site_list_export <- site_list_join$Name
    y_axis_label_list <- site_list_join$Parameter

    # ### Plot the annual time series ###
    # site_list <- c("NBCMaxReusableContents", "NBCMaxContents",
    #                "BarkerMaxReusableContents", "BarkerMaxContents",
    #                "BoulderMaxReusableContents", "BoulderMaxContents")
    # title_list <- c("NBCMaxReusableContents", "NBCMaxContents",
    #                 "BarkerMaxReusableContents", "BarkerMaxContents",
    #                 "BoulderMaxReusableContents", "BoulderMaxContents")


    n_site_list <- length(site_list)
    y_axis_max_list <- c(12500, 12500, 12500, 2200, 5000, 35000)
    y_lab_list <- c(rep("Flow (af)", 7), rep("Contents (af)",3))


    # make a list of the things we want to plot
    filter_items_temp <- extract2 %>%
      ungroup() %>%
      distinct(., Group)



    p <- list()
    for (i in 1:dim(filter_items_temp)[1]){

      # extract data for 1 reservoir at a time
      extract2a <- extract2 %>%
        filter(Group == as.character(filter_items_temp[i, 1]))

      # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
      p[[i]] <- ggplot(extract2a, aes_string(x = "Date", y = "Contents_af", color = "Type",
                                             linetype = "Type")) +
        geom_line() +
        theme_bw() +
        ylim(0, y_axis_max_list[i]) +
        ylab("Contents (af)") +
        xlab("Water Year") +
        ggtitle(paste(filter_items_temp[i, 1], ": Reusable Water (", compare_model_ID, ")", sep = "")) +
        theme(plot.title = element_text(size = title_size),
              axis.title = element_text(size = xaxis_size)) +
        scale_color_manual(values = c("blue", "black")) +
        scale_linetype_manual(values = c("twodash", "solid"))

      #p[[i]]

    }



    # define the plot name
    plot_title <- "2h. Reusable Water in Reservoir - Quarter-Monthly Time Series Plot"
    file_name <- paste(plot_title, " 5x1 ", sep = "")

    ggsave(
      paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
            output_folder, device_type, sep = ""),
      width = 14, height = 8,
      grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]],  nrow = 5,
                   top = plot_title,
                   right = ""))


    # define the plot name
    plot_title <- "2i. Reusable Water in Reservoir - Quarter-Monthly Time Series Plot"
    file_name <- paste(plot_title, " 1x1 ", sep = "")

    ggsave(
      paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
            output_folder, device_type, sep = ""),
      width = 14, height = 8,
      grid.arrange(p[[6]],  nrow = 1,
                   top = plot_title,
                   right = ""))






    # new WG in out -----------------------------------------------------------

    site_selection <- c("year", "qm", "Date", "ModelRun", "Decree_75_Flow", "Link_499_Flow",
                        "Link_451_Flow", "Link_452_Flow", "Link_375_Flow", "Link_388_Flow",
                        "Link_457_Flow", "Link_454_Flow", "Link_596_Flow", "Link_399_Flow")
    n_site_selection <- length(site_selection)
    site_start_no <- 5
    site_selection_short <- site_selection[site_start_no:n_site_selection]
    n_site_selection_short <- length(site_selection_short)


    cbt_windygap_list <- list()
    for (i in 1:n_file_prefix){

      cbt_windygap_list[[i]] <- data_list[[i]] %>%
        # group by ModelRun to calculate values by group
        group_by(ModelRun) %>%
        # select the columns of interest from the vector above using !!!syms to read it properly
        select(!!!syms(site_selection)) %>%
        ### calculate Boulder Res Windy Gap to the City
        # subtract storage between two time steps
        # now, group data by Year & ModelRun to sum data annually
        group_by(year, ModelRun) %>%
        # sum the sources by year
        summarise(across(site_selection[site_start_no]:site_selection[n_site_selection], sum))
      # summarize(CBT_Inflow = sum(Decree_75_Flow), WindyGap_Inflow = sum(Link_499_Flow),
      #           BoulderRes_WGtoCity = sum(BoulderResWGtoCity),
      #           SBC_ISF = sum(Link_350_Flow),
      #           BarkerRes_ReusableWater = max(DataObject_28_Flow),  #max storage by year
      #           NBCRes_ReusableWater = max(DataObject_30_Flow),     #max storage by year
      #           BoulderRes_ReusableWater = max(DataObject_29_Flow), #max storage by year
      #           BoulderRes_WGExchtoBarker = sum(DataObject_43_Flow),
      #           BoulderRes_WGExchtoNBCRes = sum(DataObject_42_Flow),
      #           BoulderRes_WGExctoUpperStor = sum(DataObject_41_Flow),
      #           NBCRes_Contents = mean(Reservoir_1_Content),
      #           COB_Reusable_Contents = max(ReuseStorage)
      #)
      cbt_windygap_list[[i]]

    }


    # merge the data from the loops together
    cbt_windygap <- bind_rows(cbt_windygap_list[[1]], cbt_windygap_list[[2]])

    ### Check the factor & levels for the 'ModelRun' column (we will plot by this)
    # factor(cbt_windygap$ModelRun)
    # levels(cbt_windygap$ModelRun)
    # # set the factor 'levels' to the correct plotting order
    cbt_windygap$ModelRun <-factor(cbt_windygap$ModelRun, levels = c(scenario_name))
    # Levels should be updated
    levels(cbt_windygap$ModelRun)

    # put to long form
    cbt_windygap2 <- cbt_windygap %>%
      # convert from 'wide' to 'long' format for plotting w/ ggplot
      pivot_longer(., cols = all_of(site_selection_short),
                   names_to = "Name", values_to = "Output")


    extract <- list()
    for (i in 1:n_file_prefix){

      extract_list[[i]] <- data_list[[i]] %>%
        # group by ModelRun to calculate values by group
        group_by(ModelRun) %>%
        # select the columns of interest from the vector above using !!!syms to read it properly
        select("year", "qm", "Date", "ModelRun", "DataObject_29_Flow", "DataObject_1_Flow",
               "DataObject_2_Flow") %>%
        # now, group data by Year & ModelRun to sum data annually
        group_by(year, ModelRun) %>%
        filter(qm == 29)

      extract_list[[i]]

    }

    # merge the data from the loops together
    extract <- bind_rows(extract_list[[1]], extract_list[[2]])

    ### Check the factor & levels for the 'ModelRun' column (we will plot by this)
    # factor(cbt_windygap$ModelRun)
    # levels(cbt_windygap$ModelRun)
    # # set the factor 'levels' to the correct plotting order
    extract$ModelRun <-factor(extract$ModelRun, levels = c(scenario_name))
    # Levels should be updated
    levels(extract$ModelRun)


    # put to long form
    extract2 <- extract %>%
      # convert from 'wide' to 'long' format for plotting w/ ggplot
      pivot_longer(., cols = c("DataObject_29_Flow", "DataObject_1_Flow", "DataObject_2_Flow"),
                   names_to = "Name", values_to = "Output") %>%
      select(-qm, -Date)


    final <- rbind(cbt_windygap2, extract2)
    final_site_list <- c(site_selection_short, "DataObject_29_Flow", "DataObject_1_Flow", "DataObject_2_Flow")
    n_final_site_list <- length(final_site_list)
    # ### Plot the annual time series ###
    # site_list <- c("CBT_Inflow", "WindyGap_Inflow",
    #                "BoulderRes_WGExchtoBarker", "BoulderRes_WGExchtoNBCRes",
    #                "BoulderRes_WGExctoUpperStor", "BoulderRes_WGtoCity",
    #                "SBC_ISF", "BarkerRes_ReusableWater", "NBCRes_ReusableWater",
    #                "BoulderRes_ReusableWater", "COB_Reusable_Contents")
    # title_list <- c("C-BT Inflow", "Windy Gap Inflow",
    #                 "Boulder Reservoir: Windy Gap Exch. to Barker Res",
    #                 "Boulder Reservoir: Windy Gap Exch. to NBC Res",
    #                 "Boulder Reservoir: Windy Gap Total Exch to Upper Storage",
    #                 "Boulder Reservoir: Windy Gap to City",
    #                 "South Boulder Creek Instream Flow",
    #                 "Barker Reservoir: Maximum Annual Reusable Water",
    #                 "NBC Reservoir: Maximum Annual Reusable Water",
    #                 "Boulder Reservoir: Maximum Annual Reusable Water",
    #                 "Barker + NBC + Boulder: Maximum Annual Reusable Contents")
    # n_site_list <- length(site_list)
    # y_axis_max_list <- c(18000, 18000, 4000, 4000, 4000, 3500, 50000, 12000, 12000, 12000, 20000)

    y_lab_list <- c(rep("Flow (af)", 10), rep("Contents (af)", 3))


    p <- list()
    for (i in 1:n_final_site_list){

      # select 1 flow data to plot per i iteration of loop
      extract_plot <- final %>%
        filter(Name == final_site_list[i])


      # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
      p[[i]] <- ggplot(extract_plot, aes_string(x = "year", y = "Output", color = "ModelRun",
                                                linetype = "ModelRun")) +
        geom_line() +
        theme_bw() +
        ylim(0, NA) +
        ylab(y_lab_list[i]) +
        xlab("Water Year") +
        ggtitle(extract_plot$Name[1]) +
        theme(plot.title = element_text(size = title_size),
              axis.title = element_text(size = xaxis_size))

      # p[[i]]

    }

    # define the plot name
    plot_title <- "2j. CBT-Windy Gap in out - Time Series Plot"
    file_name <- paste(plot_title, " 3x2 ", sep = "")

    # save the plot
    ggsave(
      paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
            output_folder, device_type, sep = ""),
      width = 14, height = 8,
      grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]],
                   p[[7]], p[[8]], p[[9]], p[[10]], p[[11]], p[[12]], p[[13]], nrow = 7,
                   top = plot_title,
                   right = ""))

    rm(cbt_windygap, cbt_windygap2, extract, extract2, final, final_site_list, n_final_site_list)



    # CBT WG Boulder Res qm output 2k -----------------------------------------

    extract <- list()
    for (i in 1:n_file_prefix){

      extract_list[[i]] <- data_list[[i]] %>%
        # group by ModelRun to calculate values by group
        group_by(ModelRun) %>%
        # select the columns of interest from the vector above using !!!syms to read it properly
        select("year", "qm", "Date", "ModelRun", "DataObject_29_Flow", "DataObject_1_Flow",
               "DataObject_2_Flow") %>%
        # now, group data by Year & ModelRun to sum data annually
        group_by(year, ModelRun)

      extract_list[[i]]

    }

    # merge the data from the loops together
    extract <- bind_rows(extract_list[[1]], extract_list[[2]])

    ### Check the factor & levels for the 'ModelRun' column (we will plot by this)
    # factor(cbt_windygap$ModelRun)
    # levels(cbt_windygap$ModelRun)
    # # set the factor 'levels' to the correct plotting order
    extract$ModelRun <-factor(extract$ModelRun, levels = c(scenario_name))
    # Levels should be updated
    levels(extract$ModelRun)


    # put to long form
    extract2 <- extract %>%
      # convert from 'wide' to 'long' format for plotting w/ ggplot
      pivot_longer(., cols = c("DataObject_29_Flow", "DataObject_1_Flow", "DataObject_2_Flow"),
                   names_to = "Name", values_to = "Output")
    extrac2_site_list <- c("DataObject_29_Flow", "DataObject_1_Flow", "DataObject_2_Flow")
    n_extrac2_site_list <- length(extrac2_site_list)


    p <- list()
    for (i in 1:n_extrac2_site_list){

      # select 1 flow data to plot per i iteration of loop
      extract_plot <- extract2 %>%
        filter(Name == extrac2_site_list[i])


      # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
      p[[i]] <- ggplot(extract_plot, aes_string(x = "Date", y = "Output", color = "ModelRun",
                                                linetype = "ModelRun")) +
        geom_line() +
        theme_bw() +
        ylim(0, NA) +
        ylab(y_lab_list[i]) +
        xlab("Water Year") +
        ggtitle(extract_plot$Name[1]) +
        theme(plot.title = element_text(size = title_size),
              axis.title = element_text(size = xaxis_size))

      # p[[i]]

    }

    # define the plot name
    plot_title <- "2k. Boulder Res QM - Time Series Plot"
    file_name <- paste(plot_title, " 3x1 ", sep = "")

    # save the plot
    ggsave(
      paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
            output_folder, device_type, sep = ""),
      width = 14, height = 8,
      grid.arrange(p[[1]], p[[2]], p[[3]], nrow = 3,
                   top = plot_title,
                   right = ""))

    rm(n_extrac2_site_list, extrac2_site_list, extract, extract2)

    # Windy Gap Reuse & Wittemyer Pond ----------------------------------------


    wittemyer_pond_list <- list()
    for (i in 1:n_file_prefix){

      wittemyer_pond_list[[i]] <- data_list[[i]] %>%
        # DO 27 = treated reusable water from Boulder WWTP
        # Link 524 = Wittemyer pond recapture of WWTP reusable water
        # Link 592 = recapture of Boulder GREP releases
        # Decree 106 = Wittemyer pond first fill right
        # Link 525 = Wittemyer pond releases
        # Reservoir 13 = Wittemyer pond contents
        select(year, qm, Date, ModelRun, DataObject_27_Flow, Link_524_Flow,
               Link_592_Flow, Decree_106_Flow, Link_525_Flow) %>%
        # group by Year & ModelRun to calculate values by group & year
        group_by(year, ModelRun) %>%
        # sum the sources by year
        summarize(BoulderWWTPTreatedReuseWater = sum(DataObject_27_Flow),
                  WittemyerRecaptureWWTPReuse = sum(Link_524_Flow),
                  WittemyerRecaptureGREP = sum(Link_592_Flow),
                  WittemyerFirstFillRight = sum(Decree_106_Flow),
                  WittReleases = sum(Link_525_Flow)) %>%
        rowwise() %>% mutate(WittTotalInflow = sum(WittemyerRecaptureWWTPReuse, WittemyerRecaptureGREP,
                                                   WittemyerFirstFillRight))
      ## faster way to do this (but it doesn't adjust column names)
      # summarise(across(Link_447_Flow:Link_406_Flow, sum))
      wittemyer_pond_list[[i]]

    }

    # merge the data from the loops together
    wittemyer_pond <- bind_rows(wittemyer_pond_list[[1]], wittemyer_pond_list[[2]])

    # # filter out the No Reuse scenario (not applicable results right now)
    # wittemyer_pond <- wittemyer_pond %>%
    #   filter(ModelRun == "Base Reuse")

    ### Check the factor & levels for the 'ModelRun' column (we will plot by this)
    #factor(wittemyer_pond$ModelRun)
    #levels(wittemyer_pond$ModelRun)
    # # set the factor 'levels' to the correct plotting order
    wittemyer_pond$ModelRun <-factor(wittemyer_pond$ModelRun, levels = c(scenario_name))
    # Levels should be updated
    levels(wittemyer_pond$ModelRun)


    ### filter out the No Reuse Scenario
    # convert data from wide to long to make a stacked area plot
    wittemyer_pond2 <- wittemyer_pond %>%
      select(year, ModelRun, WittemyerRecaptureWWTPReuse, WittemyerRecaptureGREP, WittemyerFirstFillRight) %>%
      #filter(ModelRun == scenario_name[1]) %>%
      pivot_longer(., cols = c(WittemyerRecaptureWWTPReuse, WittemyerRecaptureGREP,
                               WittemyerFirstFillRight),
                   names_to = "Inflow", values_to = "Flow_af")
    wittemyer_pond2


    ### Time Series Plots for demand met by source ###
    site_list <- c("BoulderWWTPTreatedReuseWater", "WittemyerRecaptureWWTPReuse",
                   "WittemyerRecaptureGREP", "WittemyerFirstFillRight",
                   "WittTotalInflow", "WittReleases")
    title_list <- c("Boulder WWTP Reusable Return Flows", "Wittemyer Recapture of WWTP Reusable Water",
                    "Wittemyer Recapture of GREP", "Wittemyer First Fill Right",
                    "Wittemyer Total Inflow", "Wittemyer Pond Releases")
    n_site_list <- length(site_list)
    #color_list <- brewer.pal(4, "Set1")
    # y_axis_max_list <- c(12000, 12000, 6000, 3000)
    #y_axis_max_list <- c(12000, 12000, 12000, 12000)
    p <- list()

    for (i in 1:n_site_list){

      # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
      p[[i]] <- ggplot(wittemyer_pond, aes_string(x = "year", y = site_list[i], color = "ModelRun",
                                                  linetype = "ModelRun")) +
        geom_line() +  #col = color_list[i]
        theme_bw() +
        ylim(0, 5000) +
        ylab("Flow (af)") +
        xlab("Water Year") +
        ggtitle(title_list[i]) +
        theme(plot.title = element_text(size = title_size),
              axis.title = element_text(size = xaxis_size))
      #p[[i]]

    }

    g <- list()
    for (i in 1:n_scenario_name){

      # subset the data for only the data needed & filter for the necessary scenario
      extract_plot <- wittemyer_pond2 %>%
        filter(ModelRun == scenario_name[i])

      # make a stacked area plot of the reusable supply scenario
      g[[i]] <- ggplot(extract_plot, aes_string(x = "year", y = "Flow_af", fill = "Inflow")) +
        geom_bar(position="stack", stat="identity", color = "black") +
        #geom_area() +  #col = color_list[i]
        theme_bw() +
        ylim(0, 3000) +
        ylab("Flow (af)") +
        xlab("Water Year") +
        ggtitle(paste0("Wittemyer Pond: Annual Inflow by Source", " (", scenario_name[i], ")")) +
        theme(plot.title = element_text(size = title_size),
              axis.title = element_text(size = xaxis_size))
      #scale_fill_manual(values = c("#0073C2FF", "#EFC000FF", ))
      #g

    }


    # define the plot name
    plot_title <- "3b. Wittemyer Pond Total Annual Flow Time Series"
    file_name <- paste(plot_title, " 3x2 ", sep = "")

    # save the plot
    ggsave(
      paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
            output_folder, device_type, sep = ""),
      width = 14, height = 8,
      grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], nrow = 3,
                   top = plot_title,
                   right = ""))


    # define the plot name
    plot_title <- "3a. Wittemyer Pond Total Annual Inflow by Source"
    file_name <- paste(plot_title, " 1x1 ", sep = "")

    # save the plot
    ggsave(
      paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
            output_folder, device_type, sep = ""),
      width = 14, height = 8,
      grid.arrange(g[[1]], g[[2]], nrow = 2,
                   top = plot_title,
                   right = ""))


    rm(p, g, wittemyer_pond, wittemyer_pond2, wittemyer_pond_list, site_list,
       title_list, n_site_list)


    # Get & Plot Wittemyer Pond Average (qm & annual) Contents --------------------------------------


    wittemyer_contents_annual_list <- list()
    for (i in 1:n_file_prefix){

      wittemyer_contents_annual_list[[i]] <- data_list[[i]] %>%
        # DO 27 = treated reusable water from Boulder WWTP
        # Link 524 = Wittemyer pond recapture of WWTP reusable water
        # Link 592 = recapture of Boulder GREP releases
        # Decree 106 = Wittemyer pond first fill right
        # Link 525 = Wittemyer pond releases
        # Reservoir 13 = Wittemyer pond contents
        select(year, qm, Date, ModelRun, Reservoir_13_Content) %>%
        # group by Year & ModelRun to calculate values by group & year
        group_by(year, ModelRun) %>%
        # sum the sources by year
        summarize(Witt_annual_max_contents = max(Reservoir_13_Content),
                  Witt_annual_avg_contents = mean(Reservoir_13_Content))
      ## faster way to do this (but it doesn't adjust column names)
      # summarise(across(Link_447_Flow:Link_406_Flow, sum))
      wittemyer_contents_annual_list[[i]]

    }

    # use only the reuse scenario data
    wittemyer_contents_annual <- bind_rows(wittemyer_contents_annual_list[[1]], wittemyer_contents_annual_list[[2]])

    # filter out the No Reuse scenario (not applicable results right now)
    # wittemyer_contents_annual <- wittemyer_contents_annual %>%
    #   filter(ModelRun == scenario_name[1])

    ### Check the factor & levels for the 'ModelRun' column (we will plot by this)
    #factor(wittemyer_contents_annual$ModelRun)
    #levels(wittemyer_contents_annual$ModelRun)
    # # set the factor 'levels' to the correct plotting order
    wittemyer_contents_annual$ModelRun <-factor(wittemyer_contents_annual$ModelRun, levels = c(scenario_name))
    # Levels should be updated
    levels(wittemyer_contents_annual$ModelRun)



    ### Plot the annual wittemyer contents
    site_list <- c("Witt_annual_max_contents", "Witt_annual_avg_contents")
    title_list <- c("Wittemyer Pond Maximum Annual Contents", "Wittemyer Pond Average Annual Contents")
    n_site_list <- length(site_list)
    # y_axis_max_list <- c(12000, 12000, 6000, 3000)
    #y_axis_max_list <- c(12000, 12000, 12000, 12000)
    g <- list()

    for (i in 1:n_site_list){

      # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
      g[[i]] <- ggplot(wittemyer_contents_annual, aes_string(x = "year", y = site_list[i], color = "ModelRun",
                                                             linetype = "ModelRun")) +
        geom_line() +  #col = color_list[i]
        theme_bw() +
        ylim(0, 2200) +
        ylab("Contents (af)") +
        xlab("Water Year") +
        ggtitle(title_list[i]) +
        theme(plot.title = element_text(size = title_size),
              axis.title = element_text(size = xaxis_size))
      #g[[i]]

    }
    #g[[1]]


    ### calculate monthly wittemyer contents
    wittemyer_contents_qm_list <- list()
    for (i in 1:n_file_prefix){

      wittemyer_contents_qm_list[[i]] <- data_list[[i]] %>%
        # DO 27 = treated reusable water from Boulder WWTP
        # Link 524 = Wittemyer pond recapture of WWTP reusable water
        # Link 592 = recapture of Boulder GREP releases
        # Decree 106 = Wittemyer pond first fill right
        # Link 525 = Wittemyer pond releases
        # Reservoir 13 = Wittemyer pond contents
        select(year, qm, Date, ModelRun, Reservoir_13_Content) %>%
        # group by Year & ModelRun to calculate values by group & year
        group_by(qm, ModelRun) %>%
        # sum the sources by year
        summarize(Witt_qm_contents = mean(Reservoir_13_Content))
      ## faster way to do this (but it doesn't adjust column names)
      # summarise(across(Link_447_Flow:Link_406_Flow, sum))
      wittemyer_contents_qm_list[[i]]

    }

    # merge the data from the loops together
    wittemyer_contents_qm <- bind_rows(wittemyer_contents_qm_list[[1]], wittemyer_contents_qm_list[[2]])

    # # filter out the No Reuse scenario (not applicable results right now)
    # wittemyer_contents_qm <- wittemyer_contents_qm %>%
    #   filter(ModelRun == scenario_name[1])

    ### Check the factor & levels for the 'ModelRun' column (we will plot by this)
    #factor(wittemyer_contents_qm$ModelRun)
    #levels(wittemyer_contents_qm$ModelRun)
    # # set the factor 'levels' to the correct plotting order
    wittemyer_contents_qm$ModelRun <-factor(wittemyer_contents_qm$ModelRun, levels = c(scenario_name))
    # Levels should be updated
    levels(wittemyer_contents_qm$ModelRun)


    ### Plot the monthly wittemyer contents
    site_list <- c("Witt_qm_contents")
    title_list <- c("Wittemyer Pond Average Quarter-Monthly Contents")
    n_site_list <- length(site_list)
    # y_axis_max_list <- c(12000, 12000, 6000, 3000)
    #y_axis_max_list <- c(12000, 12000, 12000, 12000)
    h <- list()

    for (i in 1:n_site_list){

      # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
      h[[i]] <- ggplot(wittemyer_contents_qm, aes_string(x = "qm", y = site_list[i], color = "ModelRun",
                                                         linetype = "ModelRun")) +
        geom_line() +  #col = color_list[i]
        theme_bw() +
        ylim(0, 2200) +
        ylab("Contents (af)") +
        xlab("Quarter Month") +
        ggtitle(title_list[i]) +
        theme(plot.title = element_text(size = title_size),
              axis.title = element_text(size = xaxis_size))
      # h[[i]]

    }
    #h[[1]]

    # define the plot name
    plot_title <- "3c. Wittemyer Pond Average Annual Contents"
    file_name <- paste(plot_title, " 3x1 ", sep = "")

    # save the plot
    ggsave(
      paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
            output_folder, device_type, sep = ""),
      width = 14, height = 8,
      grid.arrange(g[[1]], g[[2]], h[[1]], nrow =3,
                   top = plot_title,
                   right = ""))


    rm(g, h, wittemyer_contents_annual, wittemyer_contents_annual_list, wittemyer_contents_qm,
       wittemyer_contents_qm_list, site_list, title_list, n_site_list)


    # Wittemyer Qm Time Series ------------------------------------------------


    wittemyer_qm_timeseries_list <- list()
    for (i in 1:n_file_prefix){

      wittemyer_qm_timeseries_list[[i]] <- data_list[[i]] %>%
        # DO 27 = treated reusable water from Boulder WWTP
        # Link 524 = Wittemyer pond recapture of WWTP reusable water
        # Link 592 = recapture of Boulder GREP releases
        # Decree 106 = Wittemyer pond first fill right
        # Link 525 = Wittemyer pond releases
        # Reservoir 13 = Wittemyer pond contents
        select(year, qm, Date, ModelRun, Reservoir_13_Content, DataObject_27_Flow,
               Link_524_Flow, Link_592_Flow, Decree_106_Flow, Link_525_Flow) %>%
        rename(Witt_qm_contents = Reservoir_13_Content, BoulderWWTPTreatedReuseWater = DataObject_27_Flow,
               WittemyerRecaptureWWTPReuse = Link_524_Flow, WittemyerRepactureGREP = Link_592_Flow,
               WittemyerFirstFillRight = Decree_106_Flow) %>%
        # group by Year & ModelRun to calculate values by group & year
        group_by(year, ModelRun)
      # sum the sources by year
      # summarize(Witt_annual_max_contents = max(Reservoir_13_Content),
      #           Witt_annual_avg_contents = mean(Reservoir_13_Content),
      #           BoulderWWTPTreatedReuseWater = sum(DataObject_27_Flow),
      #           WittemyerRecaptureWWTPReuse = sum(Link_524_Flow),
      #           WittemyerRepactureGREP = sum(Link_592_Flow),
      #           WittemyerFirstFillRight = sum(Decree_106_Flow))
      ## faster way to do this (but it doesn't adjust column names)
      # summarise(across(Link_447_Flow:Link_406_Flow, sum))
      wittemyer_qm_timeseries_list[[i]]

    }

    # use only the reuse scenario data
    wittemyer_qm_timeseries <- bind_rows(wittemyer_qm_timeseries_list[[1]], wittemyer_qm_timeseries_list[[2]])

    # filter out the No Reuse scenario (not applicable results right now)
    wittemyer_qm_timeseries <- wittemyer_qm_timeseries %>%
      filter(ModelRun == scenario_name[1])


    ### Check the factor & levels for the 'ModelRun' column (we will plot by this)
    #factor(wittemyer_qm_timeseries$ModelRun)
    #levels(wittemyer_qm_timeseries$ModelRun)
    # # set the factor 'levels' to the correct plotting order
    wittemyer_qm_timeseries$ModelRun <-factor(wittemyer_qm_timeseries$ModelRun, levels = c(scenario_name))
    # Levels should be updated
    levels(wittemyer_qm_timeseries$ModelRun)


    ### Plot the data - time series plots ###
    site_list <- c("Witt_qm_contents", "BoulderWWTPTreatedReuseWater", "WittemyerRecaptureWWTPReuse",
                   "WittemyerRepactureGREP", "WittemyerFirstFillRight")
    title_list <- c("Wittemyer Pond Contents", "Boulder WWTP Reusable Return Flows",
                    "Wittemyer Recapture of WWTP Reusable Water",
                    "Wittemyer Recapture of GREP", "Wittemyer First Fill Right")
    n_site_list <- length(site_list)
    #color_list <- brewer.pal(4, "Set1")
    # y_axis_max_list <- c(12000, 12000, 6000, 3000)
    #y_axis_max_list <- c(12000, 12000, 12000, 12000)
    p <- list()

    for (i in 1:n_site_list){

      # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
      p[[i]] <- ggplot(wittemyer_qm_timeseries, aes_string(x = "Date", y = site_list[i], color = "ModelRun",
                                                           linetype = "ModelRun")) +
        geom_line() +  #col = color_list[i]
        theme_bw() +
        #ylim(0, y_axis_max_list[i]) +
        ylab("Flow (af)") +
        xlab("Water Year") +
        ggtitle(title_list[i]) +
        theme(plot.title = element_text(size = title_size),
              axis.title = element_text(size = xaxis_size))
      #p[[i]]

    }



    # define the plot name
    plot_title <- "3d. Wittemyer Pond Quarter-Monthly Time Series Plot"
    file_name <- paste(plot_title, " 5x1 ", sep = "")

    # save the plot
    ggsave(
      paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
            output_folder, device_type, sep = ""),
      width = 14, height = 8,
      grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], nrow = 5,
                   top = plot_title,
                   right = ""))


    rm(p, wittemyer_qm_timeseries_list, wittemyer_qm_timeseries, site_list, title_list,
       n_site_list)


    # Gross Reservoir Pool Analysis -------------------------------------------


    grep_qm_timeseries_list <- list()
    for (i in 1:n_file_prefix){

      grep_qm_timeseries_list[[i]] <- data_list[[i]] %>%
        # Reservoir 21 = Boulder/Lafayette environmental storage in Gross Res
        # DO 31 = Boulder GREP contents
        # DO 32 = Lafayette GREP contents
        # Link 587 = Total Lafayette water to GREP
        # Link 588 = Boulder exchange water to GREP
        # Link 541 = Gross Res Environmental pool releases
        # DO 37 = Boulder Release from GREP
        # DO 38 = Lafayette water released from GREP
        # Link 350 = South Boulder Creek ISF Link
        select(year, qm, Date, ModelRun, Reservoir_21_Content, DataObject_31_Flow, DataObject_32_Flow,
               Link_587_Flow, Link_588_Flow, Link_541_Flow, DataObject_37_Flow,
               DataObject_38_Flow, Link_350_Flow) %>%
        # group by Year & ModelRun to calculate values by group & year
        group_by(year, ModelRun) %>%
        # sum the sources by year
        summarize(COB_Laf_GREP_contents = mean(Reservoir_21_Content),
                  Boulder_GREP_contents = mean(DataObject_31_Flow),
                  Lafayette_GREP_contents = mean(DataObject_32_Flow),
                  LafayetteFlowToGREP = sum(Link_587_Flow), BoulderExchangeToGREP = sum(Link_588_Flow),
                  BoulderReleaseGREP = sum(DataObject_37_Flow), LafayetteReleaseGREP = sum(DataObject_38_Flow),
                  SBC_ISF = sum(Link_350_Flow))
      # # faster way to do this (but it doesn't adjust column names)
      # summarise(across(Link_447_Flow:Link_406_Flow, sum))
      grep_qm_timeseries_list[[i]]

    }

    # use only the reuse scenario data
    grep_qm_timeseries <- bind_rows(grep_qm_timeseries_list[[1]], grep_qm_timeseries_list[[2]])


    ### Check the factor & levels for the 'ModelRun' column (we will plot by this)
    #factor(grep_qm_timeseries$ModelRun)
    #levels(grep_qm_timeseries$ModelRun)
    # # set the factor 'levels' to the correct plotting order
    grep_qm_timeseries$ModelRun <-factor(grep_qm_timeseries$ModelRun,
                                         levels = c(scenario_name))
    # Levels should be updated
    levels(grep_qm_timeseries$ModelRun)


    # filter out the No Reuse scenario (not applicable results right now)
    # grep_qm_timeseries <- grep_qm_timeseries %>%
    #   filter(ModelRun == scenario_name[1])



    ### Time Series Plots for demand met by source ###
    site_list <- c("COB_Laf_GREP_contents", "Boulder_GREP_contents", "Lafayette_GREP_contents",
                   "LafayetteFlowToGREP", "BoulderExchangeToGREP",
                   "BoulderReleaseGREP", "LafayetteReleaseGREP", "SBC_ISF")
    title_list <- c("Boulder and Lafayette Gross Environmental Pool Contents (Average Annual)",
                    "City of Boulder GREP Contents (Average Annual)",
                    "City of Lafayette GREP Contents (Average Annual)",
                    "Lafayette Flow To GREP (Total Annual)",
                    "Boulder Exchange To GREP (Total Annual)",
                    "Boulder Releases from GREP (Total Annual)", "Lafayette Releases from GREP (Total Annual)",
                    "South Boulder Creek Instream Flow (Total Annual)")
    n_site_list <- length(site_list)
    #color_list <- brewer.pal(4, "Set1")
    # y_axis_max_list <- c(12000, 12000, 6000, 3000)
    y_axis_max_list <- c(7000, 7000, 7000, 5000, 5000, 5000, 5000, 50000)
    y_axis_label_list <- c(rep("Contents (af)", 3), rep("Flow (af)", 5))
    p <- list()

    for (i in 1:n_site_list){

      if(i <= 7){ # use the filtered data (2) for Reuse On only

        # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
        p[[i]] <- ggplot(grep_qm_timeseries, aes_string(x = "year", y = site_list[i], color = "ModelRun",
                                                        linetype = "ModelRun")) +
          geom_line() +  #col = color_list[i]
          theme_bw() +
          ylim(0, y_axis_max_list[i]) +
          ylab(y_axis_label_list[i]) +
          xlab("Water Year") +
          ggtitle(title_list[i]) +
          theme(plot.title = element_text(size = title_size),
                axis.title = element_text(size = xaxis_size))

      }else { # use the non-filtered data for Reuse ON vs Reuse oFF

        # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
        p[[i]] <- ggplot(grep_qm_timeseries, aes_string(x = "year", y = site_list[i], color = "ModelRun",
                                                        linetype = "ModelRun")) +
          geom_line() +  #col = color_list[i]
          theme_bw() +
          ylim(0, y_axis_max_list[i]) +
          ylab(y_axis_label_list[i]) +
          xlab("Water Year") +
          ggtitle(title_list[i]) +
          theme(plot.title = element_text(size = title_size),
                axis.title = element_text(size = xaxis_size))
      }




    }

    # define the plot name
    plot_title <- "5. GREP Annual Time Series Plots"
    file_name <- paste(plot_title, " 4x2 ", sep = "")

    # save the plot
    ggsave(
      paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
            output_folder, device_type, sep = ""),
      width = 14, height = 8,
      grid.arrange(p[[1]], p[[8]], p[[2]], p[[3]], p[[4]], p[[5]],
                   p[[6]], p[[7]], nrow = 4,
                   top = plot_title,
                   right = ""))

    rm(p)



    # Panama Reservoir Analysis -----------------------------------------------


    panama_annual_summary_list <- list()
    for (i in 1:n_file_prefix){

      panama_annual_summary_list[[i]] <- data_list[[i]] %>%
        # Reservoir 25 = COB space in Panama Res
        # Decree 125 = COB 2020 water right for Panama
        # Link 571 = COB Buolder & WhiteRock Ditch to COB Panama
        # Link 572 = COB Panama Res water right from Leggett
        # Link 573 = COB Panama Res stored reusable release
        # Link 617 = Reusable Flows to Panama via Leggett Ditch
        # Link 569 = Reusable water exchange to Barker Storage
        # Link 568 = Reusable water exchange to NBC Res
        # Link 588 = Bolder exchange into GREP
        select(year, qm, Date, ModelRun, Reservoir_25_Content, Link_571_Flow,
               Link_572_Flow, Link_617_Flow, Link_618_Flow, Link_573_Flow) %>%
        # Decree_125_Flow
        # group by Year & ModelRun to calculate values by group & year
        group_by(year, ModelRun) %>%
        # sum the sources by year
        summarize(COB_Panama_contents_Avg = mean(Reservoir_25_Content),
                  COB_Panama_contents_Min = min(Reservoir_25_Content),
                  COB_Panama_contents_Max = max(Reservoir_25_Content),
                  #COB_2020_WaterRight_Panama = sum(Decree_125_Flow),
                  BoulderWhiterock_ToPanama = sum(Link_571_Flow),
                  COBPanamaResWR_FromLeggett = sum(Link_572_Flow),
                  RecaptureGREPRelease_viaLeggett = sum(Link_617_Flow),
                  PanamaRecapture_WWTPReuseWater = sum(Link_618_Flow),
                  COBPanama_ReusableRelease = sum(Link_573_Flow))
      # # faster way to do this (but it doesn't adjust column names)
      # summarise(across(Link_447_Flow:Link_406_Flow, sum))
      panama_annual_summary_list[[i]]

    }

    # use only the reuse scenario data
    panama_annual_summary <- bind_rows(panama_annual_summary_list[[1]], panama_annual_summary_list[[2]])


    ### Check the factor & levels for the 'ModelRun' column (we will plot by this)
    #factor(panama_annual_summary$ModelRun)
    #levels(panama_annual_summary$ModelRun)
    # # set the factor 'levels' to the correct plotting order
    panama_annual_summary$ModelRun <-factor(panama_annual_summary$ModelRun,
                                            levels = c(scenario_name))
    # Levels should be updated
    levels(panama_annual_summary$ModelRun)


    # # filter out the No Reuse scenario (not applicable results right now)
    # panama_annual_summary <- panama_annual_summary %>%
    #   filter(ModelRun == scenario_name[1])


    ### Time Series Plots for demand met by source ###
    site_list <- c("COB_Panama_contents_Avg", "COB_Panama_contents_Min", "COB_Panama_contents_Max",
                   "BoulderWhiterock_ToPanama", "COBPanamaResWR_FromLeggett",
                   "RecaptureGREPRelease_viaLeggett", "PanamaRecapture_WWTPReuseWater",
                   "COBPanama_ReusableRelease")
    title_list <- c("Boulder Pool Contents in Panama Reservoir (Average Annual)",
                    "Boulder Pool Contents in Panama Reservoir (Min Annual)",
                    "Boulder Pool Contents in Panama Reservoir (Max Annual)",
                    "COB Boulder & Whiterock Ditch to COB Panama (Total Annual) (Link 571)",
                    "COB Panama Reservoir Water Right From Leggett (Total Annual) (Link 572)",
                    "Reusable Water to Panama Res from Leggett Ditch (Total Annual) (Link 617)",
                    "COB Recapture of WWTP Reusable Water (Total Annual) (Link 618)",
                    "COB Panama Res Reusable Release (Total Annual) (Link 573)")
    n_site_list <- length(site_list)
    #color_list <- brewer.pal(4, "Set1")
    # y_axis_max_list <- c(12000, 12000, 6000, 3000)
    y_axis_max_list <- c(5000, 5000, 5000, 6000, 6000, 6000, 6000, 6000)
    y_axis_label_list <- c(rep("Contents (af)", 3), rep("Flow (af)", 5))
    p <- list()

    for (i in 1:n_site_list){

      # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
      p[[i]] <- ggplot(panama_annual_summary, aes_string(x = "year", y = site_list[i], color = "ModelRun",
                                                         linetype = "ModelRun")) +
        geom_line() +  #col = color_list[i]
        theme_bw() +
        ylim(0, y_axis_max_list[i]) +
        ylab(y_axis_label_list[i]) +
        xlab("Water Year") +
        ggtitle(title_list[i]) +
        theme(plot.title = element_text(size = title_size),
              axis.title = element_text(size = xaxis_size))


    }

    # define the plot name
    plot_title <- "4b. Panama Reservoir Annual Time Series Plots"
    file_name <- paste(plot_title, " 5x2 ", sep = "")

    # save the plot
    ggsave(
      paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
            output_folder, device_type, sep = ""),
      width = 14, height = 8,
      grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]],
                   p[[7]],p[[8]],  nrow = 4,
                   top = plot_title, right = ""))

    rm(p, panama_annual_summary_list, panama_annual_summary,
       site_list, title_list, n_site_list)


    # Panama Reservoir Water Type Bar Plot ------------------------------------

    # make a look up between CRAM element and object 'type'
    cram_type = data.frame(Name = c("Link_617_Flow", "Link_618_Flow",
                                    "Decree_125_Content", "Link_571_Flow",
                                    "Link_572_Flow", "Link_573_Flow"),
                           Type = c("Inflow", "Inflow",
                                    "Decree Capacity", "Inflow",
                                    "Inflow", "Outflow"))

    # select the sites you want to plot
    site_selection <- c("year", "qm", "Date", "ModelRun", "Link_617_Flow",
                        "Link_618_Flow", "Decree_125_Content", "Link_571_Flow",
                        "Link_572_Flow", "Link_573_Flow")
    n_site_selection <- length(site_selection)


    ### run an annual analysis
    extract_list <- list()
    for (i in 1:n_file_prefix){

      extract_list[[i]] <- data_list[[i]] %>%
        # select the columns of interest from the vector above using !!!syms to read it properly
        select(!!!syms(site_selection)) %>%
        group_by(year, ModelRun) %>%
        # faster way to do this (but it doesn't adjust column names)
        # get the annual inflows/outflows to Panama Res
        summarise(across(site_selection[5]:site_selection[n_site_selection], sum))

    }
    #extract_list[[i]]

    # extract the decree capacity at end of the water rights administration QM (25)
    extract_decree_list <- list()
    for (i in 1:n_file_prefix){

      extract_decree_list[[i]] <- data_list[[i]] %>%
        # select the columns of interest
        select(year, qm, ModelRun, Decree_125_Content) %>%
        # grab the max capacity at qm 25
        filter(qm == 24) %>%
        group_by(year, ModelRun)
      # faster way to do this (but it doesn't adjust column names)
      # get the annual inflows/outflows to Panama Res
      # summarise(across(site_selection[5]:site_selection[n_site_selection], sum))

    }
    #extract_decree_list[[i]]
    extract_decree <- bind_rows(extract_decree_list[[1]], extract_decree_list[[2]]) %>%
      #arrange data by ModelRun then year to match other datasets
      arrange(., ModelRun, year)


    # merge the data from the loops together
    extract <- bind_rows(extract_list[[1]], extract_list[[2]]) %>%
      group_by(ModelRun) %>%
      # arrange to match data with extract_decree
      arrange(., ModelRun, year) %>%
      # add the 'extract_decree' dataset
      bind_cols(., extract_decree) %>%
      select(-Decree_125_Content...5, -ModelRun...11, -year...9, -qm) %>%
      rename(Decree_125_Content = Decree_125_Content...12, year = year...1,
             ModelRun = ModelRun...2) %>%
      # convert data from 'wide' format to 'long' format to plot with ggplot
      pivot_longer(cols = site_selection[5:n_site_selection], values_to = "Flow_af") %>%
      # add some data descriptors
      left_join(., cram_type, by = c("name" = "Name")) %>%
      left_join(., definitions, by = c("name" = "Name"))

    extract_levels_list <- extract %>%
      filter(year == 1915 & ModelRun == scenario_name[1])
    extract_levels_list

    definitions2 <- definitions %>%
      filter(Name == "Link_617_Flow" | Name == "Link_618_Flow")
    definitions2


    ### Check the factor & levels for the 'ModelRun' column (we will plot by this)
    # factor(extract$Description)
    # levels(extract$Description)
    # # set the factor 'levels' to the correct plotting order
    extract$Description <-factor(extract$Description,
                                 levels = c("Recapture of Boulder GREP release via Leggett to CoB Panama",
                                            "CoB Panama recapture of WWTP Reusable Water",
                                            "CoB Boulder & Whiterocks Ditch to CoB Panama",
                                            "CoB Panama Reservoir Water Right fom Leggett",
                                            "CoB 2020 Water Right for Panama",
                                            "CoB Panama Resv Stored Reusable Release"))
    # Levels should be updated
    # levels(extract$Description)



    g <- list()
    for (i in 1:n_scenario_name){

      # subset the data for only the data needed & filter for the necessary scenario
      extract_plot <- extract %>%
        filter(ModelRun == scenario_name[i])


      # make a stacked area plot of the reusable supply scenario
      g[[i]] <- ggplot() +
        geom_bar(data = filter(extract_plot, Type == "Inflow"),
                 aes_string(x = "year", y = "Flow_af", fill = "Description"),
                 position="stack", stat="identity", color = "black", size = 0.05) + #
        # geom_line(data = filter(extract_plot, Type == "Decree Capacity"),
        #           aes_string(x = "year", y = "Flow_af", color = "Description"),
        #           size = 0.75) +
        scale_color_manual(values = "black") +
        theme_bw() +
        ylim(0, 6000) +
        ylab("Flow (af)") +
        xlab("Water Year") +
        ggtitle(paste(scenario_name[i], ": Panama Reservoir Inflows by Year", sep = "")) +
        theme(plot.title = element_text(size = title_size),
              axis.title = element_text(size = xaxis_size))
      #scale_fill_manual(values = c("#0073C2FF", "#EFC000FF", ))
      #g[[i]]


    }

    # define the plot name
    plot_title <- "4a. Panama Reservoir Inflow Water Type"
    file_name <- paste(plot_title, " 2x1 ", sep = "")

    ggsave(
      paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
            output_folder, device_type, sep = ""),
      width = 14, height = 8,
      grid.arrange(g[[1]], g[[2]], nrow = 2,
                   top = plot_title,
                   right = ""))


    # Get & Plot Panama Pond Average (qm & annual) Contents --------------------------------------

    # select the sites you want to plot
    site_selection <- c("year", "qm", "Date", "ModelRun", "Reservoir_25_Content")
    n_site_selection <- length(site_selection)
    site_start_no <- 5
    site_selection_short <- site_selection[site_start_no:n_site_selection]
    n_site_selection_short <- length(site_selection_short)

    extract_list <- list()
    for (i in 1:n_file_prefix){

      extract_list[[i]] <- data_list[[i]] %>%
        # select the columns of interest from the vector above using !!!syms to read it properly
        select(!!!syms(site_selection)) %>%
        group_by(year, ModelRun) %>%
        # faster way to do this (but it doesn't adjust column names)
        summarize(panama_annual_max_contents = max(Reservoir_25_Content),
                  panama_annual_avg_contents = mean(Reservoir_25_Content))
      # get the annual inflows/outflows to Panama Res
      # summarise(across(site_selection[5]:site_selection[n_site_selection], sum))

    }


    # combine the data and process the data
    extract <- bind_rows(extract_list[[1]], extract_list[[2]]) %>%
      # convert from 'wide' to 'long' format for plotting w/ ggplot
      pivot_longer(., cols = c(panama_annual_max_contents, panama_annual_avg_contents),
                   names_to = "Name", values_to = "Output")

    # set the levels/factors
    extract$ModelRun <-factor(extract$ModelRun, levels = c(scenario_name))



    ### Plot the annual wittemyer contents
    site_list <- c("panama_annual_max_contents", "panama_annual_avg_contents")
    title_list <- c("Panama Reservoir Maximum Annual Contents", "Panama Reservoir Average Annual Contents")
    n_site_list <- length(site_list)
    g <- list()

    for (i in 1:n_site_list){

      # subset the data for only the data needed & filter for the necessary scenario
      extract_plot <- extract %>%
        filter(Name == site_list[i])

      g[[i]] <- ggplot() +
        geom_line(data = extract_plot,
                  aes_string(x = "year", y = "Output", color = "ModelRun", linetype = "ModelRun")) +
        ylab("Contents (af)") +
        xlab("Water Year") +
        ggtitle(title_list[i]) +
        theme_bw() +
        theme(plot.title = element_text(size = title_size),
              axis.title = element_text(size = xaxis_size))

      # # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
      # g[[i]] <- ggplot(wittemyer_contents_annual, aes_string(x = "year", y = site_list[i], color = "ModelRun",
      #                                                        linetype = "ModelRun")) +
      #   geom_line() +  #col = color_list[i]
      #   theme_bw() +
      #   ylim(0, 2200) +
      #


    }
    #g[[2]]

    # define the plot name
    plot_title <- "4d. Panama Reservoir Average Annual Contents"
    file_name <- paste(plot_title, " 2x1 ", sep = "")

    ggsave(
      paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
            output_folder, device_type, sep = ""),
      width = 14, height = 8,
      grid.arrange(g[[1]], g[[2]], nrow = 2,
                   top = plot_title,
                   right = ""))


    # Panama Res QM Time Series Plots -----------------------------------------

    # select the sites you want to plot
    site_selection <- c("year", "qm", "Date", "ModelRun", "Reservoir_25_Content",
                        "Link_571_Flow", "Link_572_Flow", "Link_617_Flow",
                        "Link_618_Flow",  "Link_573_Flow")
    n_site_selection <- length(site_selection)
    site_start_no <- 5
    site_selection_short <- site_selection[site_start_no:n_site_selection]
    n_site_selection_short <- length(site_selection_short)


    extract_list <- list()
    for (i in 1:n_file_prefix){

      extract_list[[i]] <- data_list[[i]] %>%
        # select the columns of interest from the vector above using !!!syms to read it properly
        select(!!!syms(site_selection)) %>%
        group_by(year, ModelRun)
      # faster way to do this (but it doesn't adjust column names)
      # get the annual inflows/outflows to Panama Res
      # summarise(across(site_selection[5]:site_selection[n_site_selection], sum))

    }


    # combine the data and process the data
    extract <- bind_rows(extract_list[[1]], extract_list[[2]]) %>%
      # convert from 'wide' to 'long' format for plotting w/ ggplot
      pivot_longer(., cols = all_of(site_selection_short),
                   names_to = "Name", values_to = "Output") %>%
      #join the CRAM model descriptions to this dataset
      left_join(., definitions, by = "Name") %>%
      # filter out the No Reuse scenario (no reuse, no Panama results to present)
      filter(ModelRun == scenario_name[1])


    # plot the output
    p <- list()
    for (i in 1:n_site_selection_short){

      # select 1 flow data to plot per i iteration of loop
      extract_plot <- extract %>%
        filter(Name == site_selection_short[i])

      # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
      p[[i]] <- ggplot(extract_plot, aes_string(x = "Date", y = "Output", color = "Name")) +
        geom_line() +  #col = color_list[i]
        theme_bw() +
        #ylim(0, y_axis_max_list[i]) +
        ylab(extract_plot$Units[1]) +
        xlab("Water Year") +
        ggtitle(extract_plot$Description[1]) +
        theme(plot.title = element_text(size = title_size),
              axis.title = element_text(size = xaxis_size),
              legend.position = "none")
      # p[[i]]

    }



    # define the plot name
    plot_title <- "4c. Panama Reservoir Quarter-Monthly Time Series Plot"
    file_name <- paste(plot_title, " 5x1 ", sep = "")

    # save the plot
    ggsave(
      paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
            output_folder, device_type, sep = ""),
      width = 14, height = 8,
      grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], nrow = 6,
                   top = plot_title,
                   right = ""))




    # Instream Flow Calcs -----------------------------------------------------

    ### DRY YEAR
    # upper ISF Oct-Apr 5 cfs
    # upper ISF May-Sep 7 cfs
    # Lower ISF Oct-Apr 1.5 cfs
    # Lower ISF May-Sep 2.5 cfs
    UPPER_DRY_OCTAPR_CFS = 5
    UPPER_DRY_MAYSEP_CFS = 7
    LOWER_DRY_OCTAPR_CFS = 1.5
    LOWER_DRY_MAYSEP_CFS = 2.5

    ### AVG or WET YEAR
    # upper ISF Oct-Apr 7 cfs
    # upper ISF May-Sep 10 cfs
    # Lower ISF Oct-Apr 2.5 cfs
    # Lower ISF May-Sep 4 cfs
    UPPER_AVG_OCTAPR_CFS = 7
    UPPER_AVG_MAYSEP_CFS = 10
    LOWER_AVG_OCTAPR_CFS = 2.5
    LOWER_AVG_MAYSEP_CFS = 4


    # select the sites you want to plot
    site_selection <- c("year", "qm", "wyqm", "Date", "ModelRun", "Link_350_Flow",
                        "Link_61_Flow", "Link_63_Flow", "DataObject_35_Flow", "Reservoir_21_Content",
                        "DataObject_34_Flow", "DataObject_33_Flow", "Link_42_Flow")
    n_site_selection <- length(site_selection)

    extract_list <- list()
    for (i in 1:n_file_prefix){

      extract_list[[i]] <- data_list[[i]] %>%
        group_by(year, ModelRun) %>%
        select(!!!syms(site_selection)) %>%
        left_join(., isf_year_type, by = "wyqm") %>%
        mutate(GREP_Upper_ISF_cfs = if_else(DataObject_33_Flow == 350,
                                            (Link_350_Flow / DaysInQM / 1.9835),
                                            (Link_42_Flow / DaysInQM / 1.9835))) %>%
        #select(-Link_350_Flow, -Link_61_Flow, -Link_63_Flow) %>%
        mutate(GREP_Upper_ISF_target = if_else(!!sym(ISF_list[i]) == "DRY",
                                               ifelse(qm <= 28, UPPER_DRY_OCTAPR_CFS, UPPER_DRY_MAYSEP_CFS),      #if yes (DRY)
                                               ifelse(qm <= 28, UPPER_AVG_OCTAPR_CFS, UPPER_AVG_MAYSEP_CFS))) %>% # if no (AVG+)
        mutate(Upper_ISF_shortage_cfs = if_else((GREP_Upper_ISF_cfs >= GREP_Upper_ISF_target),
                                                0,
                                                round(GREP_Upper_ISF_cfs - GREP_Upper_ISF_target, 1))) %>%
        mutate(GREP_Lower_ISF_cfs = if_else(DataObject_34_Flow == 63,
                                            (Link_63_Flow / DaysInQM / 1.9835),
                                            (Link_61_Flow / DaysInQM / 1.9835))) %>%
        #select(-Link_350_Flow, -Link_61_Flow, -Link_63_Flow) %>%
        mutate(GREP_Lower_ISF_target = if_else(!!sym(ISF_list[i]) == "DRY",
                                               ifelse(qm <= 28, LOWER_DRY_OCTAPR_CFS, LOWER_DRY_MAYSEP_CFS),      #if yes (DRY)
                                               ifelse(qm <= 28, LOWER_AVG_OCTAPR_CFS, LOWER_AVG_MAYSEP_CFS))) %>% # if no (AVG+)
        mutate(Lower_ISF_shortage_cfs = if_else((GREP_Lower_ISF_cfs >= GREP_Lower_ISF_target),
                                                0,
                                                round(GREP_Lower_ISF_cfs - GREP_Lower_ISF_target, 1))) %>%
        mutate(Upper_ISF_shortage_af = Upper_ISF_shortage_cfs * 1.9835 * DaysInQM) %>%
        mutate(Lower_ISF_shortage_af = Lower_ISF_shortage_cfs * 1.9835 * DaysInQM)


    }

    # process the 'compare' site first
    extract <- bind_rows(extract_list[[1]], extract_list[[2]]) %>%
      group_by(year, ModelRun) %>%
      summarise(Annual_Upper_ISF_Shortage_af = sum(Upper_ISF_shortage_af),
                Annual_Lower_ISF_Shortage_af = sum(Lower_ISF_shortage_af),
                COB_Laf_GREP_contents = mean(Reservoir_21_Content))

    ### Check the factor & levels for the 'ModelRun' column (we will plot by this)
    #factor(extract$ModelRun)
    #levels(extract$ModelRun)
    # # set the factor 'levels' to the correct plotting order
    extract$ModelRun <-factor(extract$ModelRun,
                              levels = c(scenario_name[1], scenario_name[2]))
    # Levels should be updated
    levels(extract$ModelRun)


    site_list <- c("Annual_Upper_ISF_Shortage_af", "Annual_Lower_ISF_Shortage_af", "COB_Laf_GREP_contents")
    title_list <- c("Annual_Upper_ISF_Shortage_af", "Annual_Lower_ISF_Shortage_af", "Boulder + Lafayette GREP Contents (Annual Average)")
    n_site_list <- length(site_list)
    y_axis_label_list <- c("Flow (af)", "Flow (af)", "Contents (af)")
    p <- list()
    for (i in 1:n_site_list){

      # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
      p[[i]] <- ggplot(extract, aes_string(x = "year", y = site_list[i], color = "ModelRun",
                                           linetype = "ModelRun")) +
        geom_line() +  #col = color_list[i]
        theme_bw() +
        #ylim(0, y_axis_max_list[i]) +
        ylab(y_axis_label_list[i]) +
        xlab("Water Year") +
        ggtitle(title_list[i]) +
        theme(plot.title = element_text(size = title_size),
              axis.title = element_text(size = xaxis_size))


    }
    #p[[2]]

    # define the plot name
    plot_title <- "6a. Instream Flow Annual Shortage Plots"
    file_name <- paste(plot_title, " 2x2 ", sep = "")

    # save the plot
    ggsave(
      paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
            output_folder, device_type, sep = ""),
      width = 14, height = 8,
      grid.arrange(p[[1]], p[[2]], p_drought_triggers, p[[3]], nrow = 2,
                   top = plot_title, right = ""))



    ### make tables of the ISF data
    extract_tables_upper <- extract %>%
      group_by(ModelRun) %>%
      mutate(Upper_ISF_binary = if_else(Annual_Upper_ISF_Shortage_af < 0, 1, 0)) %>%
      mutate(Lower_ISF_binary = if_else(Annual_Lower_ISF_Shortage_af < 0, 1, 0)) %>%
      summarise('Upper ISF Shortage (mean)' = round(mean(Annual_Upper_ISF_Shortage_af),1),
                'Upper ISF Shortage (min)' = round(min(Annual_Upper_ISF_Shortage_af),1),
                'Upper ISF Shortage Years (count)' = sum(Upper_ISF_binary)
      )
    extract_tables_upper


    extract_tables_lower <- extract %>%
      group_by(ModelRun) %>%
      mutate(Upper_ISF_binary = if_else(Annual_Upper_ISF_Shortage_af < 0, 1, 0)) %>%
      mutate(Lower_ISF_binary = if_else(Annual_Lower_ISF_Shortage_af < 0, 1, 0)) %>%
      summarise('Lower ISF Shortage (mean)' = round(mean(Annual_Lower_ISF_Shortage_af),1),
                'Lower ISF Shortage (min)' = round(min(Annual_Lower_ISF_Shortage_af),1),
                'Lower ISF Shortage Years (count)' = sum(Lower_ISF_binary)
      )
    extract_tables_lower


    ### export the two datasets as tables to ggarrange
    # set the theme, sizes
    size <- 1
    size1 <- 1
    tt <- ttheme_default(core = list(fg_params=list(cex = size)),
                         colhead = list(fg_params=list(cex = size1)),
                         rowhead = list(fg_params=list(cex = size)))


    ### Table 1
    tbl_temp_a <- tableGrob(extract_tables_upper, theme = tt, rows = NULL)
    # add grid around the headers
    tbl_temp_a <- gtable_add_grob(tbl_temp_a,
                                  grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                                  t = 1, l = 1, r = ncol(tbl_temp_a))
    # add box around the first column of data
    tbl_temp_a <- gtable_add_grob(tbl_temp_a,
                                  grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                                  t = 1, b = nrow(tbl_temp_a), l = 1, r = 1)
    # add box around the second model run of data
    tbl_temp_a <- gtable_add_grob(tbl_temp_a,
                                  grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                                  t = 1, b = nrow(tbl_temp_a), l = 2, r = 4)
    # # add box around the second model run of data
    # tbl_temp_a <- gtable_add_grob(tbl_temp_a,
    #                               grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
    #                               t = 1, b = nrow(tbl_temp_a), l = 5, r = ncol(tbl_temp_a))

    grid.draw(tbl_temp_a)


    ### Table 2
    tbl_temp_b <- tableGrob(extract_tables_lower, theme = tt, rows = NULL)
    # add grid around the headers
    tbl_temp_b <- gtable_add_grob(tbl_temp_b,
                                  grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                                  t = 1, l = 1, r = ncol(tbl_temp_b))
    # add box around the first column of data
    tbl_temp_b <- gtable_add_grob(tbl_temp_b,
                                  grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                                  t = 1, b = nrow(tbl_temp_b), l = 1, r = 1)
    # add box around the second model run of data
    tbl_temp_b <- gtable_add_grob(tbl_temp_b,
                                  grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                                  t = 1, b = nrow(tbl_temp_b), l = 2, r = 4)
    # # add box around the second model run of data
    # tbl_temp_b <- gtable_add_grob(tbl_temp_b,
    #                               grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
    #                               t = 1, b = nrow(tbl_temp_b), l = 5, r = ncol(tbl_temp_b))

    grid.draw(tbl_temp_b)


    plot_title <- "6b. Instream Flow Annual Shortage Tables"
    file_name <- paste(plot_title, " 2x2 ", sep = "")


    # save the plot
    # pdf(
    #   paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
    #         output_folder, ".pdf", sep = ""),
    #   width = 14, height = 8)

    # grid.arrange(tbl_temp_a, tbl_temp_b, nrow = 2,
    #              top = plot_title,
    #              right = "", bottom = "",
    #              layout_matrix = rbind(c(1, 1),
    #                                    c(3, 3)))

    tbl_temp_b2 <- grid.arrange(tbl_temp_b, nrow = 1,
                                top = plot_title,
                                right = "", bottom = "",
                                layout_matrix = rbind(c(1, 1),
                                                      c(3, 3)))

    ggsave(
      filename =  paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",                    output_folder, ".png", sep = ""),
      width = 12,
      height = 8,
      tbl_temp_b2
    )
    grid.text("Annual Upper Instream Flow Shortages (af)", x = unit(0.5, "npc"), y = unit(0.81, "npc"),
              gp = gpar(fontsize = 14))
    grid.text("Annual Lower Instream Flow Shortages (af)", x = unit(0.5, "npc"), y = unit(0.34, "npc"),
              gp = gpar(fontsize = 14))
    # grid.text("Monthly C-BT Water", x = unit(0.5, "npc"), y = unit(0.52, "npc"),
    #           gp = gpar(fontsize = 14))

    dev.off()

  }
  # South Platte Calls Analysis ---------------------------------------------

  # CBT Boulder Res qm output 6c -----------------------------------------

  # showing DO30, DO2, and Res 12, that would be great

  extract <- list()

  # i <- 1
  for (i in 1:n_file_prefix){

    extract_list[[i]] <- data_list[[i]] %>%
      # group by ModelRun to calculate values by group
      group_by(ModelRun) %>%
      # select the columns of interest from the vector above using !!!syms to read it properly
      select("year", "qm", "Date", "ModelRun", "DataObject_30_Flow", "DataObject_2_Flow",
             "Reservoir_12_Content") %>%
      # now, group data by Year & ModelRun to sum data annually
      group_by(year, ModelRun)

    extract_list[[i]]

  }

  # merge the data from the loops together
  extract <- bind_rows(extract_list[[1]], extract_list[[2]])

  ### Check the factor & levels for the 'ModelRun' column (we will plot by this)
  # factor(cbt_windygap$ModelRun)
  # levels(cbt_windygap$ModelRun)
  # # set the factor 'levels' to the correct plotting order
  extract$ModelRun <-factor(extract$ModelRun, levels = c(scenario_name))
  # Levels should be updated
  levels(extract$ModelRun)


  # put to long form
  extract2 <- extract %>%
    # convert from 'wide' to 'long' format for plotting w/ ggplot
    pivot_longer(., cols = c("DataObject_30_Flow", "DataObject_2_Flow", "Reservoir_12_Content"),
                 names_to = "Name", values_to = "Output")
  extrac2_site_list <- c("DataObject_30_Flow", "DataObject_2_Flow", "Reservoir_12_Content")
  n_extrac2_site_list <- length(extrac2_site_list)


  p <- list()
  for (i in 1:n_extrac2_site_list){

    # select 1 flow data to plot per i iteration of loop
    extract_plot <- extract2 %>%
      filter(Name == extrac2_site_list[i])


    # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
    p[[i]] <- ggplot(extract_plot, aes_string(x = "Date", y = "Output", color = "ModelRun",
                                              linetype = "ModelRun")) +
      geom_line() +
      theme_bw() +
      ylim(0, NA) +
      ylab(y_lab_list[i]) +
      xlab("Water Year") +
      ggtitle(extract_plot$Name[1]) +
      theme(plot.title = element_text(size = title_size),
            axis.title = element_text(size = xaxis_size))

    # p[[i]]

  }

  # define the plot name
  plot_title <- "2k. Boulder Res QM - Time Series Plot"
  file_name <- paste(plot_title, " 3x1 ", sep = "")

  # save the plot
  ggsave(
    paste(model_folder, "/", output_folder, "/", file_name,base_model_version, " ",
          output_folder, device_type, sep = ""),
    width = 14, height = 8,
    grid.arrange(p[[1]], p[[2]], p[[3]], nrow = 3,
                 top = plot_title,
                 right = ""))

  rm(n_extrac2_site_list, extrac2_site_list, extract, extract2)

}
