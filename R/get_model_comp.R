# function takes in 2 model datasets to compare and outputs plots into a new output folder
# Author: Angus Watters

# clear R working directory
# rm(list = ls())

# library(tidyverse)
# library(lubridate)
# library(gridExtra)
# library(RColorBrewer)
# library(gtable)
# library(grid)

source("R/make_lookup.R")
source("R/make_plots.R")
source("R/parse_directory.R")
source("R/process_inputs.R")
source("R/utils.R")


# # model_dir
# base_path  <- "D:/cob/latest/latest"
#
# # base and comp model names
# base_model <- "0000.DRRP_WEP_2023_003_ID1_Base"
# comp_model <- "0101.DRRP_WEP_2023_003_ID1_2050_18pctOutdoorReduction_7525"
#
# # read in the quarter-monthly to date converter
# date_df    <- readr::read_csv("data-raw/qm_to_date_conversion.csv")
#
# # borrowing on or off input
# borrow     <- "off"
#
# # instream flow values
# upper_dry_oct_apr_cfs = NULL
# upper_dry_may_sep_cfs = NULL
# lower_dry_oct_apr_cfs = NULL
# lower_dry_may_sep_cfs = NULL
# upper_avg_oct_apr_cfs = NULL
# upper_avg_may_sep_cfs = NULL
# lower_avg_oct_apr_cfs = NULL
# lower_avg_may_sep_cfs = NULL
#
# # start and end years
# start_year = 1915
# end_year   = 2014
#
# # title and axis font sizes
# title_size = 10
# xaxis_size = 9
#
# # path to create output directory and save plots into
# save_path  <- "D:/cob/latest/latest"
#
# get_model_comp(
#   base_path  = "D:/cob/latest/latest",
#   base_model = "0000.DRRP_WEP_2023_003_ID1_Base",
#   comp_model = "0101.DRRP_WEP_2023_003_ID1_2050_18pctOutdoorReduction_7525",
#   date_df    = date_df,
#   borrow     = "off",
#   start_year = 1915,
#   end_year   = 2014,
#   title_size = 10,
#   xaxis_size = 9,
#   save_path  = "D:/cob/latest/latest"
# )


get_model_comp <- function(
  base_path,
  base_model,
  comp_model,
  date_df,
  borrow,
  upper_dry_oct_apr_cfs = NULL,
  upper_dry_may_sep_cfs = NULL,
  lower_dry_oct_apr_cfs = NULL,
  lower_dry_may_sep_cfs = NULL,
  upper_avg_oct_apr_cfs = NULL,
  upper_avg_may_sep_cfs = NULL,
  lower_avg_oct_apr_cfs = NULL,
  lower_avg_may_sep_cfs = NULL,
  start_year = 1915,
  end_year   = 2014,
  title_size = 10,
  xaxis_size = 9,
  save_path
) {

  # info on model files
  model_dirs  <- parse_directory(base_folder = base_path)

  # path to ISF data
  isf_path <-
    model_dirs %>%
    dplyr::filter(grepl("ISF", file)) %>%
    .$path

  # path to Quota data
  quota_path <-
    model_dirs %>%
    dplyr::filter(grepl("Quota", file)) %>%
    .$path

  # base model paths
  base_mods <-
    model_dirs %>%
    dplyr::filter(
      grepl(base_model, file)
      # grepl("DRRP_DroughtPlan_2020_055a_ID1_7525", file)
    )

  # comparison model paths
  comp_mods <-
    model_dirs %>%
    dplyr::filter(
      grepl(comp_model, file)
    )

  # base model annual summary
  base_summary <-
    base_mods %>%
    dplyr::filter(
      output == "OutputAnnualSummary"
    )

  # comparison model annual summary
  comp_summary <-
    comp_mods %>%
    dplyr::filter(
      output == "OutputAnnualSummary"
    )

  # Base output sheet
  base_out <-
    base_mods %>%
    dplyr::filter(
      output == "OutputSheet"
    )

  # comparison output sheet
  comp_out <-
    comp_mods %>%
    dplyr::filter(
      output == "OutputSheet"
    )

  # Scenario names
  scenario_name <- c(
    paste0(
      base_out$model_num,  "-", base_out$model_id,
      ifelse(base_out$extra_info == "", "", paste0("_", base_out$extra_info)),
      "-",
      base_out$model_version
    ),
    paste0(
      comp_out$model_num,  "-", comp_out$model_id,
      ifelse(comp_out$extra_info == "", "", paste0("_", comp_out$extra_info)),
      "-",
      comp_out$model_version
    )
  )

  # output folder name
  output_folder <- paste0(
    base_out$model_id, "-", base_out$model_num, "-",  gsub("v", "", base_out$model_version),
    ifelse(base_out$extra_info != "", paste0("-", base_out$extra_info), ""),
    " vs. ",
    comp_out$model_id,  "-", comp_out$model_num, "-",  gsub("v", "", comp_out$model_version),
    ifelse(comp_out$extra_info != "", paste0("-", comp_out$extra_info), "")
  )

  # Folder to hold all outputs/plots from this script
  output_dir <- paste0(save_path, "/output")

  # check if directory already exists, if it doesn't create the directory
  if (!dir.exists(output_dir)) {

    message(paste0("Creating output folder:\n\n  ", output_dir))

    # create /output directory
    dir.create(output_dir)

    # create model folder directory
    dir.create(paste0(output_dir, "/", output_folder))

  } else {

    if(!dir.exists(paste0(output_dir, "/", output_folder))) {

      message(paste0("Creating model comparison folder:\n\n  ", output_dir))

      # create model folder directory
      dir.create(paste0(output_dir, "/", output_folder))

    } else {

      # if model comparison folder already exists
      message(paste0("Model comparison folder already exists:\n\n  ", output_dir, "/", output_folder))

    }
  }

  # path to save out destination
  model_comp_dir <- paste0(output_dir, "/", output_folder)

  # scenario_name

  # names of columns to select from ISF dataset
  model_id_subs <- c(
    paste0(base_out$model_id, base_out$model_num),
    paste0(comp_out$model_id, comp_out$model_num)
  )

  # Load ISF data and select columns of interest
  isf_year_type <-
    process_isf(
      isf_path = isf_path
    ) %>%
    dplyr::select(wyqm, days_in_qm, dplyr::contains(model_id_subs))

  # ***********************
  # ---- Preprocessing ----
  # ***********************

  quota <-
    process_quota(
      quota_path = quota_path,
      model_ids  = model_id_subs,
      start_year = start_year,
      end_year   = end_year
    )

  quota <-
    dplyr::bind_rows(
      dplyr::mutate(quota, scenario = scenario_name[1]),
      dplyr::mutate(quota, scenario = scenario_name[2])
    ) %>%
    dplyr::mutate(
      model_run = factor(scenario, levels = c(rev(scenario_name)))
    ) %>%
    dplyr::select(-scenario)

  # ********************
  # ---- Quota plot ----
  # ********************

  message(paste0("Plotting: Quota data"))

  # quota plot
  quota_plot <- make_quota_plot(
    df         = quota,
    title_size = title_size,
    xaxis_size = xaxis_size
  )

  # list of model dataframes
  mod_lst <- list(base_out, comp_out)

  # loop through model run output sheets and read in and process data
  outputs <- lapply(1:length(mod_lst), function(y) {

    outs <- process_output(
      file_df = mod_lst[[y]],
      date_df = date_df
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

  # read in annual data
  data_annual <- readr::read_csv(
    base_summary$path,
    col_names = FALSE
  )

  # ******************
  # ---- Table 1A ----
  # ******************

  # message(paste0("Page - 1A"))

  mod_summary_lst <- list(base_summary, comp_summary)

  # loop through model run output sheets and read in and process data
  data_annual_lst <- lapply(1:length(mod_summary_lst), function(y) {

    message(paste0("Processing annual summary:\n", mod_summary_lst[[y]]$file))

    process_drought_metrics(
      summary_path = mod_summary_lst[[y]]$path,
      model_run    = scenario_name[y]
    )

  }) %>%
    dplyr::bind_rows()

  # Drought response table
  tbl_temp <- make_drought_table(
    df = data_annual_lst
  )

  # *****************
  # ---- Plot 1A ----
  # *****************

  # May 1 Annual PSI and Reservoir Storage Plots

  # loop through model run output sheets and extract drought indices for each model run
  drought_index <- lapply(1:length(scenario_name), function(y) {

    message(paste0("Processing drought indices - ", scenario_name[y]))
    process_drought_index(
      output_df = outputs,
      model_run = scenario_name[y],
      qm        = 29
    )

  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      model_run = factor(model_run, levels = c(rev(scenario_name)))
    )

  # drought plots to loop over
  drought_plot_lst <- na.omit(unique(drought_index$title))

  drought_plots <- lapply(1:length(drought_plot_lst), function(z) {

    extract_df <-
      drought_index %>%
      dplyr::filter(title == drought_plot_lst[z])
    # dplyr::rename("Model run" = model_run)

    message(paste0("Plotting: ", drought_plot_lst[z]))

    # Drought response plot
    if(drought_plot_lst[z] == "Drought Response Level") {

      dplot <- make_drought_response_plot(
        df         = extract_df,
        plot_name  = drought_plot_lst[z],
        ylab_title = unique(extract_df$ylabs),
        title_size = title_size,
        xaxis_size = xaxis_size
      )

    }

    # Drought response plot
    if(drought_plot_lst[z] == "Projected Storage Index") {

      dplot <- make_psi_plot(
        df         = extract_df,
        plot_name  = drought_plot_lst[z],
        ylab_title = unique(extract_df$ylabs),
        title_size = title_size,
        xaxis_size = xaxis_size
      )

    }

    # Drought response plot
    if(!drought_plot_lst[z] %in% c("Projected Storage Index", "Drought Response Level")) {

      dplot <- make_res_content_plot(
        df                = extract_df,
        plot_name         = drought_plot_lst[z],
        ylab_title        = unique(extract_df$ylabs),
        storage_max_hline = unique(extract_df$storage_max),
        title_size        = title_size,
        xaxis_size        = xaxis_size
      )

    }

    dplot

  }) %>%
    stats::setNames(c(drought_plot_lst))

  # save the plot
  ggplot2::ggsave(
    filename = paste0(model_comp_dir, "/", "1a. May 1 Annual Reservoir Contents - Time Series Plot 3x2.png"),
    width  = 16,
    height = 8,
    gridExtra::grid.arrange(
      drought_plots[["Projected Storage Index"]],
      drought_plots[["Drought Response Level"]],
      drought_plots[["Barker Reservoir Contents"]],
      drought_plots[["NBC Reservoir Contents"]],
      drought_plots[["Total Upper Reservoir Contents"]],
      tbl_temp,
      nrow   = 3,
      top    = "1a. May 1 Annual Reservoir Contents - Time Series Plot",
      right  = "",
      bottom = ""
    )
  )

  # *****************
  # ---- Plot 1B ----
  # *****************

  # message(paste0("Page - 1B"))
  # Mass Balance by Source calculations and plots

  # calculate mass balance by source
  mass_bal_source <-
    outputs %>%
    process_mass_balance_source() %>%
    dplyr::mutate(
      model_run = factor(model_run, levels = c(rev(scenario_name)))
    )

  # extra sites of interest (Not CBT_Inflow or WindyGap_)
  mass_source_sites <- unique(mass_bal_source$name)[!grepl("CBT_Inflow|WindyGap_Inflow", unique(mass_bal_source$name))]

  # loop through each site and plot
  mass_bal_source_lst <- lapply(1:length(mass_source_sites), function(i) {

    message(paste0("Plotting: Mass Balance by Source - ", mass_source_sites[i]))

    extract_df <-
      mass_bal_source %>%
      dplyr::filter(name == mass_source_sites[i])
    # dplyr::rename("Model run" = model_run)

    mass_bal_plot <- make_mass_balance_plot(
      df         = extract_df,
      plot_title = unique(extract_df$title),
      ymax       = unique(extract_df$ylabs_max),
      title_size = title_size,
      xaxis_size = xaxis_size
    )

    mass_bal_plot

  }) %>%
    stats::setNames(c(mass_source_sites))

  # save the gridded mass balance plots
  ggplot2::ggsave(
    filename = paste0(model_comp_dir, "/", "1b. Annual Supply by Water Type with Demands - Time Series Plot 3x2.png"),
    width    = 14,
    height   = 8,
    plot     = gridExtra::arrangeGrob(
      mass_bal_source_lst[["Direct_Flow_Rights"]],
      mass_bal_source_lst[["Reservoir_Release"]],
      mass_bal_source_lst[["Direct_Exchange"]],
      mass_bal_source_lst[["COB_Water_Demand"]],
      mass_bal_source_lst[["COB_Indoor_Demand"]],
      mass_bal_source_lst[["COB_Outdoor_Demand"]],
      nrow   = 3,
      top    = "1b. Annual Supply by Water Type with Demands - Time Series Plot",
      right  = "",
      left   = "",
      bottom = ""
    )
  )


  # *****************
  # ---- Plot 1C ----
  # *****************

  # message(paste0("Page - 1C"))

  # Mass Balance by Source calculations and plots

  # calculate mass balance by source
  mass_bal_pipe <-
    outputs %>%
    process_mass_balance_pipeline() %>%
    dplyr::mutate(
      model_run = factor(model_run, levels = c(rev(scenario_name)))
    )

  # extra mass balance pipeline sites of interest
  mass_pipe_sites <- unique(mass_bal_pipe$name)

  # loop through each site and plot
  mass_bal_pipe_lst <- lapply(1:length(mass_pipe_sites), function(i) {

    message(paste0("Plotting: Mass Balance by Source - ", mass_pipe_sites[i]))

    extract_df <-
      mass_bal_pipe %>%
      dplyr::filter(name == mass_pipe_sites[i])
    # dplyr::rename("Model run" = model_run)

    mass_bal_plot <- make_mass_balance_plot(
      df         = extract_df,
      plot_title = unique(extract_df$title),
      ymax       = unique(extract_df$ylabs_max),
      title_size = title_size,
      xaxis_size = xaxis_size
    )

    mass_bal_plot

  }) %>%
    stats::setNames(c(mass_pipe_sites))

  # save the plot
  ggplot2::ggsave(
    filename = paste0(model_comp_dir, "/", "1c. Annual Water Delivery by Pipeline - Time Series Plot 2x2.png"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      mass_bal_pipe_lst[["LakewoodtoBetasso"]],
      mass_bal_pipe_lst[["BarkerGravitytoBetasso"]],
      mass_bal_pipe_lst[["BoulderRestoWTP"]],
      mass_bal_pipe_lst[["FarmersRighttoWTP"]],
      nrow   = 2,
      top    = "1c. Annual Water Delivery by Pipeline - Time Series Plot",
      right  = "",
      bottom = ""
    )
  )

  # **********************
  # ---- Plot 2C + 2D ----
  # **********************

  # CBT, Windy Gap, Reusable Water Exchange Analysis
  reuse_water_exchange <-
    outputs %>%
    process_reuse_water_exchange() %>%
    dplyr::mutate(
      model_run = factor(model_run, levels = c(rev(scenario_name)))
    )

  # extra mass balance pipeline sites of interest
  reuse_sites <- unique(reuse_water_exchange$name)

  # loop through each site and plot
  reuse_water_lst <- lapply(1:length(reuse_sites), function(i) {

    message(paste0("Plotting: Reusable Water Exchange Analysis - ", reuse_sites[i]))

    extract_df <-
      reuse_water_exchange %>%
      dplyr::filter(name == reuse_sites[i])
    # dplyr::rename("Model run" = model_run)

    reuse_plot <- make_reuse_water_exchange_plots(
      df         = extract_df,
      plot_title = unique(extract_df$title),
      ylab_title = unique(extract_df$ylabs_title),
      ymax       = unique(extract_df$ylabs_max),
      title_size = title_size,
      xaxis_size = xaxis_size
    )

    reuse_plot

  }) %>%
    stats::setNames(c(reuse_sites))

  # save the plot 2C
  ggplot2::ggsave(
    filename = paste0(model_comp_dir, "/", "2c. CBT-Windy Gap Exchange Total Annual - Time Series Plot 3x2.png"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      reuse_water_lst[["CBT_Inflow"]],
      quota_plot,
      reuse_water_lst[["WindyGap_Inflow"]],
      reuse_water_lst[["BoulderRes_WGExchtoBarker"]],
      reuse_water_lst[["BoulderRes_WGExchtoNBCRes"]],
      reuse_water_lst[["BoulderRes_WGExctoUpperStor"]],
      nrow   = 3,
      top    = "2c. CBT-Windy Gap Exchange Total Annual - Time Series Plot",
      right  = "",
      bottom = ""
    )
  )

  # save plot 2D
  ggplot2::ggsave(
    filename = paste0(model_comp_dir, "/", "2d. Reusable Water Annual Storage - Time Series Plot 3x2.png"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      reuse_water_lst[["WindyGap_Inflow"]],
      reuse_water_lst[["BoulderRes_WGExctoUpperStor"]],
      reuse_water_lst[["BarkerRes_ReusableWater"]],
      reuse_water_lst[["NBCRes_ReusableWater"]],
      reuse_water_lst[["BoulderRes_ReusableWater"]],
      reuse_water_lst[["COB_Reusable_Contents"]],
      nrow   = 3,
      top    = "2d. Reusable Water Annual Storage - Time Series Plot",
      right  = "",
      bottom = ""
    )
  )

  # ***********************
  # ---- Plot 2A + 2AB ----
  # ***********************

  # CBT, Windy Gap, Reusable Water Exchange Analysis
  cbt_quota <-
    outputs %>%
    process_cbt_quota(
      quota_df       = quota,
      definitions_df = definitions,
      borrow         = borrow
    ) %>%
    dplyr::mutate(
      model_run = factor(model_run, levels = c(rev(scenario_name))),
      year = as.numeric(year)
    )

  # unique quota runs
  cbt_quota_runs <- unique(cbt_quota$model_run)

  # message(paste0("Plotting: CBT Quota Components"))

  # plot 2A, loop through each site and plot
  cbt_quota_used_lst <- lapply(1:length(cbt_quota_runs), function(i) {

    message(paste0("Plotting: CBT Used Quota Summary - ", cbt_quota_runs[i]))

    cbt_used_plots <-
      make_cbt_used_plot(
        df         = cbt_quota,
        mod_run    = cbt_quota_runs[i],
        title_size = title_size,
        xaxis_size = xaxis_size
      )


    cbt_used_plots

  }) %>%
    stats::setNames(c(cbt_quota_runs))

  # plot 2AB, loop through each site and plot
  cbt_quota_unused_lst <- lapply(1:length(cbt_quota_runs), function(i) {

    # message(paste0("Plotting: ", cbt_quota_runs[i]))
    message(paste0("Plotting: CBT Unused Quota Summary - ", cbt_quota_runs[i]))

    cbt_unused_plots <-
      make_cbt_unused_plot(
        df         = cbt_quota,
        mod_run    = cbt_quota_runs[i],
        title_size = title_size,
        xaxis_size = xaxis_size
      )


    cbt_unused_plots

  }) %>%
    stats::setNames(c(cbt_quota_runs))

  # save plot 2A
  ggplot2::ggsave(
    filename = paste0(model_comp_dir, "/", "2a. C-BT Annual Water Use 2x1.png"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      cbt_quota_used_lst[[cbt_quota_runs[[2]]]],
      cbt_quota_used_lst[[cbt_quota_runs[[1]]]],
      nrow   = 2,
      top    = "2a. C-BT Annual Water Use",
      right  = "",
      bottom = ""
    )
  )

  # save plot 2AB
  ggplot2::ggsave(
    filename = paste0(model_comp_dir, "/", "2ab. COB C-BT Annual Unused Water 2x1.png"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      cbt_quota_unused_lst[[cbt_quota_runs[[2]]]],
      cbt_quota_unused_lst[[cbt_quota_runs[[1]]]],
      nrow   = 2,
      top    = "2ab. COB C-BT Annual Unused Water",
      right  = "",
      bottom = ""
    )
  )

  # *******************
  # ---- Table 2AC ----
  # *******************

  message(paste0("Table: CBT Annual Water Use"))

  # CBT, Windy Gap, Reusable Water Exchange Analysis
  cbt_quota_tbl  <-
    cbt_quota %>%
    process_cbt_quota_tbl() %>%
    make_cbt_quota_tbl2()

  # Save table 2AC
  gt::gtsave(
    data     = cbt_quota_tbl,
    filename = paste0(model_comp_dir, "/", "2ac. COB C-BT Annual Water Use table.png"),
    zoom     = 1
  )

  # *****************
  # ---- Plot 2E ----
  # *****************

  # Reservoir Reusable Storage Annual (only for comparison model)
  res_reuse_storage <-
    outputs %>%
    process_res_reuse_storage(definition_df = definitions) %>%
    dplyr::mutate(
      model_run = factor(model_run, levels = c(rev(scenario_name)))
    ) %>%
    dplyr::filter(model_run == scenario_name[2])

  # Reservoir Reusable Storage Annual Groups to plot
  res_reuse_storage_sites <- unique(res_reuse_storage$Group)
  comp_mod_id             <- comp_out$model_id

  # plot 2AB, loop through each site and plot
  res_reuse_stor_lst <- lapply(1:length(res_reuse_storage_sites), function(i) {

    message(paste0("Plotting: Reservoir Reusable water - ", res_reuse_storage_sites[i]))

    # extract dataframe
    extract_df <-
      res_reuse_storage %>%
      dplyr::filter(Group == res_reuse_storage_sites[i]) %>%
      dplyr::mutate(
        year = as.numeric(year)

      )

    # refactor levels
    lvls <- c(unique(extract_df$Type)[!grepl("Reusable Water", unique(extract_df$Type))], "Reusable Water")

    extract_df <-
      extract_df %>%
      dplyr::mutate(
        Type = factor(Type, levels = lvls)
      )

    # Horizontal line y intercept point
    storage_max <- unique(extract_df$storage_max)

    res_reuse_plot <-
      make_res_reusable_water_plot(
        df               = extract_df,
        timescale        = "annual",
        storage_max      = storage_max,
        plot_title       = res_reuse_storage_sites[i],
        compare_model_id = comp_mod_id,
        title_size       = title_size,
        xaxis_size       = xaxis_size
      )
    res_reuse_plot

  }) %>%
    stats::setNames(c(res_reuse_storage_sites))

  # save plot 2E
  ggplot2::ggsave(
    filename = paste0(model_comp_dir, "/", "2e. Reusable Water in Reservoir - Average Annual Time Series Plot 2x2.png"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      res_reuse_stor_lst[["NBC Reservoir"]],
      res_reuse_stor_lst[["Barker Reservoir"]],
      res_reuse_stor_lst[["Boulder Reservoir"]],
      nrow   = 2,
      top    = "2e. Reusable Water in Reservoir - Average Annual Time Series Plot",
      right  = "",
      bottom = ""
    )
  )

  # *****************
  # ---- Plot 2F ----
  # *****************

  # Reservoir Reusable Storage Annual (only for comparison model)
  res_reuse_storage_qm <-
    outputs %>%
    process_res_reuse_storage_qm(definition_df = definitions) %>%
    dplyr::mutate(
      model_run = factor(model_run, levels = c(rev(scenario_name)))
    ) %>%
    dplyr::filter(model_run == scenario_name[2])

  # Reservoir Reusable Storage Annual Groups to plot
  res_reuse_storage_qm_sites <- unique(res_reuse_storage_qm$Group)

  # plot 2AB, loop through each site and plot
  res_reuse_stor_qm_lst <- lapply(1:length(res_reuse_storage_qm_sites), function(i) {

    message(paste0("Plotting: Reservoir Reusable water - ", res_reuse_storage_qm_sites[i]))

    # extract dataframe
    extract_df <-
      res_reuse_storage_qm %>%
      dplyr::filter(Group == res_reuse_storage_qm_sites[i]) %>%
      dplyr::mutate(
        qm = as.numeric(qm)
      )

    # refactor levels
    lvls <- c(unique(extract_df$Type)[!grepl("Reusable Water", unique(extract_df$Type))], "Reusable Water")

    extract_df <-
      extract_df %>%
      dplyr::mutate(
        Type = factor(Type, levels = lvls)
      )

    # Horizontal line y intercept point
    storage_max <- unique(extract_df$storage_max)

    res_reuse_qm_plot <-
      make_res_reusable_water_plot(
        df               = extract_df,
        timescale        = "qm",
        storage_max      = storage_max,
        plot_title       = res_reuse_storage_sites[i],
        compare_model_id = comp_mod_id,
        title_size       = title_size,
        xaxis_size       = xaxis_size
      )
    res_reuse_qm_plot

  }) %>%
    stats::setNames(c(res_reuse_storage_qm_sites))

  # save plot 2E
  ggplot2::ggsave(
    filename = paste0(model_comp_dir, "/", "2f. Reusable Water in Reservoir - Average Quarter-Monthly Plot 2x2.png"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      res_reuse_stor_qm_lst[["NBC Reservoir"]],
      res_reuse_stor_qm_lst[["Barker Reservoir"]],
      res_reuse_stor_qm_lst[["Boulder Reservoir"]],
      nrow   = 2,
      top    = "2f. Reusable Water in Reservoir - Average Quarter-Monthly Plot",
      right  = "",
      bottom = ""
    )
  )


  # **********************
  # ---- Plot 2H + 2I ----
  # **********************

  reuse_res_content <-
    outputs %>%
    process_reusable_res_content(definition_df = definitions) %>%
    dplyr::mutate(
      model_run = factor(model_run, levels = c(rev(scenario_name)))
    )  %>%
    dplyr::filter(model_run == scenario_name[2])

  # unique sites
  reuse_res_cont_sites <- unique(reuse_res_content$Group)

  # plot 2H and 2I, loop through each site and plot
  reuse_res_cont_lst <- lapply(1:length(reuse_res_cont_sites), function(i) {

    message(paste0("Plotting: Reservoir Reusable water - ", reuse_res_cont_sites[i]))

    # extract dataframe
    extract_df <-
      reuse_res_content %>%
      dplyr::filter(Group == reuse_res_cont_sites[i]) %>%
      dplyr::mutate(
        wyqm = as.Date(paste0(wyqm, "-01")),
        qm = as.numeric(qm)
      )

    # refactor levels
    lvls <- c(unique(extract_df$Type)[!grepl("Reusable Water", unique(extract_df$Type))], "Reusable Water")

    extract_df <-
      extract_df %>%
      dplyr::mutate(
        Type = factor(Type, levels = lvls)
      )

    # Horizontal line y intercept point
    storage_max <- unique(extract_df$storage_max)

    res_reuse_cont_plot <-
      make_res_reusable_water_plot(
        df               = extract_df,
        timescale        = "date",
        storage_max      = storage_max,
        plot_title       = reuse_res_cont_sites[i],
        compare_model_id = comp_mod_id,
        title_size       = title_size,
        xaxis_size       = xaxis_size
      )
    res_reuse_cont_plot

  }) %>%
    stats::setNames(c(reuse_res_cont_sites))

  # save plot 2F 2I
  ggplot2::ggsave(
    filename = paste0(model_comp_dir, "/", "2h. Reusable Water in Reservoir - Quarter-Monthly Time Series Plot 2x2.png"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      reuse_res_cont_lst[["NBC Reservoir"]],
      reuse_res_cont_lst[["Barker Reservoir"]],
      reuse_res_cont_lst[["Boulder Reservoir"]],
      reuse_res_cont_lst[["Wittemyer"]],
      reuse_res_cont_lst[["Panama"]],
      nrow   = 5,
      top    = "2h. Reusable Water in Reservoir - Quarter-Monthly Time Series Plot",
      right  = "",
      bottom = ""
    )
  )

  # save plot 2F 2I
  ggplot2::ggsave(
    filename = paste0(model_comp_dir, "/", "2i. Reusable Water in Reservoir - Quarter-Monthly Time Series Plot 1x1.png"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      reuse_res_cont_lst[["All 5 Reservoirs"]],
      nrow   = 1,
      top    = "2i. Reusable Water in Reservoir - Quarter-Monthly Time Series Plot",
      right  = "",
      bottom = ""
    )
  )

  # *****************
  # ---- Plot 2J ----
  # *****************
  # new WG in out

  # CBT Windygap process
  cbt_wg <-
    outputs %>%
    process_cbt_windygap() %>%
    dplyr::mutate(
      model_run = factor(model_run, levels = c(scenario_name))
    )

  # sites to loop over and plot
  cbt_wg_sites <- unique(cbt_wg$name)

  # plot 2AB, loop through each site and plot
  cbt_wg_lst <- lapply(1:length(cbt_wg_sites), function(i) {

    extract_df <-
      cbt_wg %>%
      dplyr::filter(name == cbt_wg_sites[i])

    message(paste0("Plotting: CBT Windygap - ", cbt_wg_sites[i]))

    cbt_wg_plot <-
      make_cbt_windygap_plot(
        df         = extract_df,
        site       = cbt_wg_sites[i],
        title_size = title_size,
        xaxis_size = xaxis_size
      )


    cbt_wg_plot

  }) %>%
    stats::setNames(c(cbt_wg_sites))

  # save plot 2J
  ggplot2::ggsave(
    filename = paste0(model_comp_dir, "/", "2j. Boulder Reservoir Inflow Outflow - Time Series Plot 5x2.png"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      cbt_wg_lst[["Decree_75_Flow"]],
      cbt_wg_lst[["Link_499_Flow"]],
      cbt_wg_lst[["Link_451_Flow"]],
      cbt_wg_lst[["Link_452_Flow"]],
      cbt_wg_lst[["Link_375_Flow"]],
      cbt_wg_lst[["Link_457_Flow"]],
      cbt_wg_lst[["Link_454_Flow"]],
      cbt_wg_lst[["DataObject_29_Flow"]],
      cbt_wg_lst[["DataObject_1_Flow"]],
      cbt_wg_lst[["DataObject_2_Flow"]],
      nrow   = 5,
      top    = "2j. Boulder Reservoir Inflow Outflow - Time Series Plot",
      right  = ""
    )
  )

  # *****************
  # ---- Plot 2K ----
  # *****************

  # CBT WG Boulder Res qm output
  cbt_wg_boulder <-
    outputs %>%
    process_wg_boulder(definitions_df = definitions) %>%
    dplyr::mutate(
      model_run = factor(model_run, levels = c(scenario_name))
    )

  # max value for yaxis of plots
  ymax <- round_by(x = max(cbt_wg_boulder$output), by = 500, buffer = 500)

  # unique sites to plot over
  cbt_wg_boulder_sites <- unique(cbt_wg_boulder$name)

  # plot 2K, loop through each site and plot
  cbt_wg_boulder_lst <- lapply(1:length(cbt_wg_boulder_sites), function(i) {

    extract_df <-
      cbt_wg_boulder %>%
      dplyr::filter(name == cbt_wg_boulder_sites[i])

    message(paste0("Plotting: WG Boulder Res - ", cbt_wg_boulder_sites[i]))

    wg_boulder_plot <-
      make_wg_boulder_plot(
        df         = extract_df,
        site       = cbt_wg_boulder_sites[i],
        desc       = unique(extract_df$description)[1],
        ymax       = ymax,
        title_size = title_size,
        xaxis_size = xaxis_size
      )

    wg_boulder_plot

  }) %>%
    stats::setNames(c(cbt_wg_boulder_sites))

  # save plot 2K
  ggplot2::ggsave(
    filename = paste0(model_comp_dir, "/", "2k. Boulder Res QM - Time Series Plot 3x1.png"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      cbt_wg_boulder_lst[["DataObject_1_Flow"]],
      cbt_wg_boulder_lst[["DataObject_29_Flow"]],
      cbt_wg_boulder_lst[["DataObject_1_Flow + DataObject_29_Flow"]],
      nrow   = 3,
      top    = "2k. Boulder Res QM - Time Series Plot",
      right  = ""
    )
  )

  # *****************
  # ---- Plot 2L ----
  # *****************

  # CBT Boulder Res qm output 2l

  # process data for plot 2L
  boulder_res <-
    outputs %>%
    process_boulder_res(definitions_df = definitions) %>%
    dplyr::mutate(
      model_run = factor(model_run, levels = c(scenario_name))
    )

  # max value for yaxis of plots
  ymax <- round_by(x = max(boulder_res$output), by = 500, buffer = 0)

  # unique sites to plot over
  boulder_res_sites <- unique(boulder_res$name)

  # plot 2L, loop through each site and plot
  boulder_res_lst <- lapply(1:length(boulder_res_sites), function(i) {

    extract_df <-
      boulder_res %>%
      dplyr::filter(name == boulder_res_sites[i])

    message(paste0("Plotting: CBT Boulder Res QM - ", boulder_res_sites[i]))

    boulder_res_plot <-
      make_boulder_res_plot(
        df         = extract_df,
        site       = boulder_res_sites[i],
        desc       = unique(extract_df$description)[1],
        ymax       = ymax,
        title_size = title_size,
        xaxis_size = xaxis_size
      )

    boulder_res_plot

  }) %>%
    stats::setNames(c(boulder_res_sites))

  # save plot 2L
  ggplot2::ggsave(
    filename = paste0(model_comp_dir, "/", "2l. Boulder Reservoir QM Contents - COB & Nortern 3x1.png"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      boulder_res_lst[["DataObject_1_Flow + DataObject_29_Flow"]],
      boulder_res_lst[["DataObject_2_Flow"]],
      boulder_res_lst[["Reservoir_12_Content"]],
      nrow   = 3,
      top    = "2l. Boulder Reservoir QM Contents - COB & Nortern",
      right  = ""
    )
  )

  # *****************
  # ---- Plot 3A ----
  # *****************

  wittemyer_sources <-
    outputs %>%
    process_wg_wittemyer_source() %>%
    dplyr::mutate(
      model_run = factor(model_run, levels = c(scenario_name))
    )

  # unique model runs to plot
  runs_lst <- unique(wittemyer_sources$model_run)

  # plot 3A, loop through each site and plot
  wittemyer_sources_lst <- lapply(1:length(runs_lst), function(i) {

    extract_df <-
      wittemyer_sources %>%
      dplyr::filter(model_run == runs_lst[i])

    message(paste0("Plotting: WG Wittemyer by source (Model run: ", runs_lst[i]), ")")

    wittemyer_sources_plot <-
      make_wittemyer_sources_plot(
        df         = extract_df,
        mod_run    = runs_lst[i],
        ymax       = 3000,
        title_size = title_size,
        xaxis_size = xaxis_size
      )

    wittemyer_sources_plot

  }) %>%
    stats::setNames(c(runs_lst))

  # save plot 3A
  ggplot2::ggsave(
    filename = paste0(model_comp_dir, "/", "3a. Wittemyer Pond Total Annual Inflow by Source 1x1.png"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      wittemyer_sources_lst[[2]],
      wittemyer_sources_lst[[1]],
      nrow   = 2,
      top    = "3a. Wittemyer Pond Total Annual Inflow by Source",
      right  = ""
    )
  )

  # *****************
  # ---- Plot 3B ----
  # *****************

  wittemyer_ts <-
    outputs %>%
    process_wg_wittemyer_ts() %>%
    dplyr::mutate(
      model_run = factor(model_run, levels = c(scenario_name))
    )

  # unique model runs to plot
  witt_ts_sites <- unique(wittemyer_ts$name)

  # plot 3B, loop through each site and plot
  wittemyer_ts_lst <- lapply(1:length(witt_ts_sites), function(i) {

    extract_df <-
      wittemyer_ts %>%
      dplyr::filter(name == witt_ts_sites[i])

    message(paste0("Plotting: WG Wittemyer flow timeseries - ", witt_ts_sites[i]))

    wittemyer_ts <-
      make_wittemyer_ts_plot(
        df         = extract_df,
        site       = unique(extract_df$title)[1],
        ymax       = 5000,
        title_size = title_size,
        xaxis_size = xaxis_size
      )

    wittemyer_ts

  }) %>%
    stats::setNames(c(witt_ts_sites))

  # save plot 3B
  ggplot2::ggsave(
    filename = paste0(model_comp_dir, "/", "3b. Wittemyer Pond Total Annual Flow Time Series 3x2.png"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      wittemyer_ts_lst[["BoulderWWTPTreatedReuseWater"]],
      wittemyer_ts_lst[["WittemyerRecaptureWWTPReuse"]],
      wittemyer_ts_lst[["WittemyerRecaptureGREP"]],
      wittemyer_ts_lst[["WittemyerFirstFillRight"]],
      wittemyer_ts_lst[["WittTotalInflow"]],
      wittemyer_ts_lst[["WittReleases"]],
      nrow   = 3,
      top    = "3b. Wittemyer Pond Total Annual Flow Time Series",
      right  = ""
    )
  )

  # *****************
  # ---- Plot 3C ----
  # *****************

  # annual wittemyer contents (year)
  wittemyer_cont_yr <-
    outputs %>%
    process_wg_wittemyer_content(timescale = "year") %>%
    dplyr::mutate(
      model_run = factor(model_run, levels = c(scenario_name))
    )

  # monthly wittemyer contents (qm)
  wittemyer_pond_qm <-
    outputs %>%
    process_wg_wittemyer_content(timescale = "qm") %>%
    dplyr::mutate(
      model_run = factor(model_run, levels = c(scenario_name))
    )

  # unique sites
  witt_cont_yr_sites <- unique(wittemyer_cont_yr$name)

  # plot 3C, loop through each site and plot
  wittemyer_cont_yr_lst <- lapply(1:length(witt_cont_yr_sites), function(i) {

    extract_df <-
      wittemyer_cont_yr %>%
      dplyr::filter(name == witt_cont_yr_sites[i])

    message(paste0("Plotting: WG Wittemyer Annual content - ", witt_cont_yr_sites[i]))

    # annual wittemyer contents (year)
    wittemyer_cont_plot <-
      make_wittemyer_ts_plot(
        df         = extract_df,
        site       = unique(extract_df$title)[1],
        ymax       = 2000,
        title_size = title_size,
        xaxis_size = xaxis_size,
        timescale  = "year"
      )

    wittemyer_cont_plot

  }) %>%
    stats::setNames(c(witt_cont_yr_sites))

  # monthly wittemyer contents plot (qm)
  wittemyer_cont_qm_plot <- make_wittemyer_ts_plot(
    df         = wittemyer_pond_qm,
    site       = unique(wittemyer_pond_qm$title)[1],
    ymax       = 2000,
    title_size = title_size,
    xaxis_size = xaxis_size,
    timescale  = "qm"
  )

  # add average monthly (qm) content plot to list of other wittemyer content plots
  wittemyer_cont_yr_lst$Witt_monthly_avg_contents <- wittemyer_cont_qm_plot

  # save plot 3C
  ggplot2::ggsave(
    filename = paste0(model_comp_dir, "/", "3c. Wittemyer Pond Average Annual Contents 3x1.png"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      wittemyer_cont_yr_lst[["Witt_annual_max_contents"]],
      wittemyer_cont_yr_lst[["Witt_annual_avg_contents"]],
      wittemyer_cont_yr_lst[["Witt_monthly_avg_contents"]],
      nrow   = 3,
      top    = "3c. Wittemyer Pond Average Annual Contents",
      right  = ""
    )
  )

  # *****************
  # ---- Plot 3D ----
  # *****************

  # Wittemyer Qm Time Series

  witt_qm_ts <-
    outputs %>%
    process_wittemyer_qm_ts(
      mod_run = scenario_name[1]
    ) %>%
    dplyr::mutate(
      model_run = factor(model_run, levels = c(scenario_name))
    )

  # unique sites
  witt_qm_sites <- unique(witt_qm_ts$name)

  # plot 3D, loop through each site and plot
  witt_qm_ts_lst <- lapply(1:length(witt_qm_sites), function(i) {

    extract_df <-
      witt_qm_ts %>%
      dplyr::filter(name == witt_qm_sites[i])

    message(paste0("Plotting: WG Wittemyer QM Timeseries - ", witt_qm_sites[i]))

    # annual wittemyer contents (year)
    witt_qm_ts_plot <-
      make_wittemyer_date_plot(
        df         = extract_df,
        site       = unique(extract_df$title)[1],
        ymax       = 2000,
        title_size = title_size,
        xaxis_size = xaxis_size
      )

    witt_qm_ts_plot

  }) %>%
    stats::setNames(c(witt_qm_sites))

  # save plot 3D
  ggplot2::ggsave(
    filename = paste0(model_comp_dir, "/", "3d. Wittemyer Pond Quarter-Monthly Time Series Plot 5x1.png"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      witt_qm_ts_lst[["Reservoir_13_Content"]],
      witt_qm_ts_lst[["DataObject_27_Flow"]],
      witt_qm_ts_lst[["Link_524_Flow"]],
      witt_qm_ts_lst[["Link_592_Flow"]],
      witt_qm_ts_lst[["Decree_106_Flow"]],
      nrow   = 5,
      top    = "3d. Wittemyer Pond Quarter-Monthly Time Series Plot",
      right  = ""
    )
  )

  # *****************
  # ---- Plot 4A ----
  # *****************

  # Panama Reservoir Water Type Bar Plot

  res_water_type <-
    outputs %>%
    process_res_water_type(
      definitions_df = definitions,
      qm_filter      = 24
    )

  # unique model runs to plot
  runs_lst <- unique(res_water_type$model_run)

  # plot 4A, loop through each site and plot
  res_water_type_lst <- lapply(1:length(runs_lst), function(i) {

    extract_df <-
      res_water_type %>%
      dplyr::filter(Type == "Inflow", model_run == runs_lst[i])

    message(paste0("Plotting: Panama Reservoir Inflows by year (Model run - ", runs_lst[i]), ")")

    res_water_type_plot <-
      make_res_water_type_plot(
        df         = extract_df,
        mod_run    = runs_lst[i],
        ymax       = 6000,
        title_size = title_size,
        xaxis_size = xaxis_size
      )

    res_water_type_plot

  }) %>%
    stats::setNames(c(runs_lst))

  # save plot 4A
  ggplot2::ggsave(
    filename = paste0(model_comp_dir, "/", "4a. Panama Reservoir Inflow Water Type 2x1.png"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      res_water_type_lst[[2]],
      res_water_type_lst[[1]],
      nrow   = 2,
      top    = "4a. Panama Reservoir Inflow Water Type",
      right  = ""
    )
  )

  # *****************
  # ---- Plot 4B ----
  # *****************

  panama_res <-
    outputs %>%
    process_panama_res(definitions_df = definitions) %>%
    dplyr::mutate(
      model_run = factor(model_run, levels = c(scenario_name))
    )

  # dplyr::group_by(name)
  # panama_res %>%
  #     # dplyr::group_by(name) %>%
  #     dplyr::group_split() %>%
  #     # purrr::set_names(unlist(dplyr::group_keys(panama_res)))
  #     stats::setNames(unlist(group_keys(panama_res)))

  # list of site names
  names_lst <- unique(panama_res$name)

  # plot 4B, loop through each site and plot
  panama_res_lst <- lapply(1:length(names_lst), function(i) {

    # filter down to site
    extract_df <-
      panama_res %>%
      dplyr::filter(name == names_lst[i])

    message(paste0("Plotting: Panama Reservoir Annual - ", names_lst[i]))

    # make plot
    panama_res_plot <-
      make_panama_res_plot(
        df         = extract_df,
        site       = unique(extract_df$title),
        units      = unique(extract_df$units),
        ymax       = 6000,
        title_size = title_size,
        xaxis_size = xaxis_size
      )

    panama_res_plot

  }) %>%
    stats::setNames(c(names_lst))

  # names(panama_res_lst)

  # save plot 4B
  ggplot2::ggsave(
    filename = paste0(model_comp_dir, "/", "4b. Panama Reservoir Annual Time Series Plots 4x2.png"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      panama_res_lst[["Reservoir_25_Content_avg"]],
      panama_res_lst[["Reservoir_25_Content_min"]],
      panama_res_lst[["Reservoir_25_Content_max"]],
      panama_res_lst[["Link_571_Flow"]],
      panama_res_lst[["Link_572_Flow"]],
      panama_res_lst[["Link_617_Flow"]],
      panama_res_lst[["Link_618_Flow"]],
      panama_res_lst[["Link_573_Flow"]],
      nrow   = 4,
      top    = "4b. Panama Reservoir Annual Time Series Plots",
      right  = ""
    )
  )

  # *****************
  # ---- Plot 4C ----
  # *****************

  # Panama Reservoir Analysis
  panama_res_qm <-
    outputs %>%
    process_panama_res_qm(definitions_df = definitions) %>%
    dplyr::mutate(
      model_run = factor(model_run, levels = c(scenario_name))
    )

  # list of site names
  names_lst <- unique(panama_res_qm$name)

  # plot 4C, loop through each site and plot
  panama_res_qm_lst <- lapply(1:length(names_lst), function(i) {

    # filter down to site
    extract_df <-
      panama_res_qm %>%
      dplyr::filter(name == names_lst[i])

    message(paste0("Plotting: Panama Reservoir QM - ", names_lst[i]))

    # make plot
    panama_qm_plot <-
      make_panama_res_qm_plot(
        df         = extract_df,
        site       = unique(extract_df$description),
        units      = unique(extract_df$units),
        ymax       = 6000,
        title_size = title_size,
        xaxis_size = xaxis_size
      )

    panama_qm_plot

  }) %>%
    stats::setNames(c(names_lst))

  # save plot 4C
  save_plot(
    plot_lst  = panama_res_qm_lst,
    base_dir  = model_comp_dir,
    plot_name = "4c. Panama Reservoir Quarter-Monthly Time Series Plots",
    width     = 14,
    height    = 8,
    nrows     = 6
  )

  # *****************
  # ---- Plot 4D ----
  # *****************

  # Panama Reservoir Analysis
  panama_pond <-
    outputs %>%
    process_panama_pond_year(definitions_df = definitions) %>%
    dplyr::mutate(
      model_run = factor(model_run, levels = c(scenario_name))
    )

  # list of site names
  names_lst <- unique(panama_pond$name)

  # plot 4D, loop through each site and plot
  panama_pond_lst <- lapply(1:length(names_lst), function(i) {

    # filter down to site
    extract_df <-
      panama_pond %>%
      dplyr::filter(name == names_lst[i])

    message(paste0("Plotting: Panama Reservoir Pond QM - ", names_lst[i]))

    # make plot
    panama_pond_plot <-
      make_panama_res_plot(
        df         = extract_df,
        site       = unique(extract_df$title),
        units      = unique(extract_df$units),
        ymax       = 6000,
        title_size = title_size,
        xaxis_size = xaxis_size
      )

    panama_pond_plot

  }) %>%
    stats::setNames(c(names_lst))

  # save plot 4C
  save_plot(
    plot_lst  = panama_pond_lst,
    base_dir  = model_comp_dir,
    plot_name = "4d. Panama Reservoir Average Annual Contents",
    width     = 14,
    height    = 8,
    nrows     = 2
  )

  # ****************
  # ---- Plot 5 ----
  # ****************

  # Gross Reservoir Pool Analysis
  grep_analysis <-
    outputs %>%
    process_grep_analysis() %>%
    dplyr::mutate(
      model_run = factor(model_run, levels = c(scenario_name))
    )

  # list of site names
  names_lst <- unique(grep_analysis$name)

  # plot 5, loop through each site and plot
  grep_analysis_lst <- lapply(1:length(names_lst), function(i) {

    # filter down to site
    extract_df <-
      grep_analysis %>%
      dplyr::filter(name == names_lst[i])

    # determine yaxis limits and tick mark increments based on range of max value
    if(max(extract_df$output) >= 0 & max(extract_df$output) <= 7000) {

      ymax = 7000
      y_by   = 2500

    } else {

      ymax = max(extract_df$output)
      y_by   = 10000

    }

    message(paste0("Plotting: GREP Analysis - ", names_lst[i]))

    # make plot
    grep_analysis_plot <-
      make_grep_analysis_plot(
        df         = extract_df,
        site       = unique(extract_df$title),
        units      = unique(extract_df$units),
        ymax       = ymax,
        y_by       = y_by,
        title_size = title_size,
        xaxis_size = xaxis_size
      )

    grep_analysis_plot

  }) %>%
    stats::setNames(c(names_lst))

  # save plot 5
  save_plot(
    plot_lst  = grep_analysis_lst,
    base_dir  = model_comp_dir,
    plot_name = "5. GREP Annual Time Series Plots",
    width     = 14,
    height    = 8,
    nrows     = 4
  )

  # *****************
  # ---- Plot 6A ----
  # *****************

  # Instream Flow Calcs

  # provide outputs dataframe, ISF year type dateframe, and Upper/Lower DRY/AVG Oct-Apr/May-Sep CFS values
  instream_flow <-
    outputs %>%
    process_instream_flow(
      isf_df                = isf_year_type,
      upper_dry_oct_apr_cfs = upper_dry_oct_apr_cfs,
      upper_dry_may_sep_cfs = upper_dry_may_sep_cfs,
      lower_dry_oct_apr_cfs = lower_dry_oct_apr_cfs,
      lower_dry_may_sep_cfs = lower_dry_may_sep_cfs,
      upper_avg_oct_apr_cfs = upper_avg_oct_apr_cfs,
      upper_avg_may_sep_cfs = upper_avg_may_sep_cfs,
      lower_avg_oct_apr_cfs = lower_avg_oct_apr_cfs,
      lower_avg_may_sep_cfs = lower_avg_may_sep_cfs
    ) %>%
    dplyr::mutate(
      model_run = factor(model_run, levels = c(scenario_name))
    )

  # list of site names
  names_lst <- unique(instream_flow$name)

  # plot 4D, loop through each site and plot
  instream_flow_lst <- lapply(1:length(names_lst), function(i) {

    # filter down to site
    extract_df <-
      instream_flow %>%
      dplyr::filter(name == names_lst[i])

    message(paste0("Plotting: Instream Flow - ", names_lst[i]))

    # make plot
    instream_flow_plot <-
      make_instream_flow_plot(
        df         = extract_df,
        site       = unique(extract_df$title),
        units      = unique(extract_df$units),
        title_size = title_size,
        xaxis_size = xaxis_size
      )

    instream_flow_plot

  }) %>%
    stats::setNames(c(names_lst))

  # insert the "Drought Response Level" plot into list of instream flow plots
  instream_flow_lst <- list(
    instream_flow_lst[[1]],
    instream_flow_lst[[2]],
    drought_plots[["Drought Response Level"]],
    instream_flow_lst[[3]]
  )


  # assign names to list of plots that now includes "Drought Response Level" plot
  names(instream_flow_lst) <- c(names_lst[1:length(names_lst)-1],
                                "Drought Response Level",
                                names_lst[length(names_lst)]
  )

  # save plot 6A
  save_plot(
    plot_lst  = instream_flow_lst,
    base_dir  = model_comp_dir,
    plot_name = "6a. Instream Flow Annual Shortage Plots",
    width     = 14,
    height    = 8,
    nrows     = 2
  )

  # *****************
  # ---- Plot 6B ----
  # *****************

  # Make instream flow table
  instream_flow_tbl <-
    instream_flow %>%
    make_instream_flow_tbl(
      core_size = 1,
      col_size  = 1,
      row_size  = 1
    )

  # save table 6B
  ggplot2::ggsave(
    filename = paste0(model_comp_dir, "/", "6b. Instream Flow Annual Shortage Tables 1x1.png"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      instream_flow_tbl,
      nrow          = 1,
      top           = "6b. Instream Flow Annual Shortage Tables",
      right         = "",
      bottom        = "",
      layout_matrix = rbind(c(1, 1), c(3, 3))
    )
  )

}
