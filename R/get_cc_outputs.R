get_cc_outputs <- function(
    base_mod_dir,
    comp_mod_dir,
    date_df,
    analysis_years = 4,
    title_size     = 10,
    xaxis_size     = 9,
    output_folder_name,
    scenario_name,
    save_path
) {

  # # Model versions
  # base_model_version        <-  paste0("v", base_mods$model_version)
  # compare_model_version     <-  paste0("v", comp_mods$model_version)
  # # version text strings
  # base_model_version_text    <- base_mods$model_version
  # compare_model_version_text <- comp_mods$model_version
  # # model IDs
  # base_model_ID        <- base_mods$model_id
  # compare_model_ID     <- comp_mods$model_id
  # # model ID suffix
  # base_model_ID_suffix    <- ""        # if this is not needed, keep blank ""
  # compare_model_ID_suffix <- ""        # if this is not needed, keep blank ""
  # # climate scenerios
  # base_climate         <- base_mods$model_num
  # compare_climate      <- comp_mods$model_num
  # # model prefixes
  # base_model_ID_prefix    <- base_mods$prefix
  # compare_model_ID_prefix <- comp_mods$prefix
  # # Extra info text that would be found between the model version and the model ID in the file name
  # base_model_extra_info    <- base_mods$extra_info
  # compare_model_extra_info <- comp_mods$extra_info
  # # no compact calls or compact calls data
  # base_model_ID_suffix    <- "nocc"        # no compact call (nocc)
  # compare_model_ID_suffix <- "cc"           # compact call ON (cc)

  # Folder to hold all outputs/plots from this script
  output_dir <- paste0(save_path, "/", output_folder_name)

  # check if directory already exists, if it doesn't create the directory
  if (!dir.exists(output_dir)) {

    message(paste0("Creating output folder:\n--->  ", output_dir))

    dir.create(output_dir)

  } else {

    message(paste0("Output folder already exists:\n--->  ", output_dir))

  }


  # set plot parameters
  # title_size = 10
  # xaxis_size = 9

  # number of years to analyze in ensemble model run (after the shut down year)
  # analysis_years  = 4

  # get the total number of model runs (compact call + no compact call)
  total_simulation_count <- nrow(comp_mods) * (analysis_years + 1) * 2

  # dataframe of start, end years to index
  year_subs <-
    data.frame(
      start = c(1919:2014)
    ) %>%
    dplyr::mutate(
      end   = start + analysis_years
    )

  if(length(comp_mods$scenario_name) != nrow(year_subs)){

    # message(paste0("Adding model_run column (with dummy model_runs"))

    # next model run after length of model runs
    model_run_len    <- length(comp_mods$scenario_name) + 1

    # create dummy model run names to fit year_subs dataframe
    dummy_model_runs <- paste0(substr(comp_mods$scenario_name, 0, (nchar(comp_mods$scenario_name)-2))[1],  model_run_len:nrow(year_subs))

    # join list of model runs w/ dummy model runs
    model_run_lst    <- c(comp_mods$scenario_name, dummy_model_runs)

    # add model run column
    year_subs <-
      year_subs %>%
      dplyr::mutate(
        model_run = model_run_lst
      )

  } else {

    # message(paste0("Adding model_run column"))

    # add model run
    year_subs <-
      year_subs %>%
      dplyr::mutate(
        model_run = comp_mods$scenario_name
      )

  }

  message(paste0("Loading NO comparison models..."))

  # Read in and preprocess base model output run OutputSheet (NOCC)
  base_output <- process_output(
    file_df = base_mods[1,],
    date_df = date_df,
    verbose = FALSE
  )

  # NO Compact calls data (nocc)
  # loop through NOCC/baseline dataset and filter each set into 4 year chunks
  nocc_df <- lapply(1:nrow(comp_mods), function(y) {

    message(paste0(y, "/", nrow(comp_mods), " - ", year_subs$model_run[y], " (", year_subs$start[y], " - ", year_subs$end[y], ")"))

    base_subset <-
      base_output %>%
      dplyr::filter(
        year >= year_subs$start[y],
        year <= year_subs$end[y]
      ) %>%
      dplyr::mutate(
        compact_call = base_mods$compact_call[1],
        model_run    = paste0(year_subs$model_run[y], "b"),
        model_group  = "no_compact_call"
      ) %>%
      dplyr::relocate(compact_call, model_run, model_group)

  }) %>%
    dplyr::bind_rows()

  # NO Compact calls data (nocc)
  # loop through NOCC/baseline dataset and filter each set into 4 year chunks
  # nocc_df <- lapply(1:nrow(comp_mods), function(y) {
  #
  #   message(paste0(y, "/", nrow(comp_mods)))
  #
  #   output <- process_output(
  #     file_df = base_mods[1,],
  #     date_df = date_df,
  #     verbose = FALSE
  #   ) %>%
  #     dplyr::filter(
  #       year >= year_subs$start[y],
  #       year <= year_subs$end[y]
  #     ) %>%
  #     dplyr::mutate(
  #       compact_call = base_mods$compact_call[1],
  #       model_run    = paste0(comp_mods$scenario_name[y], "b"),
  #       model_group  = "no_compact_call"
  #     ) %>%
  #     dplyr::relocate(compact_call, model_run, model_group)
  #
  # }) %>%
  #   dplyr::bind_rows()

  message(paste0("Loading compact call models..."))

  # Compact calls data (cc)
  # loop through compact calls datasets and filter each set into 4 year chunks
  cc_df <- lapply(1:nrow(comp_mods), function(y) {

    message(paste0(y, "/", nrow(comp_mods)))

    output <- process_output(
      file_df = comp_mods[y, ],
      date_df = date_df,
      verbose = FALSE
    ) %>%
      dplyr::filter(
        year >= year_subs$start[y],
        year <= year_subs$end[y]
      ) %>%
      dplyr::mutate(
        compact_call = comp_mods$compact_call[y],
        model_run    = comp_mods$scenario_name[y],
        model_group  = "compact_call"
      ) %>%
      dplyr::relocate(compact_call, model_run, model_group)

  }) %>%
    dplyr::bind_rows()

  # all model paths to create dictionary from
  all_paths <- c(base_mods$path, comp_mods$path)

  # retrieve lookup table from all Output sheets and keep the distinct rows (no duplicate definitions)
  definitions <-  lapply(1:length(all_paths), function(y) {

    message(paste0(y, "/", length(all_paths)))

    make_lookup(
      output_path = all_paths[y],
      verbose     = FALSE
    )
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct() %>%
    dplyr::bind_rows(cc_new_params())  # add additional CC parameters to definitions lookup table

  # merge all data together
  final_df <- dplyr::bind_rows(cc_df, nocc_df)

  # set the factor 'levels' to the correct plotting order
  final_df <-
    final_df %>%
    dplyr::mutate(
      order_val = dplyr::case_when(
        model_group == "compact_call"    ~ 1,
        model_group == "no_compact_call" ~ 2
      )
    ) %>%
    dplyr::group_by(model_group) %>%
    dplyr::arrange(-order_val, .by_group = F) %>%
    dplyr::ungroup() %>%
    dplyr::select(-order_val) %>%
    dplyr::mutate(
      model_run = factor(model_run)
      # model_run = factor(model_run, levels = c(unique(.$model_run)))
    )

  message(paste0("Generating plots..."))

  # ********************************
  # ---- 1A. May 1 Res Contents ----
  # ********************************

  # select the sites you want to plot
  p1a_site_selection <- c("year", "qm", "start_date", "model_run", "model_group", "Reservoir_3_Content", "Reservoir_1_Content")

  # subset dataset
  p1a_df <-
    final_df %>%
    dplyr::select(p1a_site_selection) %>%
    dplyr::mutate(
      dplyr::across(c(-year, -qm, -start_date, -model_run, -model_group), as.numeric)
    ) %>%
    dplyr::mutate(
      Total_Upper_Storage = Reservoir_3_Content + Reservoir_1_Content
      # Total_Upper_Storage = rowSums(dplyr::across(c(Reservoir_3_Content, Reservoir_1_Content)))
    )

  # filter, pivot to long format, and join the CRAM model descriptions to this dataset
  p1a_df <-
    p1a_df %>%
    dplyr::filter(qm == 29) %>%
    dplyr::group_by(year, model_run) %>%
    tidyr::pivot_longer(
      # cols      = all_of(site_selection_short),
      cols      = c(-year, -qm, -start_date, -model_run, -model_group),
      names_to  = "Name",
      values_to = "Output") %>%
    dplyr::left_join(
      definitions,
      by = "Name"
    ) %>%
    dplyr::ungroup()

  # unique names to use to make plots
  p1a_uplots <- unique(p1a_df$Name)

  # empty list to store P1A plots
  p1a_lst <- list()

  # loop over unique water sources and make plots
  for(z in 1:length(p1a_uplots)) {

    # message(paste0(z, "/", length(p1a_uplots), " - (", p1a_uplots[z], ")"))

    # ECDF data to plot
    extract_df <-
      p1a_df %>%
      dplyr::filter(Name == p1a_uplots[z])

    # ECDF plot
    extract_plot <- make_ecdf_point_plot(
      df               = extract_df,
      plot_title       = extract_df$Description[z],
      xaxis_title      = extract_df$Units[z],
      plot_title_size  = title_size,
      xaxis_title_size = xaxis_size
    )

    # add plot to list of plots
    p1a_lst[[z]] <- extract_plot

  }

  message(paste0("Plot 1A"))

  # define the plot name
  p1a_plot_title <- paste0("1a. May 1 Reservoir Contents (point)", " (", scenario_name, ")")
  p1a_file_name  <- paste(p1a_plot_title,  " 2x2", sep = "")

  # save the plot
  ggplot2::ggsave(
    # paste0(output_folder_name, "/", file_name, " ", model_version, ".pdf"),
    filename = paste0(output_dir, "/", p1a_file_name, ".pdf"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      p1a_lst[[1]], p1a_lst[[2]], p1a_lst[[3]],
      nrow  = 2,
      top   = p1a_plot_title,
      right = ""
    )
  )

  # ***************************************
  # ---- 1D May 1 Drought Metrics Plot ----
  # ***************************************

  # select the sites you want to plot
  p1d_site_selection <- c("year", "qm", "start_date", "model_run", "model_group", "DataObject_15_Flow", "DataObject_12_Flow", "DataObject_14_Flow")
  # p1d_site_selection <- c("year", "qm", "start_date", "model_run", "model_group", "DataObject_15_Flow", "DataObject_12_Flow", "DataObject_14_Flow", "DataObject_39_Flow")

  # subset dataset
  p1d_df <-
    final_df %>%
    dplyr::select(p1d_site_selection) %>%
    dplyr::mutate(
      dplyr::across(c(-year, -qm, -start_date, -model_run, -model_group), as.numeric)
    ) %>%
    dplyr::mutate(
      DataObject_15_Flow = DataObject_15_Flow/100
    ) %>%
    dplyr::filter(qm == 29) %>%
    dplyr::group_by(year, model_run) %>%
    tidyr::pivot_longer(
      cols      = c(-year, -qm, -start_date, -model_run, -model_group),
      names_to  = "Name",
      values_to = "Output") %>%
    dplyr::left_join(
      definitions,
      by = "Name"
    ) %>%
    dplyr::ungroup()

  # unique names to use to make plots
  p1d_uplots <- unique(p1d_df$Name)

  # empty list to store P1A plots
  p1d_lst <- list()

  # loop over unique water sources and make plots
  for(z in 1:length(p1d_uplots)) {

    # message(paste0(z, "/", length(p1d_uplots), " - (", p1d_uplots[z], ")"))

    # ECDF data to plot
    extract_df <-
      p1d_df %>%
      dplyr::filter(Name == p1d_uplots[z])

    # ECDF plot
    extract_plot <- make_ecdf_step_plot(
      df               = extract_df,
      plot_title       = extract_df$Description[z],
      xaxis_title      = extract_df$Units[z],
      plot_title_size  = title_size,
      xaxis_title_size = xaxis_size
    )

    # add plot to list of plots
    p1d_lst[[z]] <- extract_plot

  }

  message(paste0("Plot 1D"))

  # define the plot name
  p1d_plot_title <- paste0("1d. May 1 Drought Metrics", " (", scenario_name, ")")
  p1d_file_name <- paste(p1d_plot_title, " 2x2", sep = "")

  # save the plot
  ggplot2::ggsave(
    filename = paste0(output_dir, "/", p1d_file_name, ".pdf"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      p1d_lst[[1]], p1d_lst[[2]], p1d_lst[[3]],
      nrow  = 2,
      top   = p1d_plot_title,
      right = ""
    )
  )

  # ***********************************
  # ---- 1C May 1 Drought Triggers ----
  # ***********************************

  t1c_tbl <-
    p1d_df %>%
    dplyr::filter(Name == "DataObject_12_Flow") %>%
    dplyr::group_by(model_group) %>%
    dplyr::count(Output) %>%
    tidyr::pivot_wider(
      names_from  = model_group,
      values_from = n
    ) %>%
    dplyr::rename("Drought Response Level" = Output) %>%
    gt::gt() %>%
    gt::tab_header(
      title    = gt::md("Comparison of Drought Response Triggers"),
      subtitle = gt::md(output_folder_name)
    )

  # define the table export name & save as pdf
  t1c_plot_title <- "1c. May 1 Drought Triggers"
  t1c_file_name  <- paste(t1c_plot_title, " (", scenario_name, ")", sep = "")

  message(paste0("Table 1C"))

  # save 1C1 table
  gt::gtsave(
    data = t1c_tbl,
    file = paste0(output_dir, "/", t1c_file_name, ".pdf"),
    zoom = 1
  )

  # *********************************************
  # ---- 1C2 May 1 Drought Triggers Summary -----
  # *********************************************

  # CC Temp data count
  cc_temp <-
    p1d_df %>%
    dplyr::filter(Name == "DataObject_12_Flow") %>%
    dplyr::filter(Output == 0) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(model_group) %>%
    dplyr::count(Output) %>%
    dplyr::ungroup() %>%
    dplyr::filter(model_group == "compact_call") %>%
    .$n

  # No CC Temp data count
  nocc_temp <-
    p1d_df %>%
    dplyr::filter(Name == "DataObject_12_Flow") %>%
    dplyr::filter(Output == 0) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(model_group) %>%
    dplyr::count(Output) %>%
    dplyr::ungroup() %>%
    dplyr::filter(model_group == "no_compact_call") %>%
    .$n

  t1c2_tbl <-
    dplyr::tibble(
      Count             = c("Total Simulation", "No Drought Trigger", "No Drought Trigger (%)"),
      'compact call'    = c(total_simulation_count, cc_temp, round(cc_temp/total_simulation_count*100,1)),
      'no compact call' = c(total_simulation_count, nocc_temp, round(nocc_temp/total_simulation_count*100,1))
    ) %>%
    gt::gt() %>%
    gt::tab_header(
      title    = gt::md("No Drought Trigger Summary"),
      subtitle = gt::md(output_folder_name)
    )

  message(paste0("Table 1C2"))

  # define the table export name & save as pdf
  t1c2_plot_title <- "1c2. May 1 Drought Triggers Summary"
  t1c2_file_name <- paste(t1c2_plot_title, " (", scenario_name, ")", sep = "")

  # save 1C1 table
  gt::gtsave(
    data = t1c2_tbl,
    file = paste0(output_dir, "/", t1c2_file_name, ".pdf"),
    zoom = 1
  )

  # *********************************
  # ---- 1B June 1 Res Contents -----
  # *********************************

  # select the sites you want to plot
  p1b_site_selection <- c("year", "qm", "start_date", "model_run", "model_group", "Reservoir_3_Content", "Reservoir_1_Content")

  # subset dataset
  p1b_df <-
    final_df %>%
    dplyr::select(p1b_site_selection) %>%
    dplyr::mutate(
      dplyr::across(c(-year, -qm, -start_date, -model_run, -model_group), as.numeric)
    ) %>%
    dplyr::mutate(
      Total_Upper_Storage = Reservoir_3_Content + Reservoir_1_Content
      # Total_Upper_Storage = rowSums(dplyr::across(c(Reservoir_3_Content, Reservoir_1_Content)))
    )

  # filter, pivot to long format, and join the CRAM model descriptions to this dataset
  p1b_df <-
    p1b_df %>%
    dplyr::filter(qm == 33) %>%
    dplyr::group_by(year, model_run) %>%
    tidyr::pivot_longer(
      # cols      = all_of(site_selection_short),
      cols      = c(-year, -qm, -start_date, -model_run, -model_group),
      names_to  = "Name",
      values_to = "Output") %>%
    dplyr::left_join(
      definitions,
      by = "Name"
    ) %>%
    dplyr::ungroup()

  # unique names to use to make plots
  p1b_uplots <- unique(p1b_df$Name)

  # empty list to store P1A plots
  p1b_lst <- list()
  # rm(z,xaxis_title_size,xaxis_title, df, extract_df,plot_title_size, extract_plot, p1a_lst)

  # loop over unique water sources and make plots
  for(z in 1:length(p1b_uplots)) {

    # message(paste0(z, "/", length(p1b_uplots), " - (", p1b_uplots[z], ")"))

    # ECDF data to plot
    extract_df <-
      p1b_df %>%
      dplyr::filter(Name == p1b_uplots[z])

    # ECDF plot
    extract_plot <- make_ecdf_step_plot(
      df               = extract_df,
      plot_title       = extract_df$Description[z],
      xaxis_title      = extract_df$Units[z],
      plot_title_size  = title_size,
      xaxis_title_size = xaxis_size,
      quantile_breaks  = FALSE
    )

    # add plot to list of plots
    p1b_lst[[z]] <- extract_plot

  }

  message(paste0("Plot 1B"))

  # define the plot name
  p1b_plot_title <- paste0("1b. June 1 Reservoir Contents", " (", scenario_name, ")")
  p1b_file_name <- paste(p1b_plot_title, " 2x2", sep = "")

  # save the plot
  ggplot2::ggsave(
    filename = paste0(output_dir, "/", p1b_file_name, ".pdf"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      p1b_lst[[1]], p1b_lst[[2]], p1b_lst[[3]],
      nrow  = 2,
      top   = p1b_plot_title,
      right = ""
    )
  )

  # **********************************************
  # ---- 1E. Annual City Demand and Shortage -----
  # **********************************************

  # plot 1E sites of interest
  p1e_site_selection <- c("year", "qm", "start_date", "model_run", "model_group",
                          "DataObject_23_Flow", "DataObject_19_Flow", "DataObject_20_Flow")

  # subset dataset
  p1e_df <-
    final_df %>%
    dplyr::select(p1e_site_selection) %>%
    dplyr::mutate(
      dplyr::across(c(-year, -qm, -start_date, -model_run, -model_group), as.numeric)
    ) %>%
    # dplyr::group_by(year, model_run) %>%
    dplyr::group_by(year, model_run, model_group) %>%
    dplyr::summarise(dplyr::across(DataObject_23_Flow:DataObject_20_Flow, sum)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(
      cols      = c(-year, -model_run, -model_group),
      names_to  = "Name",
      values_to = "Output"
    ) %>%
    dplyr::left_join(
      definitions,
      by = "Name"
    )

  # unique names to use to make plots
  p1e_uplots <- unique(p1e_df$Name)

  # empty list to store 1E plots
  p1e_lst <- list()
  # rm(z,xaxis_title_size,xaxis_title, df, extract_df,plot_title_size, extract_plot, p1a_lst)

  # loop over unique water sources and make plots
  for(z in 1:length(p1e_uplots)) {

    # message(paste0(z, "/", length(p1e_uplots), " - (", p1e_uplots[z], ")"))

    # ECDF data to plot
    extract_df <-
      p1e_df %>%
      dplyr::filter(Name == p1e_uplots[z])

    # ECDF plot
    extract_plot <- make_ecdf_step_plot(
      df               = extract_df,
      plot_title       = extract_df$Description[z],
      xaxis_title      = extract_df$Units[z],
      plot_title_size  = title_size,
      xaxis_title_size = xaxis_size,
      quantile_breaks  = FALSE
    )

    # add plot to list of plots
    p1e_lst[[z]] <- extract_plot

  }

  # define the plot name
  p1e_plot_title <- paste0("1e. Annual City Demand and Shortage", " (", scenario_name, ")")
  p1e_file_name <- paste(p1e_plot_title, " 2x2", sep = "")

  message(paste0("Plot 1E"))

  # save the plot
  ggplot2::ggsave(
    filename = paste0(output_dir, "/", p1e_file_name, ".pdf"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      p1e_lst[[1]], p1e_lst[[2]], p1e_lst[[3]],
      nrow  = 2,
      top   = p1e_plot_title,
      right = ""
    )
  )


  # *******************************
  # ---- 2A. Panama Reservoir -----
  # *******************************
  # plot 2a sites of interest
  p2a_site_selection <- c("year", "qm", "start_date", "model_run", "model_group", "Reservoir_25_Content")

  # subset dataset
  p2a_df <-
    final_df %>%
    dplyr::select(p2a_site_selection) %>%
    dplyr::mutate(
      dplyr::across(c(-year, -qm, -start_date, -model_run, -model_group), as.numeric)
    ) %>%
    # dplyr::group_by(year, model_run) %>%
    dplyr::group_by(year, model_run, model_group) %>%
    dplyr::summarise(
      COB_Panama_qm_max_contents = max(Reservoir_25_Content),
      COB_Panama_qm_avg_contents = mean(Reservoir_25_Content),
      COB_Panama_qm_min_contents = min(Reservoir_25_Content)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(
      cols      = c(-year, -model_run, -model_group),
      names_to  = "Name",
      values_to = "Output"
    ) %>%
    dplyr::left_join(
      definitions,
      by = "Name"
    )

  # unique names to use to make plots
  p2a_uplots <- unique(p2a_df$Name)

  # empty list to store 1E plots
  p2a_lst <- list()
  # rm(z,xaxis_title_size,xaxis_title, df, extract_df,plot_title_size, extract_plot, p1a_lst)

  # loop over unique water sources and make plots
  for(z in 1:length(p2a_uplots)) {

    # message(paste0(z, "/", length(p2a_uplots), " - (", p2a_uplots[z], ")"))

    # ECDF data to plot
    extract_df <-
      p2a_df %>%
      dplyr::filter(Name == p2a_uplots[z])

    # ECDF plot
    extract_plot <- make_ecdf_step_plot(
      df               = extract_df,
      plot_title       = extract_df$Description[z],
      xaxis_title      = extract_df$Units[z],
      plot_title_size  = title_size,
      xaxis_title_size = xaxis_size,
      quantile_breaks  = FALSE
    )

    # add plot to list of plots
    p2a_lst[[z]] <- extract_plot

  }

  # define the plot name
  p2a_plot_title <- paste0("2a. Panama Reservoir Contents", " (", scenario_name, ")")
  p2a_file_name <- paste(p2a_plot_title, " 2x2", sep = "")

  message(paste0("Plot 2A"))

  # save the plot
  ggplot2::ggsave(
    filename = paste0(output_dir, "/", p2a_file_name, ".pdf"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      p2a_lst[[1]], p2a_lst[[2]], p2a_lst[[3]],
      nrow  = 2,
      top   = p2a_plot_title,
      right = ""
    )
  )

  # *************************************
  # ---- 2B. Wittemyer Res Contents -----
  # *************************************

  p2b_site_selection <- c("year", "qm", "start_date", "model_run", "model_group", "Reservoir_13_Content")

  # subset dataset
  p2b_df <-
    final_df %>%
    dplyr::select(p2b_site_selection) %>%
    dplyr::mutate(
      dplyr::across(c(-year, -qm, -start_date, -model_run, -model_group), as.numeric)
    ) %>%
    # dplyr::group_by(year, model_run) %>%
    dplyr::group_by(year, model_run, model_group) %>%
    dplyr::summarise(
      COB_Wittemyer_qm_max_contents = max(Reservoir_13_Content),
      COB_Wittemyer_qm_avg_contents = mean(Reservoir_13_Content),
      COB_Wittemyer_qm_min_contents = min(Reservoir_13_Content)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(
      cols      = c(-year, -model_run, -model_group),
      names_to  = "Name",
      values_to = "Output"
    ) %>%
    dplyr::left_join(
      definitions,
      by = "Name"
    )

  # unique names to use to make plots
  p2b_uplots <- unique(p2b_df$Name)

  # empty list to store 1E plots
  p2b_lst <- list()
  # rm(z,xaxis_title_size,xaxis_title, df, extract_df,plot_title_size, extract_plot, p1a_lst)

  # loop over unique water sources and make plots
  for(z in 1:length(p2b_uplots)) {

    # message(paste0(z, "/", length(p2b_uplots), " - (", p2b_uplots[z], ")"))

    # ECDF data to plot
    extract_df <-
      p2b_df %>%
      dplyr::filter(Name == p2b_uplots[z])

    # ECDF plot
    extract_plot <- make_ecdf_step_plot(
      df               = extract_df,
      plot_title       = extract_df$Description[z],
      xaxis_title      = extract_df$Units[z],
      plot_title_size  = title_size,
      xaxis_title_size = xaxis_size,
      quantile_breaks  = FALSE
    )

    # add plot to list of plots
    p2b_lst[[z]] <- extract_plot

  }

  # define the plot name
  p2b_plot_title  <- paste0("2b. Wittemyer Reservoir Contents", " (", scenario_name, ")")
  p2b_file_name   <- paste(p2b_plot_title, " 2x2", sep = "")

  message(paste0("Plot 2B"))

  # save the plot
  ggplot2::ggsave(
    filename = paste0(output_dir, "/", p2b_file_name, ".pdf"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      p2b_lst[[1]], p2b_lst[[2]], p2b_lst[[3]],
      nrow  = 2,
      top   = p2b_plot_title,
      right = ""
    )
  )

  # ******************************************
  # ---- 2C. Wittemyer + Panama Contents -----
  # ******************************************

  p2c_site_selection <- c("year", "qm", "start_date", "model_run", "model_group", "Reservoir_13_Content", "Reservoir_25_Content")

  # subset dataset
  p2c_df <-
    final_df %>%
    dplyr::select(p2c_site_selection) %>%
    dplyr::mutate(
      dplyr::across(c(-year, -qm, -start_date, -model_run, -model_group), as.numeric)
    ) %>%
    dplyr::mutate(
      Wittemyer_Panama_Storage = Reservoir_13_Content + Reservoir_25_Content
      # Wittemyer_Panama_Storage = rowSums(across(c(Reservoir_13_Content, Reservoir_25_Content)))
    ) %>%
    # dplyr::group_by(year, model_run) %>%
    dplyr::group_by(year, model_run, model_group) %>%
    dplyr::summarise(
      "Panama+Wittemyer_qm_max_contents" = max(Wittemyer_Panama_Storage),
      "Panama+Wittemyer_qm_avg_contents" = mean(Wittemyer_Panama_Storage),
      "Panama+Wittemyer_qm_min_contents" = min(Wittemyer_Panama_Storage)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(
      cols      = c(-year, -model_run, -model_group),
      names_to  = "Name",
      values_to = "Output"
    ) %>%
    dplyr::left_join(
      definitions,
      by = "Name"
    )

  # unique names to use to make plots
  p2c_uplots <- unique(p2c_df$Name)

  # empty list to store 1E plots
  p2c_lst <- list()
  # rm(z,xaxis_title_size,xaxis_title, df, extract_df,plot_title_size, extract_plot, p1a_lst)

  # loop over unique water sources and make plots
  for(z in 1:length(p2c_uplots)) {

    # message(paste0(z, "/", length(p2c_uplots), " - (", p2c_uplots[z], ")"))

    # ECDF data to plot
    extract_df <-
      p2c_df %>%
      dplyr::filter(Name == p2c_uplots[z])

    # ECDF plot
    extract_plot <- make_ecdf_step_plot(
      df               = extract_df,
      plot_title       = extract_df$Description[z],
      xaxis_title      = extract_df$Units[z],
      plot_title_size  = title_size,
      xaxis_title_size = xaxis_size,
      quantile_breaks  = FALSE
    )

    # add plot to list of plots
    p2c_lst[[z]] <- extract_plot

  }

  # define the plot name
  p2c_plot_title  <- paste0("2c. Panama + Wittemyer Reservoir Contents", " (", scenario_name, ")")
  p2c_file_name   <- paste(p2c_plot_title, " 2x2", sep = "")

  message(paste0("Plot 2C"))

  # save the plot
  ggplot2::ggsave(
    filename = paste0(output_dir, "/", p2c_file_name, ".pdf"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      p2c_lst[[1]], p2c_lst[[2]], p2c_lst[[3]],
      nrow  = 2,
      top   = p2c_plot_title,
      right = ""
    )
  )
}

# ***********************************************************************
# ***********************************************************************
