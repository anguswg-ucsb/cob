make_grep_analysis_plot <- function(
    df,
    site,
    units,
    ymax = 7000,
    y_by = 2000,
    title_size,
    xaxis_size
) {

  # df         = extract_df
  # site       = unique(extract_df$title)
  # units      = unique(extract_df$units)
  # ymax       = 7000
  # title_size = title_size
  # xaxis_size = xaxis_size

  # extract_df$output %>% max()

  grep_analysis_plot <-
    df %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(
      ggplot2::aes(x = year, y = output, color = model_run, linetype = model_run)
    ) +
    ggplot2::labs(
      title    = paste0(site),
      y        = paste0(units),
      x        = "Water year",
      color    = "Model run",
      linetype = "Model run"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, ymax),
                                breaks = c(seq(0, ymax, by = y_by))
                                ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = title_size),
      axis.title = ggplot2::element_text(size = xaxis_size)
    )

  return(grep_analysis_plot)

}

make_panama_res_qm_plot <- function(
    df,
    site,
    units,
    ymax = 6000,
    title_size,
    xaxis_size
) {

  # df         = extract_df
  # site       = unique(extract_df$name)
  # units      = unique(extract_df$units)
  # ymax       = 6000
  # title_size = title_size
  # xaxis_size = xaxis_size

  panama_res_qm_plot <-
    df %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(
      ggplot2::aes(x = date, y = output, color = model_run, linetype = model_run)
    ) +
    ggplot2::labs(
      title    = paste0(site),
      y        = paste0(units),
      x        = "Water year",
      color    = "Model run",
      linetype = "Model run"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, ymax), breaks = c(seq(0, ymax, by = 2000))) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = title_size),
      axis.title = ggplot2::element_text(size = xaxis_size)
    )

  return(panama_res_qm_plot)

}
make_panama_res_plot <- function(
    df,
    site,
    units,
    ymax = 6000,
    title_size,
    xaxis_size
    ) {
  # df         = extract_df
  # site       = unique(extract_df$title)
  # units      = unique(extract_df$units)
  # ymax       = 6000
  # title_size = title_size
  # xaxis_size = xaxis_size
  # df         = extract_df
  # site       = unique(extract_df$title)[1]
  # ymax       = 6000
  # title_size = title_size
  # xaxis_size = xaxis_size

  panama_res_plot <-
    df %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(
      ggplot2::aes(x = year, y = output, color = model_run, linetype = model_run)
    ) +
    ggplot2::labs(
      title    = paste0(site),
      y        = paste0(units),
      x        = "Water year",
      color    = "Model run",
      linetype = "Model run"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, ymax), breaks = c(seq(0, ymax, by = 2000))) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = title_size),
      axis.title = ggplot2::element_text(size = xaxis_size)
    )

  return(panama_res_plot)

}
make_res_water_type_plot <- function(
    df,
    mod_run,
    ymax = 6000,
    title_size,
    xaxis_size
) {
  # df         = extract_df
  # mod_run    = runs_lst[i]
  # ymax       = 6000
  # title_size = title_size
  # xaxis_size = xaxis_size
  # extract_df <-
  #   boulder_res %>%
  #   dplyr::filter(name == boulder_res_sites[i])
  # df         = extract_df
  # mod_run       = runs_lst[i]
  # ymax       = 3000
  # title_size = title_size
  # xaxis_size = xaxis_size

  res_water_type_plot <-
    df %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(
      ggplot2::aes(x = year, y = output, fill = description),
      position = "stack",
      color    = "black"
    ) +
    ggplot2::labs(
      title    = paste0(mod_run, ": Panama Reservoir Inflows by Year"),
      y        = "Flow (af)",
      x        = "Water year",
      fill     = "Description"
    ) +
      # ggplot2::ylim(0, ymax) +
    ggplot2::scale_y_continuous(limits = c(0, ymax), breaks = c(seq(0, ymax, by = ymax/3))) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = title_size),
      axis.title = ggplot2::element_text(size = xaxis_size)
    )

  return(res_water_type_plot)

}

make_wittemyer_date_plot <- function(
    df,
    site,
    ymax = 2000,
    title_size,
    xaxis_size
) {

  wittemyer_date_plot <-
    df %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(
      ggplot2::aes(x = date, y = output, color = model_run, linetype = model_run)
    ) +
    ggplot2::labs(
      title    = paste0(site),
      y        = "Flow (af)",
      x        = "Water year",
      color    = "Model run",
      linetype = "Model run"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, ymax), breaks = c(seq(0, ymax, by = 500))) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = title_size),
      axis.title = ggplot2::element_text(size = xaxis_size)
    )

  return(wittemyer_date_plot)

}

make_wittemyer_qm_plot <- function(
    df,
    site,
    ymax = 5000,
    title_size,
    xaxis_size
) {

  # df         = extract_df
  # site       = unique(extract_df$title)[1]
  # ymax       = 5000
  # title_size = title_size
  # xaxis_size = xaxis_size

  wittemyer_cont_qm_plot <-
    df %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(
      ggplot2::aes(x = year, y = output, color = model_run, linetype = model_run)
    ) +
    ggplot2::labs(
      title    = paste0(site),
      y        = "Flow (af)",
      x        = "Water year",
      color    = "Model run",
      linetype = "Model run"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, ymax), breaks = c(seq(0, ymax, by = 1000))) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = title_size),
      axis.title = ggplot2::element_text(size = xaxis_size)
    )

  return(wittemyer_ts_plot)

}

make_wittemyer_ts_plot <- function(
    df,
    site,
    ymax = 5000,
    title_size,
    xaxis_size,
    timescale = "year"
) {

  # df         = extract_df
  # site       = unique(extract_df$title)[1]
  # ymax       = 5000
  # title_size = title_size
  # xaxis_size = xaxis_size

  if(timescale == "year") {

    wittemyer_ts_plot <-
      df %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(
        ggplot2::aes(x = year, y = output, color = model_run, linetype = model_run)
      ) +
      ggplot2::labs(
        title    = paste0(site),
        y        = "Contents (af)",
        x        = "Water year",
        color    = "Model run",
        linetype = "Model run"
      ) +
      ggplot2::scale_y_continuous(limits = c(0, ymax), breaks = c(seq(0, ymax, by = 500))) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = title_size),
        axis.title = ggplot2::element_text(size = xaxis_size)
      )

    return(wittemyer_ts_plot)

  }

  if(timescale == "qm") {

    wittemyer_qm_plot <-
      df %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(
        ggplot2::aes(x = qm, y = output, color = model_run, linetype = model_run)
      ) +
      ggplot2::labs(
        title    = paste0(site),
        y        = "Contents (af)",
        x        = "Quarter Month",
        color    = "Model run",
        linetype = "Model run"
      ) +
      ggplot2::scale_y_continuous(limits = c(0, ymax), breaks = c(seq(0, ymax, by = 500))) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = title_size),
        axis.title = ggplot2::element_text(size = xaxis_size)
      )

    return(wittemyer_qm_plot)

  }

}

make_wittemyer_sources_plot <- function(
    df,
    mod_run,
    ymax = 3000,
    title_size,
    xaxis_size
) {

  # extract_df <-
  #   boulder_res %>%
  #   dplyr::filter(name == boulder_res_sites[i])
  # df         = extract_df
  # mod_run       = runs_lst[i]
  # ymax       = 3000
  # title_size = title_size
  # xaxis_size = xaxis_size

  wittemyer_sources_plot <-
    df %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(
      ggplot2::aes(x = year, y = output, fill = name),
      position = "stack",
      color    = "black"
    ) +
    ggplot2::labs(
      title    = paste0("Wittemyer Pond: Annual Inflow by Source (", mod_run, ")"),
      y        = "Flow (af)",
      x        = "Water year",
      fill     = "Inflow"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, ymax), breaks = c(seq(0, ymax, by = ymax/3))) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = title_size),
      axis.title = ggplot2::element_text(size = xaxis_size)
    )

  return(wittemyer_sources_plot)

}


make_boulder_res_plot <- function(
    df,
    site,
    desc,
    ymax = NA,
    title_size,
    xaxis_size
) {

  # extract_df <-
  #   boulder_res %>%
  #   dplyr::filter(name == boulder_res_sites[i])
  # df         = extract_df
  # site       = boulder_res_sites[i]
  # desc       = unique(extract_df$description)[1]
  # ymax       = ymax
  # title_size = title_size
  # xaxis_size = xaxis_size

  boulder_res_plot <-
    df %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(
      ggplot2::aes(x = date, y = output, color = model_run, linetype = model_run)
    ) +
    # ggplot2::ylim(0, ymax) +
    ggplot2::labs(
      title    = paste0(site, " = ", desc),
      y        = "Flow (af)",
      x        = "Water year",
      color    = "Model run",
      linetype = "Model run"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, ymax), breaks = c(seq(0, ymax, by = 3000))) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = title_size),
      axis.title = ggplot2::element_text(size = xaxis_size)
    )

  return(boulder_res_plot)

}

make_wg_boulder_plot <- function(
    df,
    site,
    desc,
    ymax = NA,
    title_size,
    xaxis_size
) {

 wg_boulder_plot <-
    df %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(
      ggplot2::aes(x = date, y = output, color = model_run, linetype = model_run)
    ) +
    ggplot2::ylim(0, ymax) +
    ggplot2::labs(
      title    = paste0(site, " = ", desc),
      y        = "Flow (af)",
      x        = "Water year",
      color    = "Model run",
      linetype = "Model run"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = title_size),
      axis.title = ggplot2::element_text(size = xaxis_size)
    )

  return(wg_boulder_plot)

}

make_cbt_windygap_plot <- function(
    df,
    site,
    title_size,
    xaxis_size
) {

    cbt_wg_plot <-
      df %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(
        ggplot2::aes(x = year, y = output, color = model_run, linetype = model_run)
      ) +
      # ggplot2::facet_wrap(~name, nrow = 5) +
      ggplot2::ylim(0, NA) +
      ggplot2::labs(
        title    = site,
        y        = "Flow (af)",
        x        = "Water year",
        color    = "Model run",
        linetype = "Model run"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = title_size),
        axis.title = ggplot2::element_text(size = xaxis_size)
      )

  return(cbt_wg_plot)

}

make_res_reusable_water_plot <- function(
    df,
    timescale = "annual",
    storage_max,
    plot_title,
    compare_model_id,
    title_size,
    xaxis_size
) {

  if(timescale == "annual") {


  res_reuse_plot <-
    df %>%
    ggplot2::ggplot(ggplot2::aes(x = year, y = value, color = Type, linetype = Type)) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = storage_max, color = "red") +
    ggplot2::ylim(0, 12500) +
    ggplot2::labs(
      title =  paste0(plot_title, ": Reuseable Water (", compare_model_id, ")"),
      x     = "Water Year",
      y     = "Contents (af)"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = title_size),
      axis.title = ggplot2::element_text(size = xaxis_size)
    ) +
    ggplot2::scale_color_manual(values = c("black", "blue")) +
    ggplot2::scale_linetype_manual(values = c("solid", "dashed"))

  return(res_reuse_plot)

  }

  if(timescale == "qm") {

    res_reuse_plot <-
      df %>%
      ggplot2::ggplot(ggplot2::aes(x = qm, y = value, color = Type, linetype = Type)) +
      ggplot2::geom_line() +
      ggplot2::ylim(0, 12500) +
      ggplot2::labs(
        title =  paste0(plot_title, ": Reuseable Water (", compare_model_id, ")"),
        x     = "Quarter-Month",
        y     = "Contents (af)"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = title_size),
        axis.title = ggplot2::element_text(size = xaxis_size)
      ) +
      ggplot2::scale_color_manual(values = c("black", "blue")) +
      ggplot2::scale_linetype_manual(values = c("solid", "dashed"))

    return(res_reuse_plot)

  }

  if(timescale == "date") {

    res_reuse_plot <-
      df %>%
      ggplot2::ggplot(ggplot2::aes(x = wyqm, y = value, color = Type, linetype = Type)) +
      ggplot2::geom_line() +
      ggplot2::ylim(0, storage_max) +
      ggplot2::labs(
        title =  paste0(plot_title, ": Reuseable Water (", compare_model_id, ")"),
        x     = "Water Year",
        y     = "Contents (af)"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = title_size),
        axis.title = ggplot2::element_text(size = xaxis_size)
      ) +
      ggplot2::scale_color_manual(values = c("Total Contents" = "black", "Reusable Water" = "blue")) +
      ggplot2::scale_linetype_manual(values = c("Total Contents" = "solid", "Reusable Water" = "dashed"))
      # ggplot2::scale_color_manual(values = c("black", "blue")) +
      # ggplot2::scale_linetype_manual(values = c("solid", "dashed"))

    return(res_reuse_plot)

  }

}

make_cbt_quota_tbl2 <- function(
    df
) {

  summary_tbl <-
    df %>%
    gt::gt() %>%
    gt::tab_header(
      title    = gt::md("City of Boulder Annual C-BT Water Use"),
      subtitle = gt::md(paste0(unique(df$model_run)[2], " vs. ",   unique(df$model_run)[1]))
    )

  return(summary_tbl)
}

make_cbt_quota_tbl <- function(
    df,
    size = 1
    ) {

  # set the theme, sizes
  tt <-
    gridExtra::ttheme_default(
      core    = list(fg_params=list(cex = size)),
      colhead = list(fg_params=list(cex = size)),
      rowhead = list(fg_params=list(cex = size))
      )


  # Table 1 CBT Quota
  tbl_temp_a <-
    gridExtra::tableGrob(
                        d     = df,
                        theme = tt,
                        rows  = NULL
                        ) %>%
      gtable::gtable_add_grob(
        grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
        t     = 1,
        l     = 1,
        r     = ncol(df)
        ) %>%
      gtable::gtable_add_grob(
        grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
        t     = 1,
        b     = nrow(df),
        l     = 1,
        r     = 1
        ) %>%
      gtable::gtable_add_grob(
        grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)),
        t     = 1,
        b     = nrow(df),
        l     = 2,
        r     = 4
      ) %>%
      gtable::gtable_add_grob(
        grobs = grid::rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t     = 1,
        b     = nrow(df),
        l     = 5,
        r     = ncol(df)
        )

  return(tbl_temp_a)

}
make_cbt_unused_plot <- function(
    df,
    mod_run,
    title_size,
    xaxis_size
) {

  cbt_comp_plot <-
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      data     = dplyr::filter(
        df,
        group == "CBT Summary",
        Description == "COB Unused CBT Water",
        model_run == mod_run
      ),
      # data = df,
      ggplot2::aes(x = year, y = value, fill = Description),
      position = "stack",
      stat     = 'identity',
      color    = "black",
      size     = 0.05
    ) +
    # ggplot2::scale_fill_manual(values = "cornflowerblue") +
    ggplot2::geom_line(
      data     = dplyr::filter(
        df,
        group     == "Total CBT",
        model_run == mod_run
      ),
      ggplot2::aes(x = year, y = value, color = name),
      size = 0.75
    ) +
    ggplot2::ylim(-10000, 22000) +
    ggplot2::scale_color_manual(values = "black") +
    ggplot2::scale_x_continuous(limits = c(1914, 2016), breaks = seq(1915, 2015, by = 5)) +
    ggplot2::labs(
      title = paste0(mod_run, ": CBT Water by Year"),
      y = "Flow (af)",
      x = "Water year"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = title_size),
      axis.title = ggplot2::element_text(size = xaxis_size)
    )

  return(cbt_comp_plot)

}

make_cbt_used_plot <- function(
    df,
    mod_run,
    title_size,
    xaxis_size
) {

  cbt_comp_plot <-
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      data     = dplyr::filter(
        df,
        group == "CBT Summary",
        Description == "COB Used CBT Water",
        model_run == mod_run
      ),
      # data = df,
      ggplot2::aes(x = year, y = value, fill = Description),
      position = "stack",
      stat     = 'identity',
      color    = "black",
      size     = 0.05
    ) +
    ggplot2::scale_fill_manual(values = "cornflowerblue") +
    ggplot2::geom_line(
      data     = dplyr::filter(
        df,
        group     == "Total CBT",
        model_run == mod_run
      ),
      ggplot2::aes(x = year, y = value, color = name),
      size = 0.75
    ) +
    ggplot2::ylim(NA, 22000) +
    ggplot2::scale_color_manual(values = "black") +
    ggplot2::scale_x_continuous(limits = c(1914, 2016), breaks = seq(1915, 2015, by = 5)) +
    ggplot2::labs(
      title = paste0(mod_run, ": CBT Water by Year"),
      y = "Flow (af)",
      x = "Water year"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = title_size),
      axis.title = ggplot2::element_text(size = xaxis_size)
    )

  return(cbt_comp_plot)

}
make_cbt_component_plot <- function(
    df,
    mod_run,
    title_size,
    xaxis_size
) {

  cbt_comp_plot <-
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      data     = dplyr::filter(
        df,
        group     == "CBT Component",
        model_run == mod_run
      ),
      ggplot2::aes(x = year, y = value, fill = Description),
      position = "stack",
      stat     = 'identity',
      color    = "black",
      size     = 0.05
    ) +
    ggplot2::geom_line(
      data     = dplyr::filter(
        df,
        group     == "Total CBT",
        model_run == mod_run
      ),
      ggplot2::aes(x = year, y = value, color = name),
      size = 0.75
    ) +
    ggplot2::scale_color_manual(values = "black") +
    ggplot2::scale_x_continuous(limits = c(1914, 2016), breaks = seq(1915, 2015, by = 5)) +
    ggplot2::labs(
      title = paste0(mod_run, ": CBT Water by Year"),
      y = "Flow (af)",
      x = "Water year"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = title_size),
      axis.title = ggplot2::element_text(size = xaxis_size)
    )

  return(cbt_comp_plot)

}
make_cbt_summary_plot <- function(
    df,
    mod_run,
    title_size,
    xaxis_size
) {

  cbt_summary_plot <-
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      data     = dplyr::filter(
        df,
        group       == "CBT Summary",
        Description == "COB Unused CBT Water",
        model_run   == mod_run
      ),
      ggplot2::aes(x = year, y = value, fill = Description),
      position = "stack",
      stat     = 'identity',
      color    = "black",
      size     = 0.05
    ) +
    ggplot2::geom_line(
      data     = dplyr::filter(
        df,
        group       == "Total CBT",
        model_run   == mod_run
      ),
      ggplot2::aes(x = year, y = value, color = name),
      size = 0.75
    ) +
    ggplot2::scale_color_manual(values = "black") +
    ggplot2::scale_x_continuous(limits = c(1914, 2016), breaks = seq(1915, 2015, by = 5)) +
    ggplot2::labs(
      title = paste0(mod_run, ": CBT Water by Year"),
      y = "Flow (af)",
      x = "Water year"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = title_size),
      axis.title = ggplot2::element_text(size = xaxis_size)
    )


  return(cbt_summary_plot)

}

make_reuse_water_exchange_plots <- function(
    df,
    plot_title,
    ylab_title,
    yaxis_max,
    title_size,
    xaxis_size
) {

  reuse_plot <-
    df %>%
    ggplot2::ggplot(
      ggplot2::aes(x = year, y = value, color = `Model run`, linetype = `Model run`)
    ) +
    ggplot2::geom_line() +
    ggplot2::ylim(0, yaxis_max) +
    ggplot2::labs(
      title = plot_title,
      x     = "Water Year",
      y     = ylab_title
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = title_size),
      axis.title = ggplot2::element_text(size = xaxis_size)
    )

  return(reuse_plot)

}

make_mass_balance_plot <- function(
    df,
    plot_title,
    yaxis_max,
    title_size,
    xaxis_size
) {

  mass_bal_plot <-
    df %>%
    ggplot2::ggplot(
      # ggplot2::aes(x = year, y = value, color = model_run, linetype = model_run)
      ggplot2::aes(x = year, y = value, color = `Model run`, linetype = `Model run`)
    ) +
    ggplot2::geom_line() +
    ggplot2::ylim(0, yaxis_max) +
    ggplot2::labs(
      title = plot_title,
      x     = "Water Year",
     y     = "Flow (af)"
     ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = title_size),
      axis.title = ggplot2::element_text(size = xaxis_size)
    )

  return(mass_bal_plot)

}

make_drought_response_plot <- function(
    df,
    plot_name,
    ylab_title,
    title_size,
    xaxis_size
) {

  drought_response_plot <-
    df %>%
    ggplot2::ggplot(aes(x = year, y = value, color = `Model run`,  linetype = `Model run`)) +
    ggplot2::geom_line() +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = plot_name,
      x     = "Water Year",
      y     = ylab_title
    ) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(size = title_size),
      axis.title         = ggplot2::element_text(size = xaxis_size),
      panel.grid.minor.y = ggplot2::element_blank()
    )

  return(drought_response_plot)


}

make_psi_plot <- function(
    df,
    plot_name,
    ylab_title,
    title_size,
    xaxis_size
) {

  psi_plot <-
    df %>%
    ggplot2::ggplot(aes(x = year, y = value, color = `Model run`,  linetype = `Model run`)) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = 0.40, linetype="solid", color="red") +
    ggplot2::geom_hline(yintercept = 0.55, linetype="solid", color="darkorange") +
    ggplot2::geom_hline(yintercept = 0.70, linetype="solid", color="darkgreen") +
    ggplot2::geom_hline(yintercept = 0.85, linetype="solid", color="blue") +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = plot_name,
      x     = "Water Year",
      y     = ylab_title
    ) +
    ggplot2::theme(
      plot.title = element_text(size = title_size),
      axis.title = element_text(size = xaxis_size)
    )

  return(psi_plot)


}

make_res_content_plot <- function(
    df,
    plot_name,
    ylab_title,
    storage_max_hline,
    title_size,
    xaxis_size
) {

  res_content_plot <-
    df %>%
    ggplot2::ggplot(aes(x = year, y = value, color = `Model run`,  linetype = `Model run`)) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = storage_max_hline, color = "black", size = 0.25) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = plot_name,
      x     = "Water Year",
      y     = ylab_title
    ) +
    ggplot2::theme(
      plot.title = element_text(size = title_size),
      axis.title = element_text(size = xaxis_size)
    )

  return(res_content_plot)


}

# Page 1 table
make_drought_table <- function(
    df,
    core_size = 0.65,
    col_size = 0.65,
    row_size = 0.65
) {

  # df <- data_annual_lst
  tt <-
    gridExtra::ttheme_default(
      core    = list(
        fg_params = list(cex = core_size)
      ),
      colhead = list(fg_params=list(cex = col_size)),
      rowhead = list(fg_params=list(cex = row_size))
    )

  # Table 2
  tbl_temp <- gridExtra::tableGrob(
    df,
    theme = tt,
    rows  = NULL
  )

  # add box around the column headers
  tbl_temp <- gtable::gtable_add_grob(
    tbl_temp,
    grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
    t = 1,
    l = 1,
    r = ncol(tbl_temp)
  )
  # add box around the first model run of data
  tbl_temp <- gtable::gtable_add_grob(
    tbl_temp,
    grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
    t = 2,
    b = nrow(tbl_temp),
    l = 1,
    r = ncol(tbl_temp)
  )

  # add box around the second model run of data
  tbl_temp <- gtable::gtable_add_grob(
    tbl_temp,
    grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
    t = 7,
    b = nrow(tbl_temp),
    l = 1,
    r = ncol(tbl_temp)
  )

  return(tbl_temp)
}

make_ecdf_point_plot <- function(
    df               = NULL,
    plot_title       = NULL,
    xaxis_title      = NULL,
    plot_title_size  = 10,
    xaxis_title_size = 9
    ) {



  if(is.null(df)) {
    stop(paste0("Invalid 'df' argument"))
  }

  # ECDF plot
  extract_plot <-
    df %>%
    dplyr::mutate(
      model_group = dplyr::case_when(
        model_group == "compact_call"    ~ "Compact Call",
        model_group == "no_compact_call" ~ "No Compact Call"
      )
    ) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x        = Output,
        color    = model_group,
        shape    = model_group
      )
    ) +
    ggplot2::stat_ecdf(geom = "point") +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    #geom_vline(xintercept = extract_plot_nocc$Output, linetype="solid", color="black") +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = plot_title,
      x     = xaxis_title,
      color = "Model Group",
      shape = "Model Group"
    ) +
    ggplot2::scale_y_continuous(
      name   = "Percent",
      breaks = seq(0, 1, by=0.2),
      limits = c(0,1)
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = plot_title_size),
      axis.title = ggplot2::element_text(size = xaxis_title_size)
    )

  return(extract_plot)
}


make_ecdf_step_plot <- function(
    df               = NULL,
    plot_title       = NULL,
    xaxis_title      = NULL,
    plot_title_size  = 10,
    xaxis_title_size = 9,
    quantile_breaks  = TRUE
) {

  if(is.null(df)) {
    stop(paste0("Invalid 'df' argument"))
  }

  # X axis min max values
  xaxis_min    <- floor(min(df$Output))
  xaxis_max    <- ceiling(max(df$Output))

  # breaks to step sequence by for x axis
  xaxis_breaks <- quantile(xaxis_min:xaxis_max)
  xaxis_breaks <- xaxis_breaks[5] - xaxis_breaks[4]

  # ECDF step plot
  extract_plot <-
    df %>%
    dplyr::mutate(
      model_group = dplyr::case_when(
        model_group == "compact_call"    ~ "Compact Call",
        model_group == "no_compact_call" ~ "No Compact Call"
      )
    ) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x        = Output,
        color    = model_group,
        linetype = model_group
      )
    ) +
    ggplot2::stat_ecdf(geom = "step") +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title    = plot_title,
      x        = xaxis_title,
      color    = "Model Group",
      linetype = "Model Group"
    ) +
    ggplot2::scale_y_continuous(
      name   = "Percent",
      breaks = seq(0, 1, by=0.2),
      limits = c(0,1)
    )

  # if quantile_breaks == TRUE, then break xaxis evenly by quantile distance
  if(quantile_breaks == TRUE) {

    extract_plot <-
      extract_plot +
      ggplot2::scale_x_continuous(
      name   = xaxis_title,
      breaks = seq(xaxis_min, xaxis_max, by = xaxis_breaks),
      limits = c(xaxis_min, xaxis_max)
      ) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = plot_title_size),
        axis.title = ggplot2::element_text(size = xaxis_title_size)
      )

  } else {

    extract_plot <-
      extract_plot +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = plot_title_size),
        axis.title = ggplot2::element_text(size = xaxis_title_size)
      )

  }

  return(extract_plot)
}


make_quota_plot <- function(
    df,
    title_size = 10,
    xaxis_size = 9
) {

  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  p_quota <-
    df %>%
    dplyr::rename("Model run" = model_run) %>%
    ggplot2::ggplot(aes(x = year, y = quota, color = `Model run`,  linetype = `Model run`)) +
    ggplot2::geom_line() +  #col = color_list[i]
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.20)) +
    ggplot2::labs(
      title = "Annual C-BT Quota",
      x     = "Water Year"
      # linetype = "Model Run"
      # color = ""
    ) +
    ggplot2::theme(
      plot.title = element_text(size = title_size),
      axis.title = element_text(size = xaxis_size)
    )

  return(p_quota)

}
