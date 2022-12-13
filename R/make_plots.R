make_res_reusable_water_plot <- function(
    df,
    hline_yint,
    plot_title,
    compare_model_id,
    title_size,
    xaxis_size
) {

  res_reuse_plot <-
    df %>%
    ggplot2::ggplot(ggplot2::aes(x = year, y = value, color = Type, linetype = Type)) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = hline_yint, color = "red") +
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
