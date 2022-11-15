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
      plot.title = ggplot2::element_text(size = title_size),
      axis.title = ggplot2::element_text(size = xaxis_size)
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
        plot.title = ggplot2::element_text(size = title_size),
        axis.title = ggplot2::element_text(size = xaxis_size)
      )

  } else {

    extract_plot <-
      extract_plot +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = title_size),
        axis.title = ggplot2::element_text(size = xaxis_size)
      )

  }

  return(extract_plot)
}
