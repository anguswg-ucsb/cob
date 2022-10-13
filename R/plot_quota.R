process_quota <- function(
    quota      = NULL,
    model_ids  = NULL,
    start_year = 1900,
    end_year   = 2100,
    title_size = 10,
    xaxis_size = 8,
    verbose    = TRUE
) {

  # if a character path to quota CSV, process as needed
  if(is.character(quota) == TRUE) {

    quota <- process_quota(
                quota_path = quota,
                model_ids  = model_ids,
                start_year = start_year,
                end_year   = end_year,
                verbose    = TRUE
              )
  }

  # plot quota
  quota_plot <-
    quota %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(
      ggplot2::aes(
        x        = year,
        y        = quota,
        color    = model_run,
        linetype = model_run
        ),
      size = 1
      ) +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.20)
    ) +
    ggplot2::labs(
      title = "Annual C-BT Quota",
      x     = "Water Year",
      y     = "Quota",
      color = "Model Run"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = title_size),
      axis.title = ggplot2::element_text(size = xaxis_size)
      )

  return(quota_plot)

}
