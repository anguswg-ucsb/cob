process_quota <- function(
    quota_path = NULL,
    model_ids  = NULL,
    start_year = 1900,
    end_year   = 2100,
    verbose    = TRUE
) {
  # quota_path = NULL
  # model_ids  = NULL
  # start_year = 1900
  # end_year   = 2100

  # quota_path <- path_lst[[1]]$path

  # model_ids <- tolower(paste0(path_lst[[4]]$id, path_lst[[4]]$climate))

  # convert to lower to match clean names
  model_ids <- tolower(model_ids)

  quota     <- readr::read_csv(
    file           = quota_path,
    col_names      = T,
    show_col_types = FALSE
  ) %>%
    janitor::clean_names() %>%
    dplyr::select(year, model_ids) %>%
    dplyr::filter(
      year >= start_year,
      year <= end_year
      ) %>%
    tidyr::pivot_longer(
      cols      = c(model_ids),
      names_to  = "model_run",
      values_to = "quota"
      ) %>%
    dplyr::arrange(model_run, year) %>%
    dplyr::mutate(
      model_run = factor(model_run, levels = c(model_ids))
    )

  # sort IDs for plotting
  # rev(sort(stringr::str_extract(model_ids, "\\d{1}")) )

  return(quota)

}
