process_isf <- function(
    isf_path,
    model_ids,
    verbose = TRUE
) {

  # isf_path <- path_lst[[2]]$path
  # model_ids <- tolower(paste0(path_lst[[4]]$id, path_lst[[4]]$climate))

  # convert to lower to match clean names
  model_ids <- tolower(model_ids)

  # Read in ISF year type CSV
  isf <- readr::read_csv(
    file           = isf_path,
    col_names      = T,
    show_col_types = FALSE
  ) %>%
    janitor::clean_names() %>%
    dplyr::select(wyqm, days_in_qm, model_ids)

  return(isf)

}
