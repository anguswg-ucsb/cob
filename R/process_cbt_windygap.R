process_cbt_windygap <- function(df) {

  cbt_wg <-
    df %>%
    dplyr::select(
      year, qm, wyqm, model_run,
      Decree_75_Flow, Link_499_Flow,  Link_451_Flow, Link_452_Flow, Link_375_Flow,
      Link_388_Flow, Link_457_Flow, Link_454_Flow, Link_596_Flow, Link_399_Flow
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c(-model_run, -year, -qm, -wyqm),
        as.numeric)
    ) %>%
    dplyr::group_by(year, model_run) %>%
    dplyr::summarise(
      dplyr::across(c(Decree_75_Flow:Link_399_Flow), sum)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(
      cols      = c( c(-model_run, -year)),
      names_to  = "name",
      values_to = "output"
    )

  # group by ModelRun to calculate values by group
  extract_wg <-
    df %>%
    dplyr::select(
      year, qm, wyqm, model_run,
      DataObject_29_Flow, DataObject_1_Flow, DataObject_2_Flow
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c(-model_run, -year, -qm, -wyqm),
        as.numeric)
    ) %>%
    dplyr::group_by(year, model_run) %>%
    dplyr::filter(qm == 29) %>%
    dplyr::select(-qm, -wyqm) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(
      cols      = c( c(-model_run, -year)),
      names_to  = "name",
      values_to = "output"
    )

  # bind all rows
  final_wg <-
    cbt_wg %>%
    dplyr::bind_rows(extract_wg) %>%
    dplyr::mutate(year = as.numeric(year))

  return(final_wg)

}
