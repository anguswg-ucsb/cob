process_res_reuse_storage <- function(df, definition_df) {
    # df <- outputs
    # definition_df <- definitions
  res_reuse_annual <-
    df %>%
    # dplyr::filter(model_run == "7525-ID1-055a") %>%
    dplyr::select(year, qm, wyqm, model_run,
                  DataObject_30_Flow,
                  Reservoir_1_Content,
                  DataObject_28_Flow,
                  Reservoir_3_Content,
                  DataObject_29_Flow,
                  DataObject_1_Flow
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c(-model_run, -year, -wyqm),
        as.numeric)
    ) %>%
    dplyr::group_by(year, model_run) %>%
    dplyr::summarise(dplyr::across(DataObject_30_Flow:DataObject_1_Flow, mean)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(cols = c(-year, -model_run)) %>%
    dplyr::left_join(
      definition_df,
      by = c("name" = "Name")
    ) %>%
    dplyr::left_join(
      data.frame(
        Name = c("DataObject_30_Flow", "Reservoir_1_Content",
                 "DataObject_28_Flow", "Reservoir_3_Content",
                 "DataObject_29_Flow", "DataObject_1_Flow"),
        Type = c("Reusable Water", "Total Contents",
                 "Reusable Water", "Total Contents",
                 "Reusable Water", "Total COB Contents"),
        Group = c("NBC Reservoir", "NBC Reservoir",
                  "Barker Reservoir", "Barker Reservoir",
                  "Boulder Reservoir", "Boulder Reservoir")
      ),
      by = c("name" = "Name")
    ) %>%
    dplyr::mutate(
      storage_max = dplyr::case_when(
        Group == "NBC Reservoir"    ~ 6927,
        Group == "Barker Reservoir" ~ 11277,
        TRUE                        ~ NA_real_
      )
    )

  return(res_reuse_annual)

}

process_res_reuse_storage_qm <- function(df, definition_df) {
  # df <- outputs
  # definition_df <- definitions
  res_reuse_qm <-
    df %>%
    # dplyr::filter(model_run == "7525-ID1-055a") %>%
    dplyr::select(year, qm, wyqm, model_run,
                  DataObject_30_Flow,
                  Reservoir_1_Content,
                  DataObject_28_Flow,
                  Reservoir_3_Content,
                  DataObject_29_Flow,
                  DataObject_1_Flow
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c(-model_run, -year, , -qm, -wyqm),
        as.numeric)
    ) %>%
    dplyr::group_by(qm, model_run) %>%
    dplyr::summarise(dplyr::across(DataObject_30_Flow:DataObject_1_Flow, mean)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(cols = c(-qm, -model_run)) %>%
    dplyr::left_join(
      definition_df,
      by = c("name" = "Name")
    ) %>%
    dplyr::left_join(
      data.frame(
        Name = c("DataObject_30_Flow", "Reservoir_1_Content",
                 "DataObject_28_Flow", "Reservoir_3_Content",
                 "DataObject_29_Flow", "DataObject_1_Flow"),
        Type = c("Reusable Water", "Total Contents",
                 "Reusable Water", "Total Contents",
                 "Reusable Water", "Total COB Contents"),
        Group = c("NBC Reservoir", "NBC Reservoir",
                  "Barker Reservoir", "Barker Reservoir",
                  "Boulder Reservoir", "Boulder Reservoir")
      ),
      by = c("name" = "Name")
    ) %>%
    dplyr::mutate(
      storage_max = dplyr::case_when(
        Group == "NBC Reservoir"    ~ 6927,
        Group == "Barker Reservoir" ~ 11277,
        TRUE                        ~ NA_real_
      )
    )

  return(res_reuse_qm)

}

process_reusable_res_content <- function(df, definition_df) {

  reuse_content <-
    df %>%
    # dplyr::filter(model_run == "7525-ID1-055a") %>%
    dplyr::select(year, qm, wyqm, model_run,
                  DataObject_30_Flow,
                  DataObject_28_Flow,
                  DataObject_29_Flow,
                  Reservoir_13_Content,
                  Reservoir_25_Content,
                  Reservoir_1_Content,
                  Reservoir_3_Content,
                  DataObject_1_Flow
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c(-model_run, -year, , -qm, -wyqm),
        as.numeric)
    ) %>%
    dplyr::group_by(wyqm, model_run) %>%
    dplyr::mutate(
      TotalReuse    = sum(DataObject_30_Flow, DataObject_28_Flow,
                          DataObject_29_Flow,Reservoir_13_Content, Reservoir_25_Content),
      TotalContents = sum(Reservoir_1_Content, Reservoir_3_Content,
                          DataObject_1_Flow, Reservoir_13_Content, Reservoir_25_Content)
    ) %>%

    dplyr::ungroup() %>%
    tidyr::pivot_longer(cols = c(-qm, -year, -wyqm, -model_run)) %>%
    dplyr::left_join(
      definition_df,
      by = c("name" = "Name")
    ) %>%
    dplyr::left_join(
      data.frame(Name = c("DataObject_30_Flow", "Reservoir_1_Content",
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
                           "All 5 Reservoirs", "All 5 Reservoirs")),
      by = c("name" = "Name")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      storage_max = dplyr::case_when(
        Group %in% c("Barker Reservoir", "NBC Reservoir", "Boulder Reservoir") ~ 12000,
        Group == "Wittemyer"                                                   ~ 2200,
        Group == "Panama"                                                      ~ 5000,
        Group == "All 5 Reservoirs"                                            ~ 35000,
        TRUE                                                                   ~ NA_real_
      )
    )

  return(reuse_content)

}
