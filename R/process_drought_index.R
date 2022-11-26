process_drought_index <- function(
    output_df,
    model_run,
    qm
    ) {

  drought_indices <-
    output_df %>%
    dplyr::filter(model_run == !!model_run, qm == !!qm) %>%
    dplyr::select(year, qm, Date = start_date, model_run, DataObject_15_Flow, DataObject_12_Flow, Reservoir_3_Content,
                Reservoir_1_Content, DataObject_1_Flow, Reservoir_12_Content,
                DataObject_13_Flow) %>%
    dplyr::mutate(dplyr::across(c(-year, -qm, -Date, -model_run), as.numeric)) %>%
    dplyr::mutate(
      PSI = DataObject_15_Flow/100,

    ) %>%
    dplyr::mutate(PSI = DataObject_15_Flow/100) %>%
    dplyr::rename(
      DroughtResponseLevel         = DataObject_12_Flow,
      Barker_Res_Contents_af       = Reservoir_3_Content,
      NBC_Res_Contents_af          = Reservoir_1_Content,
      Boulder_Res_Contents_af      = DataObject_1_Flow,
      TotalBoulder_Res_Contents_af = Reservoir_12_Content
      ) %>%
    dplyr::mutate(
      Upper_Storage_af = Barker_Res_Contents_af + NBC_Res_Contents_af,
      year             = as.numeric(year)
    ) %>%
    dplyr::rename(Predicted_Storage_af = DataObject_13_Flow) %>%
    tidyr::pivot_longer(c(-year, -qm, -Date, -model_run)) %>%
    dplyr::mutate(
      title = dplyr::case_when(
        name == "PSI"                     ~ "Projected Storage Index",
        name == "DroughtResponseLevel"    ~ "Drought Response Level",
        name == "Barker_Res_Contents_af"  ~ "Barker Reservoir Contents",
        name == "NBC_Res_Contents_af"     ~ "NBC Reservoir Contents",
        name == "Upper_Storage_af"        ~ "Total Upper Reservoir Contents",
        name == "Boulder_Res_Contents_af" ~ "COB Boulder Reservoir Contents"
        # name == "Predicted_Storage_af"    ~ "COB Boulder Reservoir Contents"
      ),
      ylabs = dplyr::case_when(
        name == "PSI"                     ~ "PSI",
        name == "DroughtResponseLevel"    ~ "Drought Response Level",
        name == "Barker_Res_Contents_af"  ~ "Reservoir Contents (af)",
        name == "NBC_Res_Contents_af"     ~ "Reservoir Contents (af)",
        name == "Upper_Storage_af"        ~ "Reservoir Contents (af)",
        name == "Boulder_Res_Contents_af" ~ "Reservoir Contents (af)"
      ),
      storage_max = dplyr::case_when(
        name == "Barker_Res_Contents_af"  ~ 11277,
        name == "NBC_Res_Contents_af"     ~ 6927,
        name == "Upper_Storage_af"        ~ 18204,
        name == "Boulder_Res_Contents_af" ~ 18204
      )
    )

  return(drought_indices)
}
