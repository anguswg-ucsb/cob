process_res_water_type <- function(
    df,
    definitions_df,
    qm_filter = 24
) {
  # definitions_df <- definitions
  # Panama Reservoir Water Type

  # make definition column names lowercase
  names(definitions_df) <- tolower(names(definitions_df))

  res_water_type <-
    df %>%
    dplyr::select(
      year, qm, start_date, end_date, model_run,
      Link_617_Flow, Link_618_Flow, Decree_125_Content,
      Link_571_Flow, Link_572_Flow, Link_573_Flow
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c(-model_run, -year, -qm, -start_date, -end_date),
        as.numeric)
    ) %>%
    dplyr::group_by(year, model_run) %>%
    dplyr::summarise(dplyr::across(c(-qm, -start_date, -end_date), sum)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      year = as.numeric(year),
      qm   = qm_filter
    ) %>%
    tidyr::pivot_longer(
      cols      = c(-model_run, -year,  -qm),
      names_to  = "name",
      values_to = "output"
    )  %>%
    dplyr::left_join(
      cram_type(),
      by = c("name" = "Name")
    ) %>%
    dplyr::left_join(
      definitions_df,
      by = "name"
    ) %>%
    dplyr::mutate(
      description = factor(description)
    )
    # dplyr::mutate(
    #   description = dplyr::case_when(
    #     name == "COB_Total_Boulder_Res_Storage" ~ "COB Total Boulder Res Storage",
    #     TRUE                                    ~ description
    #   ),
    #   name = dplyr::case_when(
    #     name == "COB_Total_Boulder_Res_Storage" ~ "DataObject_1_Flow + DataObject_29_Flow",
    #     TRUE                                    ~ name
    #   ),
    #   parameter = dplyr::case_when(
    #     name == "COB_Total_Boulder_Res_Storage" ~ "Flow",
    #     TRUE                                    ~ name
    #   ),
    #   units = dplyr::case_when(
    #     name == "DataObject_1_Flow + DataObject_29_Flow" ~ "Flow (af)",
    #     TRUE                                    ~ units
    #   )
    # ) %>%
    # dplyr::mutate(
    #   date = start_date + floor(as.numeric(end_date - start_date))/2
    # )
    #

  # factor(res_water_type$description)
  # res_water_type$description %>% unique()
  # make definition column names lowercase
  # names(definitions_df) <- tolower(names(definitions_df))
  # dplyr::left_join(
  #   dplyr::mutate(
  #     dplyr::filter(
  #       dplyr::select(df, year, qm, model_run, Decree_125_Content),
  #       qm == qm_filter
  #     ),
  #     dplyr::across(
  #       c(-model_run, -year, -qm),
  #       as.numeric)
  #   ),
  #   by = c("year", "model_run")
  # )

  return(res_water_type)
}
process_wittemyer_qm_ts <- function(
    df,
    mod_run
) {
    # Wittemyer QM Time Series

    wittemyer_qm_ts <-
      df %>%
      dplyr::filter(model_run %in% mod_run) %>%
      dplyr::select(
        year, qm, start_date, end_date, model_run,
        Reservoir_13_Content, DataObject_27_Flow,
        Link_524_Flow, Link_592_Flow, Decree_106_Flow
      ) %>%
      dplyr::mutate(
        dplyr::across(
          c(-model_run, -year, -qm, -start_date, -end_date),
          as.numeric)
      ) %>%
      dplyr::group_by(year, model_run) %>%
      dplyr::mutate(
        date = start_date + floor(as.numeric(end_date - start_date))/2
        ) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_longer(
        cols      = c(-model_run, -year,  -qm, -start_date, -end_date, -date),
        names_to  = "name",
        values_to = "output"
      ) %>%
      dplyr::mutate(
          title = dplyr::case_when(
            name == "Reservoir_13_Content"    ~ "Wittemyer Pond Contents",
            name == "DataObject_27_Flow"      ~ "Boulder WWTP Reusable Return Flows",
            name == "Link_524_Flow"           ~ "Wittemyer Recapture of WWTP Reusable Water",
            name == "Link_592_Flow"           ~ "Wittemyer Recapture of GREP",
            name == "Decree_106_Flow"         ~ "Wittemyer First Fill Right"
          )
        )

    return(wittemyer_qm_ts)

}

process_wg_wittemyer_content <- function(
    df,
    timescale = "year"
) {

  # Get Wittemyer Pond Average (qm & annual) Contents
  if(timescale == "year") {

    # WG Boulder Res
    wittemyer_contents_yr <-
      df %>%
      dplyr::select(
        year, qm, wyqm, start_date, end_date, model_run,
        Reservoir_13_Content
      ) %>%
      dplyr::mutate(
        dplyr::across(
          c(-model_run, -year, -qm, -wyqm, -start_date, -end_date),
          as.numeric)
      ) %>%
      dplyr::group_by(year, model_run) %>%
      dplyr::summarise(
        Witt_annual_max_contents = max(Reservoir_13_Content),
        Witt_annual_avg_contents = mean(Reservoir_13_Content)
      ) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_longer(
        cols      = c(-model_run, -year),
        names_to  = "name",
        values_to = "output"
      ) %>%
      dplyr::mutate(
        year  = as.numeric(year),
        title = dplyr::case_when(
          name == "Witt_annual_max_contents" ~  "Wittemyer Pond Maximum Annual Contents",
          name == "Witt_annual_avg_contents" ~  "Wittemyer Pond Average Annual Contents"
          )
        )

    return(wittemyer_contents_yr)

  }

  if(timescale == "qm") {

    # WG Boulder Res
    wittemyer_contents_qm <-
      df %>%
      dplyr::select(
        year, qm, wyqm, start_date, end_date, model_run,
        Reservoir_13_Content
      ) %>%
      dplyr::mutate(
        dplyr::across(
          c(-model_run, -year, -qm, -wyqm, -start_date, -end_date),
          as.numeric)
      ) %>%
      dplyr::group_by(qm, model_run) %>%
      dplyr::summarise(
        Witt_qm_contents = mean(Reservoir_13_Content)
      ) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_longer(
        cols      = c(-model_run, -qm),
        names_to  = "name",
        values_to = "output"
      ) %>%
      dplyr::mutate(
        qm    = as.numeric(qm),
        title = dplyr::case_when(
          name == "Witt_qm_contents" ~  "Wittemyer Pond Average Quarter-Monthly Contents"
        )
      )

    return(wittemyer_contents_qm)

  }
}

# process_wg_reuse <- function(
#     df,
#     timescale = "year"
# ) {
#
#   # Get Wittemyer Pond Average (qm & annual) Contents
#   if(timescale == "year") {
#
#     # WG Boulder Res
#     wittemyer_contents_yr <-
#       df %>%
#       dplyr::select(
#         year, qm, wyqm, start_date, end_date, model_run,
#         Reservoir_13_Content
#       ) %>%
#       dplyr::mutate(
#         dplyr::across(
#           c(-model_run, -year, -qm, -wyqm, -start_date, -end_date),
#           as.numeric)
#       ) %>%
#       dplyr::group_by(year, model_run) %>%
#       dplyr::summarise(
#         Witt_annual_max_contents = max(Reservoir_13_Content),
#         Witt_annual_avg_contents = mean(Reservoir_13_Content)
#       ) %>%
#       dplyr::ungroup() %>%
#       tidyr::pivot_longer(
#         cols      = c(-model_run, -year),
#         names_to  = "name",
#         values_to = "output"
#       ) %>%
#       dplyr::mutate(
#         year  = as.numeric(year),
#         title = dplyr::case_when(
#           name == "Witt_annual_max_contents" ~  "Wittemyer Pond Maximum Annual Contents",
#           name == "Witt_annual_avg_contents" ~  "Wittemyer Pond Average Annual Contents"
#         )
#       )
#
#     return(wittemyer_contents_yr)
#
#   }
#
#   if(timescale == "qm") {
#
#     # WG Boulder Res
#     wittemyer_contents_qm <-
#       df %>%
#       dplyr::select(
#         year, qm, wyqm, start_date, end_date, model_run,
#         Reservoir_13_Content
#       ) %>%
#       dplyr::mutate(
#         dplyr::across(
#           c(-model_run, -year, -qm, -wyqm, -start_date, -end_date),
#           as.numeric)
#       ) %>%
#       dplyr::group_by(qm, model_run) %>%
#       dplyr::summarise(
#         Witt_qm_contents = mean(Reservoir_13_Content)
#       ) %>%
#       dplyr::ungroup() %>%
#       tidyr::pivot_longer(
#         cols      = c(-model_run, -year),
#         names_to  = "name",
#         values_to = "output"
#       ) %>%
#       dplyr::mutate(
#         year  = as.numeric(year),
#         title = dplyr::case_when(
#           name == "Witt_qm_contents" ~  "Wittemyer Pond Average Quarter-Monthly Contents"
#         )
#       )
#
#     return(wittemyer_contents_qm)
#
#   }
# }

process_wg_wittemyer_ts <- function(
    df
) {

  # definitions_df <- definitions
  # df <- outputs
  # CBT WG Boulder Res qm output

  # make definition column names lowercase
  # names(definitions_df) <- tolower(names(definitions_df))

  # WG Boulder Res
  wittemeyer_flow_ts <-
    df %>%
    dplyr::select(
      year, qm, wyqm, start_date, end_date, model_run,
      DataObject_27_Flow, Link_524_Flow, Link_592_Flow,
      Decree_106_Flow, Link_525_Flow
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c(-model_run, -year, -qm, -wyqm, -start_date, -end_date),
        as.numeric)
    ) %>%
    dplyr::group_by(year, model_run) %>%
    dplyr::summarise(
      BoulderWWTPTreatedReuseWater = sum(DataObject_27_Flow, na.rm = T),
      WittemyerRecaptureWWTPReuse  = sum(Link_524_Flow, na.rm = T),
      WittemyerRecaptureGREP       = sum(Link_592_Flow, na.rm = T),
      WittemyerFirstFillRight      = sum(Decree_106_Flow, na.rm = T),
      WittReleases                 = sum(Link_525_Flow, na.rm = T)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(year, model_run) %>%
    dplyr::mutate(
      WittTotalInflow = WittemyerRecaptureWWTPReuse + WittemyerRecaptureGREP + WittemyerFirstFillRight
    ) %>%
    dplyr::ungroup() %>%
    # dplyr::select(year, ModelRun, WittemyerRecaptureWWTPReuse, WittemyerRecaptureGREP, WittemyerFirstFillRight)
    tidyr::pivot_longer(
      cols      = c(-model_run, -year),
      names_to  = "name",
      values_to = "output"
    ) %>%
    dplyr::mutate(
      year  = as.numeric(year),
      title = dplyr::case_when(
        name == "BoulderWWTPTreatedReuseWater" ~  "Boulder WWTP Reusable Return Flows",
        name == "WittemyerRecaptureWWTPReuse"  ~  "Wittemyer Recapture of WWTP Reusable Water",
        name == "WittemyerRecaptureGREP"       ~  "Wittemyer Recapture of GREP",
        name == "WittemyerFirstFillRight"      ~  "Wittemyer First Fill Right",
        name == "WittTotalInflow"              ~  "Wittemyer Total Inflow",
        name == "WittReleases"                 ~  "Wittemyer Pond Releases"
      )
    )

  return(wittemeyer_flow_ts)
}

process_wg_wittemyer_source <- function(
    df
) {

  # definitions_df <- definitions
  # df <- outputs
  # CBT WG Boulder Res qm output

  # make definition column names lowercase
  # names(definitions_df) <- tolower(names(definitions_df))

  # WG Boulder Res
  wittemeyer_sources <-
    df %>%
    dplyr::select(
      year, qm, wyqm, start_date, end_date, model_run,
      DataObject_27_Flow, Link_524_Flow, Link_592_Flow,
      Decree_106_Flow, Link_525_Flow
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c(-model_run, -year, -qm, -wyqm, -start_date, -end_date),
        as.numeric)
    ) %>%
    dplyr::group_by(year, model_run) %>%
    dplyr::summarise(
      WittemyerRecaptureWWTPReuse  = sum(Link_524_Flow, na.rm = T),
      WittemyerRecaptureGREP       = sum(Link_592_Flow, na.rm = T),
      WittemyerFirstFillRight      = sum(Decree_106_Flow, na.rm = T)
      ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(
      cols      = c(-model_run, -year),
      names_to  = "name",
      values_to = "output"
    ) %>%
    dplyr::mutate(
      year = as.numeric(year)
    )

  return(wittemeyer_sources)
}

process_wg_boulder <- function(
    df,
    definitions_df
) {
  # definitions_df <- definitions
  # df <- outputs
  # CBT WG Boulder Res qm output

  # make definition column names lowercase
  names(definitions_df) <- tolower(names(definitions_df))

  # WG Boulder Res
  wg_boulder <-
    df %>%
    dplyr::select(
      year, qm, wyqm, start_date, end_date, model_run, DataObject_1_Flow, DataObject_29_Flow
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c(-model_run, -year, -qm, -wyqm, -start_date, -end_date),
        as.numeric)
    ) %>%
    dplyr::group_by(year, model_run) %>%
    dplyr::mutate(COB_Total_Boulder_Res_Storage = DataObject_1_Flow + DataObject_29_Flow) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(
      cols      = c(-model_run, -year, -qm, -wyqm, -start_date, -end_date),
      names_to  = "name",
      values_to = "output"
    ) %>%
    dplyr::left_join(
      definitions_df,
      by = "name"
    ) %>%
    dplyr::mutate(
      description = dplyr::case_when(
        name == "COB_Total_Boulder_Res_Storage" ~ "COB Total Boulder Res Storage",
        TRUE                                    ~ description
      ),
      name = dplyr::case_when(
        name == "COB_Total_Boulder_Res_Storage" ~ "DataObject_1_Flow + DataObject_29_Flow",
        TRUE                                    ~ name
      ),
      parameter = dplyr::case_when(
        name == "COB_Total_Boulder_Res_Storage" ~ "Flow",
        TRUE                                    ~ name
      ),
      units = dplyr::case_when(
        name == "DataObject_1_Flow + DataObject_29_Flow" ~ "Flow (af)",
        TRUE                                    ~ units
      )
    ) %>%
    dplyr::mutate(
      date = start_date + floor(as.numeric(end_date - start_date))/2
    )

  return(wg_boulder)
}


process_boulder_res <- function(
    df,
    definitions_df
) {
  # definitions_df <- definitions
  # df <- outputs

  # CBT Boulder Res qm

  # make definition column names lowercase
  names(definitions_df) <- tolower(names(definitions_df))

  # WG Boulder Res
  boulder_res <-
    df %>%
    dplyr::select(
      year, qm, wyqm, start_date, end_date, model_run,
      DataObject_1_Flow, DataObject_29_Flow, DataObject_2_Flow, Reservoir_12_Content
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c(-model_run, -year, -qm, -wyqm, -start_date, -end_date),
        as.numeric)
    ) %>%
    dplyr::group_by(year, model_run) %>%
    dplyr::mutate(COB_Total_Boulder_Res_Storage = DataObject_1_Flow + DataObject_29_Flow) %>%
    dplyr::ungroup() %>%
    dplyr::select(-DataObject_1_Flow, -DataObject_29_Flow) %>%
    tidyr::pivot_longer(
      cols      = c(-model_run, -year, -qm, -wyqm, -start_date, -end_date),
      names_to  = "name",
      values_to = "output"
    ) %>%
    dplyr::left_join(
      definitions_df,
      by = "name"
    ) %>%
    dplyr::mutate(
      description = dplyr::case_when(
        name == "COB_Total_Boulder_Res_Storage" ~ "COB Total Boulder Res Storage",
        TRUE                                    ~ description
      ),
      name = dplyr::case_when(
        name == "COB_Total_Boulder_Res_Storage" ~ "DataObject_1_Flow + DataObject_29_Flow",
        TRUE                                    ~ name
      ),
      parameter = dplyr::case_when(
        name == "COB_Total_Boulder_Res_Storage" ~ "Flow",
        TRUE                                    ~ name
      ),
      units = dplyr::case_when(
        name == "DataObject_1_Flow + DataObject_29_Flow" ~ "Flow (af)",
        TRUE                                    ~ units
      )
    ) %>%
    dplyr::mutate(
      date = start_date + floor(as.numeric(end_date - start_date))/2
    )

  return(boulder_res)
}
