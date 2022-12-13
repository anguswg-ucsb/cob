process_cbt_quota <- function(df, quota_df) {

  cbt_qm24 <-
    outputs %>%
    dplyr::mutate(
      dplyr::across(
        c(-model_run, -model_version, -model_id, -model_num,
          -extra_info, -year, -qm, -wyqm, -step, -start_date, -end_date),
        as.numeric)
    ) %>%
    dplyr::filter(qm <= 24) %>%
    dplyr::group_by(year, model_run) %>%
    dplyr::summarise(Decree75_QM1_24 = sum(Decree_75_Flow)) %>%
    dplyr::mutate(cbt_year_taken = as.numeric(year) - 1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(model_run, year) %>%
    dplyr::filter(cbt_year_taken >= 1915 & cbt_year_taken <= 2013) %>%
    dplyr::group_by(cbt_year_taken, model_run) %>%
    dplyr::select(-year) %>%
    dplyr::rename(year = cbt_year_taken)  %>%
    dplyr::arrange(model_run, year) %>%
    dplyr::mutate(year = as.character(year)) %>%
    dplyr::filter(year >= 1915 & year <= 2013) %>%
    dplyr::ungroup()

  cbt_qm25 <-
    outputs %>%
    dplyr::mutate(
      dplyr::across(
        c(-model_run, -model_version, -model_id, -model_num,
          -extra_info, -year, -qm, -wyqm, -step, -start_date, -end_date),
        as.numeric)
    ) %>%
    dplyr::filter(qm >= 25) %>%
    dplyr::group_by(year, model_run) %>%
    dplyr::summarise(Decree75_QM25_48 = sum(Decree_75_Flow))  %>%
    dplyr::arrange(model_run, year) %>%
    dplyr::mutate(year = as.character(year)) %>%
    dplyr::filter(year >= 1915 & year <= 2013) %>%
    dplyr::ungroup()

  # run an annual analysis part 1
  cbt_extract24 <-
    outputs %>%
    dplyr::select(year, qm, model_run, DataObject_3_Flow) %>%
    dplyr::filter(qm == 24) %>%
    dplyr::filter(year >= 1915 & year <= 2013) %>%
    dplyr::group_by(model_run) %>%
    dplyr::rename(qm24 = qm) %>%
    dplyr::arrange(model_run, year) %>%
    dplyr::ungroup()

  # run an annual analysis part 2
  cbt_extract25 <-
    outputs %>%
    dplyr::select(year, qm, model_run, DataObject_39_Flow, DataObject_44_Flow) %>%
    dplyr::filter(qm == 25) %>%
    dplyr::filter(year >= 1915 & year <= 2013) %>%
    dplyr::group_by(model_run) %>%
    dplyr::rename(qm25 = qm) %>%
    dplyr::arrange(model_run, year) %>%
    dplyr::ungroup()

  cbt_quota <-
    quota_df %>%
    dplyr::arrange(model_run, year) %>%
    dplyr::filter(year >= 1915 & year <= 2013) %>%
    dplyr::mutate(
      year              = as.character(year),
      COB_CBT_allotment = quota * 21174
    )

  cbt_extract <-
    cbt_extract24 %>%
    dplyr::left_join(
      cbt_extract25,
      by = c("year", "model_run")
    ) %>%
    dplyr::left_join(
      cbt_qm24,
      by = c("year", "model_run")
    ) %>%
    dplyr::left_join(
      cbt_qm25,
      by = c("year", "model_run")
    ) %>%
    dplyr::left_join(
      cbt_quota,
      by = c("year", "model_run")
    ) %>%
    dplyr::relocate(year, qm24, qm25) %>%
    dplyr::mutate(
      dplyr::across(
        c(-model_run, -year, -qm24, -qm25),
        as.numeric)
    ) %>%
    dplyr::mutate(uid = 1:n()) %>%
    dplyr::group_by(uid) %>%
    # dplyr::group_by(year, model_run)
    dplyr::mutate(
      COB_CBT_NormalUse      = sum(Decree75_QM1_24, Decree75_QM25_48),
      COB_CBT_BorrowedWinter = (DataObject_3_Flow - DataObject_44_Flow),
      COB_CBT_YeartoYearDebt = (DataObject_44_Flow),
      COB_CBT_TotalUse       = sum(COB_CBT_NormalUse, COB_CBT_BorrowedWinter, COB_CBT_YeartoYearDebt),
      COB_CBT_Unused         = round(COB_CBT_allotment - COB_CBT_TotalUse, 0)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-uid) %>%
    tidyr::pivot_longer(cols = c(-year, -qm24, -qm25, -model_run, -quota)) %>%
    dplyr::left_join(
      data.frame(
        name = c("DataObject_3_Flow",
                 "Decree75_QM1_24",
                 "Decree75_QM25_48",
                 "DataObject_44_Flow",
                 "COB_CBT_allotment",
                 "COB_CBT_TotalUse",
                 "COB_CBT_Unused",
                 "COB_CBT_NormalUse",
                 "COB_CBT_BorrowedWinter",
                 "COB_CBT_YeartoYearDebt"),
        group = c("CBT Model Component",
                  "CBT Model Component",
                  "CBT Model Component",
                  "CBT Model Component",
                  "Total CBT",
                  "CBT Summary",
                  "CBT Summary",
                  "CBT Component",
                  "CBT Component",
                  "CBT Component")
      ),
      by = c("name")
    ) %>%
    dplyr::left_join(
      dplyr::bind_rows(definitions, cbt_new_params()),
      by = c("name" = "Name")
    )

  return(cbt_extract)

}
