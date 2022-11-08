process_annual_summary <- function(
    summary_path,
    model_run
) {

  # read in annual summary data
  summary_df <-
    readr::read_csv(
      summary_path,
      col_names      = FALSE,
      show_col_types = FALSE
    )

  # extract drought metrics of interest
  summary_df <- summary_df[132:136, 11:12]

  # rename columns and add model run column
  summary_df <-
    summary_df %>%
    stats::setNames(c("DroughtResponse", "Count")) %>%
    dplyr::mutate(
      model_run       = model_run,
      CityReliability = c(NA, NA, 1, 2, 3)
    ) %>%
    dplyr::mutate(
      new_col = dplyr::case_when(
        DroughtResponse %in% c("0") ~ "NA",
        DroughtResponse %in% c("1", "2") ~ "ep",
        TRUE ~ Count
      )
    )

  # get weird sum
  count_sum <-
    summary_df %>%
    dplyr::filter(new_col == "ep") %>%
    dplyr::mutate(
      Count = as.numeric(Count)
    ) %>%
    .$Count %>%
    sum() %>%
    as.character()

  # finalize summary w/ exceedance percent
  summary_df <-
    summary_df %>%
    dplyr::mutate(
      ExceedancePercent = dplyr::case_when(
        is.na(CityReliability) == TRUE ~ "NA",
        CityReliability == "1"         ~ count_sum,
        TRUE                           ~ new_col
      ),
      Criteria = c(NA, NA, 5, 1, 0.1),
      PassFail = dplyr::case_when(
        ExceedancePercent > Criteria ~ "fail",
        TRUE                         ~ "pass"
      )
    ) %>%
    dplyr::select(-new_col) %>%
    dplyr::relocate(model_run, DroughtResponse, Count, CityReliability, ExceedancePercent, Criteria, PassFail) %>%
    stats::setNames(c("Model Run", "Drought Response", "Count of Triggers",
                      "City Reliability", "Reliability Percent",
                      "Reliability Criteria", "Pass/Fail"))

  return(summary_df)

}
