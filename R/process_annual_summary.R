process_annual_summary <- function(
    file_df,
    date_df,
    verbose = TRUE
) {
  file_df <- model_dirs[2,]
  file_df
  date_df     = date_convert
  # verbose     = TRUE
  # rm(file_df, out, out2, y, verbose, date_df, output_path, model_ver, model_id, model_num)

  # path to file
  output_path <- file_df$path

  # model version
  model_ver   <- file_df$model_version

  # Model ID
  model_id    <- file_df$model_id

  # Model number
  model_num   <- file_df$model_num

  # Model number
  extra_info  <- file_df$extra_info

  if(verbose == TRUE) {
    message(paste0("Processing OutputSheet...",
                   "\nFilename: ", basename(output_path),
                   "\nModel Version: ", model_ver)
    )
  }

  # Read in OutputSheet CSV
  out <- readr::read_csv(
    file           = output_path,
    col_names      = FALSE,
    col_types      = readr::cols(.default="c"),
    show_col_types = FALSE
  )

  # probs <- readr::problems()

  # make sure columns are correctly read in, if not, enter correct value given from readr::problems()
  # out <-
  #   out %>%
  #   replace_probs(
  #     prob_df = probs,
  #     verbose = FALSE
  #     )

  # number of rows to skip, when "Step" is seen in column 1, read in data after that
  name_index    <- grep("Step", out$X1)

  # row of column names as a vector
  name_row <-
    out[name_index, ] %>%
    tidyr::pivot_longer(cols = everything()) %>%
    .$value

  # set output sheet names
  names(out) <- name_row

  # correct NA_NA columns made in first few columns
  out <-
    out %>%
    stats::setNames(c("year", "qm", "step", names(out)[4:length(names(out))]))

  # remove rows w/ only NA values across entire row
  out <-
    out %>%
    rm_na_rows()

  # convert dates dataframe
  cdate <-
    date_df %>%
    janitor::clean_names() %>%
    dplyr::mutate(dplyr::across(where(is.numeric), as.character))

  # remove descriptive rows
  out <- out[(name_index+1):nrow(out),]

  # remove columns w/ only NA values in entire column
  out <- rm_na_cols(out)

  # join date conversion w/ output sheet
  out <-
    out %>%
    dplyr::left_join(
      dplyr::select(cdate,
                    qm,
                    year = water_year,
                    start_date, end_date
      ),
      by = c("qm", "year")
    ) %>%
    dplyr::mutate(
      model_version  = model_ver,
      model_id       = model_id,
      model_num      = model_num,
      extra_info     = extra_info,
      wyqm           = paste(year, qm, sep = '-'),
      start_date     = as.Date(start_date, format="%m/%d/%Y", tz = "UTC"),
      end_date       = as.Date(end_date, format="%m/%d/%Y", tz = "UTC")
    ) %>%
    dplyr::relocate(model_version, model_id, model_num, extra_info, year, qm, wyqm, step, start_date, end_date)

  return(out)

}


process_drought_metrics <- function(
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

process_psi <- function(
    summary_path,
    model_run
) {
  # y =1
  # summary_path = mod_summary_lst[[y]]$path
  # model_run    = scenario_name[y]
  # read in annual summary data
  summary_df <-
    readr::read_csv(
      summary_path,
      col_names      = FALSE,
      show_col_types = FALSE
    )

  # extract drought metrics of interest
  summary_df2 <- summary_df[21:120, 1:23]

  # clean up dataset
  summary_df2 <-
    summary_df2 %>%
    dplyr::mutate(
    model_run       = model_run
  ) %>%
    stats::setNames(c("year", "blank1", "blank2", "COBTOtalDemandFlow", "COBTotalDemandShortage",
                      "COBDemandMaytoApr", "LakewoodPipeBetass", "BarkerPipeBetass",
                      "May1MtnStorage", "May1COBStorage", "PSI", "DroughtResponseLevel", "IndoorDemand",
                      "PRVrelease", "COBTotalDemand", "EOYResStorage", "DemandinExcessTreatment",
                      "MaxDemandforDRI", "DirectFlowDemand", "SouthPlatteCall", "SPCallRank",
                      "COBCBTVolume", "COBCBTWater", "ModelRun")) %>%
    rm_na_cols() %>%
    dplyr::select(year, ModelRun, May1MtnStorage, May1COBStorage, PSI, DroughtResponseLevel, COBCBTVolume, COBCBTWater) %>%
    dplyr::mutate(dplyr::across(c(May1MtnStorage, May1COBStorage, PSI, DroughtResponseLevel, COBCBTVolume, COBCBTWater), as.numeric)) %>%
    dplyr::mutate(COBCBTVolume = round(as.numeric(COBCBTVolume),1)) %>%
    dplyr::filter(year >= 2000 & year <= 2008)
  # summary_df2 %>%
  #   dplyr::select(year, ModelRun, May1MtnStorage, May1COBStorage, PSI, DroughtResponseLevel, COBCBTVolume, COBCBTWater) %>%
  #   dplyr::mutate(dplyr::across(c(-year, - )))
    # mutate(
    #   year = as.numeric(year)
    #   ) %>%
    # mutate(May1MtnStorage = as.numeric(May1MtnStorage)) %>%
    # mutate(May1COBStorage = as.numeric(May1COBStorage)) %>%
    # mutate(PSI = as.numeric(PSI)) %>%
    # mutate(DroughtResponseLevel = as.numeric(DroughtResponseLevel)) %>%
    # mutate(COBCBTVolume = round(as.numeric(COBCBTVolume),1)) %>%
    # mutate(COBCBTWater = as.numeric(COBCBTWater)) %>%
    # filter(year >= 2000 & year <= 2008)
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
