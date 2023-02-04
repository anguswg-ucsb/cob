process_instream_flow <- function(
    df,
    isf_df,
    upper_dry_oct_apr_cfs = NULL,
    upper_dry_may_sep_cfs = NULL,
    lower_dry_oct_apr_cfs = NULL,
    lower_dry_may_sep_cfs = NULL,
    upper_avg_oct_apr_cfs = NULL,
    upper_avg_may_sep_cfs = NULL,
    lower_avg_oct_apr_cfs = NULL,
    lower_avg_may_sep_cfs = NULL
) {
  # df <- outputs
  # isf_df <- isf_year_type
  # upper_dry_oct_apr_cfs = NULL
  # upper_dry_may_sep_cfs = NULL
  # lower_dry_oct_apr_cfs = NULL
  # lower_dry_may_sep_cfs = NULL
  # upper_avg_oct_apr_cfs = NULL
  # upper_avg_may_sep_cfs = NULL
  # lower_avg_oct_apr_cfs = NULL
  # lower_avg_may_sep_cfs = NULL

  # if all are NULL, provide default values
  if(all(
    is.null(upper_dry_oct_apr_cfs), is.null(upper_dry_may_sep_cfs),
    is.null(lower_dry_oct_apr_cfs), is.null(lower_dry_may_sep_cfs),
    is.null(upper_avg_oct_apr_cfs), is.null(upper_avg_may_sep_cfs),
    is.null(lower_avg_oct_apr_cfs), is.null(lower_avg_may_sep_cfs)
  )) {

    # defaults
    upper_dry_oct_apr_cfs = 5
    upper_dry_may_sep_cfs = 7
    lower_dry_oct_apr_cfs = 1.5
    lower_dry_may_sep_cfs = 2.5
    upper_avg_oct_apr_cfs = 7
    upper_avg_may_sep_cfs = 10
    lower_avg_oct_apr_cfs = 2.5
    lower_avg_may_sep_cfs = 4

  }

  # list of function inputs
  input_args <- as.list(environment())

  # check function arguments for missing/invalid inputs
  arg_lst <- check_args(
    arg_lst = input_args,
    ignore  = c("df", "isf_df"),
    f       = "any"
  )

  # if invalid/missing arguments found, stop function
  if(!is.null(arg_lst)) {

    stop(arg_lst)

  }

  # TODO make as input to parent function
  flow_yeartype <- yeartype_lst(
    upper_dry_oct_apr_cfs = upper_dry_oct_apr_cfs,
    upper_dry_may_sep_cfs = upper_dry_may_sep_cfs,
    lower_dry_oct_apr_cfs = lower_dry_oct_apr_cfs,
    lower_dry_may_sep_cfs = lower_dry_may_sep_cfs,
    upper_avg_oct_apr_cfs = upper_avg_oct_apr_cfs,
    upper_avg_may_sep_cfs = upper_avg_may_sep_cfs,
    lower_avg_oct_apr_cfs = lower_avg_oct_apr_cfs,
    lower_avg_may_sep_cfs = lower_avg_may_sep_cfs
  )

  # column names of ISF data
  # isf_cols <- names(isf_df)[!names(isf_df) %in% c("wyqm", "days_in_qm")]

  # do instream flow calculations, summarise to annual
  instream_flow <-
    df %>%
    # dplyr::filter(model_run %in% mod_run) %>%
    dplyr::select(
      year, qm, wyqm, start_date, end_date, model_run,
      Link_350_Flow,
      Link_61_Flow, Link_63_Flow, DataObject_35_Flow, Reservoir_21_Content,
      DataObject_34_Flow, DataObject_33_Flow, Link_42_Flow
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c(-model_run, -year, -qm, -wyqm, -start_date, -end_date),
        as.numeric)
    ) %>%
    dplyr::left_join(
      tidyr::pivot_longer(
        isf_df,
        cols      = c(-wyqm, -days_in_qm),
        names_to  = "isf_name",
        values_to = "isf_value"
      ),
      by = "wyqm"
    ) %>%
    dplyr::mutate(
      GREP_Upper_ISF_cfs     = dplyr::if_else(
        DataObject_33_Flow == 350,
        (Link_350_Flow/days_in_qm)/1.9835,
        (Link_42_Flow/days_in_qm)/1.9835
      ),
      GREP_Upper_ISF_target  = dplyr::if_else(
        isf_value == "DRY",
        dplyr::if_else(qm <= 28, flow_yeartype$upper_dry_oct_apr_cfs, flow_yeartype$upper_dry_may_sep_cfs),
        dplyr::if_else(qm <= 28, flow_yeartype$upper_avg_oct_apr_cfs, flow_yeartype$upper_avg_may_sep_cfs)
      ),
      Upper_ISF_shortage_cfs = dplyr::if_else(
        (GREP_Upper_ISF_cfs >= GREP_Upper_ISF_target),
        0,
        round(GREP_Upper_ISF_cfs - GREP_Upper_ISF_target, 1)
      ),
      GREP_Lower_ISF_cfs     = dplyr::if_else(
        DataObject_34_Flow == 63,
        (Link_63_Flow/days_in_qm)/1.9835,
        (Link_61_Flow/days_in_qm)/1.9835
      ),
      GREP_Lower_ISF_target  = dplyr::if_else(
        isf_value == "DRY",
        dplyr::if_else(qm <= 28, flow_yeartype$lower_dry_oct_apr_cfs, flow_yeartype$lower_dry_may_sep_cfs),
        dplyr::if_else(qm <= 28, flow_yeartype$lower_avg_oct_apr_cfs, flow_yeartype$lower_avg_may_sep_cfs)
      ),
      Lower_ISF_shortage_cfs = dplyr::if_else(
        (GREP_Lower_ISF_cfs >= GREP_Lower_ISF_target),
        0,
        round(GREP_Lower_ISF_cfs - GREP_Lower_ISF_target, 1)
      ),
      Upper_ISF_shortage_af  = Upper_ISF_shortage_cfs * 1.9835 * days_in_qm,
      Lower_ISF_shortage_af  = Lower_ISF_shortage_cfs * 1.9835 * days_in_qm
    ) %>%
    dplyr::group_by(year, model_run) %>%
    dplyr::summarise(
      Annual_Upper_ISF_Shortage_af = sum(Upper_ISF_shortage_af, na.rm = TRUE),
      Annual_Lower_ISF_Shortage_af = sum(Lower_ISF_shortage_af, na.rm = TRUE),
      COB_Laf_GREP_contents        = mean(Reservoir_21_Content, na.rm = TRUE)
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
        name == "Annual_Upper_ISF_Shortage_af"    ~ "Annual Upper ISF Shortage (AF)",
        name == "Annual_Lower_ISF_Shortage_af"    ~ "Annual Lower ISF Shortage (AF)",
        name == "COB_Laf_GREP_contents"           ~ "Boulder + Lafayette GREP Contents (Annual Average)"
        # name == "Annual_Upper_ISF_Shortage_af"    ~ "Annual_Upper_ISF_Shortage_af",
        # name == "Annual_Lower_ISF_Shortage_af"    ~ "Annual_Lower_ISF_Shortage_af",
        # name == "COB_Laf_GREP_contents"           ~ "Boulder + Lafayette GREP Contents (Annual Average)"
      ),
      units = dplyr::case_when(
        name == "Annual_Upper_ISF_Shortage_af"    ~ "Flow (af)",
        name == "Annual_Lower_ISF_Shortage_af"    ~ "Flow (af)",
        name == "COB_Laf_GREP_contents"           ~ "Contents (af)"
      ),
      ylim  = dplyr::case_when(
        name == "Annual_Upper_ISF_Shortage_af"    ~ -4500,
        name == "Annual_Lower_ISF_Shortage_af"    ~ -4500,
        name == "COB_Laf_GREP_contents"           ~ 7000
      )
    )

  return(instream_flow)

}

process_grep_analysis <- function(
    df
) {

  # make definition column names lowercase
  # names(definitions_df) <- tolower(names(definitions_df))

  # Gross Reservoir Pool Analysis
  grep_df <-
    df %>%
    dplyr::select(
      year, model_run,
      Reservoir_21_Content, DataObject_31_Flow, DataObject_32_Flow,
      Link_587_Flow, Link_588_Flow, Link_541_Flow,
      DataObject_37_Flow, DataObject_38_Flow, Link_350_Flow
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c(-year, -model_run),
        as.numeric)
    ) %>%
    dplyr::group_by(year, model_run) %>%
    dplyr::summarise(
      Reservoir_21_Content = mean(Reservoir_21_Content),
      Link_350_Flow        = sum(Link_350_Flow),
      DataObject_31_Flow   = mean(DataObject_31_Flow),
      DataObject_32_Flow   = mean(DataObject_32_Flow),
      Link_587_Flow        = sum(Link_587_Flow),
      Link_588_Flow        = sum(Link_588_Flow),
      DataObject_37_Flow   = sum(DataObject_37_Flow),
      DataObject_38_Flow   = sum(DataObject_38_Flow)
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
        name == "Reservoir_21_Content" ~ "Boulder and Lafayette Gross Environmental Pool Contents (Average Annual)",
        name == "DataObject_31_Flow"   ~ "City of Boulder GREP Contents (Average Annual)",
        name == "DataObject_32_Flow"   ~ "City of Lafayette GREP Contents (Average Annual)",
        name == "Link_587_Flow"        ~ "Lafayette Flow To GREP (Total Annual)",
        name == "Link_588_Flow"        ~ "Boulder Exchange To GREP (Total Annual)",
        name == "DataObject_37_Flow"   ~ "Boulder Releases from GREP (Total Annual)",
        name == "DataObject_38_Flow"   ~ "Lafayette Releases from GREP (Total Annual)",
        name == "Link_350_Flow"        ~ "South Boulder Creek Instream Flow (Total Annual)"
      ),
      units = dplyr::case_when(
        name == "Reservoir_21_Content" ~ "Contents (af)",
        name == "DataObject_31_Flow"   ~ "Contents (af)",
        name == "DataObject_32_Flow"   ~ "Contents (af)",
        name == "Link_587_Flow"        ~ "Flow (af)",
        name == "Link_588_Flow"        ~ "Flow (af)",
        name == "DataObject_37_Flow"   ~ "Flow (af)",
        name == "DataObject_38_Flow"   ~ "Flow (af)",
        name == "Link_350_Flow"        ~ "Flow (af)"
      )
    )
    # dplyr::left_join(
    #   dplyr::select(definitions_df, name, units),
    #   by = "name"
    # )

  return(grep_df)

}

process_panama_pond_year <- function(
    df,
    definitions_df
) {

  # df <- outputs
  # definitions_df <- definitions

  # make definition column names lowercase
  names(definitions_df) <- tolower(names(definitions_df))

  # Get Panama Pond Average (Annual) Contents
  panama_pond <-
    df %>%
    dplyr::select(
      year, model_run, Reservoir_25_Content
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c(-year, -model_run),
        as.numeric)
    ) %>%
    dplyr::group_by(year, model_run) %>%
    dplyr::summarise(
      Reservoir_25_Content_max = max(Reservoir_25_Content),
      Reservoir_25_Content_avg = mean(Reservoir_25_Content)
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
        name == "Reservoir_25_Content_max" ~ "Panama Reservoir Maximum Annual Contents",
        name == "Reservoir_25_Content_avg" ~ "Panama Reservoir Average Annual Contents"
      )
    ) %>%
    dplyr::mutate(dict_join = "Reservoir_25_Content") %>%
    dplyr::left_join(
      dplyr::select(definitions_df, name, units),
      by = c("dict_join" = "name")
    ) %>%
    dplyr::select(-dict_join)

  return(panama_pond)

}

process_panama_res_qm <- function(
    df,
    definitions_df
) {

  # df <- outputs
  # definitions_df <- definitions

  # make definition column names lowercase
  names(definitions_df) <- tolower(names(definitions_df))

  # Panama Res QM Time Series Plots
  panama_res_qm <-
    df %>%
    dplyr::select(
      year, qm, start_date, end_date, model_run,
      Reservoir_25_Content, Link_571_Flow, Link_572_Flow,
      Link_617_Flow, Link_618_Flow, Link_573_Flow
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c(-year, -model_run, -qm, -start_date, -end_date,),
        as.numeric)
    ) %>%
    dplyr::mutate(
      date = start_date + floor(as.numeric(end_date - start_date))/2
    ) %>%
    dplyr::select(-start_date, -end_date) %>%
    tidyr::pivot_longer(
      cols      = c(-model_run, -year,  -qm, -date),
      names_to  = "name",
      values_to = "output"
    ) %>%
    dplyr::left_join(
      definitions_df,
      by = "name"
    )

  return(panama_res_qm)

}

process_panama_res <- function(
    df,
    definitions_df
) {

  # df <- outputs
  # definitions_df <- definitions

  # make definition column names lowercase
  names(definitions_df) <- tolower(names(definitions_df))

  # Panama Reservoir Annual totals
  panama_res <-
    df %>%
    dplyr::select(
      year, model_run,
      Reservoir_25_Content, Link_571_Flow, Link_572_Flow,
      Link_617_Flow, Link_618_Flow, Link_573_Flow
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c(-model_run, -year),
        as.numeric)
    ) %>%
    dplyr::group_by(year, model_run) %>%
    dplyr::summarise(
      Reservoir_25_Content_avg  = mean(Reservoir_25_Content),
      Reservoir_25_Content_min  = min(Reservoir_25_Content),
      Reservoir_25_Content_max  = max(Reservoir_25_Content),
      Link_571_Flow             = sum(Link_571_Flow),
      Link_572_Flow             = sum(Link_572_Flow),
      Link_617_Flow             = sum(Link_617_Flow),
      Link_618_Flow             = sum(Link_618_Flow),
      Link_573_Flow             = sum(Link_573_Flow)
      # COB_Panama_contents_Avg         = mean(Reservoir_25_Content),
      # COB_Panama_contents_Min         = min(Reservoir_25_Content),
      # COB_Panama_contents_Max         = max(Reservoir_25_Content),
      # BoulderWhiterock_ToPanama       = sum(Link_571_Flow),
      # COBPanamaResWR_FromLeggett      = sum(Link_572_Flow),
      # RecaptureGREPRelease_viaLeggett = sum(Link_617_Flow),
      # PanamaRecapture_WWTPReuseWater  = sum(Link_618_Flow),
      # COBPanama_ReusableRelease       = sum(Link_573_Flow),
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(
      cols      = c(-model_run, -year),
      names_to  = "name",
      values_to = "output"
    ) %>%
    dplyr::mutate(
      year  = as.numeric(year),
      dict_join = dplyr::case_when(
        grepl("Reservoir_25_Content", name) ~ "Reservoir_25_Content",
        TRUE                                ~ name
        )
      ) %>%
    dplyr::left_join(
      dplyr::select(definitions_df, name, units),
      by = c("dict_join" = "name")
    ) %>%
    dplyr::mutate(
      year  = as.numeric(year),
      title = dplyr::case_when(
        name == "Reservoir_25_Content_avg" ~ "Boulder Pool Contents in Panama Reservoir (Average Annual)",
        name == "Reservoir_25_Content_min" ~ "Boulder Pool Contents in Panama Reservoir (Min Annual)",
        name == "Reservoir_25_Content_max" ~ "Boulder Pool Contents in Panama Reservoir (Max Annual)",
        name == "Link_571_Flow"            ~ "COB Boulder & Whiterock Ditch to COB Panama (Total Annual) (Link 571)",
        name == "Link_572_Flow"            ~ "COB Panama Reservoir Water Right From Leggett (Total Annual) (Link 572)",
        name == "Link_617_Flow"            ~ "Reusable Water to Panama Res from Leggett Ditch (Total Annual) (Link 617)",
        name == "Link_618_Flow"            ~ "COB Recapture of WWTP Reusable Water (Total Annual) (Link 618)",
        name == "Link_573_Flow"            ~ "COB Panama Res Reusable Release (Total Annual) (Link 573)"
        # name == "COB_Panama_contents_Avg"         ~ "Boulder Pool Contents in Panama Reservoir (Average Annual)",
        # name == "COB_Panama_contents_Min"         ~ "Boulder Pool Contents in Panama Reservoir (Min Annual)",
        # name == "COB_Panama_contents_Max"         ~ "Boulder Pool Contents in Panama Reservoir (Max Annual)",
        # name == "BoulderWhiterock_ToPanama"       ~ "COB Boulder & Whiterock Ditch to COB Panama (Total Annual) (Link 571)",
        # name == "COBPanamaResWR_FromLeggett"      ~ "COB Panama Reservoir Water Right From Leggett (Total Annual) (Link 572)",
        # name == "RecaptureGREPRelease_viaLeggett" ~ "Reusable Water to Panama Res from Leggett Ditch (Total Annual) (Link 617)",
        # name == "PanamaRecapture_WWTPReuseWater"  ~ "COB Recapture of WWTP Reusable Water (Total Annual) (Link 618)",
        # name == "COBPanama_ReusableRelease"       ~ "COB Panama Res Reusable Release (Total Annual) (Link 573)"
      )
    ) %>%
    dplyr::select(-dict_join)


  return(panama_res)
}

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
