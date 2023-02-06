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
process_cbt_quota_borrow_on <- function(
    df,
    definitions_df,
    quota_df
) {
  # quota_df <- quota
  # df <- outputs

  cbt_qm24 <-
    df %>%
    dplyr::select(year, model_run, qm, wyqm, start_date, end_date,
                  Decree_75_Content, Decree_75_Flow, DataObject_39_Flow,
                  DataObject_3_Flow, DataObject_44_Flow) %>%
    dplyr::mutate(
      dplyr::across(
        c(-model_run,  -year, -qm, -wyqm, -start_date, -end_date),
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
    df %>%
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
    df %>%
    dplyr::select(year, qm, model_run, DataObject_3_Flow) %>%
    dplyr::filter(qm == 24) %>%
    dplyr::filter(year >= 1915 & year <= 2013) %>%
    dplyr::group_by(model_run) %>%
    dplyr::rename(qm24 = qm) %>%
    dplyr::arrange(model_run, year) %>%
    dplyr::ungroup()

  # run an annual analysis part 2
  cbt_extract25 <-
    df %>%
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
    dplyr::select(-uid, -DataObject_39_Flow) %>%
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
      dplyr::bind_rows(definitions_df, cbt_new_params_on()),
      by = c("name" = "Name")
    )

  return(cbt_extract)

}

process_cbt_quota_borrow_off <- function(
    df,
    definitions_df
) {

  # Get the half of the annual CBT water (Decree 75) (QM 5-24)
  cbt_qm5_qm24 <-
    df %>%
    dplyr::mutate(
      dplyr::across(
        c(-model_run, -model_version, -model_id, -model_num,
          -extra_info, -year, -wyqm, -step, -start_date, -end_date),
        as.numeric)
    ) %>%
    dplyr::filter( qm <= 24 & qm >= 5) %>%
    dplyr::group_by(year, model_run) %>%
    dplyr::summarise(Decree75_QM5_24 = sum(Decree_75_Flow)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(model_run, year) %>%
    # dplyr::filter(year >= 1915 & year <= 2014) %>%
    dplyr::ungroup()

  cbt_qm25_qm48 <-
    df %>%
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
    # dplyr::filter(year >= 1915 & year <= 2013) %>%
    dplyr::ungroup()

  # CBT use extract decree 75 for QM's 1-24, which are off by 1 year from CBT accounting (decree 75 resets to 0 at QM25)
  cbt_qm1_qm4 <-
    df %>%
    dplyr::mutate(
      dplyr::across(
        c(-model_run, -model_version, -model_id, -model_num,
          -extra_info, -year, -qm, -wyqm, -step, -start_date, -end_date),
        as.numeric)
    ) %>%
    dplyr::filter(qm >= 1 & qm <= 4) %>%
    dplyr::group_by(year, model_run) %>%
    dplyr::summarise(Decree75_QM1_4 = sum(Decree_75_Flow)) %>%
    dplyr::mutate(cbt_year_taken = as.numeric(year) - 1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(model_run, year) %>%
    dplyr::filter(cbt_year_taken >= 1915 & cbt_year_taken <= 2014) %>%
    dplyr::bind_rows(
      data.frame(
        year           = c("2015", "2015"),
        model_run      = unique(df$model_run),
        Decree75_QM1_4 = c(0, 0),
        cbt_year_taken = c(2014, 2014)
      )
    ) %>%
    dplyr::group_by(cbt_year_taken, model_run) %>%
    dplyr::select(-year) %>%
    dplyr::rename(year = cbt_year_taken)  %>%
    dplyr::arrange(model_run, year) %>%
    dplyr::mutate(year = as.character(year)) %>%
    dplyr::relocate(year, .before = model_run) %>%
    # dplyr::filter(year >= 1915 & year <= 2013) %>%
    dplyr::ungroup()

  cbt_quota <-
    df %>%
    dplyr::arrange(model_run, year) %>%
    dplyr::filter(qm == 25) %>%
    dplyr::mutate(
      dplyr::across(
        c(-model_run, -model_version, -model_id, -model_num,
          -extra_info, -year, -qm, -wyqm, -step, -start_date, -end_date),
        as.numeric)
    ) %>%
    dplyr::mutate(
      year              = as.character(year),
      COB_CBT_allotment = (DataObject_14_Flow/100) * 21174
    ) %>%
    dplyr::select(year, model_run, DataObject_14_Flow, COB_CBT_allotment)

  # dplyr::select(year, model_run, wyqm, start_date, end_date, DataObject_14_Flow, COB_CBT_allotment)
  # cbt_qm5_qm24, cbt_qm25_qm48, cbt_qm1_qm4,cbt_quota

  cbt_join <-
    cbt_quota %>%
    dplyr::left_join(
      cbt_qm1_qm4,
      by = c("year", "model_run")
    ) %>%
    dplyr::left_join(
      cbt_qm5_qm24,
      by = c("year", "model_run")
    ) %>%
    dplyr::left_join(
      cbt_qm25_qm48,
      by = c("year", "model_run")
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c(-model_run, -year),
        as.numeric)
    ) %>%
    dplyr::mutate(uid = 1:n()) %>%
    dplyr::group_by(uid) %>%
    dplyr::mutate(
      COB_CBT_Used   = sum(Decree75_QM5_24, Decree75_QM25_48, Decree75_QM1_4),
      COB_CBT_Unused = COB_CBT_allotment - COB_CBT_Used
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-uid, -DataObject_14_Flow) %>%
    tidyr::pivot_longer(cols = c(-year, -model_run)) %>%
    dplyr::left_join(
      data.frame(
        name = c("Decree75_QM5_24",
                 "Decree75_QM25_48",
                 "Decree75_QM1_4",
                 "COB_CBT_allotment",
                 "COB_CBT_Used",
                 "COB_CBT_Unused"
        ),
        group = c("CBT Model Component",
                  "CBT Model Component",
                  "CBT Model Component",
                  "Total CBT",
                  "CBT Summary",
                  "CBT Summary"
        )
      ),
      by = c("name")
    ) %>%
    dplyr::left_join(
      dplyr::bind_rows(definitions_df, cbt_new_params_off()),
      by = c("name" = "Name")
    )

  return(cbt_join)

}

process_cbt_quota <- function(
    df,
    quota_df,
    definitions_df,
    borrow = "off"
) {

  # if borrow is not set to "on" or "off", throw an error
  if(!borrow %in% c("on", "off")) {

    stop(paste0("Invalid 'borrow' argument, 'borrow' must be either 'on' or 'off'"))

  }

  # if borrow is set to "off"
  if(borrow == "off") {

    cbt_quota <- process_cbt_quota_borrow_off(
      df             = df,
      definitions_df = definitions_df
    )

    return(cbt_quota)

  }

  # if borrow is set to "on"
  if(borrow == "on") {

    cbt_quota <- process_cbt_quota_borrow_on(
      df             = df,
      definitions_df = definitions_df,
      quota_df       = quota_df
    )

    return(cbt_quota)

  }

}
#
# process_cbt_quota1 <- function(df, quota_df) {
#
#   cbt_qm24 <-
#     df %>%
#     dplyr::mutate(
#       dplyr::across(
#         c(-model_run, -model_version, -model_id, -model_num,
#           -extra_info, -year, -qm, -wyqm, -step, -start_date, -end_date),
#         as.numeric)
#     ) %>%
#     dplyr::filter(qm <= 24) %>%
#     dplyr::group_by(year, model_run) %>%
#     dplyr::summarise(Decree75_QM1_24 = sum(Decree_75_Flow)) %>%
#     dplyr::mutate(cbt_year_taken = as.numeric(year) - 1) %>%
#     dplyr::ungroup() %>%
#     dplyr::arrange(model_run, year) %>%
#     dplyr::filter(cbt_year_taken >= 1915 & cbt_year_taken <= 2013) %>%
#     dplyr::group_by(cbt_year_taken, model_run) %>%
#     dplyr::select(-year) %>%
#     dplyr::rename(year = cbt_year_taken)  %>%
#     dplyr::arrange(model_run, year) %>%
#     dplyr::mutate(year = as.character(year)) %>%
#     dplyr::filter(year >= 1915 & year <= 2013) %>%
#     dplyr::ungroup()
#
#   cbt_qm25 <-
#     df %>%
#     dplyr::mutate(
#       dplyr::across(
#         c(-model_run, -model_version, -model_id, -model_num,
#           -extra_info, -year, -qm, -wyqm, -step, -start_date, -end_date),
#         as.numeric)
#     ) %>%
#     dplyr::filter(qm >= 25) %>%
#     dplyr::group_by(year, model_run) %>%
#     dplyr::summarise(Decree75_QM25_48 = sum(Decree_75_Flow))  %>%
#     dplyr::arrange(model_run, year) %>%
#     dplyr::mutate(year = as.character(year)) %>%
#     dplyr::filter(year >= 1915 & year <= 2013) %>%
#     dplyr::ungroup()
#
#   # run an annual analysis part 1
#   cbt_extract24 <-
#     df %>%
#     dplyr::select(year, qm, model_run, DataObject_3_Flow) %>%
#     dplyr::filter(qm == 24) %>%
#     dplyr::filter(year >= 1915 & year <= 2013) %>%
#     dplyr::group_by(model_run) %>%
#     dplyr::rename(qm24 = qm) %>%
#     dplyr::arrange(model_run, year) %>%
#     dplyr::ungroup()
#
#   # run an annual analysis part 2
#   cbt_extract25 <-
#     df %>%
#     dplyr::select(year, qm, model_run, DataObject_39_Flow, DataObject_44_Flow) %>%
#     dplyr::filter(qm == 25) %>%
#     dplyr::filter(year >= 1915 & year <= 2013) %>%
#     dplyr::group_by(model_run) %>%
#     dplyr::rename(qm25 = qm) %>%
#     dplyr::arrange(model_run, year) %>%
#     dplyr::ungroup()
#
#   cbt_quota <-
#     quota_df %>%
#     dplyr::arrange(model_run, year) %>%
#     dplyr::filter(year >= 1915 & year <= 2013) %>%
#     dplyr::mutate(
#       year              = as.character(year),
#       COB_CBT_allotment = quota * 21174
#     )
#
#   cbt_extract <-
#     cbt_extract24 %>%
#     dplyr::left_join(
#       cbt_extract25,
#       by = c("year", "model_run")
#     ) %>%
#     dplyr::left_join(
#       cbt_qm24,
#       by = c("year", "model_run")
#     ) %>%
#     dplyr::left_join(
#       cbt_qm25,
#       by = c("year", "model_run")
#     ) %>%
#     dplyr::left_join(
#       cbt_quota,
#       by = c("year", "model_run")
#     ) %>%
#     dplyr::relocate(year, qm24, qm25) %>%
#     dplyr::mutate(
#       dplyr::across(
#         c(-model_run, -year, -qm24, -qm25),
#         as.numeric)
#     ) %>%
#     dplyr::mutate(uid = 1:n()) %>%
#     dplyr::group_by(uid) %>%
#     # dplyr::group_by(year, model_run)
#     dplyr::mutate(
#       COB_CBT_NormalUse      = sum(Decree75_QM1_24, Decree75_QM25_48),
#       COB_CBT_BorrowedWinter = (DataObject_3_Flow - DataObject_44_Flow),
#       COB_CBT_YeartoYearDebt = (DataObject_44_Flow),
#       COB_CBT_TotalUse       = sum(COB_CBT_NormalUse, COB_CBT_BorrowedWinter, COB_CBT_YeartoYearDebt),
#       COB_CBT_Unused         = round(COB_CBT_allotment - COB_CBT_TotalUse, 0)
#     ) %>%
#     dplyr::ungroup() %>%
#     dplyr::select(-uid) %>%
#     tidyr::pivot_longer(cols = c(-year, -qm24, -qm25, -model_run, -quota)) %>%
#     dplyr::left_join(
#       data.frame(
#         name = c("DataObject_3_Flow",
#                  "Decree75_QM1_24",
#                  "Decree75_QM25_48",
#                  "DataObject_44_Flow",
#                  "COB_CBT_allotment",
#                  "COB_CBT_TotalUse",
#                  "COB_CBT_Unused",
#                  "COB_CBT_NormalUse",
#                  "COB_CBT_BorrowedWinter",
#                  "COB_CBT_YeartoYearDebt"),
#         group = c("CBT Model Component",
#                   "CBT Model Component",
#                   "CBT Model Component",
#                   "CBT Model Component",
#                   "Total CBT",
#                   "CBT Summary",
#                   "CBT Summary",
#                   "CBT Component",
#                   "CBT Component",
#                   "CBT Component")
#       ),
#       by = c("name")
#     ) %>%
#     dplyr::left_join(
#       dplyr::bind_rows(definitions, cbt_new_params()),
#       by = c("name" = "Name")
#     )
#
#   return(cbt_extract)
#
# }

process_cbt_quota_tbl <- function(
    summary_df
) {

  cbt_extract_tbl <-
    summary_df %>%
    dplyr::filter(name %in% c("COB_CBT_Used", "COB_CBT_Unused")) %>%
    tidyr::pivot_wider(
      id_cols     = c(year, model_run),
      names_from  = name,
      values_from = value
    ) %>%
    dplyr::group_by(model_run) %>%
    dplyr::summarise(
      'CBT Use (mean)'    = round(mean(COB_CBT_Used),0), 'CBT Use (min)' = min(COB_CBT_Used),
      'CBT Use (max)'     = max(COB_CBT_Used),
      'Unused CBT (mean)' = round(mean(COB_CBT_Unused),0), 'Unused CBT (min)' = min(COB_CBT_Unused),
      'Unused CBT (max)'  = max(COB_CBT_Unused)
    ) %>%
    dplyr::ungroup()


  return(cbt_extract_tbl)

}



process_cbt_windygap <- function(
    df,
    qm_filter = 29
) {

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
    dplyr::filter(qm == qm_filter) %>%
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
# CBT, Windy Gap, Reusable Water Exchange Analysis
process_reuse_water_exchange <- function(df) {

  reuse_water_exchange <-
    df %>%
    dplyr::mutate(
      dplyr::across(
        c(-model_run, -model_version, -model_id, -model_num,
          -extra_info, -year, -qm, -wyqm, -step, -start_date, -end_date),
        as.numeric)
    ) %>%
    dplyr::group_by(model_run) %>%
    dplyr::mutate(
      DO29diff = lag(DataObject_29_Flow) - DataObject_29_Flow,
    ) %>%
    dplyr::group_by(model_run, wyqm) %>%
    dplyr::mutate(
      BoulderResWGtoCity = max(min(DO29diff, Link_388_Flow, na.rm = TRUE), 0, na.rm = TRUE),
      ReuseStorage       = sum(DataObject_28_Flow, DataObject_30_Flow, DataObject_29_Flow)
    ) %>%
    dplyr::relocate(DO29diff, Link_388_Flow, BoulderResWGtoCity, DataObject_28_Flow, DataObject_30_Flow, DataObject_29_Flow, ReuseStorage) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(year, model_run) %>%
    dplyr::summarise(
      CBT_Inflow                  = sum(Decree_75_Flow), WindyGap_Inflow = sum(Link_499_Flow),
      BoulderRes_WGtoCity         = sum(BoulderResWGtoCity),
      SBC_ISF                     = sum(Link_350_Flow),
      BarkerRes_ReusableWater     = max(DataObject_28_Flow),  # max storage by year
      NBCRes_ReusableWater        = max(DataObject_30_Flow),  # max storage by year
      BoulderRes_ReusableWater    = max(DataObject_29_Flow),  # max storage by year
      BoulderRes_WGExchtoBarker   = sum(DataObject_43_Flow),
      BoulderRes_WGExchtoNBCRes   = sum(DataObject_42_Flow),
      BoulderRes_WGExctoUpperStor = sum(DataObject_41_Flow),
      # NBCRes_Contents             = mean(Reservoir_1_Content),
      COB_Reusable_Contents       = max(ReuseStorage)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(cols = c(-year, -model_run)) %>%
    dplyr::mutate(
      year  = as.numeric(year),
      title = dplyr::case_when(
        name == "CBT_Inflow"                   ~  "C-BT Inflow",
        name == "WindyGap_Inflow"              ~  "Windy Gap Inflow",
        name == "BoulderRes_WGExchtoBarker"    ~  "Boulder Reservoir: Windy Gap Exch. to Barker Res",
        name == "BoulderRes_WGExchtoNBCRes"    ~  "Boulder Reservoir: Windy Gap Exch. to NBC Res",
        name == "BoulderRes_WGExctoUpperStor"  ~  "Boulder Reservoir: Windy Gap Total Exch to Upper Storage",
        name == "BoulderRes_WGtoCity"          ~  "Boulder Reservoir: Windy Gap to City",
        name == "SBC_ISF"                      ~  "South Boulder Creek Instream Flow",
        name == "BarkerRes_ReusableWater"      ~  "Barker Reservoir: Maximum Annual Reusable Water",
        name == "NBCRes_ReusableWater"         ~  "NBC Reservoir: Maximum Annual Reusable Water",
        name == "BoulderRes_ReusableWater"     ~  "Boulder Reservoir: Maximum Annual Reusable Water",
        name == "COB_Reusable_Contents"        ~  "Barker + NBC + Boulder: Maximum Annual Reusable Contents"
      ),
      ylabs_max = dplyr::case_when(
        name == "CBT_Inflow"                   ~  18000,
        name == "WindyGap_Inflow"              ~  18000,
        name == "BoulderRes_WGExchtoBarker"    ~  4000,
        name == "BoulderRes_WGExchtoNBCRes"    ~  4000,
        name == "BoulderRes_WGExctoUpperStor"  ~  4000,
        name == "BoulderRes_WGtoCity"          ~  3500,
        name == "SBC_ISF"                      ~  50000,
        name == "BarkerRes_ReusableWater"      ~  12000,
        name == "NBCRes_ReusableWater"         ~  12000,
        name == "BoulderRes_ReusableWater"     ~  12000,
        name == "COB_Reusable_Contents"        ~  20000
      ),
      ylabs_title = dplyr::case_when(
        name == "CBT_Inflow"                   ~  "Flow (af)",
        name == "WindyGap_Inflow"              ~  "Flow (af)",
        name == "BoulderRes_WGExchtoBarker"    ~  "Flow (af)",
        name == "BoulderRes_WGExchtoNBCRes"    ~  "Flow (af)",
        name == "BoulderRes_WGExctoUpperStor"  ~  "Flow (af)",
        name == "BoulderRes_WGtoCity"          ~  "Flow (af)",
        name == "SBC_ISF"                      ~  "Flow (af)",
        name == "BarkerRes_ReusableWater"      ~  "Contents (af)",
        name == "NBCRes_ReusableWater"         ~  "Contents (af)",
        name == "BoulderRes_ReusableWater"     ~  "Contents (af)",
        name == "COB_Reusable_Contents"        ~  "Contents (af)"
      )
    )

  return(reuse_water_exchange)
}
process_mass_balance_source <- function(df) {

  # calculate mass balance by source
  mass_bal <-
    df %>%
    dplyr::mutate(
      dplyr::across(
        c(-model_run, -model_version, -model_id, -model_num,
          -extra_info, -year, -qm, -wyqm, -step, -start_date, -end_date),
        as.numeric)
    ) %>%
    dplyr::group_by(year, model_run) %>%
    dplyr::summarise(
      Direct_Exchange     = sum(Link_447_Flow),
      Reservoir_Release   = sum(Link_448_Flow),
      Direct_Flow_Rights  = sum(Link_406_Flow),
      COB_Water_Demand    = sum(DataObject_23_Flow),
      COB_Indoor_Demand   = sum(Demand_58_Flow),
      COB_Outdoor_Demand  = sum(Demand_91_Flow),
      CBT_Inflow          = sum(Decree_75_Flow),
      WindyGap_Inflow     = sum(Link_499_Flow)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(cols = c(-year, -model_run)) %>%
    dplyr::mutate(
      year  = as.numeric(year),
      title = dplyr::case_when(
        name == "Direct_Flow_Rights"  ~  "COB Demand met by Direct Flow Rights",
        name == "Reservoir_Release"   ~  "COB Demand met by Reservoir Releases",
        name == "Direct_Exchange"     ~  "COB Demand met by Direct Exchange",
        name == "COB_Water_Demand"    ~  "COB Total Demand",
        name == "COB_Indoor_Demand"   ~  "COB Indoor Demand",
        name == "COB_Outdoor_Demand"  ~  "COB Outdoor Demand"
      ),
      ylabs_max = dplyr::case_when(
        name == "Direct_Flow_Rights"  ~  25000,
        name == "Reservoir_Release"   ~  25000,
        name == "Direct_Exchange"     ~  25000,
        name == "COB_Water_Demand"    ~  30000,
        name == "COB_Indoor_Demand"   ~  30000,
        name == "COB_Outdoor_Demand"  ~  30000
      )
    )

  return(mass_bal)

}

process_mass_balance_pipeline <- function(df) {

  # calculate mass balance by source
  mass_bal_pipe <-
    df %>%
    dplyr::mutate(
      dplyr::across(
        c(-model_run, -model_version, -model_id, -model_num,
          -extra_info, -year, -qm, -wyqm, -step, -start_date, -end_date),
        as.numeric)
    ) %>%
    dplyr::group_by(year, model_run) %>%
    dplyr::mutate(
      BoulderWTPDeliveryTemp = dplyr::across(c(Link_380_Flow, Link_520_Flow), mean, na.rm=T)
    ) %>%
    dplyr::summarise(
      LakewoodtoBetasso      = sum(Link_255_Flow),
      BarkerGravitytoBetasso = sum(Link_263_Flow),
      BoulderRestoWTP        = sum(BoulderWTPDeliveryTemp),
      FarmersRighttoWTP      = sum(Link_435_Flow)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(cols = c(-year, -model_run)) %>%
    dplyr::mutate(
      year  = as.numeric(year),
      title = dplyr::case_when(
        name == "LakewoodtoBetasso"       ~  "Lakewood Pipeline to Betasso WTP",
        name == "BarkerGravitytoBetasso"  ~  "Barker Gravity Line to Betasso WTP",
        name == "BoulderRestoWTP"         ~  "Boulder Reservoir to 63rd St WTP",
        name == "FarmersRighttoWTP"       ~  "Farmers Right to WTP"
      ),
      ylabs_max = dplyr::case_when(
        name == "LakewoodtoBetasso"       ~  15000,
        name == "BarkerGravitytoBetasso"  ~  15000,
        name == "BoulderRestoWTP"         ~  15000,
        name == "FarmersRighttoWTP"       ~  15000
      )
    )

  return(mass_bal_pipe)

}

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
# file_df is a row of a dataframe created from process_directory() function
process_output <- function(
    file_df,
    date_df,
    verbose = TRUE
) {

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
process_annual_summary <- function(
    file_df,
    date_df,
    verbose = TRUE
) {

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

process_isf <- function(
    isf_path,
    # model_ids,
    verbose = TRUE
) {

  # model_dirs$model_id
  # model_ids <- isf_df
  # isf_df
  # isf_path <- path_lst[[2]]$path
  # model_ids <- tolower(paste0(path_lst[[4]]$id, path_lst[[4]]$climate))

  # convert to lower to match clean names
  # model_ids <- tolower(model_ids)

  # Read in ISF year type CSV
  isf <- readr::read_csv(
    file           = isf_path,
    col_names      = T,
    show_col_types = FALSE
  ) %>%
    janitor::clean_names()
  # dplyr::select(wyqm, days_in_qm, dplyr::contains(model_ids))

  return(isf)

}

process_quota <- function(
    quota_path = NULL,
    model_ids  = NULL,
    start_year = 1900,
    end_year   = 2100,
    verbose    = TRUE
) {

  # model_ids <- tolower(paste0(path_lst[[4]]$id, path_lst[[4]]$climate))
  # quota_path =  dplyr::filter(model_dirs, grepl("Quota", file))$path
  # model_ids  = unique(na.omit(model_dirs$model_id))
  # start_year = 1900
  # end_year   = 2100
  # verbose    = TRUE
  # rm(end_year, start_year, verbose, quota_path, model_ids)

  # convert to lower to match clean names
  model_ids <- tolower(model_ids)

  quota     <- readr::read_csv(
    file           = quota_path,
    col_names      = T,
    show_col_types = FALSE
  ) %>%
    janitor::clean_names() %>%
    dplyr::select(year, dplyr::contains(model_ids)) %>%
    dplyr::filter(
      year >= start_year,
      year <= end_year
    ) %>%
    tidyr::pivot_longer(
      cols      = c(-year),
      names_to  = "model_run",
      values_to = "quota"
    ) %>%
    dplyr::arrange(model_run, year)
  # dplyr::mutate(
  #   model_run = factor(model_run, levels = c(model_ids))
  # )

  # sort IDs for plotting
  # rev(sort(stringr::str_extract(model_ids, "\\d{1}")) )

  return(quota)

}

get_cc_outputs <- function(
    base_mod_dir,
    comp_mod_dir,
    date_df,
    analysis_years = 4,
    title_size     = 10,
    xaxis_size     = 9,
    output_folder_name,
    scenario_name,
    save_path
) {

  # # Model versions
  # base_model_version        <-  paste0("v", base_mods$model_version)
  # compare_model_version     <-  paste0("v", comp_mods$model_version)
  # # version text strings
  # base_model_version_text    <- base_mods$model_version
  # compare_model_version_text <- comp_mods$model_version
  # # model IDs
  # base_model_ID        <- base_mods$model_id
  # compare_model_ID     <- comp_mods$model_id
  # # model ID suffix
  # base_model_ID_suffix    <- ""        # if this is not needed, keep blank ""
  # compare_model_ID_suffix <- ""        # if this is not needed, keep blank ""
  # # climate scenerios
  # base_climate         <- base_mods$model_num
  # compare_climate      <- comp_mods$model_num
  # # model prefixes
  # base_model_ID_prefix    <- base_mods$prefix
  # compare_model_ID_prefix <- comp_mods$prefix
  # # Extra info text that would be found between the model version and the model ID in the file name
  # base_model_extra_info    <- base_mods$extra_info
  # compare_model_extra_info <- comp_mods$extra_info
  # # no compact calls or compact calls data
  # base_model_ID_suffix    <- "nocc"        # no compact call (nocc)
  # compare_model_ID_suffix <- "cc"           # compact call ON (cc)

  # Folder to hold all outputs/plots from this script
  output_dir <- paste0(save_path, "/", output_folder_name)

  # check if directory already exists, if it doesn't create the directory
  if (!dir.exists(output_dir)) {


    message(paste0("Creating output folder:\n--->  ", output_dir))

    dir.create(output_dir)

  } else {

    message(paste0("Output folder already exists:\n--->  ", output_dir))

  }


  # set plot parameters
  # title_size = 10
  # xaxis_size = 9

  # number of years to analyze in ensemble model run (after the shut down year)
  # analysis_years  = 4

  # get the total number of model runs (compact call + no compact call)
  total_simulation_count <- nrow(comp_mods) * (analysis_years + 1) * 2

  # dataframe of start, end years to index
  year_subs <-
    data.frame(
      start = c(1919:2014)
    ) %>%
    dplyr::mutate(
      end   = start + analysis_years
    )

  if(length(comp_mods$scenario_name) != nrow(year_subs)){

    # message(paste0("Adding model_run column (with dummy model_runs"))

    # next model run after length of model runs
    model_run_len    <- length(comp_mods$scenario_name) + 1

    # create dummy model run names to fit year_subs dataframe
    dummy_model_runs <- paste0(substr(comp_mods$scenario_name, 0, (nchar(comp_mods$scenario_name)-2))[1],  model_run_len:nrow(year_subs))

    # join list of model runs w/ dummy model runs
    model_run_lst    <- c(comp_mods$scenario_name, dummy_model_runs)

    # add model run column
    year_subs <-
      year_subs %>%
      dplyr::mutate(
        model_run = model_run_lst
      )

  } else {

    # message(paste0("Adding model_run column"))

    # add model run
    year_subs <-
      year_subs %>%
      dplyr::mutate(
        model_run = comp_mods$scenario_name
      )

  }

  message(paste0("Loading NO comparison models..."))

  # Read in and preprocess base model output run OutputSheet (NOCC)
  base_output <- process_output(
    file_df = base_mods[1,],
    date_df = date_df,
    verbose = FALSE
  )

  # NO Compact calls data (nocc)
  # loop through NOCC/baseline dataset and filter each set into 4 year chunks
  nocc_df <- lapply(1:nrow(comp_mods), function(y) {

    message(paste0(y, "/", nrow(comp_mods), " - ", year_subs$model_run[y], " (", year_subs$start[y], " - ", year_subs$end[y], ")"))

    base_subset <-
      base_output %>%
      dplyr::filter(
        year >= year_subs$start[y],
        year <= year_subs$end[y]
      ) %>%
      dplyr::mutate(
        compact_call = base_mods$compact_call[1],
        model_run    = paste0(year_subs$model_run[y], "b"),
        model_group  = "no_compact_call"
      ) %>%
      dplyr::relocate(compact_call, model_run, model_group)

  }) %>%
    dplyr::bind_rows()

  # NO Compact calls data (nocc)
  # loop through NOCC/baseline dataset and filter each set into 4 year chunks
  # nocc_df <- lapply(1:nrow(comp_mods), function(y) {
  #
  #   message(paste0(y, "/", nrow(comp_mods)))
  #
  #   output <- process_output(
  #     file_df = base_mods[1,],
  #     date_df = date_df,
  #     verbose = FALSE
  #   ) %>%
  #     dplyr::filter(
  #       year >= year_subs$start[y],
  #       year <= year_subs$end[y]
  #     ) %>%
  #     dplyr::mutate(
  #       compact_call = base_mods$compact_call[1],
  #       model_run    = paste0(comp_mods$scenario_name[y], "b"),
  #       model_group  = "no_compact_call"
  #     ) %>%
  #     dplyr::relocate(compact_call, model_run, model_group)
  #
  # }) %>%
  #   dplyr::bind_rows()

  message(paste0("Loading compact call models..."))

  # Compact calls data (cc)
  # loop through compact calls datasets and filter each set into 4 year chunks
  cc_df <- lapply(1:nrow(comp_mods), function(y) {

    message(paste0(y, "/", nrow(comp_mods)))

    output <- process_output(
      file_df = comp_mods[y, ],
      date_df = date_df,
      verbose = FALSE
    ) %>%
      dplyr::filter(
        year >= year_subs$start[y],
        year <= year_subs$end[y]
      ) %>%
      dplyr::mutate(
        compact_call = comp_mods$compact_call[y],
        model_run    = comp_mods$scenario_name[y],
        model_group  = "compact_call"
      ) %>%
      dplyr::relocate(compact_call, model_run, model_group)

  }) %>%
    dplyr::bind_rows()

  # all model paths to create dictionary from
  all_paths <- c(base_mods$path, comp_mods$path)

  # retrieve lookup table from all Output sheets and keep the distinct rows (no duplicate definitions)
  definitions <-  lapply(1:length(all_paths), function(y) {

    message(paste0(y, "/", length(all_paths)))

    make_lookup(
      output_path = all_paths[y],
      verbose     = FALSE
    )
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct() %>%
    dplyr::bind_rows(cc_new_params())  # add additional CC parameters to definitions lookup table

  # merge all data together
  final_df <- dplyr::bind_rows(cc_df, nocc_df)

  # set the factor 'levels' to the correct plotting order
  final_df <-
    final_df %>%
    dplyr::mutate(
      order_val = dplyr::case_when(
        model_group == "compact_call"    ~ 1,
        model_group == "no_compact_call" ~ 2
      )
    ) %>%
    dplyr::group_by(model_group) %>%
    dplyr::arrange(-order_val, .by_group = F) %>%
    dplyr::ungroup() %>%
    dplyr::select(-order_val) %>%
    dplyr::mutate(
      model_run = factor(model_run)
      # model_run = factor(model_run, levels = c(unique(.$model_run)))
    )

  message(paste0("Generating plots..."))

  # ********************************
  # ---- 1A. May 1 Res Contents ----
  # ********************************

  # select the sites you want to plot
  p1a_site_selection <- c("year", "qm", "start_date", "model_run", "model_group", "Reservoir_3_Content", "Reservoir_1_Content")

  # subset dataset
  p1a_df <-
    final_df %>%
    dplyr::select(p1a_site_selection) %>%
    dplyr::mutate(
      dplyr::across(c(-year, -qm, -start_date, -model_run, -model_group), as.numeric)
    ) %>%
    dplyr::mutate(
      Total_Upper_Storage = Reservoir_3_Content + Reservoir_1_Content
      # Total_Upper_Storage = rowSums(dplyr::across(c(Reservoir_3_Content, Reservoir_1_Content)))
    )

  # filter, pivot to long format, and join the CRAM model descriptions to this dataset
  p1a_df <-
    p1a_df %>%
    dplyr::filter(qm == 29) %>%
    dplyr::group_by(year, model_run) %>%
    tidyr::pivot_longer(
      # cols      = all_of(site_selection_short),
      cols      = c(-year, -qm, -start_date, -model_run, -model_group),
      names_to  = "Name",
      values_to = "Output") %>%
    dplyr::left_join(
      definitions,
      by = "Name"
    ) %>%
    dplyr::ungroup()

  # unique names to use to make plots
  p1a_uplots <- unique(p1a_df$Name)

  # empty list to store P1A plots
  p1a_lst <- list()

  # loop over unique water sources and make plots
  for(z in 1:length(p1a_uplots)) {

    # message(paste0(z, "/", length(p1a_uplots), " - (", p1a_uplots[z], ")"))

    # ECDF data to plot
    extract_df <-
      p1a_df %>%
      dplyr::filter(Name == p1a_uplots[z])

    # ECDF plot
    extract_plot <- make_ecdf_point_plot(
      df               = extract_df,
      plot_title       = extract_df$Description[z],
      xaxis_title      = extract_df$Units[z],
      plot_title_size  = title_size,
      xaxis_title_size = xaxis_size
    )

    # add plot to list of plots
    p1a_lst[[z]] <- extract_plot

  }

  message(paste0("Plot 1A"))

  # define the plot name
  p1a_plot_title <- paste0("1a. May 1 Reservoir Contents (point)", " (", scenario_name, ")")
  p1a_file_name  <- paste(p1a_plot_title,  " 2x2", sep = "")

  # save the plot
  ggplot2::ggsave(
    # paste0(output_folder_name, "/", file_name, " ", model_version, ".pdf"),
    filename = paste0(output_dir, "/", p1a_file_name, ".pdf"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      p1a_lst[[1]], p1a_lst[[2]], p1a_lst[[3]],
      nrow  = 2,
      top   = p1a_plot_title,
      right = ""
    )
  )

  # ***************************************
  # ---- 1D May 1 Drought Metrics Plot ----
  # ***************************************

  # select the sites you want to plot
  p1d_site_selection <- c("year", "qm", "start_date", "model_run", "model_group", "DataObject_15_Flow", "DataObject_12_Flow", "DataObject_14_Flow")
  # p1d_site_selection <- c("year", "qm", "start_date", "model_run", "model_group", "DataObject_15_Flow", "DataObject_12_Flow", "DataObject_14_Flow", "DataObject_39_Flow")

  # subset dataset
  p1d_df <-
    final_df %>%
    dplyr::select(p1d_site_selection) %>%
    dplyr::mutate(
      dplyr::across(c(-year, -qm, -start_date, -model_run, -model_group), as.numeric)
    ) %>%
    dplyr::mutate(
      DataObject_15_Flow = DataObject_15_Flow/100
    ) %>%
    dplyr::filter(qm == 29) %>%
    dplyr::group_by(year, model_run) %>%
    tidyr::pivot_longer(
      cols      = c(-year, -qm, -start_date, -model_run, -model_group),
      names_to  = "Name",
      values_to = "Output") %>%
    dplyr::left_join(
      definitions,
      by = "Name"
    ) %>%
    dplyr::ungroup()

  # unique names to use to make plots
  p1d_uplots <- unique(p1d_df$Name)

  # empty list to store P1A plots
  p1d_lst <- list()

  # loop over unique water sources and make plots
  for(z in 1:length(p1d_uplots)) {

    # message(paste0(z, "/", length(p1d_uplots), " - (", p1d_uplots[z], ")"))

    # ECDF data to plot
    extract_df <-
      p1d_df %>%
      dplyr::filter(Name == p1d_uplots[z])

    # ECDF plot
    extract_plot <- make_ecdf_step_plot(
      df               = extract_df,
      plot_title       = extract_df$Description[z],
      xaxis_title      = extract_df$Units[z],
      plot_title_size  = title_size,
      xaxis_title_size = xaxis_size
    )

    # add plot to list of plots
    p1d_lst[[z]] <- extract_plot

  }

  message(paste0("Plot 1D"))

  # define the plot name
  p1d_plot_title <- paste0("1d. May 1 Drought Metrics", " (", scenario_name, ")")
  p1d_file_name <- paste(p1d_plot_title, " 2x2", sep = "")

  # save the plot
  ggplot2::ggsave(
    filename = paste0(output_dir, "/", p1d_file_name, ".pdf"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      p1d_lst[[1]], p1d_lst[[2]], p1d_lst[[3]],
      nrow  = 2,
      top   = p1d_plot_title,
      right = ""
    )
  )

  # ***********************************
  # ---- 1C May 1 Drought Triggers ----
  # ***********************************

  t1c_tbl <-
    p1d_df %>%
    dplyr::filter(Name == "DataObject_12_Flow") %>%
    dplyr::group_by(model_group) %>%
    dplyr::count(Output) %>%
    tidyr::pivot_wider(
      names_from  = model_group,
      values_from = n
    ) %>%
    dplyr::rename("Drought Response Level" = Output) %>%
    gt::gt() %>%
    gt::tab_header(
      title    = gt::md("Comparison of Drought Response Triggers"),
      subtitle = gt::md(output_folder_name)
    )

  # define the table export name & save as pdf
  t1c_plot_title <- "1c. May 1 Drought Triggers"
  t1c_file_name  <- paste(t1c_plot_title, " (", scenario_name, ")", sep = "")

  message(paste0("Table 1C"))

  # save 1C1 table
  gt::gtsave(
    data = t1c_tbl,
    file = paste0(output_dir, "/", t1c_file_name, ".pdf"),
    zoom = 1
  )

  # *********************************************
  # ---- 1C2 May 1 Drought Triggers Summary -----
  # *********************************************

  # CC Temp data count
  cc_temp <-
    p1d_df %>%
    dplyr::filter(Name == "DataObject_12_Flow") %>%
    dplyr::filter(Output == 0) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(model_group) %>%
    dplyr::count(Output) %>%
    dplyr::ungroup() %>%
    dplyr::filter(model_group == "compact_call") %>%
    .$n

  # No CC Temp data count
  nocc_temp <-
    p1d_df %>%
    dplyr::filter(Name == "DataObject_12_Flow") %>%
    dplyr::filter(Output == 0) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(model_group) %>%
    dplyr::count(Output) %>%
    dplyr::ungroup() %>%
    dplyr::filter(model_group == "no_compact_call") %>%
    .$n

  t1c2_tbl <-
    dplyr::tibble(
      Count             = c("Total Simulation", "No Drought Trigger", "No Drought Trigger (%)"),
      'compact call'    = c(total_simulation_count, cc_temp, round(cc_temp/total_simulation_count*100,1)),
      'no compact call' = c(total_simulation_count, nocc_temp, round(nocc_temp/total_simulation_count*100,1))
    ) %>%
    gt::gt() %>%
    gt::tab_header(
      title    = gt::md("No Drought Trigger Summary"),
      subtitle = gt::md(output_folder_name)
    )

  message(paste0("Table 1C2"))

  # define the table export name & save as pdf
  t1c2_plot_title <- "1c2. May 1 Drought Triggers Summary"
  t1c2_file_name <- paste(t1c2_plot_title, " (", scenario_name, ")", sep = "")

  # save 1C1 table
  gt::gtsave(
    data = t1c2_tbl,
    file = paste0(output_dir, "/", t1c2_file_name, ".pdf"),
    zoom = 1
  )

  # *********************************
  # ---- 1B June 1 Res Contents -----
  # *********************************

  # select the sites you want to plot
  p1b_site_selection <- c("year", "qm", "start_date", "model_run", "model_group", "Reservoir_3_Content", "Reservoir_1_Content")

  # subset dataset
  p1b_df <-
    final_df %>%
    dplyr::select(p1b_site_selection) %>%
    dplyr::mutate(
      dplyr::across(c(-year, -qm, -start_date, -model_run, -model_group), as.numeric)
    ) %>%
    dplyr::mutate(
      Total_Upper_Storage = Reservoir_3_Content + Reservoir_1_Content
      # Total_Upper_Storage = rowSums(dplyr::across(c(Reservoir_3_Content, Reservoir_1_Content)))
    )

  # filter, pivot to long format, and join the CRAM model descriptions to this dataset
  p1b_df <-
    p1b_df %>%
    dplyr::filter(qm == 33) %>%
    dplyr::group_by(year, model_run) %>%
    tidyr::pivot_longer(
      # cols      = all_of(site_selection_short),
      cols      = c(-year, -qm, -start_date, -model_run, -model_group),
      names_to  = "Name",
      values_to = "Output") %>%
    dplyr::left_join(
      definitions,
      by = "Name"
    ) %>%
    dplyr::ungroup()

  # unique names to use to make plots
  p1b_uplots <- unique(p1b_df$Name)

  # empty list to store P1A plots
  p1b_lst <- list()
  # rm(z,xaxis_title_size,xaxis_title, df, extract_df,plot_title_size, extract_plot, p1a_lst)

  # loop over unique water sources and make plots
  for(z in 1:length(p1b_uplots)) {

    # message(paste0(z, "/", length(p1b_uplots), " - (", p1b_uplots[z], ")"))

    # ECDF data to plot
    extract_df <-
      p1b_df %>%
      dplyr::filter(Name == p1b_uplots[z])

    # ECDF plot
    extract_plot <- make_ecdf_step_plot(
      df               = extract_df,
      plot_title       = extract_df$Description[z],
      xaxis_title      = extract_df$Units[z],
      plot_title_size  = title_size,
      xaxis_title_size = xaxis_size,
      quantile_breaks  = FALSE
    )

    # add plot to list of plots
    p1b_lst[[z]] <- extract_plot

  }

  message(paste0("Plot 1B"))

  # define the plot name
  p1b_plot_title <- paste0("1b. June 1 Reservoir Contents", " (", scenario_name, ")")
  p1b_file_name <- paste(p1b_plot_title, " 2x2", sep = "")

  # save the plot
  ggplot2::ggsave(
    filename = paste0(output_dir, "/", p1b_file_name, ".pdf"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      p1b_lst[[1]], p1b_lst[[2]], p1b_lst[[3]],
      nrow  = 2,
      top   = p1b_plot_title,
      right = ""
    )
  )

  # **********************************************
  # ---- 1E. Annual City Demand and Shortage -----
  # **********************************************

  # plot 1E sites of interest
  p1e_site_selection <- c("year", "qm", "start_date", "model_run", "model_group",
                          "DataObject_23_Flow", "DataObject_19_Flow", "DataObject_20_Flow")

  # subset dataset
  p1e_df <-
    final_df %>%
    dplyr::select(p1e_site_selection) %>%
    dplyr::mutate(
      dplyr::across(c(-year, -qm, -start_date, -model_run, -model_group), as.numeric)
    ) %>%
    # dplyr::group_by(year, model_run) %>%
    dplyr::group_by(year, model_run, model_group) %>%
    dplyr::summarise(dplyr::across(DataObject_23_Flow:DataObject_20_Flow, sum)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(
      cols      = c(-year, -model_run, -model_group),
      names_to  = "Name",
      values_to = "Output"
    ) %>%
    dplyr::left_join(
      definitions,
      by = "Name"
    )

  # unique names to use to make plots
  p1e_uplots <- unique(p1e_df$Name)

  # empty list to store 1E plots
  p1e_lst <- list()
  # rm(z,xaxis_title_size,xaxis_title, df, extract_df,plot_title_size, extract_plot, p1a_lst)

  # loop over unique water sources and make plots
  for(z in 1:length(p1e_uplots)) {

    # message(paste0(z, "/", length(p1e_uplots), " - (", p1e_uplots[z], ")"))

    # ECDF data to plot
    extract_df <-
      p1e_df %>%
      dplyr::filter(Name == p1e_uplots[z])

    # ECDF plot
    extract_plot <- make_ecdf_step_plot(
      df               = extract_df,
      plot_title       = extract_df$Description[z],
      xaxis_title      = extract_df$Units[z],
      plot_title_size  = title_size,
      xaxis_title_size = xaxis_size,
      quantile_breaks  = FALSE
    )

    # add plot to list of plots
    p1e_lst[[z]] <- extract_plot

  }

  # define the plot name
  p1e_plot_title <- paste0("1e. Annual City Demand and Shortage", " (", scenario_name, ")")
  p1e_file_name <- paste(p1e_plot_title, " 2x2", sep = "")

  message(paste0("Plot 1E"))

  # save the plot
  ggplot2::ggsave(
    filename = paste0(output_dir, "/", p1e_file_name, ".pdf"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      p1e_lst[[1]], p1e_lst[[2]], p1e_lst[[3]],
      nrow  = 2,
      top   = p1e_plot_title,
      right = ""
    )
  )


  # *******************************
  # ---- 2A. Panama Reservoir -----
  # *******************************
  # plot 2a sites of interest
  p2a_site_selection <- c("year", "qm", "start_date", "model_run", "model_group", "Reservoir_25_Content")

  # subset dataset
  p2a_df <-
    final_df %>%
    dplyr::select(p2a_site_selection) %>%
    dplyr::mutate(
      dplyr::across(c(-year, -qm, -start_date, -model_run, -model_group), as.numeric)
    ) %>%
    # dplyr::group_by(year, model_run) %>%
    dplyr::group_by(year, model_run, model_group) %>%
    dplyr::summarise(
      COB_Panama_qm_max_contents = max(Reservoir_25_Content),
      COB_Panama_qm_avg_contents = mean(Reservoir_25_Content),
      COB_Panama_qm_min_contents = min(Reservoir_25_Content)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(
      cols      = c(-year, -model_run, -model_group),
      names_to  = "Name",
      values_to = "Output"
    ) %>%
    dplyr::left_join(
      definitions,
      by = "Name"
    )

  # unique names to use to make plots
  p2a_uplots <- unique(p2a_df$Name)

  # empty list to store 1E plots
  p2a_lst <- list()
  # rm(z,xaxis_title_size,xaxis_title, df, extract_df,plot_title_size, extract_plot, p1a_lst)

  # loop over unique water sources and make plots
  for(z in 1:length(p2a_uplots)) {

    # message(paste0(z, "/", length(p2a_uplots), " - (", p2a_uplots[z], ")"))

    # ECDF data to plot
    extract_df <-
      p2a_df %>%
      dplyr::filter(Name == p2a_uplots[z])

    # ECDF plot
    extract_plot <- make_ecdf_step_plot(
      df               = extract_df,
      plot_title       = extract_df$Description[z],
      xaxis_title      = extract_df$Units[z],
      plot_title_size  = title_size,
      xaxis_title_size = xaxis_size,
      quantile_breaks  = FALSE
    )

    # add plot to list of plots
    p2a_lst[[z]] <- extract_plot

  }

  # define the plot name
  p2a_plot_title <- paste0("2a. Panama Reservoir Contents", " (", scenario_name, ")")
  p2a_file_name <- paste(p2a_plot_title, " 2x2", sep = "")

  message(paste0("Plot 2A"))

  # save the plot
  ggplot2::ggsave(
    filename = paste0(output_dir, "/", p2a_file_name, ".pdf"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      p2a_lst[[1]], p2a_lst[[2]], p2a_lst[[3]],
      nrow  = 2,
      top   = p2a_plot_title,
      right = ""
    )
  )

  # *************************************
  # ---- 2B. Wittemyer Res Contents -----
  # *************************************

  p2b_site_selection <- c("year", "qm", "start_date", "model_run", "model_group", "Reservoir_13_Content")

  # subset dataset
  p2b_df <-
    final_df %>%
    dplyr::select(p2b_site_selection) %>%
    dplyr::mutate(
      dplyr::across(c(-year, -qm, -start_date, -model_run, -model_group), as.numeric)
    ) %>%
    # dplyr::group_by(year, model_run) %>%
    dplyr::group_by(year, model_run, model_group) %>%
    dplyr::summarise(
      COB_Wittemyer_qm_max_contents = max(Reservoir_13_Content),
      COB_Wittemyer_qm_avg_contents = mean(Reservoir_13_Content),
      COB_Wittemyer_qm_min_contents = min(Reservoir_13_Content)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(
      cols      = c(-year, -model_run, -model_group),
      names_to  = "Name",
      values_to = "Output"
    ) %>%
    dplyr::left_join(
      definitions,
      by = "Name"
    )

  # unique names to use to make plots
  p2b_uplots <- unique(p2b_df$Name)

  # empty list to store 1E plots
  p2b_lst <- list()
  # rm(z,xaxis_title_size,xaxis_title, df, extract_df,plot_title_size, extract_plot, p1a_lst)

  # loop over unique water sources and make plots
  for(z in 1:length(p2b_uplots)) {

    # message(paste0(z, "/", length(p2b_uplots), " - (", p2b_uplots[z], ")"))

    # ECDF data to plot
    extract_df <-
      p2b_df %>%
      dplyr::filter(Name == p2b_uplots[z])

    # ECDF plot
    extract_plot <- make_ecdf_step_plot(
      df               = extract_df,
      plot_title       = extract_df$Description[z],
      xaxis_title      = extract_df$Units[z],
      plot_title_size  = title_size,
      xaxis_title_size = xaxis_size,
      quantile_breaks  = FALSE
    )

    # add plot to list of plots
    p2b_lst[[z]] <- extract_plot

  }

  # define the plot name
  p2b_plot_title  <- paste0("2b. Wittemyer Reservoir Contents", " (", scenario_name, ")")
  p2b_file_name   <- paste(p2b_plot_title, " 2x2", sep = "")

  message(paste0("Plot 2B"))

  # save the plot
  ggplot2::ggsave(
    filename = paste0(output_dir, "/", p2b_file_name, ".pdf"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      p2b_lst[[1]], p2b_lst[[2]], p2b_lst[[3]],
      nrow  = 2,
      top   = p2b_plot_title,
      right = ""
    )
  )

  # ******************************************
  # ---- 2C. Wittemyer + Panama Contents -----
  # ******************************************

  p2c_site_selection <- c("year", "qm", "start_date", "model_run", "model_group", "Reservoir_13_Content", "Reservoir_25_Content")

  # subset dataset
  p2c_df <-
    final_df %>%
    dplyr::select(p2c_site_selection) %>%
    dplyr::mutate(
      dplyr::across(c(-year, -qm, -start_date, -model_run, -model_group), as.numeric)
    ) %>%
    dplyr::mutate(
      Wittemyer_Panama_Storage = Reservoir_13_Content + Reservoir_25_Content
      # Wittemyer_Panama_Storage = rowSums(across(c(Reservoir_13_Content, Reservoir_25_Content)))
    ) %>%
    # dplyr::group_by(year, model_run) %>%
    dplyr::group_by(year, model_run, model_group) %>%
    dplyr::summarise(
      "Panama+Wittemyer_qm_max_contents" = max(Wittemyer_Panama_Storage),
      "Panama+Wittemyer_qm_avg_contents" = mean(Wittemyer_Panama_Storage),
      "Panama+Wittemyer_qm_min_contents" = min(Wittemyer_Panama_Storage)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(
      cols      = c(-year, -model_run, -model_group),
      names_to  = "Name",
      values_to = "Output"
    ) %>%
    dplyr::left_join(
      definitions,
      by = "Name"
    )

  # unique names to use to make plots
  p2c_uplots <- unique(p2c_df$Name)

  # empty list to store 1E plots
  p2c_lst <- list()
  # rm(z,xaxis_title_size,xaxis_title, df, extract_df,plot_title_size, extract_plot, p1a_lst)

  # loop over unique water sources and make plots
  for(z in 1:length(p2c_uplots)) {

    # message(paste0(z, "/", length(p2c_uplots), " - (", p2c_uplots[z], ")"))

    # ECDF data to plot
    extract_df <-
      p2c_df %>%
      dplyr::filter(Name == p2c_uplots[z])

    # ECDF plot
    extract_plot <- make_ecdf_step_plot(
      df               = extract_df,
      plot_title       = extract_df$Description[z],
      xaxis_title      = extract_df$Units[z],
      plot_title_size  = title_size,
      xaxis_title_size = xaxis_size,
      quantile_breaks  = FALSE
    )

    # add plot to list of plots
    p2c_lst[[z]] <- extract_plot

  }

  # define the plot name
  p2c_plot_title  <- paste0("2c. Panama + Wittemyer Reservoir Contents", " (", scenario_name, ")")
  p2c_file_name   <- paste(p2c_plot_title, " 2x2", sep = "")

  message(paste0("Plot 2C"))

  # save the plot
  ggplot2::ggsave(
    filename = paste0(output_dir, "/", p2c_file_name, ".pdf"),
    width    = 14,
    height   = 8,
    gridExtra::grid.arrange(
      p2c_lst[[1]], p2c_lst[[2]], p2c_lst[[3]],
      nrow  = 2,
      top   = p2c_plot_title,
      right = ""
    )
  )
}

# ***********************************************************************
# ***********************************************************************

