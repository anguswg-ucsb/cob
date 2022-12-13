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
