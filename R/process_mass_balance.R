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

