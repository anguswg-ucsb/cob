

make_lookup <- function(
    output_path,
    verbose = TRUE
) {

  if(verbose == TRUE) {
    message(paste0("Creating lookup table...",
                   "\nFilename: ", basename(output_path))
    )
  }

  # Read in OutputSheet CSV
  out <- readr::read_csv(
    file           = output_path,
    col_names      = FALSE,
    show_col_types = FALSE
  )

  # create a lookup for names, definitions, descriptions
  definitions <- data.frame(
    Name        = t(out[5, ]),
    Description = t(out[3, ]),
    Parameter   = t(out[4, ])
    )

  # n_definitions <- dim(definitions)

  # definitions <- definitions[4:n_definitions[1], ]

  # make a look up for parameter units
  units_df <- data.frame(
    Parameter = c("Flow", "High", "Content", "Shortage", "Priority", "Evaporation", "Low"),
    Units     = c("Flow (af)", "Flow (af)", "Contents (af)", "Flow (cfs)", "Value", "Flow (af)", "Flow (af)")
    )

  # bind the units to the definitions table
  definitions <-
    definitions %>%
    dplyr::left_join(
      units_df,
      by = "Parameter"
      ) %>%
    tibble::tibble()


  return(definitions)

}

# New compact call parameters to add to lookup table
cc_new_params <- function() {

  # add new parameters here
  new_data <-
    data.frame(
      Name        = c(
              "Total_Upper_Storage", "COB_Panama_qm_max_contents",
              "COB_Panama_qm_avg_contents", "COB_Panama_qm_min_contents",
              "COB_Wittemyer_qm_max_contents", "COB_Wittemyer_qm_avg_contents",
              "COB_Wittemyer_qm_min_contents",
              "Panama+Wittemyer_qm_max_contents",
              "Panama+Wittemyer_qm_avg_contents", "Panama+Wittemyer_qm_min_contents"
          ),
      Description = c(
              "Barker+NBC Reservoirs", "Max COB Space in Panama Res",
              "Avg COB Space in Panama Res", "Min COB Space in Panama Res",
              "Max COB Space in Wittemyer Res",
              "Avg COB Space in Wittemyer Res", "Min COB Space in Wittemyer Res",
              "Max COB Panama+Wittemyer Res",
              "Avg COB Panama+Wittemyer Res", "Min COB Panama+Wittemyer Res"
              ),
      Parameter   = rep("Content", 10,),
      Units       = rep("Contents (af)", 10)
  )

  return(new_data)

}

cbt_new_params <- function() {
  def_df <- data.frame(
    Name       = c("Decree75_QM1_24", "Decree75_QM25_48", "COB_CBT_allotment",
                   "COB_CBT_TotalUse", "COB_CBT_Unused", "COB_CBT_NormalUse",
                   "COB_CBT_BorrowedWinter", "COB_CBT_YeartoYearDebt"
    ),
    Description = c("COB CBT use QM1-24",  "COB CBT use QM25-48",  "Annual CBT Allotment (af)",
                    "COB Total CBT Water Used",  "COB Unused CBT Water",  "COB normal use of CBT water",
                    "COB borrowed CBT winter water", "COB CBT debt water"),
    Parameter   = c(rep("Flow", 8)),
    Units       = c(rep("Flow (af)", 8))
  )

  return(def_df)

}
