make_lookup <- function(
    output_path,
    verbose = TRUE
) {
  # output_path <- path_lst[[4]]$path[1]
  # output_path <- path_lst[[6]]$path[2]
  # model_ver   <- path_lst[[6]]$model_version[2]
  # output_path <- path_df$path[4]

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
    Units     = c("Flow (af)", "Flow (af)", "Contents (af)", "Flow (cfs)", "Value", "Flow (af)", "Flow af)")
    )

  # bind the units to the definitions table
  definitions <-
    definitions %>%
    dplyr::left_join(
      units_df,
      by = "Parameter"
      )


  return(definitions)

}
