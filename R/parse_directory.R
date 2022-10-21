#' Parse COB CRAM model files from directory
#' @param base_folder highest level directory housing all the model folders
#' @return dataframe with parsed file names and paths
#' @importFrom dplyr mutate relocate case_when
#' @import tidyr separate
#' @importFrom stringr str_match str_extract
#' @export
parse_directory = function(
    base_folder = NULL
    ){

  # stop function if no base_folder path is given
  if(is.null(base_folder)) {

    stop(paste0("Invalid 'base_folder'\nPlease give a path to folder containing model output sheets"))

  }

  # make dataframe of files in directory
  dir_df <- data.frame(
    file = list.files(base_folder, pattern = ".csv")
  )
  # "[^._]+(?:_[^._]+){2}(?=_[^.]*$)"
  # z <- dir_df$file[18]
  # z
  # sub('.*(\\d{4}).*', '\\1', z)
  # str <- gsub(
  #   "\\.",
  #   "_",
  #   gsub( ".*_(.*)\\.csv", "\\1", file)
  #   )
  #
  # stringr::str_extract(
  #   gsub(
  #     "\\.",
  #     "_",
  #     gsub( ".*_(.*)\\.csv", "\\1", file)
  #     ),
  #   "^[^_]+(?=_)"
  #   )
  #
  # dir_df$file[1]
  # z <- dir_df$file[1]
  dir_df <-
    dir_df %>%
    dplyr::mutate(
        parsed = gsub("^[^_]+_|_[^_]+$", "", file)
    ) %>%
    tidyr::separate(
      parsed,
      into  = c('name', 'year', 'model_version', 'model_id'),
      sep   = "_",
      extra = "drop",
      fill  = "right"
    ) %>%
    dplyr::mutate(
      base_folder    = base_folder,
      output         = stringr::str_match(
        stringr::str_extract(file,  "\\.[^_]+$"), '([^.]+)(?:.[^.]+){2}$')[,2],
      output         = dplyr::case_when(
        is.na(output) ~ name,
        TRUE          ~ output
      ),
      model_num        = stringr::str_extract(
                                  gsub(
                                    "\\.",
                                    "_",
                                    gsub( ".*_(.*)\\.csv", "\\1", file)
                                  ),
                                  "^[^_]+(?=_)"
                                ),
      plot_id          = paste0(model_id, model_num),
      path = paste0(base_folder, "/", file)
    ) %>%
    dplyr::relocate(name, year, model_version, model_id, model_num, output, plot_id, base_folder, file, path)

  return(dir_df)

  # str <- dir_df$file[1]
  # sub(".*DroughtPlan\\D*(\\d+).*", "\\1", str)
  # year <- sub(".*DroughtPlan\\D*(\\d+).*", "\\1", str)
  # strsplit(gsub("^[^_]+_|_[^_]+$", "", str), "_")

  # dir_df <-
  #   dir_df %>%
  #   dplyr::mutate(
  #     base_folder    = base_folder,
  #     year = sub(".*DroughtPlan\\D*(\\d+).*", "\\1", file),
  #     run  = sub(
  #       paste0(regex(".*"),  "DroughtPlan_", year, regex("\\D*(\\d+).*")),
  #       "\\1",
  #       file
  #     ),
  #     # start          = stringr::str_match(file, '([^.]+)(?:.[^.]+){3}$')[,2],
  #     climate        = sub('.*(\\d{4}).*', '\\1', file),
  #     model_version  = stringr::str_match(file, '([^.]+)(?:.[^.]+){2}$')[,2],
  #     id             = stringr::str_extract(file, "(ID)+([0-9]+)"),
  #     id             = dplyr::case_when(
  #       is.na(id) ~ model_version,
  #       TRUE      ~ id
  #     ),
  #     output         = stringr::str_match(
  #       stringr::str_extract(file,  "\\.[^_]+$"),
  #       '([^.]+)(?:.[^.]+){2}$')[,2],
  #     output         = dplyr::case_when(
  #       is.na(output) ~ model_version,
  #       TRUE          ~ output
  #     ),
  #     path = paste0(base_folder, "/", file)
  #   )
  # # run <-
  # #   sub(
  # #   paste0(regex(".*"),  "DroughtPlan_", year, regex("\\D*(\\d+).*")),
  # #   "\\1",
  # #   str
  # #   )
  # # parse through file names
  # dir_df <-
  #   dir_df %>%
  #   dplyr::mutate(
  #     base_folder    = base_folder,
  #     year = sub(".*DroughtPlan\\D*(\\d+).*", "\\1", file),
  #     run  = sub(
  #       paste0(regex(".*"),  "DroughtPlan_", year, regex("\\D*(\\d+).*")),
  #       "\\1",
  #       file
  #     ),
  #     # start          = stringr::str_match(file, '([^.]+)(?:.[^.]+){3}$')[,2],
  #     climate        = sub('.*(\\d{4}).*', '\\1', file),
  #     model_version  = stringr::str_match(file, '([^.]+)(?:.[^.]+){2}$')[,2],
  #     id             = stringr::str_extract(file, "(ID)+([0-9]+)"),
  #     id             = dplyr::case_when(
  #       is.na(id) ~ model_version,
  #       TRUE      ~ id
  #     ),
  #     output         = stringr::str_match(
  #                         stringr::str_extract(file,  "\\.[^_]+$"),
  #                         '([^.]+)(?:.[^.]+){2}$')[,2],
  #     output         = dplyr::case_when(
  #       is.na(output) ~ model_version,
  #       TRUE          ~ output
  #     ),
  #     path = paste0(base_folder, "/", file)
  #   ) %>%
  #   dplyr::relocate(start, model_version, output, climate, id, base_folder, file, path)
  #
  # return(dir_df)

  # stringr::str_match(string, '([^-]+)(?:-[^-]+){3}$')[,2]
  # ext            = gsub("\\.", "", stringr::str_extract(x, "\\.[^_]+$"))
}

