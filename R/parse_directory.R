#' Parse COB CRAM model files from directory
#' @param base_folder highest level directory housing all the model folders
#' @return dataframe with parsed file names and paths
#' @importFrom dplyr mutate relocate
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

  # parse through file names
  dir_df <-
    dir_df %>%
    dplyr::mutate(
      base_folder    = base_folder,
      start          = stringr::str_match(file, '([^.]+)(?:.[^.]+){3}$')[,2],
      climate        = sub('.*(\\d{4}).*', '\\1', file),
      model_version  = stringr::str_match(file, '([^.]+)(?:.[^.]+){2}$')[,2],
      id             = stringr::str_extract(file, "(ID)+([0-9]+)"),
      id             = dplyr::case_when(
        is.na(id) ~ model_version,
        TRUE      ~ id
      ),
      output         = stringr::str_match(
                          stringr::str_extract(file,  "\\.[^_]+$"),
                          '([^.]+)(?:.[^.]+){2}$')[,2],
      output         = dplyr::case_when(
        is.na(output) ~ model_version,
        TRUE          ~ output
      ),
      path = paste0(base_folder, "/", file)
    ) %>%
    dplyr::relocate(start, model_version, output, climate, id, base_folder, file, path)

  return(dir_df)

  # stringr::str_match(string, '([^-]+)(?:-[^-]+){3}$')[,2]
  # ext            = gsub("\\.", "", stringr::str_extract(x, "\\.[^_]+$"))
}

