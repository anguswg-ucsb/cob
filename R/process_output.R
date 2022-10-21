process_output <- function(
    file_df,
    date_df,
    verbose = TRUE
) {
  # output_path <- path_lst[[4]]$path[1]
  # output_path <- path_lst[[6]]$path[2]
  # model_ver   <- path_lst[[6]]$model_version[2]
  # output_path = path_df$path[y]
  # model_ver   = path_df$model_version[y]
  # model_id    = path_df$model_id[y]
  # date_df     = date_convert
  # verbose     = TRUE

  # file_df <- path_df[y, ]
  # y = 3
  # file_df     = path_df[y, ]
  # date_df     = date_convert
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
      wyqm           = paste(year, qm, sep = '-'),
      start_date     = as.Date(start_date, format="%m/%d/%Y", tz = "UTC"),
      end_date       = as.Date(end_date, format="%m/%d/%Y", tz = "UTC")
      ) %>%
    dplyr::relocate(model_version, model_id, model_num, year, qm, wyqm, step, start_date, end_date)

  return(out)

  # out[name_row, ]
  # descriptions for data in out
  # info_df      <- out[1:name_index, 1:ncol(out)]
  #
  # # locate row that contains descriptions
  # major_index    <- grep("Major", out$X1)
  #
  # # extract rows with the column names, rows from start of dataframe to one before the row that contains "Major"
  # var_names      <- out[1:(major_index-1), ]
  #
  # # verbose = TRUE
  # if(verbose == TRUE) {
  #   message(paste0("Extracting variable names... "))
  # }
  #
  # # loop over each column and concatenate all the rows of the column into a single string, to use as column headers
  # var_lst <- sapply(1:ncol(var_names), function(i){
  #
  #   if(verbose == TRUE) {
  #     message(paste0(i, "/", ncol(var_names)))
  #   }
  #
  #   vname <-
  #     var_names[,i] %>%
  #     as.vector() %>%
  #     unlist() %>%
  #     paste0(collapse = "_")
  #   vname
  #
  # })

  # get the column name & column descriptions
  # column_names <- data[5, ]
  # column_parameter <- data[4, ]
  # column_descriptions <- data[3, ]

  # locate row that contains parameters
  # param_index  <- grep("Time", out_tbl_names$X1)
  #
  # # locate row that contains name
  # name_index   <- grep("Step", out_tbl_names$X1)
  #
  # # descriptions for data in out
  # tbl_desc     <- out_tbl_names[desc_index, 1:ncol(out)]
  # desc_vect    <- unlist(as.vector(tbl_desc[1, ]))

}
