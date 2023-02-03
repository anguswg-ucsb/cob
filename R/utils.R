# save out plots
save_plot <- function(
    plot_lst,
    base_dir,
    plot_name,
    width  = 14,
    height = 8,
    nrows
) {

  # length of plot list
  x = length(plot_lst)

  # calculate number of columns
  ncols = x/nrows

  # save plot out
  ggplot2::ggsave(
    filename = paste0(base_dir, "/", plot_name, " ", nrows, "x", ncols, ".png"),
    width    = width,
    height   = height,
    gridExtra::grid.arrange(
      grobs = plot_lst,
      nrow   = nrows,
      top    = plot_name,
      right  = ""
    )
  )

}

# remove rows w/ only NAs
rm_na_rows <- function(df) {

  new_df <- df[rowSums(is.na(df)) < ncol(df), ]

  return(new_df)
}

# remove colums w/ only NAs
rm_na_cols <- function(df) {

  new_df <- df[ , colSums(is.na(df)) < nrow(df)]  # Remove columns with NA only

  return(new_df)
}

round_by <- function(
    x,
    by,
    buffer = NULL
) {

  # if no buffer is given (NULL), make buffer 0
  if(is.null(buffer)) {

    buffer = 0

  }

  # round value by increments
  rounded_val <- (round(as.numeric(x) / by) * by) + buffer

  return(rounded_val)
}

#' Remove Problems
#' @description Replace problems that arise when reading in tab seperated text file
#' using readr::read_tsv() then calling problems()
#' @param df original df
#' @param prob_df problem df
#' @param verbose logical, whether messages should output when function is run. Default is FALSE, no messages are printed
#' @return data.frame
#' @export
#' @importFrom dplyr mutate across
replace_probs <- function(df, prob_df, verbose  = FALSE) {

  if(nrow(prob_df) < 1) {

    if(verbose == TRUE) {
      message("No problems!")
    }

    return(df)

  } else {

    if(verbose == TRUE) {

      message(paste0("Fixing data in ", nrow(prob_df), "rows"))

    }

    df <- dplyr::mutate(df, dplyr::across(where(is.numeric), as.character))

    for (z in 1:nrow(prob_df)) {

      zrow     <- prob_df$row[z]
      zcol     <- prob_df$col[z]
      zreplace <- prob_df$actual[z]

      df[zrow, zcol] <- zreplace

    }

    return(df)
  }

}
