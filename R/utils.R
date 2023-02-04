#' Check all arguments of a function for any/all NULL values
#' @description Internal utils function for checking a function arguments for any/all invalid/missing arguments necessary to the function it is called within
#' @param arg_lst list of function arguments by calling 'as.list(environment())'
#' @param ignore character vector of function arguments to ignore NULL check
#' @param f character, either "any" or "all" to indicate whether to check for "any" or "all" NULL argument. If "any" then if any of the function arguments are NULL, then an error is thrown. If "all" then all relevant arguments must be NULL for an error to be thrown. Defaults to "any"
#' @noRd
#' @keywords internal
#' @return character error statement with NULL arguments listed, or NULL if no error is thrown by NULL values
check_args <- function(
    arg_lst = NULL,
    ignore  = NULL,
    f       = "any"
) {

  # if no function arguments are given, throw an error
  if(is.null(arg_lst)) {

    stop(paste0("provide a list of function arguments by calling 'as.list(environment())', within another function"))

  } else {

    # make sure provided function is either "any" or "all"
    if(!f %in% c("any", "all")) {

      stop(paste0("Invalid 'f' argument, provide either 'any' or 'all' functions to 'f' argument"))

    }

    # match user provided function
    f <- match.fun(f)

    # if certain arguments are specifically supposed to be ignored
    if(!is.null(ignore)) {

      # remove "api_key" from possible arguments
      args <- arg_lst[!names(arg_lst) %in% ignore]
      # args <- arg_lst[names(arg_lst) != ignore]

    } else {

      args <- arg_lst

    }

    # if any/all arguments are NULL, return an error statement. Otherwise return NULL if NULL check is passed
    if(f(sapply(args, is.null))) {

      return(paste0("Invalid or missing ", paste0("'", c(names(args[sapply(args, is.null)])), "'", collapse = ", "), " arguments"))

    } else {

      return(NULL)


    }

  }

}

yeartype_lst <- function(
    upper_dry_oct_apr_cfs = NULL,
    upper_dry_may_sep_cfs = NULL,
    lower_dry_oct_apr_cfs = NULL,
    lower_dry_may_sep_cfs = NULL,
    upper_avg_oct_apr_cfs = NULL,
    upper_avg_may_sep_cfs = NULL,
    lower_avg_oct_apr_cfs = NULL,
    lower_avg_may_sep_cfs = NULL
) {
    # year types and provided flows list
    lst <- list(
                        "upper_dry_oct_apr_cfs"   = upper_dry_oct_apr_cfs,
                        "upper_dry_may_sep_cfs"   = upper_dry_may_sep_cfs,
                        "lower_dry_oct_apr_cfs"   = lower_dry_oct_apr_cfs,
                        "lower_dry_may_sep_cfs"   = lower_dry_may_sep_cfs,
                        "upper_avg_oct_apr_cfs"   = upper_avg_oct_apr_cfs,
                        "upper_avg_may_sep_cfs"   = upper_avg_may_sep_cfs,
                        "lower_avg_oct_apr_cfs"   = lower_avg_oct_apr_cfs,
                        "lower_avg_may_sep_cfs"   = lower_avg_may_sep_cfs
                        )

    # yeartype_df <- data.frame(
    #   position  = c("upper", "upper", "lower", "lower", "upper","upper", "lower", "lower"),
    #   date      = c("oct_apr", "may_sep", "oct_apr", "may_sep",
    #                 "oct_apr", "may_sep", "oct_apr", "may_sep"),
    #   year_type = c("DRY", "DRY", "DRY", "DRY", "AVG+", "AVG+", "AVG+", "AVG+"),
    #   cfs       = c(
    #     upper_dry_oct_apr_cfs, upper_dry_may_sep_cfs, lower_dry_oct_apr_cfs, lower_dry_may_sep_cfs,
    #     upper_avg_oct_apr_cfs, upper_avg_may_sep_cfs, lower_avg_oct_apr_cfs, lower_avg_may_sep_cfs
    #   )
    # )

    return(lst)
}

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
