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
