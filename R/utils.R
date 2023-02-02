# shp <- sf::read_sf("D:/louisville_wildfire/counties/aoi_counties.gpkg")
#
# county_plot <-
#   ggplot2::ggplot() +
#   ggplot2::geom_sf(data = shp, ggplot2::aes(fill = county)) +
#   ggplot2::labs(
#     # title = "Spatial mask",
#     fill  = ""
#   )
#
# ggplot2::ggsave(
#   "D:/louisville_wildfire/plots/county_plot.png",
#   county_plot,
#   scale = 1,
#   width = 10,
#   height = 8
# )
#
# library(tidyterra)
# library(terra)
#
# r <- terra::rast("D:/louisville_wildfire/wildfire_risk/wildfire_risk.tif")
#
# r_agg <- terra::aggregate(r, fact = 5)
#
# wfr_plot <-
#   ggplot2::ggplot() +
#   tidyterra::geom_spatraster(data = r_agg) +
#   ggplot2::labs(
#     title = "Risk factor raster  =  (Burn Probability)  x  Σ(Flame Length(i) * damage_factor(i))",
#     fill = ""
#   ) +
#   tidyterra::scale_fill_whitebox_c(
#     palette = "deep", direction = -1
#   ) +
#   ggplot2::theme(
#     plot.title = ggplot2::element_text(size = 14, face = "bold")
#   )
# wfr_plot
# ggplot2::ggsave(
#   "D:/louisville_wildfire/plots/wildfire_risk_plot.png",
#   wfr_plot,
#   scale = 1,
#   width = 10,
#   height = 8
# )
# fgd
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
