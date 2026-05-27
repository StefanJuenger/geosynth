#' Create a Sample Frame for Municipalities
#'
#' This function constructs a sample frame based on geographic units and population thresholds,
#' ensuring a minimum number of sampling points. The function enriches the dataset with
#' municipal information, aggregates data at various geographic levels, and ensures sampling
#' requirements are met before merging the results into a final frame.
#'
#' @param .data A data frame containing georeferenced sample data with `AGS` identifiers.
#' @param year The year for which the sample frame is created (used to fetch municipality data).
#' @param geo_unit A character vector specifying the geographic unit to be used,
#'   defaulting to `c("gkpol", "regiostar7", "regiostar17")`.
#' @param inhabitants_threshold The minimum population required for inclusion (default: 50,000).
#' @param minimum_sample_points The minimum required sample points per geographic unit (default: 10).
#' @return A tibble with municipality-level sample information.
#'
#' @examples
#' # Sample data for the year 2022 with a population threshold of 100,000
#' sampled_data <- function(data, year = 2022, inhabitants_threshold = 100000)
#'
#' # Sample data using the "RegioStaR17" geographic unit
#' sampled_data <- function(data, year = 2022, geo_unit = "RegioStaR17")
#'
#' @export
create_sample_frame <- function(
    .data,
    year,
    geo_unit = c("gkpol", "regiostar7", "regiostar17"),
    inhabitants_threshold = 50000,
    minimum_sample_points = 10
) {
  geo_unit <- match.arg(geo_unit)

  # Load municipality shapefile and subset to required columns
  keep_cols <- c("ags", "lan", geo_unit, "inhabitants")

  municipality_shape <- geosynth:::load_mun_shape(year)
  municipality_shape <- municipality_shape[, keep_cols]

  # Drop geometry and join with municipality data
  mun_df <- sf::st_drop_geometry(municipality_shape)

  data_enriched <-
    sf::st_drop_geometry(.data) |>
    merge(mun_df, by = "ags", all.x = TRUE)

  # Create geo_unit column and count distinct AGS per group
  data_enriched$geo_unit <- data_enriched[[geo_unit]]

  group_key <- paste(
    data_enriched$lan, data_enriched$geo_unit,
    sep = "_"
  )

  data_enriched$n_geo_unit <- ave(
    data_enriched$ags, group_key,
    FUN = function(x) length(unique(x))
  )

  data_enriched$n_geo_unit <- as.numeric(data_enriched$n_geo_unit)

  data_enriched$n <- nrow(data_enriched)

  # Summarise per (lan, geo_unit)
  by_geo <- list(
    lan = data_enriched$lan,
    geo_unit = data_enriched$geo_unit
  )

  agg_count <- aggregate(
    list(n_resp_geo_unit = data_enriched$ags),
    by = by_geo,
    FUN = length
  )

  agg_means <- aggregate(
    data_enriched[, c("n_geo_unit", "n", "inhabitants")],
    by = by_geo,
    FUN = mean
  )

  data_enriched_summarized <-
    merge(agg_count, agg_means, by = c("lan", "geo_unit")) |>
    (\(x) x[order(x$lan), ])()

  # Count total municipalities per (lan, geo_unit) in the full shapefile
  mun_agg <- aggregate(
    list(n_geo_unit_overall = mun_df$ags),
    by = list(lan = mun_df$lan, geo_unit = mun_df[[geo_unit]]),
    FUN = length
  )

  data_enriched_summarized <- merge(
    data_enriched_summarized, mun_agg,
    by = c("lan", "geo_unit"),
    all.x = TRUE
  )

  # Place n_geo_unit_overall immediately after n_geo_unit
  col_order <- c(
    "lan", "geo_unit", "n_resp_geo_unit", "n_geo_unit",
    "n_geo_unit_overall", "n", "inhabitants"
  )

  data_enriched_summarized <- data_enriched_summarized[, col_order]

  # Identify rows that fail the sample requirements
  is_evil <-
    data_enriched_summarized$inhabitants < inhabitants_threshold &
    data_enriched_summarized$n_geo_unit_overall < minimum_sample_points

  evil_cases <- data_enriched_summarized[is_evil, ]

  # Shift geo_unit by ±1 independently for each problematic row
  safe_cases <- evil_cases

  safe_cases$geo_unit <- safe_cases$geo_unit +
    vapply(
      seq_len(nrow(safe_cases)),
      function(i) sample(c(-1L, 1L), 1L),
      integer(1L)
    )

  # Remove problematic rows (anti-join) and append adjusted replacements
  make_key <- function(df) {
    do.call(paste, c(df[, col_order], sep = "_"))
  }

  keep <- !make_key(data_enriched_summarized) %in% make_key(evil_cases)

  data_combined <- rbind(
    data_enriched_summarized[keep, ],
    safe_cases
  )

  # Re-aggregate by (lan, geo_unit)
  by_final <- list(
    lan = data_combined$lan,
    geo_unit = data_combined$geo_unit
  )

  data_enriched_summarized <- merge(
    aggregate(
      data_combined[, c("n_resp_geo_unit", "n_geo_unit")],
      by = by_final,
      FUN = sum
    ),
    aggregate(
      list(n = data_combined$n),
      by = by_final,
      FUN = mean
    ),
    by = c("lan", "geo_unit")
  )

  # Merge result back onto the municipality shapefile
  municipality_shape$geo_unit <- municipality_shape[[geo_unit]]

  result <- merge(
    municipality_shape, data_enriched_summarized,
    by = c("lan", "geo_unit"),
    all.x = TRUE
  )

  result <- result[order(result$lan), ]
  result$year <- year
  result
}
