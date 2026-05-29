#' Create a Sample Frame for Municipalities
#'
#' This function constructs a municipality-level sample frame based on
#' geographic units and population thresholds while ensuring a minimum
#' number of sampling points per aggregation unit.
#'
#' The input data are enriched with municipality-level information derived
#' from official municipality geometries. Municipality identifiers can be
#' supplied either through an existing ags column, an alternative
#' identifier column specified via mun_id, or spatially derived from
#' geometries if .data is an sf object.
#'
#' The function aggregates observations across geographic units, evaluates
#' sample size constraints, and reassigns municipalities from underpopulated
#' units to the nearest valid geographic unit before merging the results
#' back onto municipality geometries.
#'
#' @param .data A data frame or sf object containing georeferenced sample
#'   data. Municipality identifiers can either be stored in an ags column,
#'   another column specified via mun_id, or derived spatially from
#'   geometry information.
#'
#' @param year The year for which the sample frame is created. Used to load
#'   municipality geometries and metadata.
#'
#' @param mun_id Optional character string specifying the column containing
#'   municipality identifiers if different from ags.
#'
#' @param geo_unit A character vector specifying the geographic aggregation
#'   unit. Must be one of c("gkpol", "regiostar7", "regiostar17").
#'
#' @param inhabitants_threshold Minimum number of inhabitants required for
#'   inclusion in a geographic unit. Defaults to 50000.
#'
#' @param minimum_sample_points Minimum number of municipalities required
#'   within a geographic unit. Defaults to 10.
#'
#' @param verbose Logical indicating whether progress information should be
#'   printed. Defaults to TRUE.
#'
#' @return A tibble containing municipality-level sample frame information.
#'
#' @importFrom stats aggregate
#' @importFrom stats ave
#'
#' @export
#'
#' @examples
#' library(geosynth)
#'
#' data("fake_survey_coordinates")
#'
#' sample_frame <-
#'   geosynth::create_sample_frame(
#'     .data = fake_survey_coordinates,
#'     year = "2024",
#'     geo_unit = "regiostar17",
#'     inhabitants_threshold = 10000,
#'     minimum_sample_points = 10
#'   )
#'
#' # Example using a custom municipality identifier column
#' # sample_frame <-
#' #   geosynth::create_sample_frame(
#' #     .data = fake_survey_coordinates,
#' #     year = "2024",
#' #     mun_id = "municipality_id",
#' #     geo_unit = "regiostar17"
#' #   )
create_sample_frame <- function(
    .data,
    year,
    mun_id = NULL,
    geo_unit = c("gkpol", "regiostar7", "regiostar17"),
    inhabitants_threshold = 50000,
    minimum_sample_points = 10,
    verbose = TRUE
) {
  geo_unit <- match.arg(geo_unit)

  if (isTRUE(verbose)) {
    cli::cli_h1("Creating sample frame")
    cli::cli_bullets(c(
      "*" = "Number of observations: {.val {nrow(.data)}}",
      "*" = "Year: {year}",
      "*" = "Geo unit: {geo_unit}",
      "*" = "Inhabitants threshold: {inhabitants_threshold}",
      "*" = "Minimum sample points: {minimum_sample_points}"
    ))
  }

  # Load municipality geometry
  municipality_shape <- load_mun_shape(year)

  if (isTRUE(verbose)) {
    cli::cli_alert_success(
      "Loaded municipality geometry with {nrow(municipality_shape)} rows"
    )
  }

  # Resolve municipality identifier
  .data <- resolve_ags(
    .data = .data,
    year = year,
    mun_id = mun_id,
    verbose = verbose
  )

  # Keep required municipality columns
  keep_cols <- c("ags", "lan", geo_unit, "inhabitants")
  municipality_shape <- municipality_shape[, keep_cols]
  mun_df <- sf::st_drop_geometry(municipality_shape)

  # Drop geometry if necessary
  data_plain <- if (inherits(.data, "sf")) {
    sf::st_drop_geometry(.data)
  } else {
    .data
  }

  # Enrich observations with municipality attributes
  data_enriched <- merge(data_plain, mun_df, by = "ags", all.x = TRUE)

  data_enriched$geo_unit <- data_enriched[[geo_unit]]

  # Count distinct municipalities within geo-units
  group_key <- paste(data_enriched$lan, data_enriched$geo_unit, sep = "_")

  data_enriched$n_geo_unit <-
    as.numeric(ave(
      data_enriched$ags, group_key,
      FUN = function(x) length(unique(x))
    ))

  data_enriched$n <- nrow(.data)

  # Aggregate by geo-unit
  by_geo <- list(lan = data_enriched$lan, geo_unit = data_enriched$geo_unit)

  agg_count <-
    aggregate(
      list(n_resp_geo_unit = data_enriched$ags),
      by = by_geo,
      FUN = length
    )

  agg_count <- agg_count[order(agg_count$lan),]

  agg_means <-
    aggregate(
      data_enriched[, c("n_geo_unit", "n")],
      by = by_geo,
      FUN = mean
    )

  agg_means <- agg_means[order(agg_means$lan),]

  data_enriched_summarized <-
    merge(agg_count, agg_means,  by = c("lan", "geo_unit"))

  # Count total municipalities per geo-unit in the full shapefile
  mun_agg <-
    aggregate(
      list(
        n_geo_unit_overall = mun_df$ags,
        inhabitants_geo_unit = mun_df$inhabitants
      ),
      by = list(lan = mun_df$lan, geo_unit = mun_df[[geo_unit]]),
      FUN = function(x) if (is.numeric(x)) mean(x) else length(x)
    )

  mun_agg <- mun_agg[order(mun_agg$lan),]

  data_enriched_summarized <-
    merge(
      data_enriched_summarized,
      mun_agg,
      by = c("lan", "geo_unit"),
      all.x = TRUE
    )

  col_order <- c(
    "lan", "geo_unit", "n_resp_geo_unit", "n_geo_unit", "n_geo_unit_overall",
    "n", "inhabitants_geo_unit"
  )

  data_enriched_summarized <- data_enriched_summarized[, col_order]

  if (isTRUE(verbose)) {
    cli::cli_alert_success("Processed municipality sample statistics")
  }

  # Identify geo-units below population and sample point thresholds
  is_evil <-
    data_enriched_summarized$inhabitants_geo_unit < inhabitants_threshold &
    data_enriched_summarized$n_geo_unit_overall < minimum_sample_points

  evil_groups <- data_enriched_summarized[is_evil, c("lan", "geo_unit")]

  if (nrow(evil_groups) > 0) {
    if (isTRUE(verbose)) {
      cli::cli_alert_warning(
        "{nrow(evil_groups)} geo-unit(s) below thresholds - reassigning to nearest valid geo-unit"
      )
    }

    valid_units <- data_enriched_summarized[!is_evil, c("lan", "geo_unit")]

    # Find nearest valid geo_unit within the same lan for each problematic unit
    evil_groups$new_geo_unit <-
      mapply(
        function(lan_val, gu) {
          candidates <- valid_units$geo_unit[valid_units$lan == lan_val]
          if (length(candidates) == 0L) return(NA_real_)
          candidates[which.min(abs(candidates - gu))]
        },
        evil_groups$lan,
        evil_groups$geo_unit
      )

    # Reassign geo_unit at municipality level before re-aggregating
    data_enriched$geo_unit_new <- data_enriched$geo_unit

    for (k in seq_len(nrow(evil_groups))) {
      mask <-
        data_enriched$lan == evil_groups$lan[k] &
        data_enriched$geo_unit_new == evil_groups$geo_unit[k]

      data_enriched$geo_unit_new[mask] <- evil_groups$new_geo_unit[k]
    }

    # Recompute n_geo_unit after reassignment
    group_key <- paste(data_enriched$lan, data_enriched$geo_unit_new, sep = "_")

    data_enriched$n_geo_unit <-
      as.numeric(ave(
        data_enriched$ags, group_key,
        FUN = function(x) length(unique(x))
      ))

    # Re-aggregate with updated geo_unit assignments
    by_geo <-
      list(lan = data_enriched$lan, geo_unit = data_enriched$geo_unit_new)

    agg_count_new <-
      aggregate(
        list(n_resp_geo_unit = data_enriched$ags),
        by = by_geo,
        FUN = length
      )

    agg_count_new <- agg_count_new[order(agg_count_new$lan),]

    agg_means_new <-
      aggregate(
        data_enriched[, c("n_geo_unit", "n")],
        by = by_geo,
        FUN = mean
      )

    agg_means_new <- agg_means_new[order(agg_means_new$lan),]

    data_enriched_summarized <-
      merge(
        agg_count_new, agg_means_new,
        by = c("lan", "geo_unit")
      )

    data_enriched_summarized <-
      merge(
        data_enriched_summarized,
        mun_agg,
        by = c("lan", "geo_unit"),
        all.x = TRUE,
        sort = FALSE
      )

    data_enriched_summarized <- data_enriched_summarized[, col_order]
  }

  # Merge sample statistics back onto municipality geometry
  municipality_shape$geo_unit <- municipality_shape[[geo_unit]]

  result <-
    merge(
      municipality_shape,
      data_enriched_summarized[
        ,
        !names(
          data_enriched_summarized) %in% c(
            "inhabitants", "inhabitants_geo_unit"
          )
      ],
      by = c("lan", "geo_unit"),
      all.x = TRUE
    )

  result <- result[order(result$lan), ]
  result$year <- year

  if (isTRUE(verbose)) {
    cli::cli_alert_success("Finished sample frame")
  }

  tibble::as_tibble(result)
}
