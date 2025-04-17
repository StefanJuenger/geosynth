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
#'   defaulting to `c("gkpol", "RegioStaR7", "RegioStaR17")`.
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
#' @import dplyr sf glue readr rlang
#' @export
create_sample_frame <-
  function (
    .data,
    year,
    geo_unit = c("gkpol", "RegioStaR7", "RegioStaR17"),
    inhabitants_threshold = 50000,
    minimum_sample_points = 10
  ) {

    # Load the municipality shapefile for the specified year.
    # This acts as a base framework to which sample information is later added.
    municipality_shape <-
      system.file(
        "extdata",
        glue::glue("Gemeindegrenzen_{year}_mit_Einwohnerzahl.geojson.rds"),
        package = "geosynth"
      ) |>
      readr::read_rds() |>
      dplyr::select(AGS, LAN, dplyr::any_of(geo_unit), EWZ) |>
      sf::st_drop_geometry()

    # Enrich data with municipality information by joining on AGS (municipality ID).
    data_enriched <-
      .data |>
      dplyr::left_join(municipality_shape, by = "AGS")

    # Assign geographic unit for structuring and count unique municipalities per unit.
    data_enriched <-
      data_enriched |>
      dplyr::mutate(geo_unit = .data[[geo_unit]]) |>
      dplyr::group_by(LAN, geo_unit) |>
      dplyr::mutate(n_geo_unit = dplyr::n_distinct(AGS), .after = AGS) |>
      dplyr::ungroup() |>
      dplyr::mutate(n = dplyr::n())

    # Summarize data to have a single entry per state-geo_unit combination.
    # Uses mean aggregation since values do not vary within groups.
    data_enriched_summarized <-
      data_enriched |>
      dplyr::group_by(LAN, geo_unit) |>
      dplyr::summarise(
        n_resp_geo_unit = mean(dplyr::n()), # Respondents per geo unit
        n_geo_unit = mean(n_geo_unit), # Number of municipalities per geo unit
        n = mean(n), # Total respondents
        EWZ = mean(EWZ), # Average population
        .groups = "drop"
      ) |>
      dplyr::arrange(LAN)

    # Ensure a minimum number of sample points is met.
    municipality_shape_summarized <-
      municipality_shape |>
      sf::st_drop_geometry() |>
      dplyr::group_by(LAN, !!!rlang::syms(geo_unit)) |>
      dplyr::summarise(
        n_geo_unit_overall = mean(dplyr::n()), # Mean count of geo units
        .groups = "drop"
      )

    # Merge overall municipality counts with summarized data.
    data_enriched_summarized <-
      dplyr::left_join(
        data_enriched_summarized,
        municipality_shape_summarized,
        by = c("LAN", geo_unit = geo_unit)
      ) |>
      dplyr::relocate(n_geo_unit_overall, .after = n_geo_unit)

    # Identify cases where sample requirements are not met.
    data_enriched_summarized_evil_cases <-
      data_enriched_summarized |>
      sf::st_drop_geometry() |>
      dplyr::filter(
        EWZ < inhabitants_threshold, n_geo_unit_overall < minimum_sample_points
      )

    # Adjust problematic cases by shifting geo_unit up or down.
    data_enriched_summarized_safe <-
      data_enriched_summarized_evil_cases |>
      dplyr::mutate(geo_unit = geo_unit + sample(c(-1, 1), 1))

    # Final adjustment: combine safe cases and filter out original problematic ones.
    data_enriched_summarized <-
      data_enriched_summarized |>
      dplyr::bind_rows(data_enriched_summarized_safe) |>
      dplyr::anti_join(data_enriched_summarized_evil_cases) |>
      dplyr::group_by(LAN, geo_unit) |>
      dplyr::summarise(
        n_resp_geo_unit = sum(n_resp_geo_unit),
        n_geo_unit = sum(n_geo_unit),
        n = mean(n),
        .groups = "drop"
      )

    # Merge the finalized sample frame with the municipality shapefile.
    municipality_shape |>
      dplyr::mutate(geo_unit = .data[[geo_unit]]) |>
      dplyr::left_join(
        data_enriched_summarized, by = c("LAN", "geo_unit")
      ) |>
      dplyr::arrange(LAN)
  }
