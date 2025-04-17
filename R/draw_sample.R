#' Draw a Sample Based on a Defined Sample Frame
#'
#' This function selects a sample of INSPIRE grid cells from a pre-defined sample frame,
#' considering population data for weighting. It supports randomization.
#'
#' @param sample_frame A custom sample frame, if provided, overrides the default.
#' @param custom_year A custom year for retrieving municipality and census data. If NULL,
#'   the function extracts the year from `survey`.
#' @param randomize Logical. If TRUE (default), shuffles the final sample.
#'
#' @return A tibble with selected INSPIRE grid cell identifiers.
#' @import dplyr sf glue readr rlang tidyr purrr stringr
#' @export
draw_sample <- function(
    sample_frame = NULL,
    year = NULL,
    randomize = TRUE
) {

  # Define AGS column name for census lookup
  year_ags <- paste0("AGS_", year)

  # Load municipality shape file for additional attributes
  municipalities_shape <-
    system.file(
      "extdata",
      glue::glue("Gemeindegrenzen_{year}_mit_Einwohnerzahl.geojson.rds"),
      package = "geosynth"
    ) |>
    readr::read_rds()

  # Load census data containing INSPIRE grid cells and population information
  census_inhabitants <-
    system.file(
      "extdata",
      "census_inhabitants.rds",
      package = "geosynth"
    ) |>
    readr::read_rds()

  # ---- Step 1: Select Municipalities for Sampling ----

  sample_municipalities <-
    sample_frame |>
    dplyr::left_join(
      municipalities_shape |>
        sf::st_drop_geometry() |>
        dplyr::select(AGS, EWZ),
      by = c("AGS", "EWZ")
    ) |>
    tidyr::drop_na(n_geo_unit) |>
    dplyr::group_split(LAN, geo_unit) |>
    purrr::map(~{
      # Number of sample points per geographic unit
      n_sample_points <- .x$n_geo_unit[1]
      # Draw municipalities weighted by population
      dplyr::slice_sample(.x, n = n_sample_points, weight_by = EWZ)
    }) |>
    dplyr::bind_rows()

  # Adjust sample realization count
  sample_municipalities <- sample_municipalities |>
    dplyr::mutate(n_resp_realize = ceiling(n_resp_geo_unit / n_geo_unit))

  # Filter census data to keep only relevant AGS entries
  census_ags <- census_inhabitants |>
    sf::st_drop_geometry() |>
    dplyr::select(year_ags) |>
    dplyr::distinct() |>
    dplyr::pull()

  sample_municipalities <- sample_municipalities |>
    dplyr::filter(AGS %in% census_ags)

  # ---- Step 2: Adjust Census Data for Sampling ----

  census_inhabitants <- census_inhabitants |>
    dplyr::group_by(!!rlang::sym(year_ags)) |>
    dplyr::mutate(
      Einwohner_mean = mean(Einwohner),
      Einwohner = ifelse(Einwohner_mean == -1, 3, Einwohner)
    ) |>
    dplyr::ungroup()

  # ---- Step 3: Draw INSPIRE Grid Cells ----

  drawn_sample <-
    sample_municipalities |>
    dplyr::group_split(dplyr::row_number()) |>
    purrr::map(~{
      census_inhabitants |>
        dplyr::filter(!!rlang::sym(year_ags) == .x$AGS[1], Einwohner >= 3) |>
        dplyr::select(Gitter_ID_1km, Einwohner) |>
        dplyr::slice_sample(
          n = .x$n_resp_realize, weight_by = Einwohner, replace = TRUE
        ) |>
        dplyr::select(Gitter_ID_1km)
    }) |>
    dplyr::bind_rows() |>
    dplyr::slice_sample(n = mean(sample_frame$n, na.rm = TRUE))

  # Randomize the sample if required
  if (isTRUE(randomize)) {
    drawn_sample <- drawn_sample[sample(nrow(drawn_sample)),]
  }

  drawn_sample
}
