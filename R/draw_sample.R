#' Draw a Sample Based on a Defined Sample Frame
#'
#' This function selects a sample of INSPIRE grid cells from a pre-defined sample frame,
#' considering population data for weighting. It supports randomization.
#'
#' @param sample_frame A custom sample frame, if provided, overrides the default.
#' @param randomize Logical. If TRUE (default), shuffles the final sample.
#'
#' @return A tibble with selected INSPIRE grid cell identifiers.
#' @export
draw_sample <- function(sample_frame = NULL, randomize = TRUE) {
  # Define AGS column name for census lookup
  year_ags <- paste0("ags_", sample_frame$year[1])

  # Load municipality shapefile for additional attributes
  municipalities_shape <- load_mun_shape(sample_frame$year[1])

  # Load census data with INSPIRE grid cells and population info
  census_inhabitants <- load_census()

  # ---- Step 1: Select Municipalities for Sampling ----
  mun_attrs <- sf::st_drop_geometry(municipalities_shape)[
    , c("ags", "inhabitants")
  ]

  sample_municipalities <- merge(
    sample_frame, mun_attrs,
    by = c("ags", "inhabitants"),
    all.x = TRUE
  )

  sample_municipalities <-
    sample_municipalities[!is.na(sample_municipalities$n_geo_unit), ]

  # Draw municipalities weighted by population, per (lan, geo_unit) group
  groups <- split(
    sample_municipalities,
    list(sample_municipalities$lan, sample_municipalities$geo_unit),
    drop = TRUE
  )

  sample_municipalities <-
    lapply(groups, function(grp) {
      n_pts <- min(grp$n_geo_unit[1], nrow(grp))
      idx <- sample(nrow(grp), size = n_pts, prob = grp$inhabitants)
      grp[idx, ]
    }) |>
    do.call(what = rbind, args = _)

  # Adjust sample realization count
  sample_municipalities$n_resp_realize <- ceiling(
    sample_municipalities$n_resp_geo_unit / sample_municipalities$n_geo_unit
  )

  # Filter to municipalities present in the census data
  census_ags <- unique(census_inhabitants[[year_ags]])

  sample_municipalities <-
    sample_municipalities[sample_municipalities$ags %in% census_ags, ]

  # ---- Step 2: Adjust Census Data for Sampling ----
  grp_key <- census_inhabitants[[year_ags]]

  census_inhabitants$inhabitants_mean <- ave(
    census_inhabitants$inhabitants, grp_key, FUN = mean
  )

  census_inhabitants$inhabitants <- ifelse(
    census_inhabitants$inhabitants_mean == -1, 3,
    census_inhabitants$inhabitants
  )

  # ---- Step 3: Draw INSPIRE Grid Cells ----
  n_final <- round(mean(sample_frame$n, na.rm = TRUE))

  drawn_sample <-
    lapply(seq_len(nrow(sample_municipalities)), function(i) {
      row_i <- sample_municipalities[i, ]

      eligible <- census_inhabitants[
        census_inhabitants[[year_ags]] == row_i$ags &
          census_inhabitants$inhabitants >= 3,
        c("inspid1km", "inhabitants")
      ]

      idx <- sample(
        nrow(eligible),
        size = row_i$n_resp_realize,
        prob = eligible$inhabitants,
        replace = TRUE
      )

      result <- eligible[idx, "inspid1km"]
      result$ags <- row_i$ags
      result
    }) |>
    do.call(what = rbind, args = _)

  drawn_sample <- drawn_sample[sample(nrow(drawn_sample), n_final), ]

  # Exclude municipalities flagged in the sample frame
  evil_municipalities <-
    sample_frame$ags[is.na(sample_frame$n_resp_geo_unit)]

  drawn_sample <-
    drawn_sample[!(drawn_sample$ags %in% evil_municipalities), ]

  # Randomize the sample if required
  if (isTRUE(randomize)) {
    drawn_sample <- drawn_sample[sample(nrow(drawn_sample)), ]
  }

  drawn_sample
}

# draw_sample <- function(sample_frame = NULL, randomize = TRUE) {
#
#   # Define AGS column name for census lookup
#   year_ags <- paste0("ags_", sample_frame$year[1])
#
#   # Load municipality shape file for additional attributes
#   municipalities_shape <- load_mun_shape(sample_frame$year[1])
#
#   # Load census data containing INSPIRE grid cells and population information
#   census_inhabitants <- load_census()
#
#   # ---- Step 1: Select Municipalities for Sampling ----
#   sample_municipalities <-
#     sample_frame |>
#     dplyr::left_join(
#       municipalities_shape |>
#         sf::st_drop_geometry() |>
#         dplyr::select(ags, inhabitants),
#       by = c("ags", "inhabitants")
#     ) |>
#     tidyr::drop_na(n_geo_unit) |>
#     dplyr::group_split(lan, geo_unit) |>
#     purrr::map(~{
#       # Number of sample points per geographic unit
#       n_sample_points <- .x$n_geo_unit[1]
#       # Draw municipalities weighted by population
#       dplyr::slice_sample(.x, n = n_sample_points, weight_by = inhabitants)
#     }) |>
#     dplyr::bind_rows()
#
#   # Adjust sample realization count
#   sample_municipalities <-
#     sample_municipalities |>
#     dplyr::mutate(n_resp_realize = ceiling(n_resp_geo_unit / n_geo_unit))
#
#   # Filter census data to keep only relevant AGS entries
#   census_ags <-
#     census_inhabitants |>
#     sf::st_drop_geometry() |>
#     dplyr::select(year_ags) |>
#     dplyr::distinct() |>
#     dplyr::pull()
#
#   sample_municipalities <-
#     sample_municipalities |>
#     dplyr::filter(ags %in% census_ags)
#
#   # ---- Step 2: Adjust Census Data for Sampling ----
#   census_inhabitants <-
#     census_inhabitants |>
#     dplyr::group_by(!!rlang::sym(year_ags)) |>
#     dplyr::mutate(
#       inhabitants_mean = mean(inhabitants),
#       inhabitants = ifelse(inhabitants_mean == -1, 3, inhabitants)
#     ) |>
#     dplyr::ungroup()
#
#   # ---- Step 3: Draw INSPIRE Grid Cells ----
#   drawn_sample <-
#     sample_municipalities |>
#     dplyr::group_split(dplyr::row_number()) |>
#     purrr::map(~{
#       sample_i <-
#         census_inhabitants |>
#         dplyr::filter(!!rlang::sym(year_ags) == .x$ags[1], inhabitants >= 3) |>
#         dplyr::select(inspid1km, inhabitants) |>
#         dplyr::slice_sample(
#           n = .x$n_resp_realize, weight_by = inhabitants, replace = TRUE
#         ) |>
#         dplyr::select(inspid1km)
#
#       sample_i |>
#         dplyr::mutate(ags = .x$ags[1])
#     }) |>
#     dplyr::bind_rows() |>
#     dplyr::slice_sample(n = mean(sample_frame$n, na.rm = TRUE))
#
#   # Make sure coordinates are not drawn from flagged sample frame municipalities
#   evil_municipalities <-
#     sample_frame |>
#     dplyr::filter(is.na(n_resp_geo_unit)) |>
#     dplyr::pull(ags)
#
#   drawn_sample <-
#     drawn_sample |>
#     dplyr::filter(!(ags %in% evil_municipalities))
#
#   # Randomize the sample if required
#   if (isTRUE(randomize)) {
#     drawn_sample <- drawn_sample[sample(nrow(drawn_sample)),]
#   }
#
#   drawn_sample
# }
