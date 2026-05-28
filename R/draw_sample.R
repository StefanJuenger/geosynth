#' Draw a Sample Based on a Defined Sample Frame
#'
#' This function draws a synthetic sample based on a pre-defined sample frame.
#' Municipalities and INSPIRE grid cells are sampled using population-weighted
#' probabilities derived from census data. Depending on the selected return
#' values, the function can return municipality identifiers, INSPIRE grid cell
#' identifiers, spatial coordinates, or combinations thereof.
#'
#' @param sample_frame A sample frame created with
#'   [create_sample_frame()].
#' @param randomize Logical. If `TRUE` (default), randomizes the order of the
#'   final sample.
#' @param return Character vector specifying which information should be
#'   returned. Possible values are `"inspid1km"`, `"ags"`, and `"coords"`.
#'   If `"coords"` is included, an `sf` object with geometries is returned.
#'   Otherwise, a tibble without geometries is returned. Defaults to
#'   `c("inspid1km", "ags", "coords")`.
#' @param verbose Logical. Show function progress and status messages
#'   (default `TRUE`).
#'
#' @return
#' Depending on `return`, either:
#'
#' * an `sf` object containing sampled geometries and selected attributes, or
#' * a tibble containing sampled municipality and/or INSPIRE grid cell
#'   identifiers.
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
#' synthetic_sample <-
#'   geosynth::draw_sample(
#'     sample_frame = sample_frame
#'   )
#'
#' synthetic_ags <-
#'   geosynth::draw_sample(
#'     sample_frame = sample_frame,
#'     return = "ags"
#'   )
#'
#' synthetic_coords <-
#'   geosynth::draw_sample(
#'     sample_frame = sample_frame,
#'     return = c("coords", "ags")
#'   )
draw_sample <-
  function(
    sample_frame = NULL,
    return = c("inspid1km", "ags", "coords"),
    randomize = TRUE,
    verbose = TRUE
    ) {

    if (length(return) == 0) {
      return <- c("inspid1km", "ags", "coords")
    }

  if (isTRUE(verbose)) {
    cli::cli_h1("Drawing sample")

    cli::cli_bullets(c(
      "*" = "Year: {sample_frame$year[1]}",
      "*" = "Municipalities in sample frame: {nrow(sample_frame)}",
      "*" = "Randomization: {randomize}"
    ))
  }

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

  if (isTRUE(verbose)) {
    cli::cli_alert_success(
      "{nrow(sample_municipalities)} municipalities selected for sampling"
    )
  }

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

  if (isTRUE(verbose)) {
    pb <- cli::cli_progress_bar(
      name = "Drawing coordinates",
      total = nrow(sample_municipalities)
    )
  }

  drawn_list <- vector("list", nrow(sample_municipalities))

  for (i in seq_len(nrow(sample_municipalities))) {

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

    drawn_list[[i]] <- result

    if (isTRUE(verbose)) {
      cli::cli_progress_update(id = pb, set = i)
    }
  }

  drawn_sample <- do.call(rbind, drawn_list)

  drawn_sample <- drawn_sample[sample(nrow(drawn_sample), n_final), ]

  if (isTRUE(verbose)) {
    cli::cli_progress_done(id = pb)
  }

  # Exclude municipalities flagged in the sample frame
  evil_municipalities <-
    sample_frame$ags[is.na(sample_frame$n_resp_geo_unit)]

  drawn_sample <-
    drawn_sample[!(drawn_sample$ags %in% evil_municipalities), ]

  if (isTRUE(verbose)) {
    cli::cli_alert_success(
      "Sample of {nrow(drawn_sample)} coordinates drawn"
    )
  }

  # Randomize the sample if required
  if (isTRUE(randomize)) {
    drawn_sample <- drawn_sample[sample(nrow(drawn_sample)), ]
  }

  if (isTRUE(verbose)) {
    cli::cli_alert_success(
      "Finished drawing sample"
    )
  }

  return <- match.arg(return, several.ok = TRUE)

  geom_enabled <- "coords" %in% return
  keep_cols <- setdiff(return, "coords")

  drawn_sample <- do.call(rbind, drawn_list)
  drawn_sample <- drawn_sample[sample(nrow(drawn_sample), n_final), ]

  if (geom_enabled) {
    drawn_sample <- sf::st_as_sf(drawn_sample)
  } else {
    drawn_sample <- sf::st_drop_geometry(drawn_sample)
  }

  if (length(keep_cols) > 0) {
    drawn_sample <-
      drawn_sample[, intersect(names(drawn_sample), keep_cols), drop = FALSE]
  }

  drawn_sample
}
