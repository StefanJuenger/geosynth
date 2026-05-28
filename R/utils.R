#' @noRd
load_mun_shape <- function(year) {
  get(paste0("mun_", year))
}

#' @noRd
load_census <- function() {
  get("census_inhabitants")
}

#' @noRd
resolve_ags <- function(
    .data,
    year,
    mun_id = NULL,
    verbose = TRUE
) {

  # Case 1: AGS already exists
  if ("ags" %in% names(.data)) {

    if (isTRUE(verbose)) {
      cli::cli_alert_success("Using existing 'ags' column")
    }

    return(.data)
  }

  # Case 2: Alternative municipality identifier supplied
  if (!is.null(mun_id) && mun_id %in% names(.data)) {
    .data$ags <- .data[[mun_id]]

    if (isTRUE(verbose)) {
      cli::cli_alert_success("Using municipality identifier column '{mun_id}'")
    }

    return(.data)
  }

  # Case 3: Derive AGS spatially from geometry
  if (inherits(.data, "sf")) {
    if (isTRUE(verbose)) {
      cli::cli_alert_info(
        paste0(
          "No municipality identifier supplied ",
          "- deriving AGS from geometry using ffm::bkg_admin_archive()"
        )
      )
    }

    mun_data <- ffm::bkg_admin_archive(level = "gem", year = year)

    mun_data_ags <- mun_data |> sf::st_drop_geometry() |> _[, "AGS"]
    names(mun_data_ags) <- "ags"
    mun_data <- cbind(mun_data_ags, mun_data[, "geometry"])
    mun_data <- sf::st_as_sf(mun_data)

    if (sf::st_crs(.data) != sf::st_crs(mun_data)) {
      mun_data <- sf::st_transform(mun_data, sf::st_crs(.data))
    }

    joined <-
      sf::st_join(
        .data,
        mun_data[, "ags"],
        join = sf::st_nearest_feature,
        left = TRUE
      )

    .data$ags <- joined$ags

    # Validate successful assignment
    n_missing <- sum(is.na(.data$ags))

    if (n_missing > 0) {
      cli::cli_abort(c(
        "Failed to derive municipality identifiers for all observations.",
        "x" = "{n_missing} observations could not be matched spatially.",
        "i" = "Check CRS compatibility and geometry validity."
      ))
    }

    if (isTRUE(verbose)) {
      cli::cli_alert_success("Derived municipality identifiers spatially")
    }

    return(.data)
  }

  # Case 4: No valid identifier strategy available
  cli::cli_abort(c(
    "No municipality identifier available.",
    "x" = "Column 'ags' not found.",
    "x" = "Specified 'mun_id' column not found.",
    "x" = "Input is not an sf object.",
    "i" = "Provide a municipality identifier column or spatial geometry."
  ))
}
