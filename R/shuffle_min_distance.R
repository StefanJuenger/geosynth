#' Shuffle Synthetic Data to Maintain Minimum Distance
#'
#' This function attempts to shuffle rows in a synthetic dataset so that
#' each point is at least `min_km` kilometers away from its corresponding
#' point in the original dataset. It performs iterative reassignments
#' for problematic points until either the distance condition is satisfied
#' or `max_tries` is reached.
#'
#' @param synthetic_data An `sf` object with the synthetic spatial data to shuffle.
#' @param original_data An `sf` object containing the original spatial data.
#' @param min_km Minimum allowed distance (in kilometers) between original
#'   and corresponding synthetic points. Default is 50.
#' @param max_tries Maximum number of attempts to achieve the minimum distance
#'   condition. Default is 10,000.
#' @param verbose Show output from function (default TRUE)
#'
#' @return An `sf` object where the synthetic data has been shuffled such that
#'   all points are at least `min_km` apart from the original data.
#' @export
#' @examples
#' library(geosynth)
#'
#' data("fake_survey_coordinates")
#'
#' sample_frame <-
#'  geosynth::create_sample_frame(
#'    .data = fake_survey_coordinates,
#'    year = "2024",
#'    geo_unit = "regiostar17",
#'    inhabitants_threshold = 10000,
#'    minimum_sample_points = 10
#'  )
#'
#' synthetic_sample <- geosynth::draw_sample(sample_frame = sample_frame)
#'
#' synthetic_shuffled <-
#'   geosynth::shuffle_min_distance(
#'     synthetic_data = synthetic_sample,
#'     original_data = fake_survey_coordinates,
#'     min_km = 5   # kilometers
#'   )
shuffle_min_distance <-
  function(
    synthetic_data,
    original_data,
    min_km = 50,
    max_tries = 1000,
    verbose = TRUE
    ) {

    if (isTRUE(verbose)) {
      cli::cli_h1("Shuffling synthetic data")

      cli::cli_bullets(c(
        "*" = "Observations synthetic data: {nrow(synthetic_data)}",
        "*" = "Observations original data: {nrow(original_data)}",
        "*" = "Min distance: {min_km} km",
        "*" = "Max tries: {max_tries}"
      ))
    }

    if (min_km < 0) {
      stop("Negative mininum distance detected. Choose a positive one.")
    }

    if (sf::st_crs(synthetic_data) != sf::st_crs(original_data)) {
      original_data <-
        sf::st_transform(original_data, sf::st_crs(synthetic_data))
    }

    if (isTRUE(verbose) &&
        sf::st_crs(synthetic_data) != sf::st_crs(original_data)) {

      cli::cli_alert_info(
        "Transformed CRS of original data to match synthetic data"
        )
    }

    n <- nrow(original_data)
    current_synthetic <- synthetic_data

    if (isTRUE(verbose)) {
      pb <- cli::cli_progress_bar(
        name = "Optimizing spatial distances",
        total = max_tries
      )
    }

    for (i in 1:max_tries) {

      distances <-
        sf::st_distance(original_data, current_synthetic, by_element = TRUE)

      distances_km <- as.numeric(distances) / 1000

      too_close_idx <- which(distances_km < min_km)

      if (length(too_close_idx) == 0) {

        if (isTRUE(verbose)) {
          cli::cli_progress_done(id = pb)
          cli::cli_alert_success("Success after {i} tries")
        }

        return(current_synthetic)
      }

      replacement_indices <- sample(1:n, length(too_close_idx), replace = FALSE)

      current_synthetic[too_close_idx, ] <-
        synthetic_data[replacement_indices, ]

      if (isTRUE(verbose)) {
        cli::cli_progress_update(id = pb, set = i)
      }
    }

    if (isTRUE(verbose)) {
      cli::cli_progress_done(id = pb)
      cli::cli_alert_error(
        "Could not satisfy minimum distance after {max_tries} tries"
      )
    }
  }
