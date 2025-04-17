#' Shuffle Synthetic Data to Maintain Minimum Distance
#'
#' This function attempts to shuffle rows in a synthetic dataset so that
#' each point is at least `min_km` kilometers away from its corresponding
#' point in the original dataset. It performs iterative reassignments
#' for problematic points until either the distance condition is satisfied
#' or `max_tries` is reached.
#'
#' @param original_data An `sf` object containing the original spatial data.
#' @param synthetic_data An `sf` object with the synthetic spatial data to shuffle.
#' @param min_km Minimum allowed distance (in kilometers) between original
#'   and corresponding synthetic points. Default is 50.
#' @param max_tries Maximum number of attempts to achieve the minimum distance
#'   condition. Default is 10,000.
#'
#' @return An `sf` object where the synthetic data has been shuffled such that
#'   all points are at least `min_km` apart from the original data.
#'
#' @examples
#' # Assuming `orig` and `synth` are sf POINT objects with equal rows:
#' # shuffled <- shuffle_min_distance(orig, synth, min_km = 50)
#'
#' @export
shuffle_min_distance <-
  function(original_data, synthetic_data, min_km = 50, max_tries = 10000) {
    n <- nrow(original_data)
    current_synthetic <- synthetic_data

    for (i in 1:max_tries) {
      # Compute pairwise distances between original and synthetic (by element)
      distances <- sf::st_distance(original_data, current_synthetic, by_element = TRUE)

      # Convert distance units to kilometers
      distances_km <- as.numeric(units::set_units(distances, "km"))

      # Identify indices where distance is below the minimum threshold
      too_close_idx <- which(distances_km < min_km)

      # If all distances are acceptable, return the synthetic data
      if (length(too_close_idx) == 0) {
        message(paste("Success after", i, "tries!"))
        return(current_synthetic)
      }

      # Replace problematic synthetic points with randomly selected ones
      replacement_indices <- sample(1:n, length(too_close_idx), replace = FALSE)
      current_synthetic[too_close_idx, ] <- synthetic_data[replacement_indices, ]
    }

    # If condition not satisfied within max_tries, throw an error
    stop("Could not satisfy minimum distance after max_tries")
  }
