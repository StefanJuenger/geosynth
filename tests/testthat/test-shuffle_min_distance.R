# Tests for geosynth::shuffle_min_distance()

data("fake_survey_coordinates", package = "geosynth")

# Shared setup: sample frame + synthetic sample used across tests
sample_frame <- create_sample_frame(
  .data = fake_survey_coordinates,
  year = "2022",
  geo_unit = "regiostar17",
  inhabitants_threshold = 10000,
  minimum_sample_points = 10
)

synthetic_sample <- draw_sample(sample_frame = sample_frame)

# ── Output structure ───────────────────────────────────────────────────────────

test_that("shuffle_min_distance() returns an sf object", {
  result <- shuffle_min_distance(
    original_data  = fake_survey_coordinates,
    synthetic_data = synthetic_sample,
    min_km         = 5
  )
  expect_s3_class(result, "sf")
})

test_that("shuffle_min_distance() preserves the number of rows", {
  result <- shuffle_min_distance(
    original_data  = fake_survey_coordinates,
    synthetic_data = synthetic_sample,
    min_km         = 5
  )
  expect_equal(nrow(result), nrow(synthetic_sample))
})

test_that("shuffle_min_distance() preserves the column names of the synthetic input", {
  result <- shuffle_min_distance(
    original_data  = fake_survey_coordinates,
    synthetic_data = synthetic_sample,
    min_km         = 5
  )
  expect_identical(names(result), names(synthetic_sample))
})

test_that("shuffle_min_distance() preserves the CRS of the synthetic input", {
  result <- shuffle_min_distance(
    original_data  = fake_survey_coordinates,
    synthetic_data = synthetic_sample,
    min_km         = 5
  )
  expect_equal(sf::st_crs(result), sf::st_crs(synthetic_sample))
})

# ── Distance guarantee ─────────────────────────────────────────────────────────

test_that("shuffle_min_distance() enforces the minimum distance for all point pairs", {
  min_km <- 5
  result <- shuffle_min_distance(
    original_data  = fake_survey_coordinates,
    synthetic_data = synthetic_sample,
    min_km         = min_km
  )

  # Project both to a metre-based CRS for distance calculation
  orig_m   <- sf::st_transform(fake_survey_coordinates, crs = 3035)
  result_m <- sf::st_transform(result, crs = 3035)

  distances_m <- as.numeric(
    sf::st_distance(result_m, orig_m, by_element = TRUE)
  )
  expect_true(all(distances_m >= min_km * 1000))
})

test_that("shuffle_min_distance() with min_km = 0 still returns a valid sf object", {
  result <- shuffle_min_distance(
    original_data  = fake_survey_coordinates,
    synthetic_data = synthetic_sample,
    min_km         = 0
  )
  expect_s3_class(result, "sf")
})

test_that("shuffle_min_distance() with a large min_km still returns an sf object", {
  # 200 km is large but Germany's extent allows some valid assignments
  expect_no_error(
    shuffle_min_distance(
      original_data  = fake_survey_coordinates,
      synthetic_data = synthetic_sample,
      min_km         = 200
    )
  )
})

# ── Row permutation ────────────────────────────────────────────────────────────

test_that("shuffle_min_distance() changes the row order relative to the input", {
  result <- shuffle_min_distance(
    original_data  = fake_survey_coordinates,
    synthetic_data = synthetic_sample,
    min_km         = 100
  )
  # The shuffled coordinates should not be identical to the input
  expect_false(identical(
    sf::st_coordinates(result),
    sf::st_coordinates(synthetic_sample)
  ))
})

test_that("shuffle_min_distance() does not introduce new coordinate values", {
  result <- shuffle_min_distance(
    original_data  = fake_survey_coordinates,
    synthetic_data = synthetic_sample,
    min_km         = 5
  )
  # All coordinates in the result must have existed in the synthetic input
  coords_result <- sf::st_coordinates(result)
  coords_synth  <- sf::st_coordinates(synthetic_sample)
  expect_true(all(
    apply(coords_result, 1, function(r) any(apply(coords_synth, 1, function(s) all(r == s))))
  ))
})

# ── Input validation ───────────────────────────────────────────────────────────

test_that("shuffle_min_distance() errors on mismatched row counts", {
  expect_error(
    shuffle_min_distance(
      original_data  = fake_survey_coordinates[1:5, ],
      synthetic_data = synthetic_sample,  # different nrow
      min_km         = 5
    )
  )
})

test_that("shuffle_min_distance() errors on non-sf original_data", {
  expect_error(
    shuffle_min_distance(
      original_data  = as.data.frame(fake_survey_coordinates),
      synthetic_data = synthetic_sample,
      min_km         = 5
    )
  )
})

test_that("shuffle_min_distance() errors on non-sf synthetic_data", {
  expect_error(
    shuffle_min_distance(
      original_data  = fake_survey_coordinates,
      synthetic_data = as.data.frame(synthetic_sample),
      min_km         = 5
    )
  )
})

test_that("shuffle_min_distance() errors on negative min_km", {
  expect_error(
    shuffle_min_distance(
      original_data  = fake_survey_coordinates,
      synthetic_data = synthetic_sample,
      min_km         = -1
    )
  )
})
