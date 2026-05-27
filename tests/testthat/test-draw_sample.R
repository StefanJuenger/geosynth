# Tests for geosynth::draw_sample()

data("fake_survey_coordinates", package = "geosynth")

# Shared sample frame used across tests
sample_frame <-
  create_sample_frame(
    .data = fake_survey_coordinates,
    year = "2022",
    geo_unit = "regiostar17",
    inhabitants_threshold = 100000,
    minimum_sample_points = 10
  )

# Shared drawn sample used across many tests
drawn_sample <- draw_sample(sample_frame = sample_frame)

# ── Output structure ───────────────────────────────────────────────────────────

test_that("draw_sample() returns an sf object", {
  expect_s3_class(drawn_sample, "sf")
})

test_that("draw_sample() returns the correct number of rows", {
  expect_equal(nrow(drawn_sample), nrow(fake_survey_coordinates))
})

test_that("draw_sample() returns point geometries", {
  expect_true(all(sf::st_geometry_type(drawn_sample) == "POINT"))
})

test_that("draw_sample() returns a valid CRS", {
  expect_false(is.na(sf::st_crs(drawn_sample)))
})

# ── Message behaviour ───────────────────────────────────────────────────────
test_that("create_sample_frame(..., verbose = FALSE) is silent", {
  expect_no_message(
    draw_sample(sample_frame = sample_frame, verbose = FALSE)
  )
})

# ── Safety measures ───────────────────────────────────────────────────────────

test_that("draw_sample() does not pick flagged municipalities", {

  sample_frame_evil_cases <-
    sample_frame |>
    dplyr::filter(is.na(n_resp_geo_unit)) |>
    dplyr::pull(ags)

  drawn_sample_ags <- drawn_sample$ags

  expect_all_false(drawn_sample_ags %in% sample_frame_evil_cases)
})

# ── Spatial plausibility ───────────────────────────────────────────────────────

test_that("draw_sample() places all points within Germany's bounding box", {
  drawn_sample_wgs  <- sf::st_transform(drawn_sample, crs = 4326)
  coords <- sf::st_coordinates(drawn_sample_wgs)

  # Approximate bounding box for Germany
  expect_true(all(coords[, "X"] >= 5.8  & coords[, "X"] <= 15.1))
  expect_true(all(coords[, "Y"] >= 47.2 & coords[, "Y"] <= 55.1))
})

test_that("draw_sample() produces no missing coordinates", {
  expect_false(anyNA(sf::st_coordinates(drawn_sample)))
})

# ── Reproducibility ────────────────────────────────────────────────────────────

test_that("draw_sample() produces different results on repeated calls (stochastic)", {
  result_a <- draw_sample(sample_frame = sample_frame)
  result_b <- draw_sample(sample_frame = sample_frame)

  # Coordinates should differ between draws (sampling is random)
  expect_false(identical(
    sf::st_coordinates(result_a),
    sf::st_coordinates(result_b)
  ))
})

test_that("draw_sample() is reproducible with set.seed()", {
  set.seed(42)
  result_a <- draw_sample(sample_frame = sample_frame)
  set.seed(42)
  result_b <- draw_sample(sample_frame = sample_frame)
  expect_identical(
    sf::st_coordinates(result_a),
    sf::st_coordinates(result_b)
  )
})

# ── Input validation ───────────────────────────────────────────────────────────

test_that("draw_sample() errors on missing sample_frame", {
  expect_error(draw_sample())
})

test_that("draw_sample() errors on a non-sf sample_frame", {
  expect_error(draw_sample(sample_frame = data.frame(x = 1)))
})
